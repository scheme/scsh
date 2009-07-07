;;; Code for processing file names with a glob pattern.

;;; Copyright (c) 1994 by David Albertz (dalbertz@clark.lcs.mit.edu).
;;; Copyright (c) 1994 by Olin Shivers   (shivers@clark.lcs.mit.edu).
;;; See file COPYING.

;;; Usage:	(glob pattern-list)
;;;                 pattern-list := a list of glob-pattern strings

;;; Return:	list of file names (strings)
;;;             The files "." and ".." are never returned by glob.
;;;             Dot files will only be returned if the first character
;;;             of a glob pattern is a ".".

;;; The empty pattern matches nothing.
;;; A pattern beginning with / starts at root; otherwise, we start at cwd.
;;; A pattern ending with / matches only directories, e.g., "/usr/man/man?/"

(define (glob . pattern-list)
  ;; Expand out braces, and apply GLOB-ONE-PATTERN to all the result patterns.
  (apply append
	 (map glob-one-pattern
	      (apply append (map glob-remove-braces pattern-list)))))


(define (glob-one-pattern pattern)
  (let ((plen (string-length pattern)))
    (if (zero? plen) '()
	(let ((directories-only? (char=? #\/ (string-ref pattern (- plen 1))))
	      (patterns (split-file-name pattern))) ; Must be non-null.
	  (if (equal? "" (car patterns))
	      (really-glob ""   (cdr patterns) directories-only?)	; root
	      (really-glob "."  patterns       directories-only?))))))	; cwd


(define (really-glob root-file patterns directories-only?)
  ;; This is the heart of the matcher.
  (let recur ((file root-file)
	      (pats patterns)
	      (sure? #f))	; True if we are sure this file exists.
    (if (pair? pats)
	(let ((pat (car pats))
	      (pats (cdr pats))
	      (dir (file-name-as-directory file)))
	  (receive (winners sure?) (glob-subpat dir pat)
	    (apply append (map (lambda (f)
				 (recur (string-append dir f) pats sure?))
			       winners))))

	;; All done.
	(if directories-only?
	    (if (maybe-isdir? file)
		(list (file-name-as-directory file))
		'())
	    (if (or sure? (file-exists? file))
		(list file)
		'())))))


;;; Return the elts of directory FNAME that match pattern PAT.
;;; If PAT contains no wildcards, we cheat and do not match the
;;; constant pattern against every file in FNAME/; we just 
;;; immediately return FNAME/PAT. In this case, we indicate that we 
;;; aren't actually sure the file exists by returning a true SURE?
;;; value. Not only does this vastly speed up the matcher, it also
;;; allows us to match the constant patterns "." and "..".

(define (glob-subpat fname pat) ; PAT doesn't contain a slash.
  (cond ((string=? pat "") (values '() #t))

	((constant-glob? pat)
	 (values (cons (glob-unquote pat) '()) #f)) ; Don't check filesys.
	
	(else (let* ((dots? (char=? #\. (string-ref pat 0))) ; Match dot files?
		     (candidates (maybe-directory-files fname dots?))
		     (re (glob->regexp pat)))
		(values (filter (lambda (f) (regexp-search? re f)) candidates)
			#t))))) ; These guys exist for sure.

;;; The initial special-case above isn't really for the fast-path; it's
;;; an obscure and unlikely case. But since we have to check pat[0] for an 
;;; initial dot, we have to do the check anyway...


;;; Translate a brace-free glob pattern to a regular expression.

(define glob->regexp
  (let ((dot-star (re-repeat 0 #f re-any))) ; ".*" or (* any)
    (lambda (pat)
      (let ((pat-len (string-length pat))

	    (str-cons (lambda (chars res)	; Reverse CHARS and cons the
			(if (pair? chars)	; result string-re onto RES.
			    (cons (re-string (list->string (reverse chars)))
				  res)
			    res))))

	;; We accumulate chars into CHARS, and coalesce into a single string
	;; with STR-CONS when we run across a non-char.
	(let lp ((chars '())
		 (res (list re-bos))
		 (i 0))
	  (if (= i pat-len)
	      (re-seq (reverse (cons re-eos (str-cons chars res))))

	      (let ((c (string-ref pat i))
		    (i (+ i 1)))
		(case c
		  ((#\\) (if (< i pat-len)
			     (lp (cons (string-ref pat i) chars)
				 res (+ i 1))
			     (error "Ill-formed glob pattern -- ends in backslash" pat)))

		  ((#\*) (lp '()
			     (cons dot-star (str-cons chars res))
			     i))
		  ((#\?) (lp '()
			     (cons re-any (str-cons chars res))
			     i))
				
		  ((#\[) (receive (re i) (parse-glob-bracket pat i)
			   (lp '()
			       (cons re (str-cons chars res))
			       i)))

		  (else  (lp (cons c chars) res i))))))))))


;;; A glob bracket expression is [...] or [^...].
;;; The body is a sequence of <char> and <char>-<char> ranges.
;;; A <char> is any character except right-bracket, carat, hypen or backslash,
;;; or a backslash followed by any character at all.

(define (parse-glob-bracket pat i)
  (let ((pat-len (string-length pat)))
    (receive (negate? i) (if (and (< i pat-len) (char=? #\^ (string-ref pat i)))
			     (values #t (+ i 1))
			     (values #f i))

      (let lp ((elts '()) (i i))
	(if (>= i pat-len)
	    (error "Ill-formed glob pattern -- no terminating close-bracket" pat)

	    (let ((c (string-ref pat i))
		  (i (+ i 1)))
	      (case c
		((#\])
		 (let ((cset (fold (lambda (elt cset)
				     (if (char? elt)
					 (char-set-adjoin! cset elt)
					 (ucs-range->char-set! (char->ascii (car elt))
							       (+ 1 (char->ascii (cdr elt)))
							       #f cset)))
				   (char-set-copy char-set:empty)
				   elts)))
		   (values (re-char-set (if negate?
					    (char-set-complement! cset)
					    cset))
			   i)))

		((#\\)
		 (if (>= i pat-len)
		     (error "Ill-formed glob pattern -- ends in backslash" pat)
		     (lp (cons (string-ref pat i) elts) (+ i 1))))

		((#\-)
		 (cond ((>= i pat-len)
			(error "Ill-formed glob pattern -- unterminated range." pat))
		       ((or (null? elts) (not (char? (car elts))))
			(error "Ill-formed glob pattern -- range has no beginning." pat))
		       (else (lp (cons (cons (car elts) (string-ref pat i)) elts)
				 (+ i 1)))))

		(else (lp (cons c elts) i)))))))))


;;; Is the glob pattern free of *'s, ?'s and [...]'s?
(define (constant-glob? pattern)
  (let ((patlen (string-length pattern)))
    (let lp ((i 0))
      (or (= i patlen)
	  (let ((next-i (+ i 1)))
	    (case (string-ref pattern i)
	      ((#\\) ; Escape char
	       (if (= next-i patlen)
		   (error "Ill-formed glob pattern -- ends in backslash"
			  pattern)
		   (lp (+ next-i 1))))
	      ((#\* #\? #\[) #f)
	      (else  (lp next-i))))))))


;;; Make an effort to get the files in the putative directory PATH.
;;; If PATH isn't a directory, or some filesys error happens (such
;;; as a broken symlink, or a permissions problem), don't error out,
;;; just quietly return the empty list.

(define (maybe-directory-files path dotfiles?)
  (with-errno-handler ((errno data)
		       (else '()))	; On any error, return ().
    (directory-files (if (string=? path "") "." path) dotfiles?)))

;;; Make an effort to find out if the file is a directory. If there's
;;; any error, return #f.

(define (maybe-isdir? path)
  (with-errno-handler ((errno data)
		       (else #f))	; On any error, return #f.
    (file-directory? path)))



;;; This section of code is responsible for processing the braces in glob
;;; patterns. I.e., "{foo,bar}/*.c" -> ("foo/*.c" "bar/*.c")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (append-suffix strs suffix)
  (map (lambda (s) (string-append s suffix)) strs))

(define (cross-append prefixes suffixes)
  (apply append (map (lambda (sfx) (append-suffix prefixes sfx)) suffixes)))

;;; Parse a glob pattern into an equivalent series of brace-free patterns.
;;; The pattern starts at START and is terminated by (1) end of string,
;;; (2) an unmatched close brace, or (3) a comma (if COMMA-TERMINATES? is set).
;;; Returns two values:
;;; - the list of patterns
;;; - the string index after the pattern terminates. This points at
;;;   the comma or brace if they terminated the scan, since they are
;;;   not part of the pattern.

(define (parse-glob-braces pattern start comma-terminates?)
  (let ((pattern-len (string-length pattern))
	(finish (lambda (prefixes pat)
		  (append-suffix prefixes (list->string (reverse pat))))))

    (let lp ((i start)
	     (prefixes '(""))
	     (pat '()))
      (if (= i pattern-len)
	  (values (finish prefixes pat) i)
	  
	  (let ((c (string-ref pattern i)))
	    (case c
	      ((#\{)
	       (let ((prefixes (append-suffix prefixes 
					      (list->string (reverse pat)))))
		 (receive (pats i)
		     (parse-comma-sequence pattern (+ i 1))
		   (lp i (cross-append prefixes pats) '()))))
	      ((#\\)
	       (let ((next-i (+ i 1)))
		 (if (= next-i pattern-len)
		     (error "Dangling escape char in glob pattern" pattern)
                     (if (memv (string-ref pattern next-i) '(#\{, #\,, #\},#\\))
                         (lp (+ next-i 1)
                             prefixes
                             (cons (string-ref pattern next-i) pat))
                         (lp (+ i 1)
                             prefixes
                             (cons (string-ref pattern i) pat))))))
	      ((#\,)
	       (if comma-terminates?
		   (values (finish prefixes pat) i)
		   (lp (+ i 1) prefixes (cons c pat))))

	      ((#\})
	       (values (finish prefixes pat) i))

	      (else
	       (lp (+ i 1) prefixes (cons c pat)))))))))


;;; Parse the internals of a {foo,bar,baz} brace list from a glob pattern.
;;; START is the index of the char following the open brace.
;;; Returns two values:
;;; - an equivalent list of brace-free glob patterns
;;; - the index of the char after the terminating brace

(define (parse-comma-sequence pattern start)
  (let ((pattern-len (string-length pattern)))
    (let lp ((i start)
	     (patterns '()))	; The list of comma-separated patterns read.

      (if (= i pattern-len)
	  (error "Glob brace-expression pattern not terminated" pattern)
	  (receive (pats i) (parse-glob-braces pattern i #t)
	    (let ((patterns (append patterns pats)))
	      (if (= i pattern-len)
		  (error "Unterminated brace in glob pattern" pattern)
		  (let ((c (string-ref pattern i)))
		    (case c
		      ((#\})
		       (values patterns (+ i 1)))
		      ((#\,)
		       (lp (+ i 1) patterns))
		      (else
		       (error "glob parser internal error" pattern i)))))))))))

(define (glob-remove-braces pattern)
  (receive (pats i) (parse-glob-braces pattern 0 #f)
    (if (= i (string-length pattern)) pats
	(error "Unmatched close brace in glob pattern" pattern i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Convert a string into a glob pattern that matches that string exactly --
;;; in other words, quote the \ * ? [] and {} chars with backslashes.
(define (glob-quote string)
  (let lp ((i (- (string-length string) 1))
	   (result '()))
    (if (< i 0) (list->string result)
	(lp (- i 1)
	    (let* ((c (string-ref string i))
		   (result (cons c result)))
	      (if (memv c '(#\[ #\] #\* #\? #\{ #\} #\\))
		  (cons #\\ result)
		  result))))))

(define (glob-unquote string)
  (let ((len (string-length string)))
    (let lp ((i 0)
             (result '()))
      (if (= i len)
          (list->string (reverse result))
          (let* ((c (string-ref string i)))
            (if (char=? c #\\)
                (let ((next-i (+ i 1)))
                  (if (= next-i len)
                      (error "Dangling escape char in glob pattern" string)
                      (let ((quoted (string-ref string next-i)))
                        (lp (+ i 2)
                            (cons quoted result)))))
                (lp (+ i 1)
                    (cons c result))))))))


