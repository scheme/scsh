;;; Code for processing Unix file names.
;;; Copyright (c) 1992 by Olin Shivers (shivers@lcs.mit.edu).
;;; See file COPYING

;;; We adhere to Posix file name rules, plus we treat files beginning with
;;; ~ as absolute paths.

;;; Relevant bits of CScheme:
;;;    pathnm sfile strnin unxcwd unxdir unxpar unxprm unxpth unxunp wrkdir

(define (file-name-directory? fname)
  (or (string=? fname "")			; Note! "" is directory (cwd)
      (char=? #\/ (string-ref fname (- (string-length fname) 1)))))

(define (file-name-non-directory? fname)
  (or (string=? fname "")			; and file-name (root).
      (not (char=? #\/ (string-ref fname (- (string-length fname) 1))))))

(define (file-name-as-directory fname)
  (if (string=? fname ".")
      ""
      (let ((len (string-length fname)))
	(if (and (> len 0)
		 (char=? #\/ (string-ref fname (- len 1))))
	    fname
	    (string-append fname "/")))))


;;; Return #f if str doesn't contain a slash at all.
(define (last-non-slash str)
  (let lp ((i (- (string-length str) 1)))
    (and (>= i 0)
	 (if (char=? #\/ (string-ref str i))
	     (lp (- i 1))
	     i))))

(define (directory-as-file-name fname)
  (let ((len (string-length fname)))
    (if (zero? len)
	"."		; "" -> "."
	;; Trim trailing slashes.
	(cond ((last-non-slash fname) =>
	       (lambda (i)
		 (if (= i (- len 1))
		     fname ; No slash.
		     (substring fname 0 (+ i 1))))) ; Trim slashes.

	      ;;; Solid slashes -- invoke weird Posix rule.
	      (else (if (= len 2) "//" "/"))))))


(define (ensure-file-name-is-directory fname)
  (if (string=? fname "")
      ""
      (file-name-as-directory fname)))


(define (ensure-file-name-is-nondirectory fname)
  (if (string=? fname "")
      ""
      (directory-as-file-name fname)))


(define (file-name-absolute? fname)
  (or (= (string-length fname) 0)
      (char=? #\/ (string-ref fname 0))
      (char=? #\~ (string-ref fname 0))))


;;; Returns FNAME's directory component in *directory form.*
(define (file-name-directory fname)
  (cond ((string-index-right fname #\/) =>
	 (lambda (rslash)
	   (if (last-non-slash fname)
	       (substring fname 0 (+ 1 rslash))
	       ""))) ; Posix strangeness: solid slashes are root.
	(else "")))


(define (file-name-nondirectory fname)
  (cond ((string-index-right fname #\/) =>
	 (lambda (rslash)
	   (if (last-non-slash fname)
	       (substring fname (+ 1 rslash) (string-length fname))
	       fname)))	; Posix strangeness: solid slashes are root.
	(else fname)))

    
(define (split-file-name fname)
  (let* ((fname (ensure-file-name-is-nondirectory fname))
	 (len (string-length fname)))
    (let split ((start 0))
      (cond ((>= start len) '())
	    ((string-index fname #\/ start) =>
	     (lambda (slash)
	       (cons (substring fname start slash)
		     (split (+ slash 1)))))
	    (else (list (substring fname start len)))))))


(define (path-list->file-name pathlist . maybe-dir)
  (let ((root (ensure-file-name-is-nondirectory (:optional maybe-dir ".")))
	;; Insert slashes *between* elts of PATHLIST.
	(w/slashes (if (pair? pathlist)
		       (let insert-slashes ((pathlist pathlist))
			 (let ((elt (car pathlist))
			       (pathlist (cdr pathlist)))
			   (cons elt (if (pair? pathlist)
					 (cons "/" (insert-slashes pathlist))
					 '()))))
		       '(""))))
    (apply string-append
	   (if (and (pair? pathlist)
		    (string=? "" (car pathlist)))
               (if (null? (cdr pathlist)) ; special case for pathlist = '("")
                   '("/")
                   w/slashes) ; Absolute path not relocated.
	       (cons (file-name-as-directory root) w/slashes)))))
		   

(define (parse-file-name fname)
  (let ((nd (file-name-nondirectory fname)))
    (values (file-name-directory fname)
	    (file-name-sans-extension nd)
	    (file-name-extension nd))))


;;; Return the index of the . separating the extension from the rest of
;;; the file name. If no extension, returns an index pointing off the
;;; end of the string, i.e. (string-length fname). "Dot-files," such as
;;; /usr/shivers/.login are not considered extensions.

(define (file-name-extension-index fname)
  (let ((dot (string-index-right fname #\.))
	(slash (string-index-right fname #\/)))
    (if (and dot
	     (> dot 0)
	     (if slash (> dot slash) #t)
	     (not (char=? #\/ (string-ref fname (- dot 1)))))
	dot
	(string-length fname))))

(define (file-name-sans-extension fname)
  (substring fname 0 (file-name-extension-index fname)))

(define (file-name-extension fname)
  (substring fname (file-name-extension-index fname)
	           (string-length fname)))

(define (replace-extension fname ext)
  (string-append (file-name-sans-extension fname) ext))


(define (resolve-tilde-file-name fname)
  (let ((len (string-length fname)))
    (if (and (> len 0) (char=? #\~ (string-ref fname 0)))
	(let ((tilde->homedir (lambda (end)
				(if (= end 1)
				    home-directory ; Just ~
				    (let* ((user (substring fname 1 end))
					   (ui (name->user-info user)))
				      (user-info:home-dir ui))))))
	  (cond ((string-index fname #\/ 1) =>
		 (lambda (slash)
		   (string-append (tilde->homedir slash) "/"
				  (substring fname (+ slash 1) len))))
		(else (tilde->homedir len))))
	fname)))

(define (resolve-file-name fname . maybe-root)
  (let* ((root (ensure-file-name-is-nondirectory (:optional maybe-root ".")))
	 (fname (ensure-file-name-is-nondirectory fname)))
    (if (zero? (string-length fname))
	"/"
	(let ((c (string-ref fname 0)))
	  (cond ((char=? #\/ c) fname) 	; Absolute file name.

		((char=? #\~ c) 	; ~ file name
		 (resolve-tilde-file-name fname))

		(else (string-append (file-name-as-directory root) fname)))))))


;;; - Remove leading and internal occurrences of dot. A trailing dot
;;;   is left alone, in case the parent is a symlink.
;;; - Remove internal and trailing double-slashes. A leading double-slash
;;;   is left alone, in accordance w/Posix. However, triple and more leading
;;;   slashes are reduced to a single slash, in accordance w/Posix.
;;; - Double-dots are left alone, in case they come after symlinks.

(define (simplify-file-name fname)
  ;; First, we simplify leading multiple slashes:
  ;; 1 or >2 slashes -> /, 2 slashes -> //
  (receive (slashes fname)
	   (let ((len (string-length fname)))
	     (if (and (> len 0) (char=? #\/ (string-ref fname 0)))
		 (let ((j (let lp ((i 1)) ; j is index of first non-slash.
			    (if (and (< i len)
				     (char=? (string-ref fname i) #\/))
				(lp (+ i 1))
				i))))
		   (if (< j 3)
		       (values (substring fname 0 j); One or two slashes - OK.
			       (substring fname j len))
		       (values "/" (substring fname (- j 1) len))))
		 (values "" fname)))

    ;; At this point, all leading slashes have been pulled off of FNAME.
    ;; Any remaining repeated slashes are fair game for removal.
    (let* ((path-list (split-file-name fname))
	   (ans (if (pair? path-list)
		    (reverse (let lp ((path-list path-list)
				      (ans (list slashes)))
			       (let ((elt (car path-list))
				     (path-list (cdr path-list)))
				 (if (pair? path-list)
				     (lp path-list
					 (if (or (string=? "." elt) ; kill .
						 (string=? "" elt)) ; and //
					     ans
					     `("/" ,elt ,@ans)))
				     (cons elt ans)))))
		    (list slashes))))
      (apply string-append ans))))


(define (expand-file-name fname . maybe-dir)
  (simplify-file-name (apply resolve-file-name fname maybe-dir)))


(define (absolute-file-name fname . maybe-root)
  (let ((fname (ensure-file-name-is-nondirectory fname)))
    (if (zero? (string-length fname))
	"/"
	(simplify-file-name
	  (if (char=? #\/ (string-ref fname 0))
	      fname 	; Absolute file name.
	      (let ((root (:optional maybe-root (cwd))))
		(string-append (file-name-as-directory root) fname)))))))


(define (home-dir . maybe-user)
  (if (pair? maybe-user)
      (let ((user (car maybe-user)))
	(ensure-file-name-is-nondirectory
	    (or (%homedir user)
		(error "Cannot get user's home directory"
		       user))))
      home-directory))


;;; (home-file [user] fname)

(define (home-file arg1 . maybe-arg2)
  (receive (dir fname)
	   (if (pair? maybe-arg2)
	       (values (home-dir arg1) (car maybe-arg2))
	       (values home-directory  arg1))
    (string-append (file-name-as-directory dir) fname)))


;;; Ugh.
(define (substitute-env-vars str)
  (let lp ((ans '()) (s str))
    (let ((len (string-length s)))
      (cond
        ((zero? len) (apply string-append (reverse! ans)))
	((string-index s #\$) =>
	 (lambda (i)
	   (let ((ans (cons (substring s 0 i) ans))
		 (s (substring s (+ i 1) len))
		 (len (- len (+ i 1))))
	     (if (zero? len)
		 (lp ans "")
		 (let ((next-char (string-ref s 0)))
		   (cond ((char=? #\{ next-char)
			  (cond ((string-index s #\}) =>
				 (lambda (i)
				   (lp (cons (getenv (substring s 1 i)) ans)
				       (substring s (+ i 1) len))))
				(else (error "Unbalanced ${ delimiter in string" s))))
			 (else
			  (let ((i (or (string-index s #\/) len)))
			    (lp (cons (getenv (substring s 0 i)) ans)
				(substring s i len))))))))))
	(else (lp (cons s ans) ""))))))
