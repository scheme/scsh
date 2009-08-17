;;; Here documents in Scheme for scsh scripts.
;;; These are like "here documents" for sh and csh shell scripts
;;; (i.e., the <<EOF redirection).
;;; Copyright (c) 1995 by Olin Shivers.

;;; There are two kinds of here string, both introduced by the #< read macro.
;;; 
;;; Character-delimited here strings:
;;; A *character-delimited* here string has the form 
;;;     #<x...stuff...x
;;; where x is any single character (except <, see below), which is used
;;; to delimit the string bounds.
;;; Here are some examples:
;;; 
;;;     Here string syntax		Ordinary syntax
;;;     ------------------		---------------
;;;     #<|Hello, world.|		"Hello, world."
;;;     #<!"Ouch," he said.!		"\"Ouch,\" he said."
;;; 
;;; There is *no* interpretation of characters within the here string;
;;; characters are all copied verbatim.
;;; 
;;; Line-delimited here strings:
;;; If the macro begins "#<<" then it introduces a *line-delimited* here
;;; string.  These are similar to the "here documents" of sh and
;;; csh. Line-delimited here strings are delimited by the line of text coming
;;; after the "#<<" sequence. For example:
;;; 
;;; #<FOO
;;; Hello, there.
;;; This is read by Scheme as a string,
;;; terminated by the first occurrence
;;; of newline-F-O-O-newline or newline-F-O-O-eof.
;;; FOO
;;; 
;;; Thus, 
;;;     #<foo
;;;     Hello, world.
;;;     foo
;;; is the same thing as
;;;     "Hello, world."
;;; 
;;; These are useful for writing down long, constant strings -- such
;;; as long, multi-line FORMAT strings, or arguments to Unix programs, e.g.
;;; 	;; Free up some disk space for my netnews files.
;;; 	(run (csh -c #<EOF
;;; 	cd /urops
;;; 	rm -rf *
;;; 	echo All done.
;;;
;;; 	EOF
;;;     ))
;;;
;;; The advantage they have over the double-quote syntax -- "Hello, world." --
;;; is that there is no need to backslash-quote special characters internal
;;; to the string, such as the double-quote or backslash characters.

;;; Line-delimited here-string syntax: 
;;; The characters "#<<" introduce the here-string.  The characters
;;; between the "#<<" and the next newline are the *delimiter line*. *All* chars
;;; between the "#<<" and the next newline comprise the delimiter line --
;;; including any white space. The newline char separates the delimiter line
;;; from the body of the string. The string body is terminated by a line of
;;; text which exactly matches the delimiter line.  This terminating line can
;;; be ended by either a newline or end-of-file.  Absolutely *no*
;;; interpretation is done on the input string.  Control chars, white space,
;;; quotes, backslash chars -- everything is copied as-is.  The newline
;;; immediately preceding the terminating delimiter line is *not* included in
;;; the result string (leave an extra blank line if you need to put a final
;;; newline in the here string -- see the example above).  If EOF is
;;; encountered before reading the end of the here string, an error is
;;; signalled.

(define (read-here-string port)
  (make-immutable!
   (let ((delim-char (read-char port)))
     (cond ((eof-object? delim-char)
	    (reading-error port "EOF while reading #< here-string delimiter."))

	   ((char=? delim-char #\<)	; It's a #<<EOF long here-string.
	    (read-line-delimited-here-string port))

	   ;; It's short: #<|Here's the string.|
	   (else (receive (str terminator)
		          (read-delimited (char-set delim-char) port 'split)
		   (if (eof-object? terminator)
		       (reading-error port "EOF while reading #< here-string.")
		       str)))))))


(define (read-line-delimited-here-string port)
  ;; First, read in the delimiter line.
  (let ((delim (read-line port)))
    (cond ((eof-object? delim)
	   (reading-error port
			  "EOF while reading #<< here-string delimiter line."))
 	  ((= 0 (string-length delim))
 	   (reading-error port "#<< here-string empty delimiter line"))

	  (else
	   (let lp ((text '()))
	     (receive (line terminator) (read-line port 'split)
	       (cond ((equal? line delim)
		      ;; We're done. The last newline must be stripped
		      ;; off (with the CDR application).
		      (if (pair? text)
			  (apply string-append (reverse (cdr text)))
			  ""))

		     ((eof-object? terminator)
		      (reading-error port "EOF while reading #<< here-string."))

		     (else (lp `(,(string terminator) ,line . ,text))))))))))


(define-sharp-macro #\<
  (lambda (c port)
    (read-char port)		; Snarf the first < char.
    (read-here-string port)))
