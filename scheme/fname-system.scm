;;; User / Group dependent stuff

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

(define (expand-file-name fname . maybe-dir)
  (simplify-file-name (apply resolve-file-name fname maybe-dir)))

;;; Process state dependent stuff

(define (absolute-file-name fname . maybe-root)
  (let ((fname (ensure-file-name-is-nondirectory fname)))
    (if (zero? (string-length fname))
	"/"
	(simplify-file-name
	  (if (char=? #\/ (string-ref fname 0))
	      fname 	; Absolute file name.
	      (let ((root (:optional maybe-root (cwd))))
		(string-append (file-name-as-directory root) fname)))))))

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
