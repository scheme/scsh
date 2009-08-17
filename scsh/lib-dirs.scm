;;; More imports for the new library-search facility:
;;;     HANDLE: with-handler
;;;     LIST-LIB: any
;;;     SCSH-LEVEL-0: directory-files open-input-file file-directory?
;;;     SCSH-LEVEL-0: getenv
;;;     SCSH-LEVEL-0: the file-name procs

(define default-lib-dirs '("/usr/local/lib/scsh/modules/"))

(define (set-default-lib-dirs! path-list)
  (set! default-lib-dirs path-list))

;;; Search library dirs for FILE.
(define (find-library-file file lib-dirs script-file)
  (letrec ((recur (lambda (dir)
;		    (format (error-output-port) "flf -- entering ~a\n" dir)
		    (let* ((f (string-append dir file)))	; Resolve it.
		      (or (check-file-for-open f)		; Found it.
			  (any (lambda (f)			; Search subdirs.
				 (let ((dir (string-append dir f "/")))
				   (and (file-directory?/safe dir) (recur dir))))
			       (directory-files/safe dir)))))))
    (any (lambda (dir)
	   (cond ((eq? dir 'script-dir)
		  (let* ((script-dir (file-name-directory script-file))
			 (fname (string-append script-dir file)))
		    (check-file-for-open fname)))

		 ;; Ends in / means recursive search.
		 ((file-name-directory? dir)
		  (recur dir))

		 (else (check-file-for-open (absolute-file-name file dir)))))
	 lib-dirs)))
			    

;;; (in-any-event abort-exp body ...)
;;; If *anything* goes wrong, bag the BODY forms, and eval ABORT-EXP instead.

(define-syntax in-any-event
  (syntax-rules ()
    ((in-any-event abort-exp body ...)
     (call-with-current-continuation
      (lambda (ret)
	(with-handler (lambda (condition more) (ret abort-exp))
		      (lambda () body ...)))))))

(define (check-file-for-open f)
  (in-any-event #f (let ((iport (open-input-file f)))
		     (close-input-port iport)
		     f)))                      ; Any error, say false.

(define (directory-files/safe dir)
  (in-any-event '() (directory-files dir)))	; Any error, say ().

(define (file-directory?/safe f)
  (in-any-event #f (file-directory? f)))	; Any error, say false.

(define (resolve-dir-name dir)
  (if (file-name-directory? dir)
      (file-name-as-directory (resolve-file-name dir))
      (resolve-file-name dir)))

;;; Expand out env vars & ~user home dir prefixes.
(define (expand-lib-dir dir)
  (substitute-env-vars (resolve-dir-name dir)))

;;; Parse up the $SCSH_LIB_DIRS path list.
(define (parse-lib-dirs-env-var)
   (let ((s (getenv "SCSH_LIB_DIRS")))
     (if (not s)
         default-lib-dirs
         (with-current-input-port (make-string-input-port s)
            (let recur ()
              (let ((val (read)))
                (cond ((eof-object? val) '())
                      ((string? val) (cons val (recur)))
                      ((not val) (append default-lib-dirs (recur)))
                      (else 
                       (error 
                        (format #f
                                (string-append
                                 "Illegal path element in $SCSH_LIB_DIRS\n"
                                 "$SCSH_LIB_DIRS: ~a\n"
                                 "The following element is not a string or #f: ~a")
                                s val))))))))))

;; We don't want to try to parse $SCSH_LIB_DIRS until we actually
;; need the value -- if the user is using the -lp-default switch,
;; for example, a parse error shouldn't effect the startup.
(define %lib-dirs #f)
(define reinit-lib-dirs 
  (make-reinitializer (lambda () (set! %lib-dirs #f))))

(define (lib-dirs)
  (if (not %lib-dirs) (set! %lib-dirs (parse-lib-dirs-env-var)))
  %lib-dirs)

;; Don't export -- direct modification of %lib-dirs 
(define (set-lib-dirs! val) (set! %lib-dirs val))

(define (lib-dirs-append-script-dir!)
  (set-lib-dirs! (append (lib-dirs) '(script-dir))))

(define (lib-dirs-prepend-script-dir!)
  (set-lib-dirs! (cons 'script-dir (lib-dirs))))

(define (reset-lib-dirs!)
  (set-lib-dirs! default-lib-dirs))

(define (clear-lib-dirs!)
  (set-lib-dirs! '()))

(define (lib-dirs-prepend! dir)
  (set-lib-dirs! (cons dir (lib-dirs))))

(define (lib-dirs-append! dir)
  (set-lib-dirs! (append (lib-dirs) (list dir))))
