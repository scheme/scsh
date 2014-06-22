(define (call/temp-file writer user)
  (let ((fname #f))
    (dynamic-wind
      (lambda () (if fname (error "Can't wind back into a CALL/TEMP-FILE")
                     (set! fname (create-temp-file))))
      (lambda ()
        (with-output-to-file fname writer)
        (user fname))
      (lambda () (if fname (delete-file fname))))))

;;; Create a new temporary file and return its name.
;;; The optional argument specifies the filename prefix to use, and defaults
;;; to "/tmp/<pid>.", where <pid> is the current process' id. The procedure
;;; scans through the files named <prefix>0, <prefix>1, ... until it finds a
;;; filename that doesn't exist in the filesystem. It creates the file with
;;; permission #o600, and returns the filename.
;;;

(define (create-temp-file . maybe-prefix)
  (let ((oflags (file-options write-only
                              create
                              exclusive)))
    (apply temp-file-iterate
           (lambda (fname)
             (close-output-port (open-file fname oflags (file-mode owner-read owner-write)))
             fname)
           (if (null? maybe-prefix) '()
               (list (string-append (constant-format-string (car maybe-prefix))
                                    ".~a"))))))

(define (initial-temp-file)
  (let ((tmpdir (getenv "TMPDIR")))
    (string-append
     (if tmpdir
         tmpdir
         "/var/tmp")
     "/"
     (number->string (pid))
     "~a")))

(define *temp-file-template* (make-fluid (initial-temp-file)))

(define temp-file-reinitializer
  (make-reinitializer
   (lambda ()
     (set-fluid! *temp-file-template* (initial-temp-file)))))

(define (temp-file-iterate maker . maybe-template)
  (let ((template (:optional maybe-template (fluid *temp-file-template*))))
    (let loop ((i 0))
      (if (> i 1000) (error "Can't create temp-file")
          (let ((fname (format #f template (number->string i))))
            (receive retvals (with-errno-handler
                               ((errno data)
                                ((exist acces) #f))
                               (maker fname))
              (if (car retvals) (apply values retvals)
                  (loop (+ i 1)))))))))


;; Double tildes in S.
;; Using the return value as a format string will output exactly S.
(define (constant-format-string s)      ; Ugly code. Would be much clearer
  (let* ((len (string-length s))        ; if written with string SRFI.
         (tilde? (lambda (s i) (char=? #\~ (string-ref s i))))
         (newlen (do ((i (- len 1) (- i 1))
                      (ans 0 (+ ans (if (tilde? s i) 2 1))))
                     ((< i 0) ans)))
         (fs (make-string newlen)))
    (let lp ((i 0) (j 0))
      (cond ((< i len)
             (let ((j (cond ((tilde? s i) (string-set! fs j #\~) (+ j 1))
                            (else j))))
               (string-set! fs j (string-ref s i))
               (lp (+ i 1) (+ j 1))))))
    fs))


;;; Roughly equivalent to (pipe).
;;; Returns two file ports [iport oport] open on a temp file.
;;; Use this when you may have to buffer large quantities between
;;; writing and reading. Note that if the consumer gets ahead of the
;;; producer, it won't hang waiting for input, it will just return
;;; EOF. To play it safe, make sure that the producer runs to completion
;;; before starting the consumer.
;;;
;;; The temp file is deleted before TEMP-FILE-CHANNEL returns, so as soon
;;; as the ports are closed, the file's disk storage is reclaimed.

(define (temp-file-channel)
  (let* ((fname (create-temp-file))
         (iport (open-input-file fname))
         (oport (open-output-file fname)))
    (delete-file fname)
    (values iport oport)))
