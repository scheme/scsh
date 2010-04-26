;;; Directory stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (directory-files . args)
  (with-resources-aligned
   (list cwd-resource euid-resource egid-resource)
   (lambda ()
     (let-optionals args ((dir       ".")
                          (dotfiles? #f))
       (check-arg string? dir directory-files)
       (let* ((files (map (lambda (file) (os-string->string file))
                          (list-directory (ensure-file-name-is-nondirectory dir))))
              (files-sorted (sort-list! files filename<=)))
         (if dotfiles? files-sorted
             (filter (lambda (f) (not (dotfile? f)))
                     files-sorted)))))))

(define (dotfile? f)
  (char=? (string-ref f 0) #\.))

(define (filename<= f1 f2)
  (if (dotfile? f1)
      (if (dotfile? f2)
          (string<= f1 f2)
          #t)
      (if (dotfile? f2)
          #f
          (string<= f1 f2))))

(define (read-directory-stream directory)
  (let ((dir-stream (s48-read-directory-stream directory)))
    (if dir-stream
        (os-string->string dir-stream)
        dir-stream)))
