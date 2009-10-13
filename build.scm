#! /usr/bin/scsh -s
!#

(let ((cfiles (list "scsh/syscalls" "scsh/cstuff" "scsh/scsh"
                    "scsh/sighandlers"))
      (cflags '(-c -g -O2))
      (ldflags '(-shared -rdynamic))
      (cc 'gcc)
      (ld 'gcc))
  (if (equal? (argv 1 #f) "clean")
      (begin (for-each (lambda (file)
                         (run (rm ,(string-append file ".o"))))
                       cfiles)
             (run (rm "scsh/scsh.so")))
      (begin
        (for-each (lambda (file)
                    (run (,cc ,@cflags -o ,(string-append file ".o")
                              ,(string-append file ".c"))))
                  cfiles)
        (run (,ld ,@ldflags -o "scsh/scsh.so"
                  ,@(map (lambda (x) (string-append x ".o")) cfiles))))))
