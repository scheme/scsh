;;; -*- mode: scheme48; scheme48-package: (exec) -*-

(config)

(load "scsh/scsh-read.scm")
(open 'scsh-reader)
(set-reader 'scsh-read)
(load "rx/interfaces.scm"
      "rx/packages.scm"
      "scsh/scsh-interfaces.scm"
      "scsh/scsh-package.scm"
      "scsh/machine/packages.scm"
      "scsh/lib/char-package.scm"
      "scsh/lib/ccp-pack.scm"
      "test/test-packages.scm")

(user)
(open 'scsh-reader)
(set-reader 'scsh-read)

(open 'filenames)
;;; Set this to wherever the scsh directory is...
(run '(set-global-translation! "=scshexternal/" "/media/nordostlich/scsh/scsh/"))

;;; This is to avoid scheme48's deadlock detection.
(open 'threads)
(run '(spawn (lambda ()
               (sleep (* 1000 60 60 24 365)))))
