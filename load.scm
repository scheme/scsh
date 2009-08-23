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
      "scsh/lib/ccp-pack.scm")

(user)
