;;; -*- mode: scheme48; scheme48-package: (exec) -*-

(config)

(load "scsh-read.scm")
(open 'scsh-reader)
(set-reader 'scsh-read)
(load "scsh-interfaces.scm"
      "scsh-package.scm"
      "machine/packages.scm"
      "let-opt.scm"
      "rx/packages.scm"
      "lib/char-package.scm"
      "lib/ccp-pack.scm")

(user)
