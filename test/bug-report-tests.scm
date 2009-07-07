;;; Test for the bug reports from user- and hacker-archives
;;; Author: Christoph Hetz

;; for  testing: (certainly the path will be an other on other systems...)

;; ,open define-record-types handle
;; ,config ,load C:/cygwin/home/mephisto/cvs-scsh/scsh/scsh/test/test-packages.scm
;; ,load C:/cygwin/home/mephisto/cvs-scsh/scsh/scsh/test/test-base.scm
;; load this file
;; (test-all)

(add-test! '2002-05-86-regexp-weirdness 'archive-users
  (lambda ()
    (let ((x (rx "{ OK=#t }")))
      (and (not (regexp-search? x "foo"))
	   (regexp-search? x "...{ OK=#t }...")))))

(add-test! '2003-12-74-select-ports-with-zero-timeout 'archive-users
  (lambda ()
    (select-ports 0 (current-input-port))))