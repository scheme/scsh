;;; Test for the functions in section 3.6 of the scsh-manual "user and group database access"
;;; Author: Christoph Hetz

;; for  testing: (certainly the path will be an other on other systems...)

;; ,open define-record-types handle
;; ,config ,load C:/cygwin/home/mephisto/cvs_scsh/scsh/scsh/test/test-packages.scm
;; ,load C:/cygwin/home/mephisto/cvs_scsh/scsh/scsh/test/test-base.scm
;; load this file
;; (test-all)

;; *** tests ***

(add-test! 'user-info 'user-and-group-db-access
  (lambda ()
    (let* ((user-0 (user-info (getenv "USER")))
	   (user-name (user-info:name user-0))
	   (user-id (user-info:uid user-0))
	   (user-gid (user-info:gid user-0))
	   (user-hdir (user-info:home-dir user-0))
	   (user-shell (user-info:shell user-0))
	   (group-0 (group-info user-gid))
	   (group-name (group-info:name group-0))
	   (group-id (group-info:gid group-0))
	   (group-mem (group-info:members group-0)))
      (and (string? user-name)
	   (integer? user-id)
	   (integer? user-gid)
	   (string? user-hdir)
	   (string? user-shell)
	   (string? group-name)
	   (integer? group-id)
	   (list? group-mem)
	   (equal? user-name (user-info:name (user-info user-id)))
	   (equal? (user-info:name (user-info (getenv "USER"))) user-name)
	   (equal? group-id (group-info:gid (group-info group-name)))))))