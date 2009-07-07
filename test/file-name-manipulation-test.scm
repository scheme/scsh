;;; Test for function in section 5.1 of the scsh-manual "file-name-... , diretory-... , ..."
;;; Author: Christoph Hetz

;; for now just the examples from the manual will be tested

;; for  testing: (certainly the path will be an other on other systems...)

;; ,open define-record-types handle
;; ,config ,load C:/cygwin/home/mephisto/cvs-scsh/scsh/scsh/test/test-packages.scm
;; ,load C:/cygwin/home/mephisto/cvs-scsh/scsh/scsh/test/test-base.scm
;; load this file
;; (test-all)

;; *** tests ***

(add-test! 'file-name-directory? 'file-name-manipulation
  (lambda ()
    (and (not (file-name-directory? "src/des"))
	 (file-name-directory? "src/des/")
	 (file-name-directory? "/")
	 (not (file-name-directory? "."))
	 (file-name-directory? ""))))

(add-test! 'file-name-non-directory? 'file-name-manipulation
  (lambda ()
    (and (file-name-non-directory? "src/des")
	 (not (file-name-non-directory? "src/des/"))
	 (not (file-name-non-directory? "/"))
	 (file-name-non-directory? ".")
	 (file-name-non-directory? ""))))

(add-test! 'file-name-as-directory 'file-name-manipulation
  (lambda ()
    (and (equal? "src/des/"
		 (file-name-as-directory "src/des"))
	 (equal? "src/des/"
		 (file-name-as-directory "src/des/"))
	 (equal? ""
		 (file-name-as-directory "."))
	 (equal? "/"
		 (file-name-as-directory "/"))
	 (equal? "/"
		 (file-name-as-directory "")))))

(add-test! 'directory-as-file-name 'file-name-manipulation
  (lambda ()
    (and (equal? "foo/bar"
		 (directory-as-file-name "foo/bar/"))
	 (equal? "foo/bar"
		 (directory-as-file-name "foo/bar"))
	 (equal? "/"
		 (directory-as-file-name "/"))
	 (equal? "."
		 (directory-as-file-name "")))))

(add-test! 'file-name-absolute? 'file-name-manipulation
  (lambda ()
    (and (file-name-absolute? "/usr/shievers")
	 (not (file-name-absolute? "src/des"))
	 (file-name-absolute? "/src/des")
	 (file-name-absolute? ""))))

(add-test! 'file-name-directory 'file-name-manipuation
  (lambda () 
    (and (equal? "/usr/"
		 (file-name-directory "/usr/bcd"))
	 (equal? "/usr/bcd/"
		 (file-name-directory "/usr/bcd/"))
	 (equal? "bdc/"
		 (file-name-directory "bdc/.login"))
	 (equal? ""
		 (file-name-directory "main.c"))
	 (equal? ""
		 (file-name-directory "/"))
	 (equal? ""
		 (file-name-directory "")))))

(add-test! 'file-name-nondirectory 'file-name-manipulation
  (lambda ()
    (and (equal? "ian"
		 (file-name-nondirectory "/usr/ian"))
	 (equal? ""
		 (file-name-nondirectory "/usr/ian/"))
	 (equal? ".login"
		 (file-name-nondirectory "ian/.login"))
	 (equal? "main.c"
		 (file-name-nondirectory "main.c"))
	 (equal? ""
		 (file-name-nondirectory ""))
	 (equal? "/"
		 (file-name-nondirectory "/")))))

(add-test! 'split-file-name 'file-name-manipulation
  (lambda ()
    (and (equal? '("src" "des" "main.c")
		 (split-file-name "src/des/main.c"))
	 (equal? '("" "src" "des" "main.c")
		 (split-file-name "/src/des/main.c"))
	 (equal? '("main.c")
		 (split-file-name "main.c"))
	 (equal? '("")
		 (split-file-name "/")))))

(add-test! 'path-list->file-name 'file-name-manipulation
  (lambda ()
    (and (equal? "src/des/main.c"
		 (path-list->file-name '("src" "des" "main.c")))
	 (equal? "/src/des/main.c"
		 (path-list->file-name '("" "src" "des" "main.c")))
	 (equal? "/usr/shivers/src/des/main.c"
		 (path-list->file-name '("src" "des" "main.c")
				       "/usr/shivers")))))

(add-test! 'file-name-extension 'file-name-manipulation
  (lambda ()
    (and (equal? ".c"
		 (file-name-extension "main.c"))
	 (equal? ".old"
		 (file-name-extension "main.c.old"))
	 (equal? ""
		 (file-name-extension "/usr/shivers"))
	 (equal? "."
		 (file-name-extension "foo."))
	 (equal? "."
		 (file-name-extension "foo.."))
	 (equal? ""
		 (file-name-extension "/usr/shivers/.login")))))

(add-test! 'file-name-sans-extension 'file-name-manipulation
  (lambda ()
    (and (equal? "main"
		 (file-name-sans-extension "main.c"))
	 (equal? "main.c"
		 (file-name-sans-extension "main.c.old"))
	 (equal? "/usr/shivers"
		 (file-name-sans-extension "/usr/shivers"))
	 (equal? "foo"
		 (file-name-sans-extension "foo."))
	 (equal? "foo."
		 (file-name-sans-extension "foo.."))
	 (equal? "/usr/shivers/.login"
		 (file-name-sans-extension "/usr/shivers/.login")))))

(add-test! 'parse-file-name 'file-name-manipulation
  (lambda ()
    (let* ((fname "/usr/shivers/main.c")
	   (f (file-name-nondirectory fname)))
      (equal? (list (file-name-directory fname)
		    (file-name-sans-extension f)
		    (file-name-extension f))
	      (call-with-values
		  (lambda ()
		    (parse-file-name fname))
		(lambda (a b c)
		  (list a b c)))))))

(add-test! 'replace-extension 'file-name-manipulation
  (lambda ()
    (let ((fname "/usr/shivers/main.c")
	  (ext "old"))
      (equal? (string-append (file-name-sans-extension fname) ext)
	      (replace-extension fname ext)))))

(add-test! 'simplify-file-name 'file-name-manipulation
  (lambda ()
    (and (equal? "/usr/shivers"
		 (simplify-file-name "/usr/shivers"))
	 (equal? "/usr/shivers"
		 (simplify-file-name "////usr//shivers/"))
	 (equal? "/usr/shivers/."
		 (simplify-file-name "////usr/shivers/."))
	 (equal? "//usr/shivers"
		 (simplify-file-name "//usr/shivers/"))
	 (equal? "/usr/shivers/../test"
		 (simplify-file-name "////usr/shivers/../test/")))))

(add-test! 'resolve-file-name 'file-name-manipulation
  (lambda ()
    (and (equal? (resolve-file-name "~")
		 (home-dir))
	 (string? (resolve-file-name "~/c/main.c" "/usr/bin")))))

(add-test! 'expand-file-name 'file-name-manipulation
  (lambda ()
    (equal? (expand-file-name "~/..///c/bin/main.out" "/usr/bin")
	    (simplify-file-name (resolve-file-name "~/..///c/bin/main.out" "/usr/bin")))))

(add-test! 'absolute-file-name 'file-name-manipulation
  (lambda ()
    (equal? (absolute-file-name "~/c/bin/c.out" "/usr/local")
	    "/usr/local/~/c/bin/c.out")))

;;(add-test! 'home-dir 'file-name-manipulation
;;  was tested with resolve-file-name

(add-test! 'home-file 'file-name-manipulation
  (lambda ()
    (equal? (home-file "man")
	    (resolve-file-name "~/man"))))