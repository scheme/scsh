;;; Test for the function in section 7 of the scsh-manual "Reading Delimited strings"
;;; Author: Christoph Hetz

;; for  testing: (certainly the path will be an other on other systems...)

;; ,open define-record-types handle
;; ,config ,load C:/cygwin/home/mephisto/cvs-scsh/scsh/scsh/test/test-packages.scm
;; ,load C:/cygwin/home/mephisto/cvs-scsh/scsh/scsh/test/test-base.scm
;; load this file
;; (test-all)


(add-test! 'read-line-test 'reading-delimited-strings
  (lambda ()
    ((lambda (in-port)
       (and (equal? "zeile 1"
		    (read-line in-port))
	    (equal? "zeile 2"
		    (read-line in-port))
	    (equal? "zeile 3"
		    (read-line in-port))))
     (make-string-input-port "zeile 1\nzeile 2\nzeile 3"))))

(add-test! 'read-line-trim-test 'reading-delimited-strings ;; same as without trim
  (lambda ()
    ((lambda (in-port)
       (and (equal? "zeile 1"
		    (read-line in-port 'trim))
	    (equal? "zeile 2"
		    (read-line in-port 'trim))
	    (equal? "zeile 3"
		    (read-line in-port 'trim))))
     (make-string-input-port "zeile 1\nzeile 2\nzeile 3"))))

(add-test! 'read-line-peek-test 'reading-delimited-strings
  (lambda ()
    ((lambda (in-port)
       (and (equal? "zeile 1"
		    (read-line in-port 'peek))
	    (equal? ""
		    (read-line in-port 'peek))
	    (equal? ""
		    (read-line in-port 'peek))))
     (make-string-input-port "zeile 1\nzeile 2\nzeile 3"))))

(add-test! 'read-line-concat-test 'reading-delimited-strings
  (lambda ()
    ((lambda (in-port)
       (and (equal? "zeile 1\n"
		    (read-line in-port 'concat))
	    (equal? "zeile 2\n"
		    (read-line in-port 'concat))
	    (equal? "zeile 3\004"
		    (read-line in-port 'concat))))
     (make-string-input-port "zeile 1\nzeile 2\nzeile 3\004")))) 

(add-test! 'read-line-split-test 'reading-delimited-strings
  (lambda ()
    ((lambda (in-port)
       (and (call-with-values (lambda () (read-line in-port 'split))
	      (lambda (a b) (and (equal? a "zeile 1")
				 (equal? b #\newline))))
	    (call-with-values (lambda () (read-line in-port 'split))
	      (lambda (a b) (and (equal? a "zeile 2")
				 (equal? b #\newline))))
	    (call-with-values (lambda () (read-line in-port 'split))
	      (lambda (a b) (and (equal? a "zeile 3")
				 (eof-object? b))))))
     (make-string-input-port "zeile 1\nzeile 2\nzeile 3"))))

(add-test! 'read-paragraph-test 'reading-delimited-strings
  (lambda ()
    ((lambda (in-port)
       (and (equal? "zeile 1\nzeile 2\nparagraph 1\n"
		    (read-paragraph in-port))
	    (equal? "zeile 1\nparagraph 2\n"
		    (read-paragraph in-port))
	    (equal? "zeile 1\nparagraph 3\n"
		    (read-paragraph in-port))))
     (make-string-input-port "zeile 1\nzeile 2\nparagraph 1\n\nzeile 1\nparagraph 2\n    \t\nzeile 1\nparagraph 3\n\n"))))

(add-test! 'read-paragraph-trim-test 'reading-delimited-strings  ;; same as without trim
  (lambda ()
    ((lambda (in-port)
       (and (equal? "zeile 1\nzeile 2\nparagraph 1\n"
		    (read-paragraph in-port 'trim))
	    (equal? "zeile 1\nparagraph 2\n"
		    (read-paragraph in-port 'trim))
	    (equal? "zeile 1\nparagraph 3\n"
		    (read-paragraph in-port 'trim))))
     (make-string-input-port "zeile 1\nzeile 2\nparagraph 1\n\nzeile 1\nparagraph 2\n    \t\nzeile 1\nparagraph 3\n\n"))))

(add-test! 'read-paragraph-concat-test 'reading-delimited-strings
  (lambda ()
    ((lambda (in-port)
       (and (equal? "zeile 1\nzeile 2\nparagraph 1\n\n"
		    (read-paragraph in-port 'concat))
	    (equal? "zeile 1\nparagraph 2\n    \t\n"
		    (read-paragraph in-port 'concat))
	    (equal? "zeile 1\nparagraph 3\n\n"
		    (read-paragraph in-port 'concat))))
     (make-string-input-port "zeile 1\nzeile 2\nparagraph 1\n\nzeile 1\nparagraph 2\n    \t\nzeile 1\nparagraph 3\n\n"))))

(add-test! 'read-paragraph-split-test 'reading-delimited-strings
  (lambda ()
    ((lambda (in-port)
       (and (call-with-values (lambda () (read-paragraph in-port 'split))
	      (lambda (a b) 
;			 (display "a1: ")(display a) 
;			 (display "b1: ")(display b)
		(and (equal? a "zeile 1\nzeile 2\nparagraph 1\n")
		     (equal? b "\n"))))
	    (call-with-values (lambda () (read-paragraph in-port 'split))
	      (lambda (a b) 
;			 (display "a2: ")(display a) 
;			 (display "b2: ")(display b)
		(and (equal? a "zeile 1\nparagraph 2\n")
		     (equal? b "    \t\n"))))
	    (call-with-values (lambda () (read-paragraph in-port 'split))
	      (lambda (a b) 
;			 (display "a3: ")(display a) 
;			 (display "b3: ")(display b)
		(and (equal? a "zeile 1\nparagraph 3\n")
		     (equal? b "\n"))))))
     (make-string-input-port "zeile 1\nzeile 2\nparagraph 1\n\nzeile 1\nparagraph 2\n    \t\nzeile 1\nparagraph 3\n\n"))))

(add-test! 'read-delimited-with-char-set-test 'reading-delimited-strings
  (lambda ()
    ((lambda (in-port)
       (and (equal? "zeile 1"
		    (read-delimited (list->char-set (list #\a #\b #\:)) in-port))
	    (equal? " nix\nzeile 2: x"
		    (read-delimited (list->char-set (list #\a #\b #\y)) in-port))))
     (make-string-input-port "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n"))))

(add-test! 'read-delimited-trim-with-char-set-test 'reading-delimited-strings
  (lambda ()
    ((lambda (in-port)
       (and (equal? "zeile 1"
		    (read-delimited (list->char-set (list #\a #\b #\:)) in-port 'trim))
	    (equal? " nix\nzeile 2: x"
		    (read-delimited (list->char-set (list #\a #\b #\y)) in-port 'trim))))
     (make-string-input-port "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n"))))

(add-test! 'read-delimited-peek-with-char-set-test 'reading-delimited-strings
  (lambda ()
    ((lambda (in-port)
       (and (equal? "zeile 1"
		    (read-delimited (list->char-set (list #\a #\b #\:)) in-port 'peek))
	    (equal? ": nix\nzeile 2: x"
		    (read-delimited (list->char-set (list #\a #\b #\y)) in-port 'peek))))
     (make-string-input-port "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n"))))

(add-test! 'read-delimited-concat-with-char-set-test 'reading-delimited-strings
  (lambda ()
    ((lambda (in-port)
       (and (equal? "zeile 1:"
		    (read-delimited (list->char-set (list #\a #\b #\:)) in-port 'concat))
	    (equal? " nix\nzeile 2: xy"
		    (read-delimited (list->char-set (list #\a #\b #\y)) in-port 'concat))))
     (make-string-input-port "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n"))))

(add-test! 'read-delimited-split-with-char-set-test 'reading-delimited-strings
  (lambda ()
    ((lambda (in-port)
       (and (call-with-values 
		(lambda () (read-delimited (list->char-set (list #\a #\b #\:)) in-port 'split))
	      (lambda (a b) (and (equal? "zeile 1" a)
				 (equal? #\: b))))
	    (call-with-values
			 (lambda () (read-delimited (list->char-set (list #\a #\b #\y)) in-port 'split))
	      (lambda (a b) (and (equal? " nix\nzeile 2: x" a)
				 (equal? #\y b))))))
     (make-string-input-port "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n"))))

;;------------------------------------------------------------------------------------------------------------------------

(add-test! 'read-delimited-with-string-test 'reading-delimited-strings
  (lambda ()
    ((lambda (in-port)
       (and (equal? "zeile 1"
		    (read-delimited "ab:" in-port))
	    (equal? " nix\nzeile 2: x"
		    (read-delimited "aby" in-port))))
     (make-string-input-port "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n"))))

(add-test! 'read-delimited-trim-with-string-test 'reading-delimited-strings
  (lambda ()
    ((lambda (in-port)
       (and (equal? "zeile 1"
		    (read-delimited "ab:" in-port 'trim))
	    (equal? " nix\nzeile 2: x"
		    (read-delimited "aby" in-port 'trim))))
     (make-string-input-port "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n"))))

(add-test! 'read-delimited-peek-with-string-test 'reading-delimited-strings
  (lambda ()
    ((lambda (in-port)
       (and (equal? "zeile 1"
		    (read-delimited "ab:" in-port 'peek))
	    (equal? ": nix\nzeile 2: x"
		    (read-delimited "aby" in-port 'peek))))
     (make-string-input-port "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n"))))

(add-test! 'read-delimited-concat-with-string-test 'reading-delimited-strings
  (lambda ()
    ((lambda (in-port)
       (and (equal? "zeile 1:"
		    (read-delimited "ab:" in-port 'concat))
	    (equal? " nix\nzeile 2: xy"
		    (read-delimited "aby" in-port 'concat))))
     (make-string-input-port "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n"))))

(add-test! 'read-delimited-split-with-string-test 'reading-delimited-strings
  (lambda ()
    ((lambda (in-port)
       (and (call-with-values 
		(lambda () (read-delimited "ab:" in-port 'split))
	      (lambda (a b) (and (equal? "zeile 1" a)
				 (equal? #\: b))))
	    (call-with-values
		(lambda () (read-delimited "aby" in-port 'split))
	      (lambda (a b) (and (equal? " nix\nzeile 2: x" a)
				 (equal? #\y b))))))
     (make-string-input-port "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n"))))

;; ---------------------------------------------------------------------------------------------------

(add-test! 'read-delimited-with-character-test 'reading-delimited-strings
  (lambda ()
    ((lambda (in-port)
       (and (equal? "zeile 1"
		    (read-delimited #\: in-port))
	    (equal? " nix\nzeile 2: x"
		    (read-delimited #\y in-port))))
     (make-string-input-port "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n"))))

(add-test! 'read-delimited-trim-with-character-test 'reading-delimited-strings
  (lambda ()
    ((lambda (in-port)
       (and (equal? "zeile 1"
		    (read-delimited #\: in-port 'trim))
	    (equal? " nix\nzeile 2: x"
		    (read-delimited #\y in-port 'trim))))
     (make-string-input-port "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n"))))

(add-test! 'read-delimited-peek-with-character-test 'reading-delimited-strings
  (lambda ()
    ((lambda (in-port)
       (and (equal? "zeile 1"
		    (read-delimited #\: in-port 'peek))
	    (equal? ": nix\nzeile 2: x"
		    (read-delimited #\y in-port 'peek))))
     (make-string-input-port "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n"))))

(add-test! 'read-delimited-concat-with-character-test 'reading-delimited-strings
  (lambda ()
    ((lambda (in-port)
       (and (equal? "zeile 1:"
		    (read-delimited #\: in-port 'concat))
	    (equal? " nix\nzeile 2: xy"
		    (read-delimited #\y in-port 'concat))))
     (make-string-input-port "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n"))))

(add-test! 'read-delimited-split-with-character-test 'reading-delimited-strings
  (lambda ()
    ((lambda (in-port)
       (and (call-with-values 
		(lambda () (read-delimited #\: in-port 'split))
	      (lambda (a b) (and (equal? "zeile 1" a)
				 (equal? #\: b))))
	    (call-with-values
		(lambda () (read-delimited #\y in-port 'split))
	      (lambda (a b) (and (equal? " nix\nzeile 2: x" a)
				 (equal? #\y b))))))
     (make-string-input-port "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n"))))

;; ===============================================================================================


;; XXX read-delimited! and %read-delimited! are confusing bugy
;(add-test! 'read-delimited!-with-char-set-test 'reading-delimited-strings
;	   (lambda ()
;	     (let ((buf "                              "))
;	       ((lambda (in-port)
;		  (read-delimited! (list->char-set (list #\a #\b #\1))
;				   buf
;				   in-port)
;		  (equal? "zeile a                       "
;			  buf))
;		(make-string-input-port "zeile a1 nix\nzeile b1 x2\nzeile c1 wieder nix\n")))))


;; ====================================================================================================

(add-test! 'skip-char-set-with-charset-test 'reading-delimited-strings
  (lambda ()
    (= 6
       (skip-char-set (list->char-set (list #\a #\b #\c))
		      (make-string-input-port "abccbaxxx")))))


(add-test! 'skip-char-set-with-string-test 'reading-delimited-strings
  (lambda ()
    (= 6
       (skip-char-set "abc"
		      (make-string-input-port "abccbaxxx")))))

(add-test! 'skip-char-set-with-character-test 'reading-delimited-strings
  (lambda ()
    (= 6
       (skip-char-set #\a
		      (make-string-input-port "aaaaaaxxx")))))
