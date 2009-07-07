;;; Test for the function in section 6 of the scsh-manual "Pattern-matching strings with regular expressions"
;;; Author: Christoph Hetz

;; for  testing load this file and call (test-all)

;; *** basic help-functions ***

(define eq-match?
  (lambda (m1 m2)
    (if (equal? m1 #f)
	(not m2)
	(and (= (match:start m1)
		(match:start m2))
	     (= (match:end m1)
		(match:end m2))
	     (equal? (match:substring m1)
		     (match:substring m2))))))

(define matches-same-signs?
  (lambda (re1 re2)
    (let loop ((i 0))
      (if (= 256 i)
	  #t
	  (let ((str (list->string (list (ascii->char i)))))
	    (if (eq-match? (string-match re1 str)
			   (string-match re2 str))
		(loop (+ i 1))
		#f))))))


;; *** help-strings *** 

(define all-signs-string
  (let loop ((i 0))
    (if (= i 256)
	""
	(string-append (list->string (list (ascii->char i)))
		       (loop (+ i 1))))))

(define test-string 
  "Dieser Test-String wurde am 29.07.2004 um 5:23PM erstellt.\na aa aaa aaaa\nab aabb aaabbb\naba abba abbba\n1 12 123 1234\nyyyyyyyyyy\n")


;; *** tests ***

(add-test! 'no-match-test 'pattern-matching
  (lambda ()
    (not (string-match (rx "xxx") test-string))))

(add-test! 'various-forms-of-non-vowels 'pattern-matching
  (lambda ()
    (and (eq-match? (string-match (rx (- alpha ("aeiouAEIOU"))) test-string)
		    (string-match (rx (- (/"azAZ") ("aeiouAEIOU"))) test-string))
	 (eq-match? (string-match (rx (- (/"azAZ") ("aeiouAEIOU"))) test-string)
		    (string-match (rx (- alpha ("aeiou") ("AEIOU"))) test-string))
	 (eq-match? (string-match (rx (- alpha ("aeiou") ("AEIOU"))) test-string)
		    (string-match (rx (w/nocase (- alpha ("aeiou")))) test-string))
	 (eq-match? (string-match (rx (w/nocase (- alpha ("aeiou")))) test-string)
		    (string-match (rx (w/nocase (- (/ "az") ("aeiou")))) test-string)))))

(add-test! '|-test 'pattern-matching
  (lambda ()
    (eq-match? (string-match (rx (| upper ("aeiou") digit)) "xxx A yyy")
	       (string-match (rx (| (/ "AZ09") ("aeiou"))) "xxx A yyy"))
    (eq-match? (string-match (rx (| upper ("aeiou") digit)) "xxx a yyy")
	       (string-match (rx (| (/ "AZ09") ("aeiou"))) "xxx a yyy"))
    (eq-match? (string-match (rx (| upper ("aeiou") digit)) "xxx 6 yyy")
	       (string-match (rx (| (/ "AZ09") ("aeiou"))) "xxx 6 yyy"))))

(add-test! 'comma-seperated-list-of-REs 'pattern-matching
  (lambda () 
    (let ((csl (lambda (re)
		 (rx (| ""
			(: ,re
			   (* ", " ,re)))))))
      (string-match (csl (rx (| "John" "Paul" "George" "Ringo")))
		    "George, Ringo, Paul, John"))))

(add-test! 'repetition-test 'pattern-matching
  (lambda ()
    (and (equal? "caaadadr"
		  (match:substring (string-match (rx (: "c" (+ (| "a" "d")) "r"))
						 "(caaadadr ...")))
	 (equal? "caaadadr"
		  (match:substring (string-match (rx (: "c" (+ ("ad")) "r"))
						 "(caaadadr ...")))
	 (equal? "caaadadr"
		  (match:substring (string-match (rx (: "c" (** 1 6 ("ad")) "r"))
						 "(caaadadr ...")))
	 (not (string-match (rx (: "c" (** 1 4 ("ad")) "r"))
						 "(caaadadr ...")))))

(add-test! 'special-cases-test 'pattern-matching
  (lambda ()
    (and (matches-same-signs? (rx any) (rx (~)))
	 (not (string-match (rx (|)) all-signs-string)))))



;XXX something is wrong with this
(add-test! 're-vs-@re-submatch-test 'pattern-matching
  (lambda ()
    (let* ((f (lambda ()
		(rx (submatch "sub-f1")
		    (submatch "sub-f2"))))
	   (re (rx (submatch (* "foo"))
		   (submatch (? "bar"))
		   ,(f)
		   (submatch "baz")))
	   (re-@ (rx (submatch (* "foo"))
		     (submatch (? "bar"))
		     ,@(f)
		     (submatch "baz")))
	   (match1 (string-match re "foofoobarsub-f1sub-f2baz"))
	   (match2 (string-match re-@ "foofoobarsub-f1sub-f2baz")))
      (and match1 
	   match2
	   (equal? "baz"
		   (match:substring match1 3))
	   (equal? "sub-f1"
		   (match:substring match2 3))
	   (equal? "sub-f2"
		   (match:substring match2 4))
	   (equal? "baz"
		   (match:substring match2 5))))))

(add-test! 'posix-string-test 'pattern-matching
  (lambda ()
    (and (string-match (rx (posix-string "[aeiou]+|x*|y{3,5}"))
		       "a")
	 (string-match (rx (posix-string "[aeiou]+|x*|y{3,5}"))
		       "x")
	 (string-match (rx (posix-string "[aeiou]+|x*|y{3,5}")) ; does not work on Solaris
		       "")
	 (string-match (rx (posix-string "[aeiou]+|x*|y{3,5}"))
		       "yyyy"))))

(add-test! 'dsm-test 'pattern-matching
  (lambda ()
    (and (equal? "hello"
		 (match:substring (string-match (rx (dsm 1 0 (submatch "hello")))
						"bla hello bla")
				  2))
	 (not (match:substring (string-match (rx (dsm 1 0 (submatch "hello")))
						"bla hello bla")
				  1))
	 (equal? "hello"
		 (match:substring (string-match (rx (dsm 2 0 (submatch "hello")))
						"bla hello bla")
				  3))
	 (not (match:substring (string-match (rx (dsm 2 0 (submatch "hello")))
						"bla hello bla")
				  1))
	 (not (match:substring (string-match (rx (dsm 2 0 (submatch "hello")))
						"bla hello bla")
				  2)))))

(add-test! 'string-regexp 'pattern-matching
  (lambda ()
    (and (equal? "erstellt."
		 (match:substring (string-match (rx "erstellt.") test-string)))
	 (not (string-match (rx "Erstellt.") test-string)))))

(add-test! 'character-set 'pattern-matching
  (lambda ()
    (eq-match? (string-match (rx ("abcde")) test-string)
	       (string-match (rx ("edcba")) test-string))))

;; fails only because of the case i = 0
; (add-test! 'any-test 'pattern-matching 
;   (lambda ()
;     (let loop ((i 0))
;       (if (= 256 i)
; 	  #t
; 	  (if (string-match (rx any) (list->string (list (ascii->char i))))
; 	      (loop (+ i 1))
; 	      #f)))))

(add-test! 'sequences-test 'pattern-matching
  (lambda ()
    (equal? "1234"
	    (match:substring (string-match (rx (: "1" any any "4")) test-string)))))

(add-test! 'choices 'pattern-matching
  (lambda ()
    (let ((m1 (string-match (rx (| "erstellt." "xxx")) test-string))
	  (m2 (string-match (rx (| "xxx" "erstellt.")) test-string)))
      (and m1
	   m2
	   (eq-match? m1 m2)))))


(add-test! '*-test 'pattern-matching
  (lambda ()
    (and (equal? ""
		 (match:substring (string-match (rx (* "y")) test-string)))
	 (equal? "D"
		 (match:substring (string-match (rx (* "D")) test-string))))))

(add-test! '+-test 'pattern-matching
  (lambda ()
    (and (equal? "yyyyyyyyyy"
		 (match:substring (string-match (rx (+ "y")) test-string)))
	 (equal? "D"
		 (match:substring (string-match (rx (+ "D")) test-string))))))

(add-test! '?-test 'pattern-matching
  (lambda ()
    (and (equal? ""
		 (match:substring (string-match (rx (? "y")) test-string)))
	 (equal? "D"
		 (match:substring (string-match (rx (? "D")) test-string))))))

(add-test! '=-from-test 'pattern-matching
  (lambda ()
    (and (equal? "yyyyy"
		 (match:substring (string-match (rx (= 5 "y")) test-string)))
	 (not (string-match (rx (= 11 "y")) test-string)))))

(add-test! '>=-from-test 'pattern-matching
  (lambda ()
    (and (equal? "yyyyyyyyyy"
		 (match:substring (string-match (rx (>= 5 "y")) test-string)))
	 (equal? "yyyyyyyyyy"
		 (match:substring (string-match (rx (>= 10 "y")) test-string)))
	 (not (string-match (rx (>= 11 "y")) test-string)))))

(add-test! '**from-to-test 'pattern-matching
  (lambda ()
    (and (equal? "yyyyyyyyyy"
		 (match:substring (string-match (rx (** 1 30 "y")) test-string)))
	 (equal? "yyyyy"
		 (match:substring (string-match (rx (** 1 5 "y")) test-string)))
	 (not (string-match (rx (** 11 12 "y")) test-string))
	 (not (string-match (rx (** 12 11 any)) test-string))
	 (equal? "" 
		 (match:substring (string-match (rx (** 0 0 any)) test-string))))))

(add-test! 'single-characters-test 'pattern-matching
  (lambda ()
    (and (eq-match? (string-match (rx ("abcd")) test-string)
		    (string-match (rx (| #\a #\b #\c #\d)) test-string))
	 (eq-match? (string-match (rx ("xy")) test-string)
		    (string-match (rx (| #\x #\y)) test-string)))))

(add-test! 'range-test 'pattern-matching
  (lambda ()
    (and (equal? "D"
		 (match:substring (string-match (rx (/ #\A #\Z #\a #\z #\0 #\9)) test-string)))
	 (equal? "D"
		 (match:substring (string-match (rx (/ #\A "Zaz0" #\9)) test-string)))
	 (equal? "i"
		 (match:substring (string-match (rx (/ #\a #\z #\0 #\9)) test-string)))
	 (equal? "i"
		 (match:substring (string-match (rx (/ #\a "z0" #\9)) test-string)))
	 (equal? "2"
		 (match:substring (string-match (rx (/ #\0 #\9)) test-string)))
	 (equal? "2"
		 (match:substring (string-match (rx (/ "0" #\9)) test-string))))))

(add-test! 'character-classes-test 'pattern-matching
  (lambda ()
    (and (eq-match? (string-match (rx lower-case) test-string)
		    (string-match (rx (- alphabetic upper-case)) test-string))
	 (eq-match? (string-match (rx upper-case) test-string)
		    (string-match (rx (- alphabetic lower-case)) test-string))
	 (equal? "2"
		 (match:substring (string-match (rx numeric) test-string)))
	 (equal? "-"
		 (match:substring (string-match (rx punctuation) test-string)))
	 (equal? " "
		 (match:substring (string-match (rx blank) test-string)))
	 (equal? " "
		 (match:substring (string-match (rx whitespace) test-string)))
	 (equal? "\n"
		 (match:substring (string-match (rx control) test-string)))
	 (equal? "D"
		 (match:substring (string-match (rx hex-digit) test-string)))
	 (equal? "D"
		 (match:substring (string-match (rx ascii) test-string))))))

(add-test! 'uncase-w/case-w/nocase-test 'pattern-matching
  (lambda ()
    (and (equal? "foo"
		 (match:substring (string-match (rx (uncase "foo")) "bla foo bla")))
	 (equal? "FOO"
		 (match:substring (string-match (rx (uncase "foo")) "bla FOO bla")))
	 (equal? "FOo"
		 (match:substring (string-match (rx (uncase "foo")) "bla FOo bla")))
	 (equal? "fOo"
		 (match:substring (string-match (rx (uncase "foo")) "bla fOo bla")))
	 (equal? "FoO"
		 (match:substring (string-match (rx (uncase "foo")) "bla FoO bla")))
	 (equal? "a"
		 (match:substring (string-match (rx (uncase (~ "a"))) "a")))
	 (equal? "A"
		 (match:substring (string-match (rx (uncase (~ "a"))) "A")))
	 (not (string-match (rx (w/nocase (~ "a"))) "aA"))
	 (string-match (rx (w/nocase "abc"
				     (* "FOO" (w/case "Bar"))
				     ("aeiou"))) 
		       "kabcfooBariou")
	 (not (string-match (rx (w/nocase "abc"
					  (* "FOO" (w/case "Bar"))
					  ("aeiou"))) 
			    "kabcfooBARiou")))))

(add-test! 'dynamic-re-test-1 'pattern-matching
  (lambda ()
    (let ((str "I am feeding the goose, you are feeding the geese.")
	  (me 1)
	  (you 2))
      (and (equal? "feeding the goose"
		   (match:substring (string-match (rx (: "feeding the "
							 ,(if (> me 1) 
							      "geese" 
							      "goose")))
						  str)))
	   (equal? "feeding the geese"
		   (match:substring (string-match (rx (: "feeding the "
							 ,(if (> you 1) 
							      "geese" 
							      "goose")))
						  str)))))))

(add-test! 'dynamic-re-test-2 'pattern-matching
  (lambda ()
    (let* ((ws (rx (+ whitespace)))
	   (date (rx (: (| "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul")
			,ws
			(| ("123456789")
			   (: ("12") digit)
			   "30"
			   "31")))))
      (and (equal? "on Mar 14"
		   (match:substring (string-match (rx (: "on " ,date)) 
						  "it was on Mar 14 ...")))
	   
	   (equal? "on May 31"
		   (match:substring (string-match (rx (: "on " ,date)) 
						  "it was on May 31 ...")))))))

(add-test! 'regexp?-test 'pattern-matching
  (lambda ()
    (and (not (regexp? "abc"))
	 (regexp? (rx "abc")))))

(add-test! 'regexp-search-test 'pattern-matching
  (lambda ()
    (and (equal? "abc"
		 (match:substring (regexp-search (rx "abc") "abcdefg")))
	 (not (regexp-search (rx "abc") "abcdefg" 3))
	 (not (regexp-search (rx "cba") "abcdefg")))))

(add-test! 'regexp-search?-test 'pattern-matching
  (lambda ()
    (and (regexp-search? (rx "abc") "abcdefg")
	 (not (regexp-search? (rx "abc") "abcdefg" 3))
	 (not (regexp-search? (rx "cba") "abcdefg")))))

(add-test! 'regexp-substitute/global-test-1 'pattern-matching
  (lambda ()
    (equal? "dry Jin"
	    (regexp-substitute/global #f (rx "Cotton") "dry Cotton"
				      'pre "Jin" 'post))))

(add-test! 'regexp-substitute/global-test-2 'pattern-matching
  (lambda ()
    (equal? "01/03/79"
	    (regexp-substitute/global #f (rx (submatch (+ digit)) "/"
					     (submatch (+ digit)) "/"
					     (submatch (+ digit)))
				      "03/01/79"
				      'pre 2 "/" 1 "/" 3 'post))))

(add-test! 'regexp-substitute/global-test-3 'pattern-matching
  (lambda ()
    (equal? "Sep 29, 1961"
	    (regexp-substitute/global #f (rx (submatch (+ digit)) "/"
					     (submatch (+ digit)) "/"
					     (submatch (+ digit)))
				      "9/29/61"
				      'pre
				      (lambda (m)
					(let ((mon (vector-ref '#("Jan" "Feb" "Mar" "Apr" "May" "Jun"
								  "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
							       (- (string->number (match:substring m 1)) 
								  1)))
					      (day (match:substring m 2))
					      (year (match:substring m 3)))
					  (string-append mon " " day ", 19" year)))
				      'post))))

(add-test! 'regexp-substitute/global-test-4 'pattern-matching
  (lambda ()
    (let ((kill-matches (lambda (re s)
			  (regexp-substitute/global #f re s 'pre 'post))))
      (equal? " will disappear, also  and "
	      (kill-matches (rx (| "Windows" "tcl" "Intel")) 
			    "Windows will disappear, also tcl and Intel")))))

(add-test! 'regexp-fold-right-test 'pattern-matching
  (lambda ()
    (equal? (list "1" "1" "2" "2")
	    (regexp-fold-right (rx digit)
			       (lambda (m i lis)
				 (cons (match:substring m 0) lis))
			       '() "a1 b1 a2 b2 ..."))))

(add-test! 'let-match-test 'pattern-matching
  (lambda ()
    (equal? "3/1/79\nmonth: 3\nday: 1\nyear: 79"
	    (let-match (regexp-search (rx (submatch (+ digit)) "/"
					  (submatch (+ digit)) "/"
					  (submatch (+ digit)))
				      "here comes a date: 3/1/79")
		       (whole-date month day year)
		       (string-append whole-date "\nmonth: " month "\nday: " day "\nyear: " year)))))

(add-test! 'if-match-test 'pattern-matching
  (lambda ()
    (and (if-match (regexp-search (rx (submatch (+ digit)) "/"
				      (submatch (+ digit)) "/"
				      (submatch (+ digit)))
				  "here comes a date: 3/1/79")
		   (whole-date month day year)
		   (and (equal? month "3")
			(equal? day "1")
			(equal? year "79"))
		   #f)
	 (if-match (regexp-search (rx (submatch (+ digit)) "/"
				      (submatch (+ digit)) "/"
				      (submatch (+ digit)))
				  "here comes a date: 3|1|79")
		   (whole-date month day year)
		   (and (equal? month "3")
			(equal? day "1")
			(equal? year "79"))
		   #t))))

(add-test! 'match-cond-test 'pattern-matching
  (lambda ()
    (let ((m "")
	  (d "")
	  (y ""))
      (and (match-cond ((regexp-search (rx (submatch (+ digit)) "/"
					   (submatch (+ digit)) "/"
					   (submatch (+ digit)))
				       "here comes a date: 3/1/79")
			(whole-date month day year)
			(begin (set! m month)
			       (set! d day)
			       (set! y year)))
		       (test (equal? m "3")
			     #t)
		       (else #f))
	   (match-cond ((regexp-search (rx (submatch (+ digit)) "/"
					   (submatch (+ digit)) "/"
					 (submatch (+ digit)))
				       "here comes a date: 4/1/79")
			(whole-date month day year)
			(begin (set! m month)
			       (set! d day)
			       (set! y year)))
		       (test (equal? m "3")
			     #t)
		       (test (if (equal? m "4")
				 m
				 d) =>
				 (lambda (month)
				   (equal? month "4")))			       
		       (else #f))
	   (match-cond ((regexp-search (rx (submatch (+ digit)) "/"
					   (submatch (+ digit)) "/"
					   (submatch (+ digit)))
				       "here comes a date: 5/1/79")
			(whole-date month day year)
			(begin (set! m month)
			       (set! d day)
			       (set! y year)))
		       (test (equal? m "3")
			     #t)
		       (test (if (equal? m "4")
				 m
				 d) =>
				 (lambda (month)
				   (equal? month "4")))			       
		       (else #t))))))

(add-test! 'flush-submatches-test 'pattern-matching
  (lambda ()
    (let ((re (rx (submatch "foo")
		  (submatch "bar"))))
      (and (= 2
	      (re-seq:tsm re))
	   (= 0
	      (re-seq:tsm (flush-submatches re)))
	   (equal? "foobar"
		   (match:substring (string-match (flush-submatches re)
						  "foobar")))))))

(add-test! 'uncase-test 'pattern-matching
  (lambda ()
    (equal? "FoO"
	    (match:substring (string-match (uncase (rx "foo"))
					   "FoO")))))

(add-test! 'simplify-regexp-test 'pattern-matching
  (lambda ()
    (and (re-dsm? (rx (: (** 0 0 (submatch "apple"))
			 (submatch "bar"))))
	 (= 2
	    (re-dsm:tsm (rx (: (** 0 0 (submatch "apple"))
			       (submatch "bar"))))))))



;; XXX perhaps only a mistake in the manual - it says: 
;; uncase-char-set was of the type: cset -> re
;; in fact it is of the type: cset -> cset
(add-test! 'uncase-char-set-test 'pattern-matching 
  (lambda ()
    (equal? "B"
	    (match:substring (string-match (uncase-char-set (list->char-set (list #\a #\b #\c)))
					   "dDBb")))))

(add-test! 'uncase-re-char-set-test 'pattern-matching 
  (lambda ()
    (equal? "d"
	    (match:substring (string-match (uncase (rx (/ "AZ")))
					   "dDBb")))))

(add-test! 'uncase-string-test 'pattern-matching
  (lambda ()
    (equal? "FoO"
	    (match:substring (string-match (uncase-string "foo")
					   "FoO")))))
(add-test! 'sre->regexp-test 'pattern-matching
  (lambda ()
    (regexp? (sre->regexp '(: "Christoph " (? "F. ") "Hetz")))))


;; XXX
;;Warning: wrong number of arguments
;;         (re-seq (re-string "Pete ") (re-repeat 1 #f (re-string "Sz")) (re-string "ilagyi"))
;;         (procedure wants: (:value))
;;         (arguments are: (:value :value :value))
;;
;;
(add-test! 'regexp->sre-test 'pattern-matching 
  (lambda ()
    (let ((re (re-seq (list (re-string "Pete ")
			    (re-repeat 1 #f (re-string "Sz"))
			    (re-string "ilagyi")))))
      (equal? '(? "Pete " (+ "Sz") "ilagyi")
	      (regexp->sre (re-repeat 0 1 re))))))

(add-test! 'char-classes+algebra-test 'pattern-matching
  (lambda ()
    (and (matches-same-signs? (rx (| alphabetic numeric))
			      (rx alphanumeric))
	 (matches-same-signs? (rx (- alphanumeric numeric))
			      (rx alphabetic))
	 (matches-same-signs? (rx (- alphanumeric alphabetic))
			      (rx numeric))
	 (matches-same-signs? (rx (& alphabetic alphanumeric))
			      (rx alphabetic))
	 (matches-same-signs? (rx (| alphabetic numeric numeric))
			      (rx alphanumeric))
	 (matches-same-signs? (rx (~ (& alphanumeric numeric)
				     graphic
				     (| upper-case numeric)))
			      (rx (- any 
				     alphanumeric
				     graphic)))
	 (matches-same-signs? (rx (/ "09"))
			      (rx numeric)))))

(add-test! 'different-ways-test 'pattern-matching
  (lambda ()
    (and (eq-match? (string-match (rx "abcde") "xxxabcdexxx")
		    (string-match (rx (: "a" "b" "c" "d" "e")) "xxxabcdexxx"))
	 (eq-match? (string-match (rx "abcde") "xxxabcdexxx")
		    (string-match (rx (: "a" (: "b" (: "c" (: "d" "e"))))) "xxxabcdexxx"))
	 (eq-match? (string-match (rx "abcde") "xxxabcdexxx")
		    (string-match (rx (: "ab" "c" (: "d" "e"))) "xxxabcdexxx"))
	 (eq-match? (string-match (rx "abcde") "xxxabcdexxx")
		    (string-match (rx (: "a" "b" "cde")) "xxxabcdexxx"))
	 (eq-match? (string-match (rx "abcde") "xxxabcdexxx")
		    (string-match (rx (: (: (: "a" "b") "c") (: "d" "e"))) "xxxabcdexxx"))
	 (eq-match? (string-match (rx "xxx" (* alphabetic) "xxx") "xxxabcdexxx")
		    (string-match (rx (+ "x") "abcde" (+ "x")) "xxxabcdexxx"))
	 (eq-match? (string-match (rx (: "x" (+ "x") (* "x"))
				      (: (? alphanumeric)
					 (? alphanumeric)
					 (? alphanumeric)
					 (? alphanumeric)
					 (? alphanumeric)
					 (? alphanumeric)
					 (? alphanumeric)
					 (? alphanumeric))
				      "xxx") 
				  "xxxabcdexxx")
		    (string-match (rx (: "xxx" (* (/ "ae")) (+ "x"))) "xxxabcdexxx"))
	 (eq-match? (string-match (rx "xxxabcdexxx") "xxxabcdexxx")
		    (string-match (rx (* alphanumeric)) "xxxabcdexxx"))
	 (eq-match? (string-match (rx (: "xxx" (: "abcde" "x" "xx"))) "xxxabcdexxx")
		    (string-match (rx (* (| (/ "ae") "x"))) "xxxabcdexxx")))))

(add-test! 'regexp-adt-re-seq-test 'pattern-matching
  (lambda ()
    (and (re-seq? (make-re-seq '("foo" "bar")))
	 (re-seq? (re-seq '("foo" "bar")))
	 (equal? '("foo" "bar")
		 (re-seq:elts (make-re-seq '("foo" "bar"))))
	 (= 2
	    (re-seq:tsm (rx (: (submatch "foo")
			       (submatch "bar"))))))))

(add-test! 'regexp-adt-re-choice-test 'pattern-matching
  (lambda ()
    (and (re-choice? (make-re-choice '("foo" "bar")))
	 (re-choice? (re-choice '("foo" "bar")))
	 (equal? '("foo" "bar")
		 (re-choice:elts (make-re-choice '("foo" "bar"))))
	 (= 2
	    (re-choice:tsm (rx (| (submatch "foo")
			       (submatch "bar"))))))))

(add-test! 'regexp-adt-re-repeat-test 'pattern-matching
  (lambda ()
    (and (re-repeat? (make-re-repeat 1 5 '("foo" "bar")))
	 (= 1
	    (re-repeat:from (make-re-repeat 1 5 '("foo" "bar"))))
	 (= 5
	    (re-repeat:to (make-re-repeat 1 5 '("foo" "bar"))))
	 (= 2
	    (re-repeat:tsm (rx (** 1 5 (submatch "foo")
				   (submatch "bar"))))))))

(add-test! 'regexp-adt-re-submatch-test 'pattern-matching
  (lambda ()
    (and (re-submatch? (make-re-submatch (rx "foo")))
	 (= 1
	    (re-submatch:pre-dsm (make-re-submatch (rx "foo") 1 0)))
	 (= 0
	    (re-submatch:post-dsm (make-re-submatch (rx "foo") 1 0)))
	 (= 3
	    (re-submatch:tsm (rx (submatch (submatch "foo")
					   (submatch "bar"))))))))

(add-test! 'regexp-adt-re-string-test 'pattern-matching
  (lambda ()
    (and (re-string? (make-re-string "abc"))
	 (re-string? (re-string "abc"))
	 (equal? "abc"
		 (re-string:chars (rx "abc"))))))

(add-test! 'regexp-adt-re-char-set-test 'pattern-matching
  (lambda ()
    (and (re-char-set? (make-re-char-set (list->char-set (list #\a #\b #\c))))
	 (re-char-set? (re-char-set (list->char-set (list #\a #\b #\c))))
	 (equal? '(#\a #\b #\c)
		 (char-set->list (re-char-set:cset 
				  (make-re-char-set (list->char-set (list #\a #\b #\c)))))))))

(add-test! 'regexp-adt-re-dsm-test 'pattern-matching
  (lambda ()
    (and (re-dsm? (make-re-dsm (rx "foo") 1 0))
;XXX	 (re-dsm? (re-dsm (rx "foo") 1 0))
	 (re-string? (re-dsm:body (make-re-dsm (rx "foo") 1 0)))
	 (= 1
	    (re-dsm:pre-dsm (make-re-dsm (rx "foo") 1 0)))
	 (= 0
	    (re-dsm:post-dsm (make-re-dsm (rx "foo") 1 0)))
	 (= 1
	    (re-dsm:tsm (make-re-dsm (rx "foo") 1 0))))))

(add-test! 'regexp-adt-re-const-test 'pattern-matching
  (lambda ()
    (and (regexp? re-bos)
	 (regexp? re-eos)
	 (regexp? re-bol)
	 (regexp? re-eol)
	 (re-bos? re-bos)
	 (re-eos? re-eos)
	 (re-bol? re-bol)
	 (re-eol? re-eol))))

(add-test! 'regexp-adt-re-const-2-test 'pattern-matching
  (lambda ()
    (and (regexp? re-trivial)
	 (re-trivial? re-trivial)
	 (regexp? re-empty)
	 (re-empty? re-empty)
	 (regexp? re-any)
	 (re-any? re-any)
	 (regexp? re-nonl)
	 (= 3
	    (re-tsm (rx (submatch (submatch "foo")
					   (submatch "bar")))))
;; XXX clean-up-cres
	 (matches-same-signs? re-any (rx any))
	 (matches-same-signs? re-nonl (rx (~ #\newline)))
	 (matches-same-signs? re-empty (rx (|)))
;; XXX error - but why?
;;	 (matches-same-signs? re-trivial (rx ""))
	 )))

;(add-test! 'if-sre-form-test 'pattern-matching
;  (lambda ()
;    (let* ((sr '(: "a" "b")))
;	   (rgxp (rx sr)))
;      (and (regexp? (if-sre-form sr
;				 (rx se)
;				 sr))
;	   (regexp? (if-sre-form rgxp
;				 (rx rgxp)
;				 rgxp))))))

;(add-test! 'sre-form?-test 'pattern-matching
;  (lambda ()
;    (let* ((sr '(: "a" "b"))
;	   (rgxp (rx sr)))
;      (and (not (sre-form? rgxp))
;	   (sre-form sr)))))
