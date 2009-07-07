;;; "Test" for the functions in section 3.10 of the scsh manual "time"
;;; "Test", because these are no real test - they just call the prozedures to check if they are implemented
;;; Author: Christoph Hetz

;; ,open define-record-types handle
;; ,config ,load C:/cygwin/home/mephisto/cvs_scsh/scsh/scsh/test/test-packages.scm
;; ,load C:/cygwin/home/mephisto/cvs_scsh/scsh/scsh/test/test-base.scm
;; load this file
;; (test-all)


;; *** tests ***

(add-test! 'time-ticks 'time
  (lambda ()
    (call-with-values
	(lambda ()
	  (time+ticks))
      (lambda (tme tcks)
	(and (number? tme)
	     (number? tcks))))))

(add-test! 'ticks/sec 'time
  (lambda ()
    (real? (ticks/sec))))

(add-test! 'date 'time
  (lambda ()
    (date? (date))))

(add-test! 'time 'time
  (lambda ()
    (integer? (time))))

(add-test! 'date->string 'time
  (lambda ()
    (string? (date->string (date)))))

(add-test! 'format-date 'time
  (lambda ()
    (string? (format-date "~a ~A ~b ~B ~c ~d ~H ~I ~j ~m ~M ~p ~S ~U ~w ~W ~x ~X ~y ~Y ~Z" 
			  (date)))))

;;; fill-in-date! is not implemented yet. 
;(add-test! 'fill-in-date! 'time
;	   (lambda ()
;	     (date? (fill-in-date! (date)))))
