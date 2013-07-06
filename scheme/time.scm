;;; Time interface for scsh.
;;; Copyright (c) 1994 by Olin Shivers.

;;; Should I have a (FILL-IN-DATE! date) procedure that fills in
;;; the redundant info in a date record?
;;; - month-day & month defined -> week-day & year-day filled in.
;;; - month-day and year-day filled in from week-day and year-day
;;;   (not provided by mktime(), but can be synthesized)
;;; - If tz-secs and tz-name not defined, filled in from current time zone.
;;; - If tz-name not defined, fabbed from tz-secs.
;;; - If tz-secs not defined, filled in from tz-name.

;;; A TIME is an instant in the history of the universe; it is location
;;; independent, barring relativistic effects. It is measured as the
;;; number of seconds elapsed since "epoch" -- January 1, 1970 UTC.

;;; A DATE is a *local* name for an instant in time -- which instant
;;; it names depends on your time zone (February 23, 1994 4:37 pm happens
;;; at different moments in Boston and Hong Kong).

;;; DATE definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; We hack this so the date maker can take take the last three slots
;;; as optional arguments.

(import-dynamic-externals "=scshexternal/time")

(define-record-type :date
  (really-make-date seconds minute hour month-day month year
                    tz-name tz-secs summer? week-day year-day)
  date?
  (seconds date:seconds set-date:seconds)
  (minute date:minute set-date:minute)
  (hour date:hour set-date:hour)
  (month-day date:month-day set-date:month-day)
  (month date:month set-date:month)
  (year date:year set-date:year)
  (tz-name date:tz-name set-date:tz-name)
  (tz-secs date:tz-secs set-date:tz-secs)
  (summer? date:summer? set-date:summer?)
  (week-day date:week-day set-date:week-day)
  (year-day date:year-day set-date:year-day))

(define (make-date s mi h md mo y . args)
  (let-optionals args ((tzn #f) (tzs #f) (s?  #f) (wd  0)  (yd  0))
                 (really-make-date s mi h md mo y tzn tzs s? wd yd)))

;;; Not exported to interface.
(define (time-zone? x)
  (or (integer? x)  ; Seconds offset from UTC.
      (string? x) ; Time zone name, e.g. "EDT"
      (not x)))   ; Local time

;;; Time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                                        ; TICKS/SEC is defined in OS-dependent code.
                                        ; C fun is OS-dependent
                                        ; TODO: all C files are identical, so move it to time1.c
                                        ; returns (list secs ticks)
(import-lambda-definition-2 %time+ticks () "time_plus_ticks")

(define (time+ticks)
  (apply values (%time+ticks)))

(define (time+ticks->time secs ticks)
  (+ secs (/ ticks (ticks/sec))))

(import-lambda-definition-2 %time () "scheme_time")

(import-lambda-definition-2 %date->time
                            (sec min hour month-day month year
                                 tz-name  ; #f or string
                                 tz-secs  ; #f or int
                                 summer?) "date2time")

(define (time . args) ; optional arg [date]
  (if (pair? args)
      (if (null? (cdr args))
          (let ((date (check-arg date? (car args) time)))
            (let ((err?.time
                   (%date->time (date:seconds   date)
                                (date:minute    date)
                                (date:hour      date)
                                (date:month-day date)
                                (date:month     date)
                                (date:year      date)
                                (date:tz-name   date) ; #f or string
                                (date:tz-secs   date) ; #f or int
                                (date:summer?   date))))
              (if (car err?.time)
                  (error "Error converting date to time." args)
                  (cdr err?.time))))
          (error "Too many arguments to TIME procedure" args))
      (%time))) ; Fast path for (time).


;;; Date
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import-lambda-definition-2 %time->date (time zone) "time2date")

(define (date . args) ; Optional args [time zone]
  (let ((time (if (pair? args)
                  (real->exact-integer (check-arg real? (car args) date))
                  (time)))
        (zone (check-arg time-zone?
                         (and (pair? args) (:optional (cdr args) #f))
                         date)))
    (apply
     (lambda (seconds minute hour month-day month
                      year tz-name tz-secs summer? week-day year-day)
       (really-make-date seconds minute hour month-day month
                   year
                   (format-time-zone (or tz-name "UTC") tz-secs)
                   tz-secs summer? week-day year-day))
     (%time->date time (if (string? zone) (string->os-byte-vector zone) zone)))))


;;; Formatting date strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (date->string date) ; Sun Sep 16 01:03:52 1973
  (format-date "~a ~b ~d ~H:~M:~S ~Y" date))

(define (format-date fmt date)
  (check-arg date? date format-date)
  (let ((result
         (%format-date (string->os-byte-vector fmt)
                       (date:seconds   date)
                       (date:minute    date)
                       (date:hour      date)
                       (date:month-day date)
                       (date:month     date)
                       (date:year      date)
                       (string->os-byte-vector (if (string? (date:tz-name date))
                                                   (date:tz-name date)
                                                   (deintegerize-time-zone (date:tz-secs date))))
                       (date:summer?   date)
                       (date:week-day  date)
                       (date:year-day  date))))
    (cond ((not result) (error "~ without argument in format-date" fmt))
          (else result))))

(import-lambda-definition-2 %format-date
                            (fmt seconds minute hour month-day month year tz-name summer? week-day
                                 year-day)
                            "format_date")

(define (compose-8/24 hi-8 lo-24)
  (let ((val (+ (arithmetic-shift hi-8 24) lo-24)))
    (if (zero? (bitwise-and hi-8 #x80)) val
        ;; Oops -- it's a negative 32-bit value.
        ;; Or in all the sign bits.
        (bitwise-ior (bitwise-not #xffffffff)
                     val))))

;;; Render a number as a two-digit base ten numeral.
;;; Pathetic. FORMAT should do this for me.
(define (two-digits n)
  (let ((s (number->string n)))
    (if (= (string-length s) 1)
        (string-append "0" s)
        s)))

;;; If time-zone is an integer, convert to a Posix-format string of the form:
;;;     UTC+hh:mm:ss
(define (deintegerize-time-zone tz)
  (if (integer? tz)
      (format-time-zone "UTC" tz)
      tz))


;;; NAME is a simple time-zone name such as "EST" or "UTC". You get them
;;; back from the Unix time functions as the values of the char *tzname[2]
;;; standard/dst vector. The problem is that these time are ambiguous.
;;; This function makes them unambiguous by tacking on the UTC offset
;;; in Posix format, such as "EST+5". You need to do this for two reasons:
;;; 1. Simple time-zone strings are not recognised at all sites.
;;;    For example, HP-UX doesn't understand "EST", but does understand "EST+5"
;;; 2. Time zones represented as UTC offsets (e.g., "UTC+5") are returned
;;;    back from the Unix time software as just "UTC", which in the example
;;;    just given is 5 hours off. Try setting TZ=UTC+5 and running the date(1)
;;;    program. It will give you EST time, but print the time zone as "UTC".
;;;    Oops.

(define (format-time-zone name offset)
  (if (zero? offset) name
      (receive (sign offset)
               (if (< offset 0)
                   (values #\+ (- offset))      ; Notice the flipped sign
                   (values #\- offset))       ; of SIGN.
               (let* ((offset (modulo offset 86400))  ; seconds/day
                      (h (quotient offset 3600))  ; seconds/hour
                      (m (quotient (modulo offset 3600) 60))
                      (s (modulo offset 60)))
                 (if (zero? s)
                     (if (zero? m)
                         (format #f "~a~a~d" name sign h) ; name+h
                         (format #f "~a~a~a:~a"   ; name+hh:mm
                                 name sign (two-digits h) (two-digits m)))
                     (format #f "~a~a~a:~a:~a"      ; name+hh:mm:ss
                             name sign
                             (two-digits h) (two-digits m) (two-digits s)))))))

(define-direct-constance time-ticks-sec
  initialize-time-ticks-sec
  reinitialize-time-ticks-sec
  ((%ticks/sec CLOCKS_PER_SEC)))

(define (ticks/sec) %ticks/sec)
