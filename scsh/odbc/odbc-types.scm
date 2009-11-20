(define-record-type :sql-date
  (make-sql-date year month day)
  sql-date?
  (year sql-date-year)
  (month sql-date-month)
  (day sql-date-day))

(define-record-discloser :sql-date
  (lambda (rec)
    `(sql-date year ,(sql-date-year rec)
	       month ,(sql-date-month rec)
	       day ,(sql-date-day rec))))

(define-exported-binding "sql-date-record-type" :sql-date)

(define-record-type :sql-time
  (make-sql-time hour minute second)
  sql-time?
  (hour sql-time-hour)
  (minute sql-time-minute)
  (second sql-time-second))

(define-record-discloser :sql-time
  (lambda (rec)
    `(sql-time hour ,(sql-time-hour rec)
	       minute ,(sql-time-minute rec)
	       second ,(sql-time-second rec))))

(define-exported-binding "sql-time-record-type" :sql-time)

(define-record-type :sql-timestamp
  (make-sql-timestamp year month day hour minute second fraction)
  sql-timestamp?
  (year sql-timestamp-year)
  (month sql-timestamp-month)
  (day sql-timestamp-day)
  (hour sql-timestamp-hour)
  (minute sql-timestamp-minute)
  (second sql-timestamp-second)
  (fraction sql-timestamp-fraction))

(define-record-discloser :sql-timestamp
  (lambda (rec)
    `(sql-timestamp year ,(sql-timestamp-year rec)
		    month ,(sql-timestamp-month rec)
		    day ,(sql-timestamp-day rec)
		    hour ,(sql-timestamp-hour rec)
		    minute ,(sql-timestamp-minute rec)
		    second ,(sql-timestamp-second rec)
		    fraction ,(sql-timestamp-fraction rec))))

(define-exported-binding "sql-timestamp-record-type" :sql-timestamp)

(define-record-type :sql-numeric
  (make-sql-numeric precision scale sign value)
  sql-numeric?
  (precision sql-numeric-precision)
  (scale sql-numeric-scale)
  (sign sql-numeric-sign)
  (value sql-numeric-value))

(define-record-discloser :sql-numeric
  (lambda (rec)
    `(sql-numeric precision ,(sql-numeric-precision rec)
		  scale ,(sql-numeric-scale rec)
		  sign ,(sql-numeric-sign rec)
		  value ,(sql-numeric-value rec))))

(define-exported-binding "sql-numeric" :sql-numeric)

