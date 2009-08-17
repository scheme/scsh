;;; An implemenation of ScshQL (or Scsh-SQL ?) with the new ODBC bindings
;;; 
;;; Eric Knauel <knauel@informatik.uni-tuebingen.de>
;;;
;;; The original API was designed by Sam Thibault (and maybe Olin Shivers) and
;;; can be found at http://web.mit.edu/samt/www/compsci/Scsh-SQL/scsh-sql.tar.gz

(define *current-db* (make-fluid #f))
(define (current-db) (fluid *current-db*))

(define (with-current-db* db thunk)
  (let-fluid *current-db* db thunk))

(define-syntax with-current-db
  (syntax-rules ()
    ((with-current-db db body1 body2 ...)
     (with-current-db* db (lambda () body1 body2 ...)))))

(define (with-open-db* dbname user pwd thunk)
  (let* ((conn-handle (open-db dbname user pwd)))
    (dynamic-wind 
     (lambda () #t)
     (lambda () 
       (let-fluid *current-db* db thunk))
     (lambda ()
       (odbc-sql-disconnect conn-handle)))))

(define-syntax with-open-db
  (syntax-rules ()
    ((with-open-db dbname user pwd body1 body2 ...)
     (with-open-db* dbname user pwd (lambda () body1 body2 ...)))))

(define (open-db database user password)
  (let* ((env-handle (odbc-alloc-environment-handle))
	 (conn-handle (odbc-alloc-connection-handle env-handle))
	 (db (make-db database user password conn-handle)))
    (begin
      (odbc-sql-connect conn-handle database user password)
      db)))

(define (set-current-db! thing)
  (if (db? thing)
      (set-fluid! *current-db* thing)
      (error "Error: set-current-db! must be called with a db as argument."))
  thing)

(define (close-db . maybe-db)
  (let* ((db (:optional maybe-db (current-db)))
	 (conn-handle (db:con db)))
    (if (connection-handle-connected? conn-handle)
	(odbc-sql-disconnect conn-handle))
    #t))

(define (call/db database user password proc)
  (let ((db (open-db database user password)))
    (proc db)
    (close-db db)))

;;; what a useless function
(define (string->sql-command sql-query)
  (if (current-db)
      (odbc-alloc-statement-handle (db:con db))
      (error "Error: You need to connect to a database first (don't ask why)")))

(define (prepare-execute-sql-args excute-sql-optionals)
  (if (null? excute-sql-optionals)
      (values (current-db) '())
      (values (car args) (cdr args))))

(define (maybe-make-statement-handle string-or-stmt-handle)
  (if (string? string-or-stmt-handle)
      (string->sql-command string->sql-command)
      string-or-stmt-handle))

(define (execute-sql command . args)
  (let-values* (((db params) (prepare-execute-sql-args args))
		((conn-handle) (db:con db))
		((stmt-handle) (maybe-make-statement-handle command)))
    ;;; maybe prepare stmt
    (if (string? command)
	(odbc-sql-prepare stmt-handle command))
    ;;; execute stmt
    (let ((ncols (odbc-sql-num-result-cols stmt-handle)))
      (if (not (null? params))
	  (bind-params stmt-handle params))
      ;;; execute
      (odbc-sql-execute stmt-handle)
      ;;; process answer
      (let ((rows-affected (odbc-sql-row-count stmt-handle)))
	(cond ((> rows-affected 0)
	       rows-affected)  ;;; must have been a DELETE, INSERT or UPDATE statement 
	      ((zero? rows-affected) #t) ;;; is this really correct?!?!?!
	      (else
	       (prepare-cursor stmt-handle ncols)))))))

;;; convert execute-sql params list to a parameter vector that is
;;; suitable for odbc-sql-bind-parameter-exec-out
;;; Well, acutally it's only an approximation
(define (make-parameter-vector stmt-handle params)
  (let ((param-descriptions (get-parameter-descriptions stmt-handle)))
    (list->vector (make-parameter-value-cell-list params param-descriptions))))

(define (make-parameter-value-cell-list params param-descriptions)
  (let ((param-descriptions-count (length param-descriptions)))
    (let loop ((params params) (index 1) (res '()))
      (if (null? params)
	  (reverse res)
	  (if (> index param-descriptions-count)
	      (loop (cdr params) (+ index 1)
		    (make-parameter-value-cell value (list-ref index param-descriptions)))
	      ;;; more parameters given than needed => error
	      ;;; don't catch error, let ODBC raise the error
	      (loop (cdr params) (+ index 1)
		    (cons value (cons sql-type-c-binary sql-type-unknown))))))))

;;; takes the value for parameter and the parameter-description
;;; (a record of type odbc-parameter) and returns a cell that is
;;; suitable to put in a parameter vector for odbc-sql-bind-parameter-exec-out
(define (make-parameter-value-cell value param-description)
  (let ((parameter-type (odbc-parameter-type param-description)))
    (cons value
	  (cons (odbc-type-identifier->c-type-identifier parameter-type)
		parameter-type))))

;;; returns a list of odbc-parameter records for all parameters
;;; of stmt-handle
(define (get-parameter-descriptions stmt-handle)
  (let ((param-count (odbc-sql-num-params stmt-handle)))
    (let loop ((param-no param-count) (res '()))
      (if (zero? param-no)
	  res
	  (loop (- param-no 1) 
		(cons (odbc-sql-describe-param stmt-handle param-no)))))))

(define (bind-params stmt params)
  (let ((parameter-vector (make-parameter-vector stmt-handle params)))
    (odbc-sql-bind-parameter-exec-out stmt-handle parameter-vector)))

(define (prepare-cursor stmt ncols)
  (let* ((table-desc-cols (prepare-cursor-get-col-descriptions stmt ncols))
	 (table-desc (make-table-desc stmt (list->vector table-desc-cols)))
	 (col-procs (prepare-cursor-make-col-procs stmt ncols (table-desc:cols table-desc))))
    (really-make-cursor table-desc ncols stmt #f col-procs)))

(define (prepare-cursor-get-col-descriptions stmt ncols)
  (let loop ((index ncols) (res '()))
    (if (zero? index)
	res
	(loop (+ index 1) 
	      (cons (odbc-sql-describe-col stmt index) res)))))

(define (prepare-cursor-make-col-procs stmt ncols table-desc-vector)
  (let loop ((index ncols) (res '()))
    (if (zero? index)
	res
	(loop (+ index 1)
	      (cons (odbc-sql-bind-col 
		     stmt index 
		     (odbc-type-identifier->c-type-identifier (vector-ref index table-desc-vector))
		     1024)
		    res)))))

(define (fetch-row cursor)
  (let loop ((index (cursor:nols cursor)) (res '()))
    (if (zero? index)
	(list->vector res)
	(loop (+ index 1) 
	      (cons ((vector-ref index (cursor:col-procs cursor))) res)))))

(define (fetch-rows cursor nrows)
  (let loop ((index nrows) (res '()))
    (if (zero? index)
	res
	(cond ((fetch-row cursor)
	       => (lambda (row)
		    (loop (- nrows 1) (cons row res))))))))

(define (fetch-all cursor)
  (error "Not implemented"))

;;; Cursors


(define (close-cursor cursor)