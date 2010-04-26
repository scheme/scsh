(define-record-type :resource
  (make-resource align! lock)
  resource?
  (align! resource-align!)
  (lock resource-lock))

(define (with-resources-aligned resources thunk)
   (let ((locks (map resource-lock resources)))
     (apply obtain-all-or-none locks)
     (for-each
      (lambda (align!) (align!))
      (map resource-align! resources))
     (let ((val (with-handler
                 (lambda (cond more)
                   (for-each release-lock locks)
                   (more))
                 thunk)))
       (for-each release-lock locks)
       val)))
