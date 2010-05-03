;;; Environment manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (var . val) / "var=val" rep conversion:

(define (split-env-string var=val)
  (let ((i (string-index var=val #\=)))
    (if i (values (substring var=val 0 i)
                  (substring var=val (+ i 1) (string-length var=val)))
        (error "No \"=\" in environment string" var=val))))

(define (env-list->alist env-list)
  (map (lambda (var=val)
         (call-with-values (lambda () (split-env-string var=val))
                           cons))
       env-list))

(define (alist->env-list alist)
  (map (lambda (var.val)
         (string->os-byte-vector
          (string-append (car var.val) "="
                         (let ((val (cdr var.val)))
                           (if (string? val) val
                               (string-join val ":"))))))
       alist))

(define (alist->env-vec alist)
  (list->vector (alist->env-list alist)))


;;; ENV->ALIST

(import-lambda-definition-2 %load-env () "scm_envvec")

(define (environ-env->alist)
  (let ((env-list.envvec (%load-env)))
    (cons (env-list->alist (car env-list.envvec))
          (cdr env-list.envvec))))


;;; ALIST->ENV

;;; (%create-env ((vector 'X) -> address))
(import-lambda-definition-2 %create-env (envvec) "create_env")

;;; assumes aligned env
(define (envvec-alist->env alist)
  (%create-env (alist->env-vec alist)))

(import-lambda-definition-2 %align-env (envvec) "align_env")

(import-lambda-definition-2 %free-env (envvec) "free_envvec")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment per thread
(define *env-cache* 'uninitialized)
(define env-lock (make-lock))

(define (install-env)
  (set! *env-cache* (environ**-read))
  (set! $env ;;; TODO The old thread-fluid will remain
        (make-preserved-thread-fluid
         (env-cache))))
;  (set! env-lock (make-lock)))

(define (env-cache)
  *env-cache*)

;; Actually do the syscall and update the cache
;; assumes the env lock obtained
(define (change-and-cache-env env)
  (environ**-set env)
  (set! *env-cache* env))

;; The thread-specific env: A thread fluid

(define $env 'empty-env-value)

(define (thread-read-env) (thread-fluid $env))
(define (thread-set-env! res) (set-thread-fluid! $env res))
(define (let-env res thunk)
  (let-thread-fluid $env res thunk))

(define (really-with-env* env thunk)
  (with-lock env-lock
    (lambda ()
      (change-and-cache-env env)))
  (let-env env thunk))

(define (align-env!)
  (let ((res (thread-read-env)))
    (if (not (env=? res (env-cache)))
        (change-and-cache-env res))))

(define (thread-change-env res)
  (with-lock env-lock
    (lambda ()
      (change-and-cache-env res)
      (thread-set-env! (env-cache)))))

(define environ-resource (make-resource align-env! env-lock))

(define env-reinitializer
  (make-reinitializer install-env))


(define-record-type :env
  (make-env envvec alist)
  env?
  (envvec env:envvec set-env:envvec)
  (alist env:alist))

(define-record-resumer :env
  (lambda (env)
    (set-env:envvec env #f)))

(define (env=? e1 e2)
  (and (env:envvec e1)
       (eq? (env:envvec e1)
            (env:envvec e2))))

(define-record-type :envvec
  (make-envvec environ)
  envvec?
  (environ envvec:environ))

(define (add-envvec-finalizer! envvec)
  (add-finalizer! envvec envvec-finalizer))

(define-exported-binding "envvec-record-type" :envvec)
(define-exported-binding "add-envvec-finalizer!" add-envvec-finalizer!)

(define (envvec-finalizer envvec)
  (%free-env envvec))

(define (environ**-read)
  (let ((alist.envvec (environ-env->alist)))
    (make-env (cdr alist.envvec) (car alist.envvec))))

(define (environ**-set env)
   (if (env:envvec env)
       (%align-env (env:envvec env))
       (set-env:envvec env (envvec-alist->env (env:alist env)))))

(define (getenv var)
  (let* ((env (thread-read-env))
         (res (assoc var (env:alist env))))
    (if res (cdr res) res)))

(define (env->alist)
  (env:alist (thread-read-env)))

(define (setenv var val)
  (let* ((env (thread-read-env))
         (alist (if val
                    (alist-update
                     var
                     val
                    (env:alist env))
                    (alist-delete
                     var
                     (env:alist env)))))
    (thread-set-env!
     (make-env
      #f
      alist))))

(define (alist->env alist)
  (thread-set-env!
   (make-env
    #f
    alist)))

(define (with-env* alist-delta thunk)
  (let ((new-env (fold (lambda (key/val alist)
                          (alist-update (car key/val) (cdr key/val) alist))
                        (env->alist)
                        alist-delta)))
    (with-total-env* new-env thunk)))

(define (with-total-env* alist thunk)
  (really-with-env* (make-env #f alist) thunk))

(define (alist-update key val alist)
  (cons (cons key val)
        (alist-delete key alist)))

;;; Remove shadowed entries from ALIST. Preserves element order.
;;; (This version shares no structure.)

(define (alist-compress alist)
  (reverse (let compress ((alist alist) (ans '()))
             (if (pair? alist)
                 (let ((key/val (car alist))
                       (alist (cdr alist)))
                   (compress alist (if (assoc (car key/val) ans) ans
                                       (cons key/val ans))))
                 ans))))

(define (add-before elt before list)
  (let rec ((list list))
    (if (pair? list)
        (let ((x (car list)))
          (if (equal? x before)
              (cons elt list)
              (cons x (rec (cdr list)))))
        (cons elt list))))

;;; In ADD-AFTER, the labelled LET adds ELT after the last occurrence of AFTER
;;; in LIST, and returns the list. However, if the LET finds no occurrence
;;; of AFTER in LIST, it returns #F instead.

(define (add-after elt after list)
  (or (let rec ((list list))
        (if (pair? list)
            (let* ((x (car list))
                   (tail (cdr list))
                   (ans (rec tail))) ; #f if AFTER wasn't encountered.
              (cond (ans (cons x ans))
                    ((equal? x after)
                     (cons x (cons elt tail)))
                    (else #f)))         ; AFTER doesn't appear in LIST.
            #f))                        ; AFTER doesn't appear in LIST.
      (cons elt list)))

(define-simple-syntax (with-env delta . body)
  (with-env* `delta (lambda () . body)))

(define-simple-syntax (with-total-env env . body)
  (with-total-env* `env (lambda () . body)))

(install-env)
