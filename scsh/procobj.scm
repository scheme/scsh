;;; Unix waitt & process objects for scsh
;;; Copyright (c) 1993, 1994, 1995 by Olin Shivers.

;;; This is a GC'd abstraction for Unix process id's.
; ;; The problem with Unix pids is (a) they clutter up the kernel
;;; process table until you wait(2) them, and (b) you can only
;;; wait(2) them once. Scsh's process objects are similar, but
;;; allow the storage to be allocated in the scsh address space,
;;; and out of the kernel process table, and they can be waited on
;;; multiple times.

;;; Process objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record proc		; A process object
  pid		; Proc's pid.
  (finished? #f)                ; Running, stopped, done
  (status (make-placeholder))	; The cached exit status of the process
  (zombie #t)                   ; Misnomer.  Whether or not the process has
                                ; (not) been waited on.
  ;; Make proc objects print like #{proc 2318}.
  ((disclose p) (list "proc" (proc:pid p) (proc:finished? p))))

;; Unfortunately there is no way to specify the name of the constructor-
;; function in Olins define-record macro, so I had to do this...
(define (make-procobj pid)
  (let ((procobj (make-proc pid)))
    (add-finalizer! procobj procobj-finalizer)
    procobj))


;; Weak pointer tables.  Much more efficient than populations.
;; Maps pids to processes.  Unexited processes are strong pointers, exited
;; procs are weak pointers (to allow gc'ing).
;;
;; JMG: why ever unexited processes were strong pointer, this won't work
;; with (autoreap-policy 'late), since then gc waits for the strong pointer
;; until it wait(2)s and the strong pointer waits for wait(2) which is
;; nothing but a deadlock

(define process-table (make-integer-table))
(make-reinitializer (lambda ()
		      (set! process-table (make-integer-table))))

(define process-table-lock (make-lock))
(define (process-table-ref n)
  (with-lock process-table-lock
	     (lambda ()
	       (weak-table-ref process-table n))))

(define (process-table-set! n val)
  (with-lock process-table-lock
	     (lambda ()
	       (weak-table-set! process-table n val))))

(define (process-table-delete-procobj! procobj)
  (with-lock process-table-lock
	     (lambda ()
	       (if (eq? (weak-table-ref process-table (proc:pid procobj))
			procobj)
		   (weak-table-set! process-table (proc:pid procobj) #f)))))

(define (maybe-pid->proc pid)
  (process-table-ref pid))

(define (pid->proc pid . maybe-probe?)
  (let ((probe? (:optional maybe-probe? #f)))
    (or (maybe-pid->proc pid)
	(case probe?
	  ((#f)     (error "Pid has no corresponding process object" pid))
	  ((create) (new-child-proc pid))
	  (else     #f)))))

;;; Coerce pids and procs to procs.

(define (->proc proc/pid)
  (cond ((proc? proc/pid) proc/pid)
	((and (integer? proc/pid) (>= proc/pid 0))
	 (pid->proc proc/pid 'create))
	(else (error "Illegal parameter" ->proc proc/pid))))


;;; Is X a pid or a proc?

(define (pid/proc? x) (or (proc? x) (and (integer? x) (>= x 0))))


;;; Process reaping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "Reaping" a process means using wait(2) to move its exit status from the
;;; kernel's process table into scsh, thus cleaning up the kernel's process
;;; table and saving the value in a gc'd data structure, where it can be
;;; referenced multiple times.
;;;
;;; - Stopped processes are never reaped, only dead ones.  (May change -df)
;;;
;;; - Stopped process status codes are never cached in proc objects,
;;;   only status codes for dead processes. So you can wait for a
;;;   dead process multiple times, but only once per process-stop.
;;;   (May change -df)
;;;
;;; - Unfortunately, reaping a process loses the information specifying its
;;;   process group, so if a process is reaped into scsh, it cannot be
;;;   waited for by WAIT-PROCESS-GROUP. Notice that only dead processes are
;;;   reaped, not suspended ones. Programs almost never use WAIT-PROCESS-GROUP
;;;   to wait for dead processes, so this is not likely to be a problem. If
;;;   it is, turn autoreaping off with (autoreap-policy #f).
;;;   (This never worked right, and it might be wiped out completely -fd)
;;;
;;; - Reaping can be encouraged by calling (REAP-ZOMBIES).

;;; (autoreap-policy [new-policy])
;;; Watch this area


;;; I'm really tired of opening everything (i.e. events) in scsh-level-0
;;; this is here until someone (Olin !!!) cleans up the scsh modules

(define next-sigevent (structure-ref sigevents next-sigevent))
(define most-recent-sigevent (structure-ref sigevents most-recent-sigevent))


(define *autoreap-policy* #f) ; Not exported from this module.

(define (autoreap-policy . maybe-policy)
  (let ((old-policy *autoreap-policy*))
    (if (pair? maybe-policy)
	(let ((new-policy (car maybe-policy)))
	  (cond ((pair? (cdr maybe-policy))
		 (error "Too many args to autoreap-policy" maybe-policy))
		((not (memq new-policy '(early late #f)))
		 (error "Illegal autoreap policy." new-policy))
		(else (set! *autoreap-policy* new-policy)
		      (cond ((eq? new-policy 'early)
			     (set-sigchld-handler! early-sigchld-handler)
			     (set-post/gc-handler! reap-need-reaping))

			    ((eq? new-policy 'late)
			     (set-sigchld-handler! late-sigchld-handler)
			     (set-post/gc-handler! reap-need-reaping))

			    (else
			     (set-sigchld-handler! noauto-sigchld-handler)
			     (set-post/gc-handler!
			      (lambda ()
				#f))))))))
    old-policy))


;;; we don't register the post/gc-handler until the first police change
;;; --- this made sense, but why?
(define *post/gc-handler*
  (lambda () (error "*post/gc-handler* was not defined")))

(define (really-set-post/gc-handler! handler)
   (set! *post/gc-handler* handler))

(define (start-set-post/gc-handler! handler)
  (set! set-post/gc-handler! really-set-post/gc-handler!)
  (set-post/gc-handler! handler)
  (spawn (lambda ()
	   (let lp ((event (most-recent-sigevent)))
	     (let ((next-event (next-sigevent event interrupt/post-gc)))
	       (*post/gc-handler*)
 	       (lp next-event))))
 	 '*post/gc-handler*-thread))

(define set-post/gc-handler! start-set-post/gc-handler!)


(define (*sigchld-handler*) (early-sigchld-handler))
(define (set-sigchld-handler! handler)
  (set! *sigchld-handler* handler))

(define (with-autoreaping thunk)
  (set! *autoreap-policy* 'early)
  (run-as-long-as
   (lambda ()
     (let lp ((event (most-recent-sigevent)))
       (let ((next-event (next-sigevent event (signal chld))))
	 (*sigchld-handler*)
	 (lp next-event))))
   thunk
   (structure-ref threads-internal spawn-on-root)
   'auto-reaping))

;;; This list contains pids whose proc-obj were gc'd before they died
;;; We try to reap them after every gc and maybe on every SIGCHLD
(define need-reaping '())

(define need-reaping-lock (make-lock))

(define (need-reaping-add! pid)
  (obtain-lock need-reaping-lock)
  (set! need-reaping (cons pid need-reaping))
  (release-lock need-reaping-lock))

(define (need-reaping-remove! pid)
  (obtain-lock need-reaping-lock)
  (set! need-reaping (delete pid need-reaping))
  (release-lock need-reaping-lock))

(define (reap-need-reaping)
  (obtain-lock need-reaping-lock)
  (set! need-reaping (filter (lambda (pid) (not (reap-pid pid))) need-reaping))
  (release-lock need-reaping-lock))

;;; reap this special pid
;;; return status or #f
(define (reap-pid pid)
  (with-lock
   wait-lock
   (lambda ()
     (let ((status (atomic-wait pid wait/poll)))
       (if status
	   (waited-by-reap pid status))
       status))))

;;; Handler for SIGCHLD according policy
(define (late-sigchld-handler) #f)

(define (early-sigchld-handler)
  (reap-zombies))

(define (noauto-sigchld-handler) #f)


;;; Finalizer for procobjs
;;;
(define (procobj-finalizer procobj)
  (process-table-delete-procobj! procobj)
  (if (not (proc:finished? procobj))
      (need-reaping-add! (proc:pid procobj))))


;;; (reap-zombies)  => bool
;;;   Move any zombies from the kernel process table into scsh.
;;;   Return true if no more outstanding children; #f if some still live.

(define (reap-zombies)
  (let lp ()
    (obtain-lock wait-lock)
    (receive (pid status)
      (%wait-any (bitwise-ior wait/poll wait/stopped-children))
      (if (and pid (not (status:stop-sig status)))
	  (begin (waited-by-reap pid status)
		 (release-lock wait-lock)
;		 (format (current-error-port)
;			 "Reaping ~d[~d]~%" pid status)
		 (lp))
	  (begin
	    (release-lock wait-lock)
	    status)))))



(define (new-child-proc pid)
  (let ((proc (make-procobj pid)))
    (process-table-set! pid proc)
    proc))

;;; (WAIT proc/pid [flags])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (wait proc/pid [flags]) => status or #f
;;;
;;; FLAGS (default 0) is the exclusive or of the following:
;;;     wait/poll
;;;		Return #f immediately if there are no
;;;		unwaited children available.
;;; 	wait/stopped-children
;;; 		Report on suspended children as well.
;;;
;;;     If the process hasn't terminated (or suspended, if wait/stopped
;;; 	is set) and wait/poll is set, return #f.
;;; (I'm working on the flags -df)

;;; JMG: We have to be careful about wait/poll and autoreap-policy:
;;; If it was 'late at anytime, we may missed the exit of pid/proc
;;; So we cannot just block and hope reap-zombies will give us the status


;;; With this lock, we ensure that only one thread may call
;;; really-wait for a given pid and manipulates the associated process object

(define wait-lock (make-lock))

(define (wait pid/proc . maybe-flags)
  (let* ((flags (:optional maybe-flags 0))
	 (proc (->proc pid/proc))
	 (win (lambda (status)
		(waited-by-wait proc status)
		status)))
    ;; save the event before we check for finished
    (let ((pre-event (most-recent-sigevent)))
      (with-lock
       wait-lock
       (lambda ()
	 (cond ((atomic-wait proc (bitwise-ior flags wait/poll)) => win)

	       ((zero? (bitwise-and flags wait/poll))
		;; we have to block and hence use the event system
		(let lp ((pre-event pre-event))
		  (cond ((atomic-wait proc (bitwise-ior flags wait/poll))
			 => win)
			(else
			 (release-lock wait-lock)
			 (let ((next-event (next-sigevent pre-event (signal chld))))
			   (obtain-lock wait-lock)
			   (lp next-event))))))
	       (else #f)))))))


;;; -> process-object proc status/#f
(define (atomic-wait proc flags)
  (cond ((proc:finished? proc)
	 (placeholder-value (proc:status proc)))
	(else (really-wait (proc:pid proc) (bitwise-ior flags wait/poll)))))

;;; This one is used, to wait on a positive pid
;;; We NEVER do a blocking wait syscall
(define (really-wait pid flags)
  (if (zero? (bitwise-and flags wait/poll))
      (error "really-wait without wait/poll"))
  (if (< pid 1)
      (error "really-wait on nonpos pid" pid))
  (receive (return_pid status)
      (%wait-pid pid flags)
   (cond ((zero? return_pid) #f)      ; failed wait/poll
	 ((= pid return_pid) status)  ; made it
	 (else (error "mismatch in really-wait"
		      return_pid pid)))))



;;; All you have to do, if pid was reaped
;;; proc_obj is maybe no longer alive
(define (waited-by-reap pid status)
  (cond ((maybe-pid->proc pid) =>
	 (lambda (proc)
	   (obituary proc status)
	   (push-reaped-proc proc)
	   ))))


;;; All you have to do, if a wait on proc was successful
(define (waited-by-wait proc status)
  (if (not (status:stop-sig status))
      (begin
	(obituary proc status)
	(mark-proc-waited! proc))))

;;; we know from somewhere that proc is dead
(define (obituary proc status)
  (if (not (proc? proc))
      (error "obituary: proc was not a procobj" proc))
  (need-reaping-remove! (proc:pid proc))     ; in case it started during 'late
  (placeholder-set! (proc:status proc) status)
  (set-proc:finished? proc #t))



;;; (wait-any [flags]) => [proc status]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     [#f #f] => non-blocking, none ready.
;;;     [#f #t] => no more.

(define (wait-any . maybe-flags)
  (let ((flags (:optional maybe-flags 0)))
    (if (zero? (bitwise-and flags wait/poll))
	(begin
	  (receive (pid status)
	     ;; before we maybe block via placeholder-value
	     ;; do a really-wait-any for the ones, missed by 'late
	    (really-wait-any (bitwise-ior flags  wait/poll))
	    (if (not pid)
		(let ((win (get-reaped-proc!)))
		  (values win (placeholder-value (proc:status win))))
		(values pid status))))

	;; The rest of this is quite crude and can be safely ignored. -df
	;; JMG: wait-any is crude and so its implementation
	;; It got even worse, now that we have this fu*$#%g 'late
	(if (maybe-obtain-lock reaped-proc-pop-lock)
	    (if (eq? reaped-proc-head reaped-proc-tail)
		;;; due to 'late we cannot be sure, that they all have been
		;;; reaped
		(begin
		  (release-lock reaped-proc-pop-lock)
		  (really-wait-any flags))
		(let* ((retnode (placeholder-value reaped-proc-head))
		       (retval (weak-pointer-ref (reaped-proc:proc retnode))))
		  (set! reaped-proc-head (reaped-proc:next retnode))
		  (release-lock reaped-proc-pop-lock)
		  (if retval
		      (values retval (placeholder-value (proc:status retval)))
		      (values #f #f))))
	    (values #f #f)))))

(define (really-wait-any flags)
  (if (zero? (bitwise-and flags wait/poll))
      (error "real-wait-any without wait/poll" flags))
  (with-lock
   wait-lock
   (lambda ()
     (receive (pid status)
        (%wait-any flags)
	(if pid
	    (let ((proc (new-child-proc pid)))
	      (waited-by-wait proc status)
	      (values proc status))
	    (values #f #f))))))


;;; (wait-process-group [proc-group flags]) => [proc status]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     [#f #f] => non-blocking, none ready.
;;;     [#f #t] => no more.
;;;
;;;
;;; If you are doing process-group waits, you do *not* want to use
;;; early autoreaping, since the reaper loses process-group information.
;;; (I'm working on it -df)

(define (wait-process-group . args)
  (let-optionals args ((proc-group 0) (flags 0))
    (let ((proc-group (cond ((integer? proc-group) proc-group)
			     ((proc? proc-group)    (proc:pid proc-group))
			     (else (error "Illegal argument" wait-process-group
					  proc-group))))
	  (win (lambda (pid status)
		 (let ((proc (pid->proc pid 'create)))
		   (if proc (waited-by-wait proc status))
		   (values proc status)))))
      ;; save the event before we check for finished
      (let ((pre-event (most-recent-sigevent)))
	(receive (pid status)
          (%wait-process-group proc-group (bitwise-ior flags wait/poll))
          (cond (pid
		 (win pid status))
		((zero? (bitwise-and flags wait/poll))
		 ;; we have to block and hence use the event system
		 (let lp ((pre-event pre-event))
		   (receive (pid status)
		      (%wait-process-group proc-group (bitwise-ior flags wait/poll))
		      (if pid
			  (win pid status)
			  (lp (next-sigevent pre-event (signal chld)))))))
		(else
		 (values #f status))))))))




;;; (%wait-any flags) (%wait-pid pid flags) (%wait-process-group pgrp flags)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Direct interfaces to waitpid(2) call. As opposed to %wait-pid this
;;; waits on any child (using -1) and gets along if no child is alive
;;; at all (i.e. catches errno/child).

;;; [#f #f] means no processes ready on a non-blocking wait.
;;; [#f #t] means no waitable process on wait-any.

(define (%wait-any flags)
  (with-errno-handler
   ((errno packet)
    ((errno/child)
     (values #f #t)))
   (receive (pid status)
       (%wait-pid -1 flags)
    (if (zero? pid)
	(values #f #f)			; None ready.
	(values pid status)))))

(define (%wait-process-group pgrp flags)
  (if (zero? (bitwise-and flags wait/poll))
      (error "really-wait without wait/poll"))
  (with-errno-handler
   ((errno packet)
    ((errno/child)
     (values #f #t)))
   (receive (pid status)
	(%wait-pid (- pgrp) flags)
     (if (zero? pid)
	 (values #f #f)			; None ready.
	 (values pid status)))))


;;; Reaped process table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; We keep track of procs that have been reaped but not yet waited on by
;;; the user's code. These proces are eligible for return by WAIT-ANY.
;;; We keep track of these so that WAIT-ANY will hand them out exactly once.
;;; What this code needs is traditional condition variables.

;;; This is (so far) reliable in the following ways:
;;; 1. No process will be returned twice by wait-any, ever.  Even two different
;;;    wait-anys.
;;; 2. Being un-reaped will not prevent garbage collection.
;;;    (actually, there seems to be a problem with this -df)
;;; 3. If a process is waited on, or is gc'ed, wait-any will do the Right
;;;    Thing.

;;; And UNreliable in the following ways:
;;; 1. If a wait and a wait-any are blocking simultaneously, the wait will
;;;    always return the object.  However, whether the wait-any will or not
;;;    is based on racing semaphores.
;;; 2. While processes can still be garbage collected, the nodes on the
;;;    wait-any list will not, and if the program never wait-any's, the queue
;;;    will snake around, eating up memory like pac-man with the munchies.
;;; 3. The process may be garbage collected before wait-any gets to it, and
;;;    that's just tough.

;;; -df

(define-record reaped-proc
  proc
  (next (make-placeholder))
  prev)

(define reaped-proc-tail (make-reaped-proc (make-weak-pointer #f) 'head))
(define reaped-proc-head reaped-proc-tail)
(define reaped-proc-push-lock (make-lock))
(define reaped-proc-pop-lock (make-lock))          ;;; Zippy sez: pop lock!

(define (push-reaped-proc proc)
  (obtain-lock reaped-proc-push-lock)
  (let ((push-me (make-reaped-proc (make-weak-pointer proc) reaped-proc-tail)))
    (placeholder-set! (reaped-proc:next reaped-proc-tail) push-me)
    (add-finalizer! proc (make-reaped-proc-finalizer push-me))
    (set! reaped-proc-tail push-me))
  (release-lock reaped-proc-push-lock))

(define (make-reaped-proc-finalizer push-me)
  (lambda ignore
    (remove-reaped-proc push-me)))

(define (remove-reaped-proc reaped-proc)
  (spawn (lambda ()                 ;This is blocking, so should run by itself
	   (set-reaped-proc:prev
	    (placeholder-value (reaped-proc:next reaped-proc))
	    (reaped-proc:prev reaped-proc))
	   (set-reaped-proc:next
	    (reaped-proc:prev reaped-proc)
	    (reaped-proc:next reaped-proc)))
  	 "reaped-proc-removing-thread"))

(define (pop-reaped-proc)
  (obtain-lock reaped-proc-pop-lock)        ;;; pop lock pop lock pop lock!
  (let ((pop-me (placeholder-value (reaped-proc:next reaped-proc-head))))
    (set! reaped-proc-head pop-me)
    (release-lock reaped-proc-pop-lock)
    (weak-pointer-ref (reaped-proc:proc pop-me))))



;;; Pop one off the list.
(define (get-reaped-proc!)
  (let loop ((try (pop-reaped-proc)))
    (if (and try (proc:zombie try))
	try
	(loop (pop-reaped-proc)))))

;;; PROC no longer eligible to be in the list. Delete it.
(define (mark-proc-waited! proc)
  (set-proc:zombie proc #f))
