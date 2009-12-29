;;; The packages that scsh uses/defines.
;;; Copyright (c) 1994 by Olin Shivers.

;;; Note: field-reader package (fr.scm) and here docs use READ-LINE.
;;; It is defined in rdelim.scm.

;;; You link up a scsh package by defining a package named OS-DEPENDENT
;;; that satisfies the interfaces for packages
;;;     buffered-io-flags
;;;     posix-fdflags
;;;     posix-errno
;;;     posix-signals
;;; Anything else it provides should be specified in an interface called
;;; os-extras-interface. See the scsh structure below.
;;; Then the scsh structure can be instantiated.
;;;
;;; The architecture directories, like next/ and irix/ and so forth,
;;; provide packages that can serve as the os-dependent package. E.g.,
;;; the next-defs package, defined in next/packages.
;;;
;;; This whole mechanism would be better solved with a functor.
;;;     -Olin

(define-structure let-opt-expanders let-opt-expanders-interface
  (open scheme (subset signals (error warn)) srfi-8)
  (files let-opt-expanders))

(define-structure let-opt let-opt-interface
  (open scheme (subset signals (error warn)) receiving)
  (for-syntax (open scheme let-opt-expanders))
  (files let-opt))

(define-structure scsh-utilities scsh-utilities-interface
  (open (subset srfi-1 (fold delete))
        (subset define-record-types (define-record-discloser))
        scheme bitwise (subset signals (error warn)) loopholes let-opt
        srfi-9 records record-types
        threads threads-internal placeholders locks)
  (files utilities))

(define-structure weak-tables weak-tables-interface
  (open scheme
        weak
        tables)
  (files weaktables))

(define-structure string-collectors string-collectors-interface
  (open scheme srfi-9)
  (files stringcoll))

(define-structure thread-fluids thread-fluids-interface
  (open scheme srfi-9 weak
        (subset define-record-types (define-record-discloser))
        threads primitives)
  (files thread-fluid))

(define-structure condition-handler condition-handler-interface
  (open scheme display-conditions conditions)
  (files condition-handler))

(define-structure delimited-readers delimited-readers-interface
  (open scheme
        byte-vectors
        (subset primitives (immutable?))
        (subset signals (error warn))
        let-opt
        receiving
        re-level-0 rx-syntax
        (subset srfi-14 (char-set x->char-set char-set-contains?))
        ascii
        i/o-internal ports)
  (files rdelim))

(define list-lib srfi-1)
(define string-lib srfi-13)
(define char-set-lib srfi-14)

;;; This guy goes into the FOR-SYNTAX part of scsh's syntax exports.
(define-structure scsh-syntax-helpers
  (export transcribe-extended-process-form)
  (open receiving       ; receive
        (subset signals (error warn))
        (subset names (generated?)) ; generated? by JMG
        scsh-utilities  ; check-arg
        scheme
        )
  (files syntax-helpers))

;;; The bufpol/{block, line, none} values
(define-structure buffered-io-flags buffered-io-flags-interface
  (open defenum-package scheme)
  (files bufpol))


(define-structures ((tty-flags tty-flags-interface)
                    (scsh-internal-tty-flags scsh-internal-tty-flags-interface))
  (open scheme ascii bitwise)
  (files tty-consts))



(define-structure scsh-version scsh-version-interface
  (open scheme)
  (files scsh-version))

;;; The scsh-level-0 package is for implementation convenience.
;;; The scsh startup and top-level modules need access to scsh
;;; procedures, but they export procedures that are themselves
;;; part of scsh. So scsh-level-0 is the core scsh stuff, which is
;;; imported by these two modules. These modules all collectively
;;; export the whole scsh enchilada.

(define-structures
  ((scsh-level-0
    (compound-interface scsh-delimited-readers-interface
                        scsh-errors-interface
                        scsh-io-interface
                        scsh-file-interface
                        scsh-process-interface
                        scsh-process-state-interface
                        scsh-user/group-db-interface
                        scsh-command-line-interface
                        scsh-signals-interface
                        scsh-environment-interface
                        scsh-home-interface
                        scsh-string-interface
                        scsh-file-names-interface
                        scsh-misc-interface
                        scsh-high-level-process-interface
                        tty-interface ; new in 0.4
                        scsh-version-interface
                        (interface-of srfi-14) ;; export this here for
                        (export ->char-set)    ;; this kludge
                        ;; This stuff would probably be better off kept
                        ;; in separate modules, but we'll toss it in for now.
                        (interface-of ascii) ; char<->ascii
                        string-ports-interface
                        uname-interface
                        ))
   (scsh-level-0-internals (export set-command-line-args!
                                   init-scsh-hindbrain
                                   initialize-cwd
                                   init-scsh-vars))
   (sigevents sigevents-interface))
  (for-syntax (open scsh-syntax-helpers scheme))
  (open (subset srfi-1 (any delete filter fold last reverse!))
        (subset srfi-13 (string<= string-join string-index string-index-right))
        (subset define-record-types (define-record-discloser))
        (subset sort (sort-list!))
        enumerated
        posix-processes
        posix-process-data
        posix-files
        posix-i/o
        posix-users
        posix-errnos
        posix-platform-names
        os-strings
        defenum-package
        external-calls
        load-dynamic-externals
        receiving
        srfi-9
        (modify formats (rename (format s48-format)))
        string-collectors
        delimited-readers
        buffered-io-flags		; stdio dependent
        ascii
        records
        extended-ports
        ports
        build
        bigbit
        bitwise
        (subset signals (error warn))
        (subset exceptions (with-exception-handler raise))
        conditions
        scsh-utilities
        handle
        fluids thread-fluids
        weak-tables
        srfi-14
        scsh-version
        tty-flags
        scsh-internal-tty-flags	   ; Not exported
        let-opt			   ; optional-arg parsing & defaulting
        architecture		   ; Was this by JMG ??
        re-level-0
        rx-syntax
        thread-fluids	; For exec-path-list
        loopholes	; For my bogus CALL-TERMINALLY implementation.
        (modify scheme
		(rename (char-ready?  s48-char-ready?)
			(read-char    s48-read-char)
			(display      s48-display)
			(newline      s48-newline)
			(write        s48-write)
			(write-char   s48-write-char)))

        low-interrupt	      ; for sighandler and procobj
	(subset interrupts (enable-interrupts!
                            disable-interrupts!
                            with-interrupts-inhibited))
        ;; all these seem to be for scsh-0.6 JMG
        (modify i/o (rename (force-output s48-force-output)))
        i/o-internal
        channels channel-i/o
        condvars
        proposals
        byte-vectors
        threads
	(subset threads-internal (spawn-on-root
                                  maybe-commit-and-block-on-queue
                                  maybe-commit-and-make-ready
				  thread-continuation
                                  thread-queue-empty?
                                  maybe-dequeue-thread!))
	locks placeholders
        primitives
        escapes
        command-levels
        features
        general-tables
        simple-syntax
        exit-hooks
        display-conditions
	queues)
  (files syntax
         scsh-condition
         syscalls
         fname
         rw
         newports
         fdports
	 event
         procobj                ; New in release 0.4.
         filesys
         fileinfo
         glob
         filemtch
         tty                    ; New in release 0.4.
         pty                    ; New in release 0.4.
         waitcodes
         scsh)
;  (optimize auto-integrate)
  (begin
    ;; work around for SRFI 14 naming fuckage
    (define ->char-set x->char-set))
  )


(define-structure defenum-package (export (define-enum-constant  :syntax)
                                          (define-enum-constants :syntax)
                                          (define-enum-constants-from-zero
                                            :syntax))
  (open scheme)
  (files enumconst)
;  (optimize auto-integrate)
  )

;;; This code opens so many modules of gruesome, low-level S48 internals
;;; that these two modules are segregated into separate packages, each
;;; exporting just two definitions.

(define-structure scsh-startup-package (export dump-scsh-program
                                               dump-scsh
                                               make-scsh-starter
                                               scsh-stand-alone-resumer)
  (open scsh-level-0-internals  ; init-scsh-* set-command-line-args!
        scsh-level-0            ; error-output-port command-line-arguments
        scsh-top-package        ; parse-switches-and-execute
        handle                  ; with-handler
        command-state           ; user-context, command-output
        write-images            ; write-image
        condition-handler       ; simple-condition-handler
        low-level               ; flush-the-symbol-table!
        package-commands-internal
        (subset filenames (translate)) ; translate
        usual-resumer           ; usual-resumer
        environments            ; with-interaction-environment
        fluids-internal            ; JMG: get-dynamic-env
        threads
	(subset threads-internal (wait-for-event))
	queues scheduler
        scsh-utilities
        interrupts
        low-interrupt
        sigevents
        primitives
        (modify scheme (hide call-with-input-file
                             call-with-output-file
                             with-input-from-file
                             with-output-to-file
                             open-input-file
                             open-output-file)))
  (files startup))

(define-structure scsh-top-package (export parse-switches-and-execute
                                           with-scsh-initialized)
  (open (modify command-processor (hide y-or-n?))
        command-levels          ; with-new-session
        conditions
        display-conditions
        ensures-loaded
        environments
        (subset signals (error warn))
        evaluation
        extended-ports
        fluids
        interfaces
        sigevents
        low-interrupt wind
        fluids-internal            ; JMG: get-dynamic-env
        handle                     ; JMG: with-handler
;       package-commands
        interrupts
        i/o
        package-commands-internal
        package-mutation
        packages
        receiving
        scsh-version
        scsh-level-0            ; with-current-input-port error-output-port
                                ; with-current-output-port exit
        scsh-level-0-internals  ; set-command-line-args! init-scsh-vars
        threads
        lib-dirs
        lib-dirs-internal
        (subset srfi-14 (char-set
                         char-set-complement!
                         char-set-contains?
                         string->char-set))
        root-scheduler          ; scheme-exit-now
        exit-hooks
        scheme)
  (files top meta-arg))

(define-structure exit-hooks exit-hooks-interface
  (open scheme
        threads)
  (begin
    (define *exit-hooks* '())
    (define (add-exit-hook! thunk)
      (set! *exit-hooks* (cons thunk *exit-hooks*)))
    (define (call-exit-hooks!)
      (for-each (lambda (thunk) (thunk)) *exit-hooks*))
    (define (call-exit-hooks-and-run thunk)
      (call-exit-hooks!)
      (thunk))))


(define-structure field-reader-package scsh-field-reader-interface
  (open receiving             ; receive
        scsh-utilities          ; deprecated-proc
        (subset signals (error warn))           ; error
        (subset srfi-13 (string-join))
        (subset srfi-14 (char-set?
                         char-set:whitespace
                         char-set
                         x->char-set
                         char-set-complement))
        delimited-readers
        re-exports
        let-opt                 ; optional-arg parsing & defaulting
        scheme
        )
  (files fr)
  ;; Handle a little bit of backwards compatibility.
  (begin (define join-strings (deprecated-proc string-join 'join-strings
                                               "Use SRFI-13 STRING-JOIN.")))
  )


(define-structures
  ((awk-expander-package (export expand-awk expand-awk/obsolete))
   (awk-support-package (export next-range next-:range
                                next-range: next-:range:)))
  (open receiving               ; receive
        ;; scsh-utilities
        (subset srfi-1 (any filter))
        (subset signals (error warn))           ; error
;       scsh-regexp-package
;       re-exports
        sre-syntax-tools
        scheme
        )
  (files awk)
;  (optimize auto-integrate)
)


(define-structure awk-package awk-interface
  (open awk-support-package     ; These packages provide all the stuff
        re-exports              ; that appears in the code produced by
        receiving               ; an awk expansion.
        scheme)
  (for-syntax (open awk-expander-package scheme))
  (begin (define-syntax awk expand-awk)
         (define-syntax awk/posix-string expand-awk/obsolete)))

;;; Exports an AWK macro that is just AWK/POSIX-STRING.
(define-structure obsolete-awk-package (export (awk :syntax))
  (open awk-package)
  (begin (define-syntax awk
           (syntax-rules () ((awk body ...) (awk/posix-string body ....))))))

(define-structure scsh
  (compound-interface (interface-of scsh-level-0)
                      (interface-of scsh-startup-package)
;                     scsh-regexp-interface
                      re-exports-interface
                      scsh-field-reader-interface       ; new in 0.3
;                     scsh-dbm-interface
                      awk-interface
                      char-predicates-interface; Urk -- Some of this is R5RS!
                      ;; dot-locking-interface
                      md5-interface
                      ;; configure-interface
                      lib-dirs-interface
                      )

  (open scsh-level-0
        scsh-level-0-internals
        re-exports
;       scsh-regexp-package
        scsh-startup-package
;       dbm
        awk-package
        field-reader-package
        char-predicates-lib     ; Urk -- Some of this is R5RS!
        ;; dot-locking
        md5
        ;; configure
        lib-dirs
        scheme)

  (access scsh-top-package)
;  (optimize auto-integrate)
  )

(define-structure scheme-with-scsh
  (compound-interface (interface-of scsh)
                      (interface-of scheme))
  (open scsh
        (modify scheme (hide call-with-input-file
                             call-with-output-file
                             with-input-from-file
                             with-output-to-file
                             open-input-file
                             open-output-file))))

(define-structure scsh-here-string-hax (export)
  (open reading
        receiving
        scsh            ; Just need the delimited readers.
        features        ; make-immutable!
        (subset srfi-14 (char-set))
        scheme)
  (files here))

(define-structure simple-syntax (export define-simple-syntax)
  (open scheme)
 (begin (define-syntax define-simple-syntax
          (syntax-rules ()
                        ((define-simple-syntax (name . pattern) result)
                         (define-syntax name (syntax-rules () ((name . pattern) result))))))))


(define-structure low-interrupt low-interrupt-interface
  (open scheme
        enumerated
        bigbit
        bitwise)
  (files low-interrupt))

;(define-structure test-package (export test-proc)
;  (open scsh-regexp-package scheme)
;  (begin (define (test-proc p)
;          (regexp-substitute p
;                             (string-match "(foo)(.*)(bar)" "Hello foo Olin bar quux")
;                             'post 3 1 2 'pre))))


(define-structure scsh-threads
  (export fork/thread
          fork/process
          wait/thread
          wait/process)
  (open scheme
	(subset scsh-level-0     (fork wait))
	(subset threads-internal (wait-for-event))
	(subset thread-fluids    (fork-thread)))
  (files threads))

;; (define-structure dot-locking dot-locking-interface
;;   (open scsh-level-0
;;         scheme
;;         let-opt
;;         threads  ; sleep
;;         random)
;;   (files dot-locking))

(define-structure libscsh (export dump-libscsh-image)
  (open scheme
        external-calls
        (subset i/o (current-error-port))
        (subset extended-ports (make-string-input-port))
        (subset handle (with-handler))
        (subset escapes (with-continuation))
        (subset environments (with-interaction-environment))
        (subset package-commands-internal (user-environment))
        (subset command-levels (user-context start-new-session))
        (subset command-processor (user-command-environment))
        (subset scsh-startup-package (dump-scsh-program)))
  (files libscsh))

(define-structure md5 md5-interface
  (open scheme
        ascii
        (subset define-record-types (define-record-discloser))
        srfi-9
        bitwise
        (subset i/o (read-block))
        (subset srfi-13 (string-fold-right))
        (subset signals (error warn))
        external-calls)
  (files md5))

;; (define-structure configure configure-interface
;;   (open scheme
;;         re-level-0 rx-syntax
;;         (subset srfi-13 (string-join)))
;;   (files configure))

(define-structures ((lib-dirs lib-dirs-interface)
                    (lib-dirs-internal lib-dirs-internal-interface))
  (open scsh-level-0
        scheme
        handle
        scsh-utilities
        (subset srfi-1 (any)))
  (files lib-dirs))