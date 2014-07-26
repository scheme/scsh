;;; The packages that scsh uses/defines.
;;; Copyright (c) 1994 by Olin Shivers.

;;; Note: field-reader package (fr.scm) and here docs use READ-LINE.
;;; It is defined in rdelim.scm.

(define-structure constance (export (define-constance :syntax)
                                    (define-direct-constance :syntax))
  (open scheme reinitializers
        (subset shared-bindings (lookup-imported-binding shared-binding-ref))
        (subset load-dynamic-externals (import-dynamic-externals)))
  (for-syntax (open scheme srfi-9))
  (files constance))

(define-structure let-opt-expanders let-opt-expanders-interface
  (open scheme (subset signals (error warn)) receiving)
  (files let-opt-expanders))

(define-structure let-opt let-opt-interface
  (open scheme (subset signals (error)))
  (for-syntax (open scheme let-opt-expanders))
  (files let-opt))

(define-structure functional-search-trees functional-search-trees-interface
  (open scheme-level-2
        exceptions
        define-record-types)
  (files functional-search-tree))

(define-structure scsh-utilities scsh-utilities-interface
  (open (subset srfi-1 (fold delete))
        (subset define-record-types (define-record-discloser))
        scheme bitwise (subset signals (error warn)) loopholes let-opt
        srfi-9 records record-types
        threads threads-internal placeholders locks)
  (files utilities))

(define-structure scsh-resources scsh-resources-interface
  (open scheme
        define-record-types
        handle
        locks
        (subset scsh-utilities (obtain-all-or-none)))
  (files resource))

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
        (subset srfi-13 (string-every))
        (subset srfi-14 (char-set x->char-set char-set-contains? char-set:whitespace))
        (subset unicode (scalar-value->char char->scalar-value))
        i/o-internal ports)
  (files rdelim))

;;; This guy goes into the FOR-SYNTAX part of scsh's syntax exports.
(define-structure scsh-syntax-helpers
  (export transcribe-extended-process-form)
  (open receiving       ; receive
        (subset signals (error warn))
        (subset names (generated?)) ; generated? by JMG
        (subset scsh-utilities (check-arg))
        scheme)
  (files syntax-helpers))

(define-structure waitcodes
  (export wait/poll
          wait/stopped-children
          status:exit-val
          status:stop-sig
          status:term-sig)
  (open scheme bitwise)
  (files waitcodes))

(define-structures ((tty-flags tty-flags-interface)
                    (scsh-internal-tty-flags scsh-internal-tty-flags-interface))
  (open scheme ascii bitwise constance (subset srfi-1 (fold)))
  (files tty-consts))

(define-structure scsh-errnos
  (export with-errno-handler*
          errno-error
          (with-errno-handler :syntax))
  (open scheme
        handle
        conditions
        exceptions
        posix-errnos)
  (files scsh-condition))

(define-structure scsh-syscall-support
  (export byte-vector->string
          (define/vector-args :syntax))
  (open scheme
        os-strings
        load-dynamic-externals)
  (files syscall-support))

(define-structure scsh-file-syscalls scsh-file-syscalls-interface
  (open scheme
        scsh-syscall-support
        (subset posix-files (create-symbolic-link))
        (subset external-calls (import-lambda-definition-2)))
  (files file-syscalls))

(define-structure scsh-version scsh-version-interface
  (open scheme)
  (files scsh-version))

(define-structure scsh-environment scsh-environment-interface
  (open scheme
        locks thread-fluids
        define-record-types
        (subset scsh-syscall-support (byte-vector->string))
        (subset os-strings (string->os-byte-vector))
        (subset record-types (define-record-resumer))
        (subset primitives (add-finalizer!))
        records
        (subset signals (error))
        (subset srfi-1 (fold filter alist-delete))
        (subset srfi-13 (string-index string-join))
        (subset scsh-utilities
                (with-lock make-reinitializer define-simple-syntax))
        (subset external-calls (import-lambda-definition-2))
        shared-bindings
        scsh-resources)
  (files environment))

(define-structure scsh-file-names scsh-file-names-interface
  (open scheme
        receiving
        let-opt
        signals
        (subset srfi-1 (reverse!))
        (subset srfi-13 (string-index string-index-right)))
  (files fname))

(define-structure scsh-directories scsh-directories-interface
  (open scheme
        (subset primitives (add-finalizer!))
        (subset srfi-1 (filter))
        (subset srfi-13 (string<=))
        (subset scsh-utilities (check-arg))
        (subset sort (sort-list!))
        define-record-types records
        let-opt
        (subset os-strings (os-string->string))
        (modify posix-files (rename (read-directory-stream
                                     s48-read-directory-stream))
                (expose open-directory-stream
                        close-directory-stream
                        read-directory-stream
                        list-directory))
        scsh-file-names
        scsh-resources
        scsh-process-state)
  (files directory))

(define-structure scsh-user/group-db scsh-user/group-db-interface
  (open scheme
        define-record-types
        receiving
        handle (subset signals (error))
        posix-users
        os-strings
        scsh-file-names
        scsh-environment)
  (files user-group))

(define-structure scsh-process-state scsh-process-state-interface
  (open scheme
        receiving
        let-opt
        locks thread-fluids
        posix-process-data
        posix-files
        posix-users
        (subset posix-processes (process-id->integer))
        os-strings
        (subset channels (set-with-fs-context-aligned*!))
        (subset signals (error))
        (subset scsh-utilities (with-lock make-reinitializer define-simple-syntax))
        scsh-resources
        scsh-file-names
        scsh-user/group-db
        (subset external-calls (import-lambda-definition-2)))
  (files process-state))

(define-structure scsh-newports scsh-newports-interface
  (open (modify scheme (rename (char-ready?  s48-char-ready?)
                               (read-char    s48-read-char)
                               (display      s48-display)
                               (newline      s48-newline)
                               (write        s48-write)
                               (write-char   s48-write-char))
                (hide call-with-input-file
                      call-with-output-file
                      with-input-from-file
                      with-output-to-file
                      open-input-file
                      open-output-file))
        (subset tables (table-set!
                        table-ref
                        make-integer-table))
        (subset weak (make-weak-pointer
                      weak-pointer?
                      weak-pointer-ref))
        (subset enumerated (enum))
        (subset byte-vectors (byte-vector-length))
        (subset placeholders (make-placeholder
                              placeholder-set!
                              placeholder-value))
        receiving
        let-opt
        (modify i/o (hide force-output
                          char-ready?
                          read-char
                          newline
                          write-char)
                (rename (force-output s48-force-output)))
        (modify formats (rename (format s48-format))
                (expose format))
        i/o-internal
        channels
        channel-i/o
        (subset channel-ports (input-channel+closer->port
                               output-channel+closer->port
                               port->channel))
        ports
        (subset threads-internal (thread-continuation))
        (subset posix-i/o (fd-port?
                           port->fd
                           dup))
        (modify posix-files (rename (open-file s48-open-file))
                (expose open-file
                        file-options
                        file-options-union
                        file-mode))
        (subset architecture (channel-status-option))
        (subset interrupts (enable-interrupts!
                            disable-interrupts!
                            with-interrupts-inhibited))
        (subset proposals (with-new-proposal
                           atomically!
                           atomically
                           provisional-cell-ref
                           provisional-cell-set!))
        cells
        (subset functional-search-trees (make-search-tree
                                         search-tree-insert
                                         search-tree-delete
                                         search-tree-ref))
        (subset condvars (make-condvar condvar-value))
        extended-ports
        scsh-utilities
        signals
        threads
        (subset srfi-1 (any filter))
        scsh-file-syscalls
        scsh-resources
        scsh-process-state
        (subset external-calls (import-lambda-definition-2)))
  (files newports))

(define-structure scsh-file scsh-file-interface
  (open (modify scheme (hide call-with-input-file
                             call-with-output-file
                             with-input-from-file
                             with-output-to-file
                             write
                             display
                             char-ready?
                             read-char
                             write-char
                             newline
                             open-input-file
                             open-output-file))
        bitwise
        define-record-types
        let-opt
        (modify i/o (hide force-output
                          newline
                          write-char
                          char-ready?
                          read-char))
        (modify posix-files (hide file-type
                                  file-info?))
        (subset os-strings (os-string->string))
        (subset scsh-utilities (define-simple-syntax deprecated-proc real->exact-integer))
        (subset signals (error))
        scsh-errnos
        scsh-file-syscalls
        scsh-file-names
        scsh-process-state
        delimited-readers
        scsh-newports)
  (files fileinfo
         file
         filesys))

(define-structure scsh-temp-files scsh-temp-files-interface
  (open (modify scheme (hide call-with-input-file
                             call-with-output-file
                             with-input-from-file
                             with-output-to-file
                             write
                             display
                             char-ready?
                             read-char
                             write-char
                             newline
                             open-input-file
                             open-output-file))
        receiving
        let-opt
        bitwise
        formats
        fluids
        (subset scsh-utilities (make-reinitializer))
        (subset signals (error))
        (subset posix-files (file-options file-mode))
        scsh-environment
        scsh-errnos
        scsh-process-state
        scsh-file
        scsh-newports)
  (files temp-file))

(define-structure scsh-globbing scsh-globbing-interface
  (open scheme
        ascii
        receiving
        (subset srfi-1 (filter fold))
        srfi-14
        re-level-0
        (subset signals (error))
        scsh-errnos
        scsh-file-names
        scsh-file
        scsh-directories)
  (files glob))

(define-structure scsh-file-matching scsh-file-matching-interface
  (open scheme
        re-level-0
        signals handle conditions
        (subset srfi-1 (filter))
        (subset srfi-13 (string-index-right))
        scsh-file-names
        scsh-globbing)
  (files filemtch))

(define-structure scsh-process-objects scsh-process-objects-interface
  (open scheme
        receiving
        threads
        locks
        placeholders
        ;; sigevents
        bitwise
        tables
        weak-tables
        weak
        waitcodes
        let-opt
        define-record-types
        (subset posix-processes (signal
                                 process-id-exit-status
                                 integer->process-id
                                 wait-for-child-process
                                 process-id-exit-status
                                 process-id-terminating-signal))
        (subset threads-internal (spawn-on-root))
        (subset primitives (add-finalizer!))
        (subset srfi-1 (delete filter))
        (subset scsh-utilities (make-reinitializer
                                with-lock run-as-long-as))
        low-interrupt
        (subset external-calls (import-lambda-definition-2))
        scsh-errnos
        scsh-file-names
        (subset signals (error)))
  (files procobj))

(define-structure scsh-fdports scsh-fdports-interface
  (open (modify scheme (hide call-with-input-file
                             call-with-output-file
                             with-input-from-file
                             with-output-to-file
                             write
                             display
                             char-ready?
                             read-char
                             write-char
                             newline
                             open-input-file
                             open-output-file))
        signals
        bitwise
        (subset posix-files (file-options file-mode))
        (subset posix-i/o (dup2))
        (subset scsh-utilities (check-arg stringify))
        scsh-file-syscalls
        scsh-newports)
  (files fdports))

(define-structure scsh-signals scsh-signals-interface
  (open scheme
        (subset signals (error))
        (subset external-calls (import-lambda-definition-2))
        (subset posix-processes (signal-os-number signal))
        scsh-process-objects)
  (files signal))

(define-structure scsh-processes scsh-process-interface
  (open (modify scheme (hide call-with-input-file
                             call-with-output-file
                             with-input-from-file
                             with-output-to-file
                             write
                             display
                             newline
                             write-char
                             char-ready?
                             read-char
                             open-input-file
                             open-output-file))
        (modify i/o (hide force-output
                          newline
                          write-char
                          char-ready?
                          read-char))
        escapes
        receiving
        let-opt
        threads
        thread-fluids
        handle
        (subset signals (error warn))
        (subset srfi-13 (string-index))
        (subset command-levels (session-started? set-batch-mode?!))
        (subset scsh-utilities (mapv! stringify))
        (subset scsh-environment (alist->env-list getenv environ-resource))
        (subset external-calls (import-lambda-definition-2))
        (subset posix-processes (signal exec-with-alias))
        (subset posix-time (current-time time-seconds))
        (subset interrupts (with-interrupts-inhibited))
        (subset display-conditions (display-condition))
        scsh-resources
        scsh-process-state
        scsh-process-objects
        scsh-file-names
        scsh-newports
        scsh-file
        scsh-fdports
        exit-hooks
        scsh-signals)
  (files process continuation))

(define-structure scsh-tty scsh-tty-interface
  (open (modify scheme (hide call-with-input-file
                             call-with-output-file
                             with-input-from-file
                             with-output-to-file
                             write
                             display
                             char-ready?
                             read-char
                             write-char
                             newline
                             open-input-file
                             open-output-file))
        (modify i/o (hide write-string
                          force-output
                          newline
                          write-char
                          char-ready?
                          read-char))
        (subset scsh-syscall-support (byte-vector->string))
        (subset load-dynamic-externals (import-dynamic-externals))
        ascii
        signals
        bitwise
        let-opt
        define-record-types
        tty-flags scsh-internal-tty-flags
        (subset posix-files (file-options file-options-on?))
        (subset external-calls (import-lambda-definition-2))
        (subset os-strings (string->os-string
                            os-string->byte-vector))
        scsh-newports
        scsh-process-objects)
  (files tty))

(define-structure scsh-stdio scsh-stdio-interface
  (open (modify scheme (hide call-with-input-file
                             call-with-output-file
                             with-input-from-file
                             with-output-to-file
                             write
                             display
                             char-ready?
                             read-char
                             write-char
                             newline
                             open-input-file
                             open-output-file))
        (subset i/o (current-error-port))
        (subset scsh-utilities (define-simple-syntax))
        scsh-fdports
        scsh-newports)
  (files stdio))

(define-structure scsh-ptys scsh-ptys-interface
  (open (modify scheme (hide call-with-input-file
                             call-with-output-file
                             with-input-from-file
                             with-output-to-file
                             write
                             display
                             char-ready?
                             read-char
                             write-char
                             newline
                             open-input-file
                             open-output-file))
        receiving
        scsh-processes
        scsh-fdports
        (subset signals (error))
        (subset external-calls (import-lambda-definition-2))
        (subset posix-files (file-options))
        (subset scsh-errnos (with-errno-handler))
        (subset scsh-syscall-support (byte-vector->string))
        scsh-newports
        scsh-stdio
        scsh-tty
        scsh-process-state)
  (files pty))

(define-structure scsh-system (compound-interface uname-interface
                                                  (export system-name))
  (open scheme
        define-record-types
        shared-bindings
        (subset scsh-syscall-support (byte-vector->string))
        (subset external-calls (import-lambda-definition-2))
        posix-platform-names)
  (files system))

(define-structure scsh-file-names-system scsh-file-names-system-interface
  (open scheme
        signals
        let-opt
        (subset srfi-1 (reverse!))
        (subset srfi-13 (string-index))
        scsh-file-names
        scsh-environment
        scsh-user/group-db
        scsh-process-state)
  (files fname-system))

(define-structure scsh-collect-ports scsh-collect-ports-interface
  (open scheme
        let-opt
        reduce
        (subset scsh-utilities (deprecated-proc))
        (subset srfi-1 (reverse!))
        delimited-readers
        string-collectors)
  (files port-collect))

(define-structure scsh-high-level-processes scsh-high-level-process-interface
  (for-syntax (open scsh-syntax-helpers scheme))
  (open (modify scheme (hide call-with-input-file
                             call-with-output-file
                             with-input-from-file
                             with-output-to-file
                             write
                             display
                             char-ready?
                             read-char
                             write-char
                             newline
                             open-input-file
                             open-output-file))
        signals
        receiving
        let-opt
        (subset scsh-utilities (define-simple-syntax))
        (subset srfi-1 (fold))
        (subset posix-i/o (dup2 open-pipe))
        scsh-temp-files
        scsh-processes scsh-process-objects
        scsh-stdio
        scsh-newports
        scsh-fdports
        scsh-collect-ports)
  (files syntax
         process-high-level))

(define-structure scsh-command-line scsh-command-line-interface
  (open scheme
        (subset signals (error)))
  (files command-line))

(define-structure scsh-time scsh-time-interface
  (open scheme define-record-types let-opt bitwise receiving formats constance
        (subset external-calls (import-lambda-definition-2))
        (subset load-dynamic-externals (import-dynamic-externals))
        (subset scsh-utilities (check-arg real->exact-integer))
        (subset signals (error warn))
        (subset os-strings (string->os-byte-vector))
        (subset scsh-syscall-support (byte-vector->string)))
  (files time))

;;; The scsh-level-0 package is for implementation convenience.
;;; The scsh startup and top-level modules need access to scsh
;;; procedures, but they export procedures that are themselves
;;; part of scsh. So scsh-level-0 is the core scsh stuff, which is
;;; imported by these two modules. These modules all collectively
;;; export the whole scsh enchilada.

(define-structures
  ((scsh-level-0
    (compound-interface scsh-delimited-readers-interface
                        scsh-io-interface
                        scsh-file-interface
                        scsh-globbing-interface
                        scsh-file-matching-interface
                        scsh-temp-files-interface
                        scsh-directories-interface
                        scsh-process-state-interface
                        scsh-process-objects-interface
                        scsh-process-interface
                        scsh-user/group-db-interface
                        scsh-command-line-interface
                        scsh-signals-interface
                        scsh-environment-interface
                        scsh-file-names-interface
                        scsh-misc-interface
                        scsh-high-level-process-interface
                        scsh-tty-interface ; new in 0.4
                        scsh-version-interface
                        scsh-file-names-system-interface
                        scsh-time-interface
                        scsh-collect-ports-interface
                        scsh-errors-interface
                        (interface-of srfi-14) ;; export this here for
                        (export ->char-set)    ;; this kludge
                        (export system-name) ; #### has nowhere else to go for now
                        ;; This stuff would probably be better off kept
                        ;; in separate modules, but we'll toss it in for now.
                        (interface-of ascii) ; char<->ascii
                        string-ports-interface
                        uname-interface))
   (scsh-level-0-internals (export set-command-line-args!
                                   init-home-directory
                                   init-exec-path-list)))
  (for-syntax (open scsh-syntax-helpers scheme))
  (open (modify scheme (hide call-with-input-file
                             call-with-output-file
                             with-input-from-file
                             with-output-to-file
                             write
                             display
                             char-ready?
                             read-char
                             write-char
                             newline
                             open-input-file
                             open-output-file))
        formats
        string-collectors
        extended-ports
        receiving
        bitwise
        delimited-readers
        ascii
        srfi-14
        scsh-version
        (subset i/o (current-error-port))
        tty-flags
        scsh-internal-tty-flags
        scsh-file-syscalls
        scsh-resources
        scsh-environment
        scsh-file-names
        scsh-directories
        scsh-user/group-db
        scsh-process-state
        scsh-newports
        scsh-file
        scsh-temp-files
        scsh-globbing
        scsh-file-matching
        scsh-process-objects
        scsh-processes
        scsh-fdports
        scsh-signals
        scsh-tty
        scsh-stdio
        scsh-ptys
        scsh-system
        scsh-file-names-system
        scsh-high-level-processes
        scsh-collect-ports
        scsh-command-line
        scsh-errnos
        scsh-time)
  (begin
    ;; work around for SRFI 14 naming fuckage
    (define ->char-set x->char-set)))

(define-structure defenum-package (export (define-enum-constant  :syntax)
                                          (define-enum-constants :syntax)
                                          (define-enum-constants-from-zero
                                            :syntax))
  (open scheme)
  (files enumconst))

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
        (subset filenames (translate))
        usual-resumer           ; usual-resumer
        environments            ; with-interaction-environment
        fluids-internal            ; JMG: get-dynamic-env
        threads
        command-processor
        (subset threads-internal (wait-for-event))
        queues scheduler
        scsh-utilities
        interrupts
        low-interrupt
        ;; sigevents
        (modify primitives (hide wait
                                 write-char
                                 read-char
                                 time))
        os-strings
        scsh-reader
        (subset packages-internal (set-package-reader!))
        (modify scheme (hide write
                             display
                             char-ready?
                             read-char
                             write-char
                             newline
                             call-with-input-file
                             call-with-output-file
                             with-input-from-file
                             with-output-to-file
                             open-input-file
                             open-output-file)))
  (files startup))

(define-structure scsh-top-package (export parse-switches-and-execute
                                           with-scsh-initialized)
  (open (modify scheme (hide call-with-input-file
                             call-with-output-file
                             with-input-from-file
                             with-output-to-file
                             write
                             display
                             char-ready?
                             read-char
                             write-char
                             newline
                             open-input-file
                             open-output-file))
        (modify command-processor (hide y-or-n?))
        command-levels          ; with-new-session
        (subset exceptions (assertion-violation))
        conditions
        (subset display-conditions (display-condition))
        ensures-loaded
        environments
        (subset compiler-envs (comp-env-macro-eval))
        (subset signals (error warn))
        evaluation
        extended-ports
        fluids
        interfaces
        ;; sigevents
        scsh-reader
        scsh-here-string-hax
        low-interrupt wind
        fluids-internal            ; JMG: get-dynamic-env
        handle                     ; JMG: with-handler
        interrupts
        (modify i/o (hide write-string
                          force-output
                          newline
                          write-char
                          char-ready?
                          read-char))
        package-commands-internal
        package-mutation
        packages
        receiving
        scsh-version
        scsh-level-0            ; with-current-input-port error-output-port with-current-output-port exit
        scsh-level-0-internals  ; set-command-line-args! init-scsh-vars
        threads
        lib-dirs
        lib-dirs-internal
        (subset srfi-14 (char-set
                         char-set-complement!
                         char-set-contains?
                         string->char-set))
        root-scheduler          ; scheme-exit-now
        exit-hooks)
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
                                               "Use SRFI-13 STRING-JOIN."))))

(define-structures
  ((awk-expander-package (export expand-awk expand-awk/obsolete))
   (awk-support-package (export next-range next-:range
                                next-range: next-:range:)))
  (open receiving               ; receive
        ;; scsh-utilities
        (subset srfi-1 (any filter))
        (subset signals (error))
        sre-syntax-tools
        scheme
        )
  (files awk))

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
                      (export error warn)
                      re-exports-interface
                      scsh-field-reader-interface       ; new in 0.3
                      awk-interface
                      char-predicates-interface
                      lib-dirs-interface)
  (open scsh-level-0
        scsh-level-0-internals
        (modify scheme (hide call-with-input-file
                             call-with-output-file
                             with-input-from-file
                             with-output-to-file
                             write
                             display
                             char-ready?
                             read-char
                             write-char
                             newline
                             char-lower-case?
                             char-upper-case?
                             char-alphabetic?
                             char-numeric?
                             char-whitespace?
                             open-input-file
                             open-output-file))
        re-exports
        scsh-startup-package
        awk-package
        field-reader-package
        char-predicates-lib
        lib-dirs
        (subset signals (error warn))))

(define-structure scsh-here-string-hax (export)
  (open scheme
        scsh-reader
        receiving
        delimited-readers
        (subset features (make-immutable!))
        (subset srfi-14 (char-set)))
  (files here))

;; (define-structure sigevents sigevents-interface
;;    (open scsh-level-0
;;          (modify scheme (hide write
;;                               display
;;                               char-ready?
;;                               read-char
;;                               write-char
;;                               newline))
;;          low-interrupt
;;          define-record-types
;;          threads
;;          (subset srfi-1 (filter))
;;          (subset scsh-utilities (run-as-long-as))
;;          (subset signals (error))
;;          (subset queues (make-queue))
;;          (subset proposals (with-new-proposal))
;;          (subset threads-internal (maybe-commit-and-make-ready
;;                                    maybe-commit-and-block-on-queue
;;                                    maybe-dequeue-thread!
;;                                    thread-queue-empty?))
;;          (subset interrupts (with-interrupts-inhibited))
;;          (subset posix-processes (name->signal
;;                                   signal-os-number
;;                                   make-signal-queue
;;                                   dequeue-signal!
;;                                   signal=?)))
;;    (files event))

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
        define-record-types
        srfi-9
        bitwise
        (subset i/o (read-block))
        (subset srfi-13 (string-fold-right))
        (subset signals (error warn))
        external-calls)
  (files md5))

(define-structures ((lib-dirs lib-dirs-interface)
                    (lib-dirs-internal lib-dirs-internal-interface))
  (open scsh-level-0
        (modify scheme (hide call-with-input-file
                             call-with-output-file
                             with-input-from-file
                             with-output-to-file
                             write
                             display
                             char-ready?
                             read-char
                             write-char
                             newline
                             open-input-file
                             open-output-file))
        handle
        scsh-utilities
        (subset signals (error))
        (subset srfi-1 (any)))
  (files lib-dirs))

(define-structure scsh-user
  (compound-interface (interface-of floatnums)
                      (interface-of srfi-1)
                      (interface-of srfi-13)
                      (interface-of srfi-14)
                      (interface-of scsh)
                      (interface-of scheme))
  (open floatnums
        srfi-1
        srfi-13
        srfi-14
        scsh-top-package
        scsh
        (modify scheme (hide call-with-input-file
                             call-with-output-file
                             with-input-from-file
                             with-output-to-file
                             open-input-file
                             open-output-file
                             char-whitespace?
                             char-numeric?
                             char-lower-case?
                             char-upper-case?
                             char-alphabetic?
                             write
                             display
                             char-ready?
                             read-char
                             write-char
                             newline
                             map
                             string-fill!
                             member
                             assoc
                             string-copy
                             string->list
                             for-each))))

(define-structure scheme-with-scsh
  (compound-interface (interface-of scsh)
                      (interface-of scheme))
  (open scsh
        (modify scheme (hide call-with-input-file
                             call-with-output-file
                             with-input-from-file
                             with-output-to-file
                             open-input-file
                             open-output-file
                             char-ready?
                             char-whitespace?
                             char-numeric?
                             char-lower-case?
                             char-upper-case?
                             char-alphabetic?
                             write
                             newline
                             read-char
                             display
                             write-char))))
