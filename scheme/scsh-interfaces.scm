;;; The module interfaces for scsh.
;;; Copyright (c) 1994 by Olin Shivers and David Albertz.
;;; Copyright (c) 1994 by Brian D. Carlstrom

(define-interface let-opt-expanders-interface
  (export expand-let-optionals
          expand-let-optionals*))

(define-interface let-opt-interface
  (export (let-optionals  :syntax)
          (let-optionals* :syntax)
          (:optional      :syntax)))

(define-interface functional-search-trees-interface
  (export make-search-tree search-tree?
	  search-tree-insert search-tree-ref
	  search-tree-delete
	  search-tree-walk))

(define-interface scsh-errors-interface
  (export errno-error
          error
          (with-errno-handler* (proc ((proc (:number :value) :values); handler
                                      (proc () :values))             ; thunk
                                     :values))
          (with-errno-handler :syntax)))

(define-interface scsh-continuations-interface
  (export call-terminally))

(define-interface scsh-file-syscalls-interface
  (export %set-cloexec
	  %open %close-fdes %dup %dup2 %pipe-fdes
	  %fd-seek
	  %char-ready-fdes?
	  %truncate-file %truncate-fdes
	  %create-symlink
	  %set-file-mode %set-fdes-mode
	  %set-file-uid&gid %set-fdes-uid&gid
	  %utime %utime-now
	  %stat-file %stat-fdes
	  %sync-file %sync-file-system))

(define buffered-io-flags-interface
  (export ((bufpol/block
            bufpol/line
            bufpol/none) :number)))

(define-interface scsh-newports-interface
  (export call/fdes
	  sleazy-call/fdes
	  fdes->inport
	  fdes->outport
	  evict-ports
	  fdport?
	  %move-fdport
	  close-fdes
	  make-input-fdport
	  make-output-fdport
	  open-fdes
	  close
	  select
	  seek/set
	  open-file
	  pipe
	  with-current-output-port*
	  close-after
	  with-current-error-port
	  with-current-output-port
	  with-current-input-port
	  flush-all-ports
	  with-current-error-port*
	  with-current-input-port*
	  close-after
	  with-current-error-port
	  with-current-output-port
	  flush-all-ports
	  error-output-port
	  close
	  release-port-handle
	  flush-all-ports-no-threads
	  select!
	  with-error-output-port*
	  release-port-handle
	  seek/end
	  port-revealed
	  select-port-channels
	  with-error-output-port
	  seek/delta
	  tell
	  seek
	  port->fdes select-ports
	  char-ready?
	  read-char
	  display write newline write-char
	  force-output
	  open-input-file
	  with-output-to-file with-input-from-file
	  call-with-input-file call-with-output-file
	  open-output-file
          init-fdports!))

(define-interface scsh-read/write-interface
  (export read-string/partial
	  read-string!/partial
	  read-string read-string!
	  write-string
	  write-string/partial))

(define-interface scsh-flock-interface
  (export lock-region?
	  lock-region:exclusive?
	  lock-region:whence
	  lock-region:start
	  lock-region:len
	  lock-region:pid		; Deprecated proc.
	  lock-region:proc
	  make-lock-region

	  lock-region
	  lock-region/no-block
	  get-lock-region
	  unlock-region
	  with-region-lock*
	  (with-region-lock :syntax)))

(define-interface scsh-io-interface
  (compound-interface buffered-io-flags-interface
                      (export close
                              close-after
                              current-error-port
                              error-output-port
                              dup
                              dup->inport
                              dup->outport
                              dup->fdes

                              force-output
                              bufpol/block
                              bufpol/line
                              bufpol/none

                              seek
                              tell
                              seek/set
                              seek/delta
                              seek/end

                              flush-all-ports
                              flush-all-ports-no-threads
                              ;; R4RS I/O procedures that scsh provides.
                              write
                              char-ready?
                              read-char
                              write-char
                              display
                              newline
                              input-port?
                              output-port?
                              call-with-input-file
                              call-with-output-file
                              with-input-from-file
                              with-output-to-file
                              open-input-file
                              open-output-file
                              format

                              init-fdports!

                              with-current-input-port*
                              (with-current-input-port :syntax)
                              with-current-output-port*
                              (with-current-output-port :syntax)
                              with-current-error-port*
                              (with-current-error-port :syntax)
                              with-error-output-port*
                              (with-error-output-port :syntax)
                              ;; set-current-input-port!
                              ;; set-current-output-port!
                              ;; set-current-error-port!
                              ;; set-error-output-port!

                              stdports->stdio
                              with-stdio-ports*
                              (with-stdio-ports :syntax)

                              call/fdes
                              release-port-handle
                              port-revealed
                              fdes->inport
                              fdes->outport
                              move->fdes
                              open-fdes
                              pipe
                              port->string
                              port->sexp-list
                              port->string-list
                              port->list
                              port-fold reduce-port
                              port->fdes
                              read-string
                              read-string!
                              read-string/partial
                              read-string!/partial

                              select select!
                              select-ports select-port-channels

                              (write-string (proc (:string &opt :value :exact-integer :exact-integer) :unspecific))
                              write-string/partial)))


(define-interface scsh-file-interface
  (export open-file
          file-options
          create-directory
          create-fifo
          create-hard-link
          create-symlink

          delete-directory
          delete-file
          delete-filesys-object
          rename-file
          set-file-mode
          set-file-owner
          set-file-group
          set-file-times
          truncate-file

          read-symlink                  ; Not POSIX.

          file-attributes               ; Deprecated;
          file-info                     ; preferred.

          file-info:type
          file-info:device
          file-info:inode
          file-info:mode
          file-info:nlinks
          file-info:uid
          file-info:gid
          file-info:size
          file-info:atime
          file-info:mtime
          file-info:ctime
          file-type
          file-group
          file-inode
          file-last-access
          file-last-mod
          file-last-status-change
          file-mode
          file-nlinks
          file-owner
          file-size
          file-symlink?
          file-directory?
          file-fifo?
          file-regular?
          file-socket?
          file-special?
          file-info-directory?
          file-info-fifo?
          file-info-regular?
          file-info-socket?
          file-info-special?
          file-info-symlink?

          file-not-readable?
          file-not-writable?
          file-not-writeable?   ; Deprecated
          file-not-executable?
          file-readable?
          file-writeable?
          file-writable?        ; Deprecated
          file-executable?
          file-info-not-readable?
          file-info-not-writable?
          file-info-not-executable?
          file-info-readable?
          file-info-writable?
          file-info-executable?

          file-not-exists?
          file-exists?

          sync-file
          sync-file-system))

(define-interface scsh-globbing-interface
  (export glob
          glob-quote
          maybe-directory-files))

(define-interface scsh-file-matching-interface
  (export file-match))

(define-interface scsh-temp-files-interface
  (export create-temp-file
          temp-file-iterate
          temp-file-channel
          *temp-file-template*))

(define-interface scsh-process-objects-interface
  (export proc?
          proc:pid
          pid->proc
          ;; autoreap-policy
          ;; with-autoreaping
          reap-zombies
          wait
          wait-any
          ;; wait-process-group
          status:exit-val
          status:stop-sig
          status:term-sig
          wait/poll
          wait/stopped-children
          new-child-proc))

(define-interface scsh-process-interface
  (export exec
          exec-path
          exec/env
          exec-path/env
          %exec
          exec-path-search
          exit
          %exit
          suspend
          fork
          %fork
          process-sleep
          process-sleep-until
          call-terminally
          halts?
          fork/pipe
          %fork/pipe
          fork/pipe+
          %fork/pipe+
          tail-pipe
          tail-pipe+
          exec-path-list
          init-exec-path-list)) ; ### should be internal

(define-interface scsh-fdports-interface
  (export move->fdes
          dup
          dup->fdes dup->inport dup->outport
          shell-open
          create+trunc))

(define-interface scsh-process-state-interface
  (export with-resources-aligned

          umask
          set-umask
          with-umask*
          (with-umask :syntax)
          umask-resource

          process-chdir
          process-cwd
          chdir
          cwd
          with-cwd*
          (with-cwd :syntax)
          cwd-resource

          pid
          parent-pid
          process-group
          set-process-group
          become-session-leader

          user-login-name
          user-uid
          user-effective-uid
          user-gid
          user-effective-gid
          user-supplementary-gids
          set-uid
          set-gid
          set-user-effective-uid
          set-user-effective-gid
          with-user-effective-uid*
          with-user-effective-gid*
          ((with-user-effective-uid
            with-user-effective-gid):syntax)
          euid-resource
          egid-resource

          process-times
          cpu-ticks/sec))


(define-interface scsh-user/group-db-interface
  (export name->user-info
          user-info
          user-info?
          user-info:name
          user-info:uid
          user-info:gid
          user-info:home-dir
          user-info:shell
          ->uid
          ->username
	  %homedir		     ; #### for scsh.scm
	  init-home-directory
	  home-directory
	  home-dir home-file
          group-info
          group-info?
          group-info:name
          group-info:gid
          group-info:members
          ->gid
          ->groupname))

(define-interface scsh-command-line-interface
  (export set-command-line-args!
          command-line-arguments
          command-line
          arg
          arg*

argv))

(define-interface scsh-signals-interface
  (export (signal :syntax)
          signal-process
          signal-process-group))

(define-interface scsh-environment-interface
  (export setenv
          getenv
          env->alist
          alist->env
          alist->env-vec ; #### for %EXEC
          alist->env-list
          alist-update
          alist-compress
          with-env*
          with-total-env*
          (with-env :syntax)
          (with-total-env :syntax)
          add-before
          add-after
          environ-resource))

(define-interface scsh-file-names-interface
  (export file-name-as-directory
          file-name-directory?
          file-name-non-directory?
          directory-as-file-name
          file-name-absolute?
          file-name-directory
          file-name-nondirectory
          ensure-file-name-is-nondirectory
	  ensure-file-name-is-directory
          split-file-name
          path-list->file-name
          file-name-extension
          file-name-sans-extension
          replace-extension
          parse-file-name
          simplify-file-name))

(define-interface scsh-file-names-system-interface
  (export resolve-tilde-file-name
          resolve-file-name
          expand-file-name
          absolute-file-name
          substitute-env-vars))

(define-interface scsh-misc-interface
  (export (receive :syntax)
          arithmetic-shift
          bitwise-and
          bitwise-ior
          bitwise-not
          bitwise-xor))


(define-interface scsh-high-level-process-interface
  (export (run :syntax)
          (exec-epf :syntax)
          (& :syntax)
          (||             :syntax)
;         (:or:           :syntax)      ; Alternate R4RS syntax for ||.
          (&&             :syntax)
          (run/collecting :syntax)
          (run/port+proc  :syntax)
          (run/port       :syntax)
          (run/strings    :syntax)
          (run/file       :syntax)
          (run/string     :syntax)
          (run/sexp       :syntax)
          (run/sexps      :syntax)
          run/collecting*
          run/port+proc*
          run/port*
          run/file*
          run/string*
          run/sexp*
          run/sexps*
          run/strings*))

(define-interface scsh-collect-ports-interface
  (export port->string
	  port->list
	  port->sexp-list
	  port->string-list
	  port-fold
	  reduce-port
          make-char-port-filter
          make-string-port-filter))

(define-interface scsh-version-interface
  (export scsh-major-version
          scsh-minor-version
          scsh-version-string
          scsh-release-name))

(define-interface exit-hooks-interface
  (export call-exit-hooks-and-run
          add-exit-hook!))

;;; This is probably bogus.
(define-interface string-ports-interface
  (export make-string-input-port
          call-with-string-output-port
          make-string-output-port
          string-output-port-output))

(define-interface string-collectors-interface
  (export make-string-collector
          collect-string!
          clear-string-collector!
          string-collector->string))

(define-interface thread-fluids-interface
  (export make-thread-fluid
          thread-fluid
          let-thread-fluid
          let-thread-fluids
          set-thread-fluid!
          make-preserved-thread-fluid
          preserve-thread-fluids
          fork-thread
          spoon))

(define-interface condition-handler-interface
  (export simple-condition-handler))

(define-interface delimited-readers-interface
  (export read-delimited read-delimited! %read-delimited!
          read-line read-paragraph
          skip-char-set))

(define-interface scsh-utilities-interface
  (export (define-simple-syntax :syntax)
          mapv
          mapv!
          vector-every?
          copy-vector
          initialize-vector
          vector-append
          vfold vfold-right
          check-arg
          deprecated-proc
          real->exact-integer
          make-reinitializer
          run-as-long-as
          obtain-all-or-none
          with-lock
          stringify
          bogus-substring-spec?))

(define-interface scsh-resources-interface
  (export with-resources-aligned
	  make-resource))

(define-interface scsh-directories-interface
  (export open-directory-stream
	  close-directory-stream
	  read-directory-stream
	  directory-files))

(define-interface weak-tables-interface
  (export make-weak-table weak-table-set! weak-table-ref weak-table-walk
          strengthen-weak-table-ref weaken-weak-table-ref))

(define-interface scsh-delimited-readers-interface
  (export read-line
          read-paragraph
          read-delimited read-delimited!
          %read-delimited!
          skip-char-set))


(define-interface scsh-stdio-interface
  (export stdports->stdio
          with-stdio-ports*
          (with-stdio-ports :syntax)))

(define-interface low-interrupt-interface
  (export number-of-interrupts
          interrupt/alrm interrupt/alarm
          interrupt/int  interrupt/keyboard
          interrupt/post-gc
          interrupt/i/o-completion
          interrupt/chld
          interrupt/cont
          interrupt/hup
          interrupt/quit
          interrupt/term
          interrupt/tstp
          interrupt/usr1
          interrupt/usr2
          interrupt/info
          interrupt/io
          interrupt/poll
          interrupt/prof
          interrupt/pwr
          interrupt/urg
          interrupt/vtalrm
          interrupt/winch
          interrupt/xcpu
          interrupt/xfsz
          interrupt-set
          interrupt-in-set?
          insert-interrupt
          remove-interrupt
          full-interrupt-set))

(define-interface uname-interface
  (export uname
          uname:os-name
          uname:node-name
          uname:release
          uname:version
          uname:machine
          :uname))
