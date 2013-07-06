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
	  with-error-output-port*
	  release-port-handle
	  seek/end
	  port-revealed
	  with-error-output-port
	  seek/delta
	  tell
	  seek
	  port->fdes
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

                              fork-pty-session
                              open-pty
                              pty-name->tty-name
                              tty-name->pty-name
                              make-pty-generator

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
          collect-char!
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

(define-interface scsh-field-reader-interface
  (export join-strings
          field-splitter infix-splitter suffix-splitter sloppy-suffix-splitter
          record-reader
          field-reader))

(define-interface scsh-delimited-readers-interface
  (export read-line
          read-paragraph
          read-delimited read-delimited!
          %read-delimited!
          skip-char-set))

(define-interface awk-interface
  (export (awk :syntax)
          (awk/posix-string :syntax)))

(define-interface scsh-dbm-interface
  (export dbm-open
          dbm-close
          dbm-delete
          dbm-fetch
          dbm-insert
          dbm-replace
          dbm-firstkey
          dbm-nextkey
          dbm-record?
          dbm-record:open?
          btree/method
          btree-info:flags
          btree-info:cachesize
          btree-info:maxkeypage
          btree-info:minkeypage
          btree-info:psize
          btree-info:lorder
          btree-info?
          make-btree-info
          hash/method
          hash-info:bsize
          hash-info:ffactor
          hash-info:nelem
          hash-info:cachesize
          hash-info:lorder
          hash-info?
          make-hash-info
          recno/method
          recno-info:flags
          recno-info:cachesize
          recno-info:psize
          recno-info:lorder
          recno-info:reclen
          recno-info:bval
          recno-info:bfname
          recno-info?
          make-recno-info))

;;; Magic flags for SCSH-TTY-INTERFACE.
(define-interface tty-flags-interface
  (export
          ;; Indices into the control char string
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Posix
          ttychar/eof
          ttychar/eol
          ttychar/delete-char
          ttychar/delete-line
          ttychar/interrupt
          ttychar/quit
          ttychar/suspend
          ttychar/start
          ttychar/stop
          ttychar/min
          ttychar/time

          ;; SVR4 & 4.3+BSD
          ttychar/delete-word
          ttychar/reprint
          ttychar/literal-next
          ttychar/discard
          ttychar/delayed-suspend
          ttychar/eol2

          ;; 4.3+BSD
          ttychar/status

          disable-tty-char

          ;; Input flag bits
          ;;;;;;;;;;;;;;;;;;
          ;; Posix
          ttyin/ignore-break
          ttyin/interrupt-on-break
          ttyin/ignore-bad-parity-chars
          ttyin/mark-parity-errors
          ttyin/check-parity
          ttyin/7bits
          ttyin/nl->cr
          ttyin/ignore-cr
          ttyin/cr->nl
          ttyin/output-flow-ctl
          ttyin/input-flow-ctl

          ;; SVR4 & 4.3+BSD
          ttyin/xon-any
          ttyin/beep-on-overflow

          ;; SVR4
          ttyin/lowercase

          ;; Output flag bits
          ;;;;;;;;;;;;;;;;;;;
          ttyout/enable                 ; opost: enable output processing

          ;; SVR4 & 4.3+BSD
          ttyout/nl->crnl               ; onlcr: map nl to cr-nl

          ;; 4.3+BSD
          ttyout/discard-eot            ; onoeot
          ttyout/expand-tabs            ; oxtabs (NOT xtabs)

          ;; SVR4
          ttyout/cr->nl                 ; ocrnl
          ttyout/fill-w/del             ; ofdel
          ttyout/delay-w/fill-char      ; ofill
          ttyout/uppercase              ; olcuc
          ttyout/nl-does-cr             ; onlret
          ttyout/no-col0-cr             ; onocr

          ;; Newline delay
          ttyout/nl-delay               ; mask (nldly)
          ttyout/nl-delay0
          ttyout/nl-delay1              ; tty 37

          ;; Horizontal-tab delay
          ttyout/tab-delay              ; mask (tabdly)
          ttyout/tab-delay0
          ttyout/tab-delay1             ; tty 37
          ttyout/tab-delay2
          ttyout/tab-delayx             ; Expand tabs (xtabs, tab3)

          ;; Carriage-return delay
          ttyout/cr-delay               ; mask (crdly)
          ttyout/cr-delay0
          ttyout/cr-delay1              ; tn 300
          ttyout/cr-delay2              ; tty 37
          ttyout/cr-delay3              ; concept 100

          ;; Vertical tab delay
          ttyout/vtab-delay             ; mask (vtdly)
          ttyout/vtab-delay0
          ttyout/vtab-delay1            ; tty 37

          ;; Backspace delay
          ttyout/bs-delay               ; mask (bsdly)
          ttyout/bs-delay0
          ttyout/bs-delay1

          ;; Form-feed delay
          ttyout/ff-delay               ; mask (ffdly)
          ttyout/ff-delay0
          ttyout/ff-delay1

          ttyout/all-delay


          ;; Control flag bits
          ;;;;;;;;;;;;;;;;;;;;
          ;; Posix
          ttyc/char-size                ; csize: character size mask
          ttyc/char-size5               ; 5 bits (cs5)
          ttyc/char-size6               ; 6 bits (cs6)
          ttyc/char-size7               ; 7 bits (cs7)
          ttyc/char-size8               ; 8 bits (cs8)
          ttyc/2-stop-bits              ; cstopb: Send 2 stop bits.
          ttyc/enable-read              ; cread: Enable receiver.
          ttyc/enable-parity            ; parenb
          ttyc/odd-parity               ; parodd
          ttyc/hup-on-close             ; hupcl: Hang up on last close.
          ttyc/no-modem-sync            ; clocal: Ignore modem lines.

          ;;  4.3+BSD
          ttyc/ignore-flags             ; cignore: ignore control flags
          ttyc/CTS-output-flow-ctl      ; ccts_oflow: CTS flow control of output
          ttyc/RTS-input-flow-ctl       ; crts_iflow: RTS flow control of input
          ttyc/carrier-flow-ctl         ; mdmbuf

          ;; Local flag bits
          ;;;;;;;;;;;;;;;;;;
          ;; POSIX
          ttyl/visual-delete            ; echoe: Visually erase chars
          ttyl/echo-delete-line         ; echok: Echo nl after line kill
          ttyl/echo                     ; echo:  Enable echoing
          ttyl/echo-nl                  ; echonl: Echo nl even if echo is off
          ttyl/canonical                ; icanon: Canonicalize input
          ttyl/enable-signals           ; isig: Enable ^c, ^z signalling
          ttyl/extended                 ; iexten:  Enable extensions
          ttyl/ttou-signal              ; tostop: SIGTTOU on background output
          ttyl/no-flush-on-interrupt    ; noflsh

          ;; SVR4 & 4.3+BSD
          ttyl/visual-delete-line       ; echoke: visually erase a line-kill
          ttyl/hardcopy-delete          ; echoprt: visual erase for hardcopy
          ttyl/echo-ctl                 ; echoctl: echo control chars as "^X"
          ttyl/flush-output             ; flusho: output is being flushed
          ttyl/reprint-unread-chars     ; pendin: retype pending input

          ;; 4.3+BSD
          ttyl/alt-delete-word          ; altwerase
          ttyl/no-kernel-status         ; nokerninfo: no kernel status on ^T

          ;; SVR4
          ttyl/case-map                 ; xcase: canonical upper/lower presentation
          ))

;;; Non-exported values required by the tty code.
(define-interface scsh-internal-tty-flags-interface
  (export baud-rates
          num-ttychars

          ;; tcflush() constants
          %flush-tty/input              ; TCIFLUSH
          %flush-tty/output             ; TCOFLUSH
          %flush-tty/both               ; TCIOFLUSH

          ;; tcflow() constants
          %tcflow/start-out             ; TCOON
          %tcflow/stop-out              ; TCOOFF
          %tcflow/start-in              ; TCION
          %tcflow/stop-in               ; TCIOFF

          ;; tcsetattr() constants
          %set-tty-info/now             ; TCSANOW   Make change immediately.
          %set-tty-info/drain           ; TCSADRAIN Drain output, then change.
          %set-tty-info/flush           ; TCSAFLUSH Drain output, flush input.
          ))


;;; POSIX termios tty control.
(define-interface scsh-tty-interface
  (compound-interface
      tty-flags-interface
      (export
          ;; The tty-info record
          tty-info?                     :tty-info
          tty-info:control-chars        set-tty-info:control-chars
          tty-info:input-flags          set-tty-info:input-flags
          tty-info:output-flags         set-tty-info:output-flags
          tty-info:control-flags        set-tty-info:control-flags
          tty-info:local-flags          set-tty-info:local-flags
          tty-info:input-speed          set-tty-info:input-speed
          tty-info:output-speed         set-tty-info:output-speed
          tty-info:min                  set-tty-info:min
          tty-info:time                 set-tty-info:time

          make-tty-info                 copy-tty-info

          tty-info
          set-tty-info/now
          set-tty-info/drain
          set-tty-info/flush

          send-tty-break
          drain-tty
          flush-tty/input
          flush-tty/output
          flush-tty/both

          start-tty-output
          stop-tty-output
          start-tty-input
          stop-tty-input

          encode-baud-rate
          decode-baud-rate

          open-control-tty
          make-control-tty
          set-tty-process-group
          tty-process-group

          tty?
          tty-file-name
          control-tty-file-name
          )))

(define-interface scsh-stdio-interface
  (export stdports->stdio
          with-stdio-ports*
          (with-stdio-ports :syntax)))

(define-interface scsh-ptys-interface
  (export fork-pty-session
	  open-pty
	  pty-name->tty-name
	  tty-name->pty-name
	  make-pty-generator))

;; (define-interface sigevents-interface
;;   (export most-recent-sigevent
;;           sigevent?
;;           next-sigevent
;;           next-sigevent/no-wait
;;           with-sigevents
;;           sigevent-type))

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

;; (define-interface dot-locking-interface
;;   (export obtain-dot-lock release-dot-lock
;;           break-dot-lock
;;           (with-dot-lock :syntax)
;;           with-dot-lock*))

(define-interface uname-interface
  (export uname
          uname:os-name
          uname:node-name
          uname:release
          uname:version
          uname:machine
          :uname))

(define-interface md5-interface
  (export make-md5-context
          md5-context?
          update-md5-context!
          md5-context->md5-digest

          md5-digest?

          md5-digest->number
          number->md5-digest

          md5-digest-for-string
          md5-digest-for-port))

;; (define-interface configure-interface
;;   (export host
;;           machine
;;           vendor
;;           os
;;           prefix
;;           exec-prefix
;;           bin-dir
;;           lib-dir
;;           include-dir
;;           man-dir
;;           lib-dirs-list
;;           libs
;;           defs
;;           cflags
;;           cppflags
;;           ldflags
;;           linker-flags
;;           compiler-flags))

(define-interface lib-dirs-interface
  (export lib-dirs
          find-library-file
          default-lib-dirs

          lib-dirs-append-script-dir!
          lib-dirs-prepend-script-dir!
          reset-lib-dirs!
          clear-lib-dirs!
          lib-dirs-prepend!
          lib-dirs-append!))

(define-interface lib-dirs-internal-interface
  (export expand-lib-dir))

(define-interface scsh-time-interface
  (export make-date
	  date?

	  date:seconds
	  date:minute	
	  date:hour   	
	  date:month-day	
	  date:month   	
	  date:year    	
	  date:tz-name	
	  date:tz-secs	
	  date:summer?	
	  date:week-day	
	  date:year-day

	  set-date:seconds
	  set-date:minute	
	  set-date:hour   	
	  set-date:month-day	
	  set-date:month   	
	  set-date:year    	
	  set-date:tz-name	
	  set-date:tz-secs	
	  set-date:summer?	
	  set-date:week-day	
	  set-date:year-day

	  time+ticks
	  ticks/sec
	  time
	  date
	  date->string
	  format-date))
