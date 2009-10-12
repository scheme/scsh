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

(define-interface posix-fdflags-interface
  (export ((open/read
            open/write
            open/read+write
            open/non-blocking
            open/append
            open/no-control-tty
            open/create
            open/truncate
            open/exclusive
            open/access-mask) :number)

          ((fcntl/dup-fdes
            fcntl/get-fdes-flags
            fcntl/set-fdes-flags
            fcntl/get-status-flags
            fcntl/set-status-flags
            fcntl/get-record-lock
            fcntl/set-record-lock
            fcntl/set-record-lock-no-block) :number)

          (fdflags/close-on-exec :number)

          ((lock/read
            lock/write
            lock/release) :number)))

(define-interface posix-errno-interface
  (export ((errno/2big
            errno/acces
            errno/again
            errno/badf
            errno/busy
            errno/child
            errno/deadlk
            errno/dom
            errno/exist
            errno/fault
            errno/fbig
            errno/intr
            errno/inval
            errno/io
            errno/isdir
            errno/mfile
            errno/mlink
            errno/nametoolong
            errno/nfile
            errno/nodev
            errno/noent
            errno/noexec
            errno/nolck
            errno/nomem
            errno/nospc
            errno/nosys
            errno/notdir
            errno/notempty
            errno/notty
            errno/nxio
            errno/perm
            errno/pipe
            errno/range
            errno/rofs
            errno/spipe
            errno/srch
            errno/xdev) :number)))

(define-interface scsh-errors-interface
  (export errno-error
          error
          (with-errno-handler* (proc ((proc (:number :value) :values); handler
                                      (proc () :values))             ; thunk
                                     :values))
          (with-errno-handler :syntax)))



(define buffered-io-flags-interface
  (export ((bufpol/block
            bufpol/line
            bufpol/none) :number)))

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
                              open-file

                              fdes-flags
                              set-fdes-flags
                              fdes-status
                              set-fdes-status

                              init-fdports!  ;added by JMG
                              port->channel  ;overwrites channel-i/o

                              force-output
                              set-port-buffering
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
                              y-or-n?
                              *y-or-n-eof-count*
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

                              lock-region?
                              lock-region:exclusive?
                              lock-region:whence
                              lock-region:start
                              lock-region:len
                              lock-region:pid   ; Deprecated proc.
                              lock-region:proc
                              make-lock-region

                              lock-region
                              lock-region/no-block
                              get-lock-region
                              unlock-region
                              with-region-lock*
                              (with-region-lock :syntax)

                              fork-pty-session
                              open-pty
                              pty-name->tty-name
                              tty-name->pty-name
                              make-pty-generator

                              with-current-input-port*
                              (with-current-input-port :syntax)
                              with-current-output-port*
                              (with-current-output-port :syntax)
                              with-current-error-port*
                              (with-current-error-port :syntax)
                              with-error-output-port*
                              (with-error-output-port :syntax)
                              set-current-input-port!
                              set-current-output-port!
                              set-current-error-port!
                              set-error-output-port!

                              stdports->stdio
                              stdio->stdports
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
  (export create-directory
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
          sync-file-system

          open-directory-stream
          close-directory-stream
          read-directory-stream

          directory-files
          glob
          glob-quote
          file-match

          create-temp-file
          temp-file-iterate
          temp-file-channel
          *temp-file-template*))


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

          proc?
          proc:pid
          pid->proc

          autoreap-policy
          with-autoreaping
          reap-zombies

          wait
          wait-any
          wait-process-group

          status:exit-val
          status:stop-sig
          status:term-sig
          wait/poll
          wait/stopped-children

          process-sleep
          process-sleep-until

          call-terminally
          halts?))


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

          system-name
          process-times
          cpu-ticks/sec))


(define-interface scsh-user/group-db-interface
  (export user-info
          user-info?
          user-info:name
          user-info:uid
          user-info:gid
          user-info:home-dir
          user-info:shell

          ->uid
          ->username

          group-info
          group-info?
          group-info:name
          group-info:gid
          group-info:members

          ->gid
          ->groupname))


(define-interface scsh-command-line-interface
  (export command-line-arguments
          command-line
          arg
          arg*
          argv))


(define-interface scsh-signals-interface
  (export signal-process
          signal-process-group
          ;; JMG: this syscall doesn't cooperate with the thread-system
          ;;      pause-until-interrupt
          itimer  ;; now defined in low-interrupt as an artificial interrupt
))


(define-interface scsh-environment-interface
  (export install-env
          setenv
          getenv
          env->alist
          alist->env
          alist-delete
          alist-update
          alist-compress
          with-env*
          with-total-env*
          (with-env :syntax)
          (with-total-env :syntax)
          add-before
          add-after
          environ-resource))


(define-interface scsh-home-interface
  (export home-directory
          exec-path-list))


;;; Kill me?
(define-interface scsh-regexp-interface
  (export string-match
          regexp-match?
          match:start
          match:end
          match:substring
          make-regexp
          ->regexp
          regexp?
          regexp-search
          regexp-substitute
          regexp-substitute/global
          regexp-quote))


(define-interface scsh-string-interface
  (export substitute-env-vars
          ;string-index string-index-right ; Now in string-lib
          ))

(define-interface scsh-file-names-interface
  (export file-name-as-directory
          file-name-directory?
          file-name-non-directory?
          directory-as-file-name
          file-name-absolute?
          file-name-directory
          file-name-nondirectory
          split-file-name
          path-list->file-name
          file-name-extension
          file-name-sans-extension
          replace-extension
          parse-file-name
          expand-file-name
          simplify-file-name
          resolve-tilde-file-name
          resolve-file-name
          absolute-file-name
          home-dir
          home-file))

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

          fork/pipe
          %fork/pipe
          fork/pipe+
          %fork/pipe+
          tail-pipe
          tail-pipe+
          run/collecting*
          run/port+proc*
          run/port*
          run/file*
          run/string*
          run/sexp*
          run/sexps*
          run/strings*

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
  (export mapv mapv! vector-every? copy-vector initialize-vector vector-append
          vfold vfold-right
          check-arg
          deprecated-proc
          real->exact-integer
          make-reinitializer
          run-as-long-as
          obtain-all-or-none))

(define-interface weak-tables-interface
  (export make-weak-table weak-table-set! weak-table-ref weak-table-walk
          strengthen-weak-table-ref weaken-weak-table-ref))

;;; semi-standard network magic numbers
;;; should be available on all platforms
;;; if not, tell us, and we'll move it
;;; to the os-dependent directory

;;; for now, all socket option magic numbers
;;; are considered machine dependent until
;;; there is a standard or a clear portable subset

(define-interface sockets-network-interface
  (export shutdown/receives
          shutdown/sends
          shutdown/sends+receives
          herror/host-not-found
          herror/try-again
          herror/no-recovery
          herror/no-data
          herror/no-address
          address-family/unspecified
          address-family/unix
          address-family/internet
          socket-type/stream
          socket-type/datagram
          socket-type/raw
          ;;socket-type/rdm
          ;;socket-type/seqpacket
          protocol-family/unspecified
          protocol-family/unix
          protocol-family/internet
          internet-address/any
          internet-address/loopback
          internet-address/broadcast
          message/out-of-band
          message/peek
          message/dont-route
          level/socket
          options/boolean
          options/value
          options/linger
          options/timeout))

(define-interface scsh-endian-interface
  (export net-to-host-32
          net-to-host-16
          host-to-net-32
          host-to-net-16))

;;; actual functions interface
(define-interface scsh-sockets-interface
  (export socket-connect
          bind-listen-accept-loop
          bind-prepare-listen-accept-loop
          socket?
          socket:family
          socket:inport
          socket:outport
          socket-address?
          socket-address:family
          internet-address->socket-address
          socket-address->internet-address
          unix-address->socket-address
          socket-address->unix-address
          create-socket
          port->socket
          close-socket
          bind-socket
          connect-socket
          connect-socket-no-wait
          connect-socket-successful?
          listen-socket
          accept-connection
          socket-remote-address
          socket-local-address
          shutdown-socket
          create-socket-pair
          receive-message
          receive-message!
          receive-message/partial
          receive-message!/partial
          send-message
          send-message/partial
          socket-option
          set-socket-option

          host-info
          host-info?
          host-info:name
          host-info:aliases
          host-info:addresses
          network-info
          network-info?
          network-info:name
          network-info:aliases
          network-info:net
          service-info
          service-info?
          service-info:name
          service-info:aliases
          service-info:port
          service-info:protocol
          protocol-info
          protocol-info?
          protocol-info:name
          protocol-info:aliases
          protocol-info:number))

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
(define-interface tty-interface
  (compound-interface
      tty-flags-interface
      (export
          ;; The tty-info record
          tty-info?                     type/tty-info
          tty-info:control-chars        set-tty-info:control-chars
          tty-info:input-flags          set-tty-info:input-flags
          tty-info:output-flags         set-tty-info:output-flags
          tty-info:control-flags        set-tty-info:control-flags
          tty-info:local-flags          set-tty-info:local-flags
          tty-info:input-speed          set-tty-info:input-speed
          tty-info:output-speed         set-tty-info:output-speed
          tty-info:min                  set-tty-info:min
          tty-info:time                 set-tty-info:time

          modify-tty-info:control-chars
          modify-tty-info:input-flags
          modify-tty-info:output-flags
          modify-tty-info:control-flags
          modify-tty-info:local-flags
          modify-tty-info:input-speed
          modify-tty-info:output-speed
          modify-tty-info:min
          modify-tty-info:time

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

(define-interface signal-handler-interface
  (export with-scsh-sighandlers
          (with-enabled-signals :syntax)
          with-enabled-signals*
          enabled-signals
          set-enabled-signals!
          set-signal-handler!))

(define-interface sigevents-interface
  (export most-recent-sigevent
          sigevent?
          next-sigevent
          next-sigevent-set
          next-sigevent/no-wait
          next-sigevent-set/no-wait
          sigevent-type
          schedule-timer-interrupt!))


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

(define-interface syslog-interface
  (export (syslog-option :syntax)
          syslog-option?

          make-syslog-options
          syslog-options?
          (syslog-options :syntax)

          (syslog-facility :syntax)
          syslog-facility?

          (syslog-level :syntax)
          syslog-level?

          make-syslog-mask
          syslog-mask?
          (syslog-mask :syntax)
          syslog-mask-all
          syslog-mask-upto

          with-syslog-destination
          set-syslog-destination!

          syslog))

(define-interface syslog-channels-interface
  (export open-syslog-channel
          close-syslog-channel
          set-syslog-channel!
          with-syslog-channel))

(define-interface uname-interface
  (export uname
          uname:os-name
          uname:node-name
          uname:release
          uname:version
          uname:machine
          type/uname))

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
