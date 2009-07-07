;;; Test for the function in section 3.12 of the scsh-manual "Terminal device control"
;;; Author: Christoph Hetz

;; for  testing: (certainly the path will be an other on other systems...)

;; ,open define-record-types handle
;; ,config ,load C:/cygwin/home/mephisto/cvs-scsh/scsh/scsh/test/test-packages.scm
;; ,load C:/cygwin/home/mephisto/cvs-scsh/scsh/scsh/test/test-base.scm
;; load this file
;; (test-all)

(add-test! 'tty-info-record-test 'terminal-devive-control
  (lambda ()
    (let ((ti (tty-info)))
      (and (string? (tty-info:control-chars ti))
	   (or (integer? (tty-info:input-flags ti))
	       (not (tty-info:input-flags ti)))
	   (or (integer? (tty-info:output-flags ti))
	       (not (tty-info:output-flags ti)))
	   (or (integer? (tty-info:control-flags ti))
	       (not (tty-info:control-flags ti)))
	   (or (integer? (tty-info:local-flags ti))
	       (not (tty-info:local-flags ti)))
	   (or (or (integer? (tty-info:input-speed ti))
                   (memq (tty-info:input-speed ti) '(exta extb)))
	       (not (tty-info:input-speed ti)))
	   (or (or (integer? (tty-info:output-speed ti))
                   (memq (tty-info:output-speed ti) '(exta extb)))
	       (not (tty-info:output-speed ti)))
	   (or (integer? (tty-info:min ti))
	       (not (tty-info:min ti)))
	   (or (integer? (tty-info:time ti))
	       (not(tty-info:time ti)))))))

(add-test! 'make-tty-info-test 'terminal-device-control
    (lambda ()
      (let* ((in-fl 770)
	     (out-fl 3)
	     (c-fl 19200)
	     (loc-fl 1482)
	     (in-spd 1200)
	     (out-spd 1200)
	     (min 1)
	     (time 0)
	     (ti (make-tty-info in-fl 
				out-fl
				c-fl
				loc-fl
				in-spd
				out-spd
				min
				time)))
	(and (= in-fl
		(tty-info:input-flags ti))
	     (= out-fl
		(tty-info:output-flags ti))
	     (= c-fl
		(tty-info:control-flags ti))
	     (= in-spd
		(tty-info:input-speed ti))
	     (= out-spd
		(tty-info:output-speed ti))
	     (= min
		(tty-info:min ti))
	     (= time
		(tty-info:time ti))))))

(add-test! 'copy-tty-test 'terminal-device-control
  (lambda ()
    (let* ((ti (tty-info))
	   (ti-c (copy-tty-info ti)))
      (and (tty-info? ti)
	   (tty-info? ti-c)
	   (equal? (tty-info:control-chars ti)
		   (tty-info:control-chars ti-c))
	   (= (tty-info:input-flags ti)
	      (tty-info:input-flags ti-c))
	   (= (tty-info:output-flags ti)
	      (tty-info:output-flags ti-c))
	   (= (tty-info:control-flags ti)
	      (tty-info:control-flags ti-c))
	   (= (tty-info:local-flags ti)
	      (tty-info:local-flags ti-c))
	   (equal? (tty-info:input-speed ti)
		   (tty-info:input-speed ti-c))
	   (equal? (tty-info:output-speed ti)
		   (tty-info:output-speed ti-c))
	   (= (tty-info:min ti)
	      (tty-info:min ti-c))
	   (= (tty-info:time ti)
	      (tty-info:time ti-c))))))

(add-test! 'tty-info-record-posix-indicies-test 'terminal-device-control
  (lambda ()
    (and ttychar/delete-char
	 ttychar/delete-line
	 ttychar/eof
	 ttychar/eol
	 ttychar/interrupt
	 ttychar/quit
	 ttychar/suspend
	 ttychar/start
	 ttychar/stop)))

(add-test! 'tty-info-record-posix-input-flags 'terminal-device-control
  (lambda ()
    (and ttyin/check-parity
	 ttyin/ignore-bad-parity-chars
	 ttyin/mark-parity-errors
	 ttyin/ignore-break
	 ttyin/interrupt-on-break
	 ttyin/7bits
	 ttyin/cr->nl
	 ttyin/ignore-cr
	 ttyin/nl->cr
	 ttyin/input-flow-ctl
	 ttyin/output-flow-ctl)))

(add-test! 'tty-info-record-posix-output-flags 'terminal-device-control
  (lambda ()
    ttyout/enable))

(add-test! 'tty-info-record-delay-constants-for-output-flags 'terminal-device-control
  (lambda ()
    (or (and ttyout/bs-delay
	     ttyout/bs-delay0
	     ttyout/bs-delay1
	     ttyout/cr-delay
	     ttyout/cr-delay0
	     ttyout/cr-delay1
	     ttyout/cr-delay2
	     ttyout/cr-delay3
	     ttyout/ff-delay
	     ttyout/ff-delay0
	     ttyout/ff-delay1
	     ttyout/tab-delay
	     ttyout/tab-delay0
	     ttyout/tab-delay1
	     ttyout/tab-delay2
	     ttyout/tab-delayx
	     ttyout/nl-delay
	     ttyout/nl-delay0
	     ttyout/nl-delay1
	     ttyout/vtab-delay
	     ttyout/vtab-delay0
	     ttyout/vtab-delay1
	     ttyout/all-delay)
	(not (and ttyout/bs-delay
		  ttyout/bs-delay0
		  ttyout/bs-delay1
		  ttyout/cr-delay
		  ttyout/cr-delay0
		  ttyout/cr-delay1
		  ttyout/cr-delay2
		  ttyout/cr-delay3
		  ttyout/ff-delay
		  ttyout/ff-delay0
		  ttyout/ff-delay1
		  ttyout/tab-delay
		  ttyout/tab-delay0
		  ttyout/tab-delay1
		  ttyout/tab-delay2
		  ttyout/tab-delayx
		  ttyout/nl-delay
		  ttyout/nl-delay0
		  ttyout/nl-delay1
		  ttyout/vtab-delay
		  ttyout/vtab-delay0
		  ttyout/vtab-delay1
		  ttyout/all-delay)))))

(add-test! 'tty-info-record-posix-control-flags 'terminal-device-control
  (lambda ()
    (and ttyc/char-size
	 ttyc/char-size5
	 ttyc/char-size6
	 ttyc/char-size7
	 ttyc/char-size8
	 ttyc/enable-parity
	 ttyc/odd-parity
	 ttyc/enable-read
	 ttyc/hup-on-close
	 ttyc/no-modem-sync
	 ttyc/2-stop-bits)))

(add-test! 'tty-info-record-4.3+bsd-control-flags 'terminal-device-control
  (lambda ()
    (or (and ttyc/ignore-flags
	     ttyc/CTS-output-flow-ctl
	     ttyc/RTS-input-flow-ctl
	     ttyc/carrier-flow-ctl)
	(not (and ttyc/ignore-flags
		  ttyc/CTS-output-flow-ctl
		  ttyc/RTS-input-flow-ctl
		  ttyc/carrier-flow-ctl)))))

(add-test! 'tty-info-record-posix-local-flags 'terminal-device-control
  (lambda ()
    (and ttyl/canonical
	 ttyl/echo
	 ttyl/echo-delete-line
	 ttyl/echo-nl
	 ttyl/visual-delete
	 ttyl/enable-signals
	 ttyl/extended
	 ttyl/no-flush-on-interrupt
	 ttyl/ttou-signal)))

(add-test! 'tty-info-record-svr4&4.3+bsd-local-flags 'terminal-device-control
  (lambda ()
    (or (and ttyl/echo-ctl
	     ttyl/flush-output
	     ttyl/hardcopy-delete
	     ttyl/reprint-unread-chars
	     ttyl/visual-delete-line
	     ttyl/alt-delete-word
	     ttyl/no-kernel-status
	     ttyl/case-map)
	(not (and ttyl/echo-ctl
		  ttyl/flush-output
		  ttyl/hardcopy-delete
		  ttyl/reprint-unread-chars
		  ttyl/visual-delete-line
		  ttyl/alt-delete-word
		  ttyl/no-kernel-status
		  ttyl/case-map)))))

(add-test! 'open-pty-test 'terminal-device-control
   (lambda ()
     (receive (pty-inport tty-name) (open-pty)
       (let ((tty-in (open-input-file tty-name)))
         (let ((pty-out (dup->outport pty-inport)))
           (write 23 pty-out)
           (newline pty-out)
           (let ((res (equal? 23 (read tty-in))))
             (close-output-port pty-out) ;; necessary on some systems for proper exit
             res))))))


;; fails on Solaris because local echo is not turned off
(add-test! 'fork-pty-session 'terminal-device-control
   (lambda ()
     (receive (process pty-in pty-out tty-name)
         (fork-pty-session (lambda ()
                             (let ((inp (read)))
                               (write (string-append inp inp)))
                             (newline)))
       (let ((ti (copy-tty-info (tty-info pty-out))))
         (set-tty-info:local-flags ti
                                   (bitwise-xor ttyl/echo (tty-info:local-flags ti)))
         (set-tty-info/now pty-out ti))
       (write "hello" pty-out)
       (newline pty-out)
       (let ((reply (read pty-in)))
         (close-output-port pty-out) ;; necessary on some systems for proper exit
         (string=? "hellohello" reply)))))

