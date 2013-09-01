;;; Constant definitions for tty control code (POSIX termios).
;;; Copyright (c) 1995 by Brian Carlstrom. See file COPYING.
;;; Largely rehacked by Olin.
;;; Rerehacked by Roderic.

;;; Non-standard (POSIX, SVR4, 4.3+BSD) things:
;;; - Some of the baud rates.

;;; Special Control Characters
;;;  Indices into the c_cc[] character array.
(define-direct-constance tty-control-chars-info
  initialize-tty-control-chars-info
  reinitialize-tty-control-chars-info
  ((ttychar/eof VEOF)                   ; ^d icanon
   (ttychar/eol VEOL)                   ;    icanon
   (ttychar/delete-char VERASE)         ; ^? icanon
   (ttychar/delete-line VKILL)          ; ^u icanon
   (ttychar/interrupt VINTR)            ; ^c isig
   (ttychar/quit VQUIT)                 ; ^\ isig
   (ttychar/suspend VSUSP)              ; ^z isig
   (ttychar/start VSTART)               ; ^q ixon, ixoff
   (ttychar/stop VSTOP)                 ; ^s ixon, ixoff
   (ttychar/min VMIN)                   ;    !icanon	; Not exported
   (ttychar/time VTIME)                 ;    !icanon	; Not exported
   ;; SVR4 & 4.3+BSD
   (ttychar/delete-word VWERASE)       ; ^w icanon
   (ttychar/reprint VREPRINT)           ; ^r icanon
   (ttychar/literal-next VLNEXT)        ; ^v iexten
   (ttychar/discard VDISCARD)           ; ^o iexten
   (ttychar/delayed-suspend VDSUSP)     ; ^y isig
   (ttychar/eol2 VEOL2)                 ;    icanon
   ;; 4.3+BSD
   (ttychar/status VSTATUS)             ; ^t icanon
   ;; Length of control-char string. Not Exported.
   (num-ttychars NCCS)))

;;; Magic "disable feature" tty character
(define disable-tty-char (ascii->char #x00))	; _POSIX_VDISABLE

;;; Flags controllling input processing
(define-direct-constance tty-input-flags
  initialize-tty-input-flags
  reinitialize-tty-input-flags
  ;; POSIX
  ((ttyin/ignore-break IGNBRK)
   (ttyin/interrupt-on-break BRKINT)
   (ttyin/ignore-bad-parity-chars IGNPAR)
   (ttyin/mark-parity-errors PARMRK)
   (ttyin/check-parity INPCK)
   (ttyin/7bits ISTRIP)
   (ttyin/nl->cr INLCR)
   (ttyin/ignore-cr IGNCR)
   (ttyin/cr->nl ICRNL)
   (ttyin/output-flow-ctl IXON)
   (ttyin/input-flow-ctl IXOFF)
   ;; SVR4 & 4.3+BSD
   (ttyin/xon-any IXANY)
   (ttyin/beep-on-overflow IMAXBEL)
   ;; SVR4
   (ttyin/lowercase IUCLC)))

;;; Flags controlling output processing
(define-direct-constance tty-output-flags
  initialize-tty-output-flags
  reinitialize-tty-output-flags
  ;; POSIX
  ((ttyout/enable OPOST)                ; enable output processing
   ;; SVR4 & 4.3+BSD
   (ttyout/nl->crnl OPOST)              ; map nl to cr-nl
   ;; 4.3+BSD
   (ttyout/discard-eot ONOEOT)
   (ttyout/expand-tabs OXTABS)
   ;; SVR4
   (ttyout/cr->nl OCRNL)
   (ttyout/fill-w/del OFDEL)
   (ttyout/delay-w/fill-char OFILL)
   (ttyout/uppercase OLCUC)
   (ttyout/nl-does-cr ONLRET)
   (ttyout/no-col0-cr ONOCR)
   ;; Newline delay
   (ttyout/nl-delay NLDLY)              ; mask
   (ttyout/nl-delay0 NL0)
   (ttyout/nl-delay1 NL1)               ; tty 37
   ;; Horizontal-tab delay
   (ttyout/tab-delay TABDLY)            ; mask
   (ttyout/tab-delay0 TAB0)
   (ttyout/tab-delay1 TAB1)             ; tty 37
   (ttyout/tab-delay2 TAB2)
   (ttyout/tab-delayx TAB3)             ; expand tabs
   ;; Carriage-return delay
   (ttyout/cr-delay CRDLY)              ; mask
   (ttyout/cr-delay0 CR0)
   (ttyout/cr-delay1 CR1)               ; tn 300
   (ttyout/cr-delay2 CR2)               ; tty 37
   (ttyout/cr-delay3 CR3)               ; concept 100
   ;; Vertical tab delay
   (ttyout/vtab-delay VTDLY)            ; mask
   (ttyout/vtab-delay0 VT0)
   (ttyout/vtab-delay1 VT1)             ; tty 37
   ;; Backspace delay
   (ttyout/bs-delay BSDLY)              ; mask
   (ttyout/bs-delay0 BS0)
   (ttyout/bs-delay1 BS1)
   ;; Form-feed delay
   (ttyout/ff-delay FFDLY)              ; mask
   (ttyout/ff-delay0 FF0)
   (ttyout/ff-delay1 FF1)))

(define	ttyout/all-delay
  (fold (lambda (flag total)
          (if flag (bitwise-ior flag total) total)) 0
        (list ttyout/nl-delay ttyout/tab-delay
              ttyout/vtab-delay ttyout/cr-delay
              ttyout/bs-delay ttyout/ff-delay)))

;;; Control flags - hacking the serial-line.
(define-direct-constance tty-control-flags
  initialize-tty-control-flags
  reinitialize-tty-control-flags
  ;; POSIX
  ((ttyc/char-size CSIZE)               ; character size mask
   (ttyc/char-size5 CS5)                ; 5 bits
   (ttyc/char-size6 CS6)                ; 6 bits
   (ttyc/char-size7 CS7)                ; 7 bits
   (ttyc/char-size8 CS8)                ; 8 bits
   (ttyc/2-stop-bits CSTOPB)            ; send 2 stop bits
   (ttyc/enable-read CREAD)             ; enable receiver
   (ttyc/enable-parity PARENB)
   (ttyc/odd-parity PARODD)
   (ttyc/hup-on-close HUPCL)            ; hang up on last close
   (ttyc/no-modem-sync CLOCAL)          ; ignore modem lines
   ;; 4.3+BSD
   (ttyc/ignore-flags CIGNORE)          ; ignore control flagsp
   (ttyc/CTS-output-flow-ctl CCTS_OFLOW) ; CTS flow control of output
   (ttyc/RTS-input-flow-ctl CRTS_IFLOW) ; RTS flow control of input
   (ttyc/carrier-flow-ctl MDMBUF)))

;;; Local flags -- hacking the tty driver / user interface.
(define-direct-constance tty-local-flags
  initialize-tty-local-flags
  reinitialize-tty-local-flags
  ((ttyl/visual-delete ECHOE)           ; visually erase chars
   (ttyl/echo-delete-line ECHOK)        ; echo nl after line kill
   (ttyl/echo ECHO)                     ; enable echoing
   (ttyl/echo-nl ECHONL)                ; echo nl even if echo is ooff
   (ttyl/canonical ICANON)              ; canonicalize input
   (ttyl/enable-signals ISIG)           ; enable ^c, ^z signalling
   (ttyl/extended IEXTEN)               ; enable extensions
   (ttyl/ttou-signal TOSTOP)            ; SIGTTOU on background output
   (ttyl/no-flush-on-interrupt NOFLSH)
   ;; SVR4 & 4.3+BSD
   (ttyl/visual-delete-line ECHOKE)     ; visually erase a line-kill
   (ttyl/hardcopy-delete ECHOPRT)       ; visual erase for hardcopy
   (ttyl/echo-ctl ECHOCLT)              ; echo control chars as "^X"
   (ttyl/flush-output FLUSHO)           ; output is being flushed
   (ttyl/reprint-unread-chars PENDIN)   ; retype pending input
   ;; 4.3+BSD
   (ttyl/alt-delete-word ALTWERASE)
   (ttyl/no-kernel-status NOKERNINFO)   ; no kernel status on ^T
   ;; SVR4
   (ttyl/case-map XCASE)))              ; canonical upper/lower presentation

;;; Baud rate flags -- The codes corresponding to baud rates. Not the rates
;;; themselves.
(define-direct-constance tty-baud-rate-flags
  initialize-tty-baud-rate-flags
  reinitialize-tty-baud-rate-flags
  ((ttyb/0 B0)
   (ttyb/50 B50)
   (ttyb/75 B75)
   (ttyb/110 B110)
   (ttyb/134 B134)
   (ttyb/150 B150)
   (ttyb/200 B200)
   (ttyb/300 B300)
   (ttyb/600 B600)
   (ttyb/1200 B1200)
   (ttyb/1800 B1800)
   (ttyb/2400 B2400)
   (ttyb/4800 B4800)
   (ttyb/9600 B9600)
   (ttyb/19200 B19200)
   (ttyb/38400 B38400)
   (ttyb/exta EXTA)
   (ttyb/extb EXTB)))

;;; Vector of (code . speed) pairs.
(define baud-rates `#((,ttyb/0 . 0)         (,ttyb/50 . 50)     (,ttyb/75 . 75)
                      (,ttyb/110 . 110)     (,ttyb/134 . 134)   (,ttyb/150 . 150)
                      (,ttyb/200 . 200)     (,ttyb/300 . 300)   (,ttyb/600 . 600)
                      (,ttyb/1200 . 1200)   (,ttyb/1800 . 1800) (,ttyb/2400 . 2400)
                      (,ttyb/4800 . 4800)   (,ttyb/9600 . 9600) (,ttyb/19200 . 19200)
                      (,ttyb/38400 . 38400) (,ttyb/exta . exta) (,ttyb/extb . extb)))

;;; tcflush() constants
(define-direct-constance tty-tcflush-flags
  initialize-tty-tcflush-flags
  reinitialize-tty-tcflush-flags
  ((%flush-tty/input TCIFLUSH)
   (%flush-tty/output TCOFLUSH)
   (%flush-tty/both TCIOFLUSH)))

;;; tcflow() constants
(define-direct-constance tty-tcflow-flags
  initialize-tty-tcflow-flags
  reinitialize-tty-tcflow-flags
  ((%tcflow/start-out TCOON)
   (%tcflow/stop-out TCOOFF)
   (%tcflow/start-in TCION)
   (%tcflow/stop-in TCIOFF)))

;;; tcsetattr() constants
(define-direct-constance tty-tcsetattr-flags
  initialize-tty-tcsetattr-flags
  reinitialize-tty-tcsetattr-flags
  ((%set-tty-info/now TCSANOW)          ; make change immediately.
   (%set-tty-info/drain TCSADRIAN)      ; drain output, then change.
   (%set-tty-info/flush TCSAFLUSH)))    ; drain output, flush input.
