;;; Constant definitions for tty control code (POSIX termios).
;;; Copyright (c) 1995 by Brian Carlstrom. See file COPYING.
;;; Largely rehacked by Olin.

;;; These constants are for IRIX, 
;;; and are taken from /usr/include/sys/termio.h
;;; 		   and /usr/include/sys/termios.h
;;; 		   and /usr/include/sys/ttydev.h (baud rates).

;;; Non-standard (POSIX, SVR4, 4.3+BSD) things:
;;; - Some of the baud rates.


;;; Special Control Characters 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Indices into the c_cc[] character array.

;;;	Name	     		Subscript	Enabled by 
;;;     ----         		---------	----------
;;;  POSIX
(define ttychar/eof		4)		; ^d icanon
(define ttychar/eol		5)		;    icanon
(define ttychar/delete-char	2)		; ^? icanon
(define ttychar/delete-line	3)		; ^u icanon
(define ttychar/interrupt	0)		; ^c isig
(define ttychar/quit		1)		; ^\ isig
(define ttychar/suspend		10)		; ^z isig
(define ttychar/start		8)		; ^q ixon, ixoff
(define ttychar/stop		9)		; ^s ixon, ixoff
(define ttychar/min		4)		;    !icanon	; Not exported
(define ttychar/time		5)		;    !icanon	; Not exported

;;; SVR4 & 4.3+BSD
(define ttychar/delete-word	14)		; ^w icanon
(define ttychar/reprint 	12)		; ^r icanon
(define ttychar/literal-next	15)		; ^v iexten
(define ttychar/discard		13)		; ^o iexten
(define ttychar/delayed-suspend	11)		; ^y isig
(define ttychar/eol2		6)		;    icanon

;;; 4.3+BSD
(define ttychar/status		#f)		; ^t icanon 

;;; Length of control-char string -- *Not Exported*
(define	num-ttychars		23)

;;; Magic "disable feature" tty character
(define disable-tty-char (ascii->char #x00))	; _POSIX_VDISABLE

;;; Flags controllling input processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  POSIX
(define ttyin/ignore-break		#o00001)	; ignbrk
(define ttyin/interrupt-on-break	#o00002)	; brkint
(define ttyin/ignore-bad-parity-chars	#o00004)	; ignpar
(define ttyin/mark-parity-errors	#o00010)	; parmrk
(define ttyin/check-parity		#o00020)	; inpck
(define ttyin/7bits			#o00040)	; istrip
(define ttyin/nl->cr			#o00100)	; inlcr
(define ttyin/ignore-cr			#o00200)	; igncr
(define ttyin/cr->nl			#o00400)	; icrnl
(define ttyin/output-flow-ctl		#o02000)	; ixon
(define ttyin/input-flow-ctl		#o10000)	; ixoff

;;; SVR4 & 4.3+BSD
(define ttyin/xon-any	       #o4000)	; ixany: Any char restarts after stop
(define ttyin/beep-on-overflow #o20000)	; imaxbel: queue full => ring bell

;;; SVR4
(define ttyin/lowercase	       #o1000)	; iuclc: Map upper-case to lower case


;;; Flags controlling output processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  POSIX 
(define	ttyout/enable		 #o000001)  ; opost: enable output processing

;;; SVR4 & 4.3+BSD
(define ttyout/nl->crnl		 #o000004)	; onlcr: map nl to cr-nl

;;; 4.3+BSD
(define ttyout/discard-eot	 #f)		; onoeot
(define ttyout/expand-tabs	 #f)		; oxtabs (NOT xtabs)

;;; SVR4
(define ttyout/cr->nl		 #o000010)	; ocrnl
(define ttyout/fill-w/del	 #o000200)	; ofdel
(define ttyout/delay-w/fill-char #o000100)	; ofill
(define ttyout/uppercase	 #o000002)	; olcuc
(define ttyout/nl-does-cr	 #o000040)	; onlret
(define ttyout/no-col0-cr	 #o000020)	; onocr

;;; Newline delay
(define	ttyout/nl-delay		#o000400)	; mask (nldly)
(define	 ttyout/nl-delay0	#o000000)
(define	 ttyout/nl-delay1	#o000400)	; tty 37 

;;; Horizontal-tab delay
(define	ttyout/tab-delay	#o014000)	; mask (tabdly)
(define	 ttyout/tab-delay0	#o000000)
(define	 ttyout/tab-delay1	#o004000)	; tty 37 
(define	 ttyout/tab-delay2	#o010000)
(define	 ttyout/tab-delayx	#o014000)	; Expand tabs (xtabs, tab3)

;;; Carriage-return delay
(define	ttyout/cr-delay		#o003000)	; mask (crdly)
(define	 ttyout/cr-delay0	#o000000)
(define	 ttyout/cr-delay1	#o001000)	; tn 300 
(define	 ttyout/cr-delay2	#o002000)	; tty 37 
(define	 ttyout/cr-delay3	#o003000)	; concept 100 

;;; Vertical tab delay 
(define	ttyout/vtab-delay	#o040000)	; mask (vtdly)
(define	 ttyout/vtab-delay0	#o000000)
(define	 ttyout/vtab-delay1	#o040000)	; tty 37 

;;; Backspace delay
(define	ttyout/bs-delay		#o020000)	; mask (bsdly)
(define	 ttyout/bs-delay0	#o000000)
(define	 ttyout/bs-delay1	#o020000)

;;; Form-feed delay
(define ttyout/ff-delay		#o100000)	; mask (ffdly)
(define	 ttyout/ff-delay0	#o000000)
(define	 ttyout/ff-delay1	#o100000)

(define	ttyout/all-delay
  (bitwise-ior (bitwise-ior (bitwise-ior ttyout/nl-delay ttyout/tab-delay)
			    (bitwise-ior ttyout/cr-delay ttyout/vtab-delay))
	       (bitwise-ior ttyout/bs-delay ttyout/ff-delay)))


;;; Control flags - hacking the serial-line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  POSIX
(define ttyc/char-size		#o00060)	; csize: character size mask 
(define  ttyc/char-size5	#o00000)	; 5 bits (cs5)
(define  ttyc/char-size6	#o00020)	; 6 bits (cs6)
(define  ttyc/char-size7	#o00040)	; 7 bits (cs7)
(define  ttyc/char-size8	#o00060)	; 8 bits (cs8)
(define ttyc/2-stop-bits	#o00100)	; cstopb: Send 2 stop bits.
(define ttyc/enable-read	#o00200)	; cread: Enable receiver.
(define ttyc/enable-parity	#o00400)	; parenb
(define ttyc/odd-parity		#o01000)	; parodd
(define ttyc/hup-on-close	#o02000)	; hupcl: Hang up on last close.
(define ttyc/no-modem-sync	#o04000)	; clocal: Ignore modem lines.

;;;  4.3+BSD
(define	ttyc/ignore-flags	 #f)	; cignore: ignore control flags 
(define ttyc/CTS-output-flow-ctl #f)	; ccts_oflow: CTS flow control of output
(define ttyc/RTS-input-flow-ctl  #f)	; crts_iflow: RTS flow control of input
(define ttyc/carrier-flow-ctl	 #f)	; mdmbuf

;;; Local flags -- hacking the tty driver / user interface.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  POSIX
(define ttyl/visual-delete    #o020)	; echoe: Visually erase chars
(define ttyl/echo-delete-line #o040)	; echok: Echo nl after line kill
(define ttyl/echo	      #o010)	; echo:  Enable echoing
(define ttyl/echo-nl	      #o100)	; echonl: Echo nl even if echo is off
(define ttyl/canonical	      #o002)	; icanon: Canonicalize input
(define ttyl/enable-signals   #o001)	; isig: Enable ^c, ^z signalling
(define ttyl/extended	      #o400)	; iexten:  Enable extensions
(define ttyl/ttou-signal   #o100000)	; tostop: SIGTTOU on background output
(define ttyl/no-flush-on-interrupt #o200) ; noflsh

;;; SVR4 & 4.3+BSD
(define ttyl/visual-delete-line	#o04000) ; echoke: visually erase a line-kill 
(define ttyl/hardcopy-delete	#o02000) ; echoprt: visual erase for hardcopy 
(define ttyl/echo-ctl		#o01000) ; echoctl: echo control chars as "^X" 
(define ttyl/flush-output	#o20000) ; flusho: output is being flushed
(define ttyl/reprint-unread-chars #o40000) ; pendin: retype pending input

;;; 4.3+BSD
(define ttyl/alt-delete-word	#f)	; altwerase
(define ttyl/no-kernel-status	#f)	; nokerninfo: no kernel status on ^T

;;; SVR4
(define ttyl/case-map #o4)	; xcase: canonical upper/lower presentation

;;; Vector of (speed . code) pairs.

(define baud-rates '#((0  . 0)		(1  . 50)	(2  .    75)
		      (3  . 110)	(4  . 134)	(5  .   150)
		      (6  . 200)	(7  . 300)	(8  .   600)
		      (9  . 1200)	(10 . 1800)     (11 .  2400)	
		      (12 . 4800)       (13 . 9600)	(14 .  19200)
		      (15 . 38400)	(14 . exta)     (15 .  extb)))

;;; tcflush() constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %flush-tty/input  0)	; TCIFLUSH
(define %flush-tty/output 1)	; TCOFLUSH
(define %flush-tty/both	  2)	; TCIOFLUSH


;;; tcflow() constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %tcflow/start-out 1)	; TCOON
(define %tcflow/stop-out  0)	; TCOOFF
(define %tcflow/start-in  3)	; TCION
(define %tcflow/stop-in   2)	; TCIOFF


;;; tcsetattr() constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %set-tty-info/now	0)	; TCSANOW   Make change immediately.
(define %set-tty-info/drain	1)	; TCSADRAIN Drain output, then change.
(define %set-tty-info/flush	2)	; TCSAFLUSH Drain output, flush input.
