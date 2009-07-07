;;; Constant definitions for tty control code (POSIX termios).
;;; Copyright (c) 1995 by Brian Carlstrom. See file COPYING.
;;; Largely rehacked by Olin.

;;; These constants are for the GNU Hurd.
;;; and are taken from /usr/include/bits/termios.h.

;;; Non-standard (POSIX, SVR4, 4.3+BSD) things:
;;; - Some of the baud rates.


;;; Special Control Characters 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Indices into the c_cc[] character array.

;;;	Name	     		Subscript	Enabled by 
;;;     ----         		---------	----------
;;;  POSIX
(define ttychar/eof		0)		; ^d icanon
(define ttychar/eol		1)		;    icanon

(define ttychar/delete-char	3)		; ^? icanon

(define ttychar/delete-line	5)		; ^u icanon

(define ttychar/interrupt	8)		; ^c isig
(define ttychar/quit		9)		; ^\ isig
(define ttychar/suspend		10)		; ^z isig

(define ttychar/start		12)		; ^q ixon, ixoff
(define ttychar/stop		13)		; ^s ixon, ixoff
(define ttychar/min		16)		;    !icanon	; Not exported
(define ttychar/time		17)		;    !icanon	; Not exported

;;; SVR4 & 4.3+BSD
(define ttychar/eol2		2)		;    icanon
(define ttychar/delete-word	4)		; ^w icanon
(define ttychar/reprint 	6)		; ^r icanon
(define ttychar/delayed-suspend	11)		; ^y isig
(define ttychar/literal-next	14)		; ^v iexten
(define ttychar/discard		15)		; ^o iexten

;;; 4.3+BSD
(define ttychar/status		18)		; ^t icanon 

;;; Length of control-char string -- *Not Exported*
(define	num-ttychars		20)

;;; Magic "disable feature" tty character
(define disable-tty-char (ascii->char #xff))	; _POSIX_VDISABLE

;;; Flags controllling input processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  POSIX
(define ttyin/ignore-break		#x00001)	; ignbrk
(define ttyin/interrupt-on-break	#x00002)	; brkint
(define ttyin/ignore-bad-parity-chars	#x00004)	; ignpar
(define ttyin/mark-parity-errors	#x00008)	; parmrk
(define ttyin/check-parity		#x00010)	; inpck
(define ttyin/7bits			#x00020)	; istrip
(define ttyin/nl->cr			#x00040)	; inlcr
(define ttyin/ignore-cr			#x00080)	; igncr
(define ttyin/cr->nl			#x00100)	; icrnl
(define ttyin/output-flow-ctl		#x00200)	; ixon
(define ttyin/input-flow-ctl		#x00400)	; ixoff

;;; SVR4 & 4.3+BSD
(define ttyin/xon-any	       #x00800)	; ixany: Any char restarts after stop
(define ttyin/beep-on-overflow #x02000)	; imaxbel: queue full => ring bell

;;; SVR4
(define ttyin/lowercase	       #x04000)	; iuclc: Map upper-case to lower case


;;; Flags controlling output processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  POSIX 
(define	ttyout/enable		 #x000001)  ; opost: enable output processing

;;; SVR4 & 4.3+BSD
(define ttyout/nl->crnl		 #x000002)	; onlcr: map nl to cr-nl

;;; 4.3+BSD
(define ttyout/discard-eot	 #x000008)	; onoeot
(define ttyout/expand-tabs	 #x000004)	; oxtabs (NOT xtabs)

;;; SVR4
(define ttyout/cr->nl		 #f)		; ocrnl
(define ttyout/fill-w/del	 #f)		; ofdel
(define ttyout/delay-w/fill-char #f)		; ofill
(define ttyout/uppercase	 #x000200)	; olcuc
(define ttyout/nl-does-cr	 #f)		; onlret
(define ttyout/no-col0-cr	 #f)		; onocr

;;; Newline delay
(define	ttyout/nl-delay		#f)	; mask (nldly)
(define	 ttyout/nl-delay0	#f)
(define	 ttyout/nl-delay1	#f)	; tty 37 

;;; Horizontal-tab delay
(define	ttyout/tab-delay	#f)	; mask (tabdly)
(define	 ttyout/tab-delay0	#f)
(define	 ttyout/tab-delay1	#f)	; tty 37 
(define	 ttyout/tab-delay2	#f)
(define	 ttyout/tab-delayx	#f)	; Expand tabs (xtabs, tab3)

;;; Carriage-return delay
(define	ttyout/cr-delay		#f)	; mask (crdly)
(define	 ttyout/cr-delay0	#f)
(define	 ttyout/cr-delay1	#f)	; tn 300 
(define	 ttyout/cr-delay2	#f)	; tty 37 
(define	 ttyout/cr-delay3	#f)	; concept 100 

;;; Vertical tab delay 
(define	ttyout/vtab-delay	#f)	; mask (vtdly)
(define	 ttyout/vtab-delay0	#f)
(define	 ttyout/vtab-delay1	#f)	; tty 37 

;;; Backspace delay
(define	ttyout/bs-delay		#f)	; mask (bsdly)
(define	 ttyout/bs-delay0	#f)
(define	 ttyout/bs-delay1	#f)

;;; Form-feed delay
(define ttyout/ff-delay		#f)	; mask (ffdly)
(define	 ttyout/ff-delay0	#f)
(define	 ttyout/ff-delay1	#f)

(define	ttyout/all-delay	#f)

;;; Control flags - hacking the serial-line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  POSIX
(define ttyc/char-size		#x00300)	; csize: character size mask 
(define  ttyc/char-size5	#x00000)	; 5 bits (cs5)
(define  ttyc/char-size6	#x00100)	; 6 bits (cs6)
(define  ttyc/char-size7	#x00200)	; 7 bits (cs7)
(define  ttyc/char-size8	#x00300)	; 8 bits (cs8)
(define ttyc/2-stop-bits	#x00400)	; cstopb: Send 2 stop bits.
(define ttyc/enable-read	#x00800)	; cread: Enable receiver.
(define ttyc/enable-parity	#x01000)	; parenb
(define ttyc/odd-parity		#x02000)	; parodd
(define ttyc/hup-on-close	#x04000)	; hupcl: Hang up on last close.
(define ttyc/no-modem-sync	#x08000)	; clocal: Ignore modem lines.

;;;  4.3+BSD
(define	ttyc/ignore-flags	#x00001)	; cignore: ignore control flags 
(define ttyc/CTS-output-flow-ctl #x00010000)	; ccts_oflow: CTS flow control of output
(define ttyc/RTS-input-flow-ctl  #x00020000)	; crts_iflow: RTS flow control of input
(define ttyc/carrier-flow-ctl	 #x00100000)	; mdmbuf

;;; Local flags -- hacking the tty driver / user interface.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  POSIX
(define ttyl/visual-delete    #x00000002)	; echoe: Visually erase chars
(define ttyl/echo-delete-line #x00000004)	; echok: Echo nl after line kill
(define ttyl/echo	      #x00000008)	; echo:  Enable echoing
(define ttyl/echo-nl	      #x00000010)	; echonl: Echo nl even if echo is off
(define ttyl/canonical	      #x00000100)	; icanon: Canonicalize input
(define ttyl/enable-signals   #x00000080)	; isig: Enable ^c, ^z signalling
(define ttyl/extended	      #x00000400)	; iexten:  Enable extensions
(define ttyl/ttou-signal      #x00400000)	; tostop: SIGTTOU on background output
(define ttyl/no-flush-on-interrupt #x80000000) ; noflsh

;;; SVR4 & 4.3+BSD
(define ttyl/visual-delete-line #x00000001); echoke: visually erase a line-kill 
(define ttyl/hardcopy-delete	#x00000020); echoprt: visual erase for hardcopy 
(define ttyl/echo-ctl		#x00000040); echoctl: echo control chars as "^X" 
(define ttyl/flush-output	#x00800000); flusho: output is being flushed
(define ttyl/reprint-unread-chars #x20000000); pendin: retype pending input

;;; 4.3+BSD
(define ttyl/alt-delete-word	#x00000200)	; altwerase
(define ttyl/no-kernel-status	#x02000000)	; nokerninfo: no kernel status on ^T

;;; SVR4
(define ttyl/case-map #f)	; xcase: canonical upper/lower presentation

;;; Vector of (speed . code) pairs.

(define baud-rates '#((0      . 0)	(50     . 50)	  (75     . 75)
		      (110    . 110)	(134    . 134)	  (150    . 150)
		      (200    . 200)	(300    . 300)	  (600    . 600)
		      (1200   . 1200)	(1800   . 1800)   (2400   . 2400)	
		      (4800   . 4800)   (9600   . 9600)   (19200  . 19200)
		      (38400  . 38400)	(19200  . exta)   (38400  . extb)
		      (57600  . 57600)	(115200 . 115200) (230400 . 230400)))

;;; tcflush() constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %flush-tty/input  1)	; TCIFLUSH
(define %flush-tty/output 2)    ; TCOFLUSH
(define %flush-tty/both	  3)	; TCIOFLUSH


;;; tcflow() constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %tcflow/start-out 2)	; TCOON
(define %tcflow/stop-out  1)	; TCOOFF
(define %tcflow/start-in  4)	; TCION
(define %tcflow/stop-in   3)	; TCIOFF


;;; tcsetattr() constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %set-tty-info/now	0)	; TCSANOW   Make change immediately.
(define %set-tty-info/drain	1)	; TCSADRAIN Drain output, then change.
(define %set-tty-info/flush	2)	; TCSAFLUSH Drain output, flush input.
(define %set-tty-info/soft	#x10)	; flag: don't alter h.w. state 
