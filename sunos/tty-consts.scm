;;; Constant definitions for tty control code (POSIX termios).
;;; Copyright (c) 1995 by Brian Carlstrom. See file COPYING.
;;; Largely rehacked by Olin.

;;; These constants are for SunOS 4.x.x, 
;;; and are taken from /usr/include/sys/termio.h
;;; 		   and /usr/include/sys/termios.h
;;; 		   and /usr/include/sys/ttydev.h

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
(define ttychar/status		16)		; ^t icanon 

;;; Length of control-char string -- *Not Exported*
(define	num-ttychars		17)

;;; Magic "disable feature" tty character
(define disable-tty-char (ascii->char #x00))	; _POSIX_VDISABLE

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
(define ttyin/output-flow-ctl		#x00400)	; ixon
(define ttyin/input-flow-ctl		#x01000)	; ixoff


;;; SVR4 & 4.3+BSD
(define ttyin/xon-any	       #x800)	; ixany: Any char restarts after stop
(define ttyin/beep-on-overflow #x2000)	; imaxbel: queue full => ring bell

;;; SVR4
(define ttyin/lowercase	       #x200)	; iuclc: Map upper-case to lower case


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
(define ttyout/cr->nl		 #x000008)	; ocrnl
(define ttyout/fill-w/del	 #x000080)	; ofdel
(define ttyout/delay-w/fill-char #x000040)	; ofill
(define ttyout/uppercase	 #x000002)	; olcuc
(define ttyout/nl-does-cr	 #x000020)	; onlret
(define ttyout/no-col0-cr	 #x000010)	; onocr

;;; Newline delay
(define	ttyout/nl-delay		#x000100)	; mask (nldly)
(define	 ttyout/nl-delay0	#x000000)
(define	 ttyout/nl-delay1	#x000100)	; tty 37 

;;; Horizontal-tab delay
(define	ttyout/tab-delay	#x001800)	; mask (tabdly)
(define	 ttyout/tab-delay0	#x000000)
(define	 ttyout/tab-delay1	#x000800)	; tty 37 
(define	 ttyout/tab-delay2	#x001000)
(define	 ttyout/tab-delayx	#x001800)	; Expand tabs (xtabs, tab3)

;;; Carriage-return delay
(define	ttyout/cr-delay		#x000600)	; mask (crdly)
(define	 ttyout/cr-delay0	#x000000)
(define	 ttyout/cr-delay1	#x000200)	; tn 300 
(define	 ttyout/cr-delay2	#x000400)	; tty 37 
(define	 ttyout/cr-delay3	#x000600)	; concept 100 

;;; Vertical tab delay 
(define	ttyout/vtab-delay	#x004000)	; mask (vtdly)
(define	 ttyout/vtab-delay0	#x000000)
(define	 ttyout/vtab-delay1	#x004000)	; tty 37 

;;; Backspace delay
(define	ttyout/bs-delay		#x002000)	; mask (bsdly)
(define	 ttyout/bs-delay0	#x000000)
(define	 ttyout/bs-delay1	#x002000)

;;; Form-feed delay
(define ttyout/ff-delay		#x008000)	; mask (ffdly)
(define	 ttyout/ff-delay0	#x000000)
(define	 ttyout/ff-delay1	#x008000)

(define	ttyout/all-delay
  (bitwise-ior (bitwise-ior (bitwise-ior ttyout/nl-delay ttyout/tab-delay)
			    (bitwise-ior ttyout/cr-delay ttyout/vtab-delay))
	       (bitwise-ior ttyout/bs-delay ttyout/ff-delay)))


;;; Control flags - hacking the serial-line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  POSIX
(define ttyc/char-size		#x00030)	; csize: character size mask 
(define  ttyc/char-size5	#x00000)	; 5 bits (cs5)
(define  ttyc/char-size6	#x00010)	; 6 bits (cs6)
(define  ttyc/char-size7	#x00020)	; 7 bits (cs7)
(define  ttyc/char-size8	#x00030)	; 8 bits (cs8)
(define ttyc/2-stop-bits	#x00040)	; cstopb: Send 2 stop bits.
(define ttyc/enable-read	#x00080)	; cread: Enable receiver.
(define ttyc/enable-parity	#x00100)	; parenb
(define ttyc/odd-parity		#x00200)	; parodd
(define ttyc/hup-on-close	#x00400)	; hupcl: Hang up on last close.
(define ttyc/no-modem-sync	#x00800)	; clocal: Ignore modem lines.

;;;  4.3+BSD
(define	ttyc/ignore-flags	 #f)	; cignore: ignore control flags 
(define ttyc/CTS-output-flow-ctl #f)	; ccts_oflow: CTS flow control of output
(define ttyc/RTS-input-flow-ctl  #f)	; crts_iflow: RTS flow control of input
(define ttyc/carrier-flow-ctl	 #f)	; mdmbuf

;;; Local flags -- hacking the tty driver / user interface.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  POSIX
(define ttyl/visual-delete    #x010)	; echoe: Visually erase chars
(define ttyl/echo-delete-line #x020)	; echok: Echo nl after line kill
(define ttyl/echo	      #x008)	; echo:  Enable echoing
(define ttyl/echo-nl	      #x040)	; echonl: Echo nl even if echo is off
(define ttyl/canonical	      #x002)	; icanon: Canonicalize input
(define ttyl/enable-signals   #x001)	; isig: Enable ^c, ^z signalling
(define ttyl/extended	     #x8000)	; iexten:  Enable extensions
(define ttyl/ttou-signal      #x100)	; tostop: SIGTTOU on background output
(define ttyl/no-flush-on-interrupt #x80) ; noflsh

;;; SVR4 & 4.3+BSD
(define ttyl/visual-delete-line #x0800); echoke: visually erase a line-kill 
(define ttyl/hardcopy-delete	#x0400); echoprt: visual erase for hardcopy 
(define ttyl/echo-ctl		#x0200); echoctl: echo control chars as "^X" 
(define ttyl/flush-output	#x2000); flusho: output is being flushed
(define ttyl/reprint-unread-chars #x4000); pendin: retype pending input

;;; 4.3+BSD
(define ttyl/alt-delete-word	#f)	; altwerase
(define ttyl/no-kernel-status	#f)	; nokerninfo: no kernel status on ^T

;;; SVR4
(define ttyl/case-map #x4)	; xcase: canonical upper/lower presentation

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
