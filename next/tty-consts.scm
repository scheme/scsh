;;; Constant definitions for tty control code (POSIX termios).
;;; Copyright (c) 1995 by Brian Carlstrom. See file COPYING.
;;; Largely rehacked by Olin.

;;; These constants are for NeXTSTEP 3.x, 
;;; and are taken from /usr/include/bsd/sys/termios.h and
;;; /usr/include/bsd/sys/ttydev.h

;;; Non-standard (POSIX, SVR4, 4.3+BSD) things:
;;; - Useless ttychar/quote char.
;;; - Two extra newline delay values
;;; - Some control and local flags:
;;;       ttyc/2-stop-bits-when-110-baud	stopb110
;;;       ttyc/parity0 				par0
;;;       ttyc/parity1				par1
;;;	  ttyl/crt-delete			echocrt
;;;	  ttyl/xlcase				xlcase		Vas ist das?
;;;	  ttyl/xeucbksp				xeucbksp	'n das?
;;; - Some baud rates


;;; Special Control Characters 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Indices into the c_cc[] character array.

;;;	Name	     		Subscript	Enabled by 
;;;     ----         		---------	----------
;;;  POSIX
(define ttychar/eof		0)		; ^d icanon
(define ttychar/eol		1)		;    icanon
(define ttychar/delete-char	2)		; ^? icanon
(define ttychar/delete-line	3)		; ^u icanon
(define ttychar/interrupt	4)		; ^c isig
(define ttychar/quit		5)		; ^\ isig
(define ttychar/suspend		6)		; ^z isig
(define ttychar/start		7)		; ^q ixon, ixoff
(define ttychar/stop		8)		; ^s ixon, ixoff
(define ttychar/min		9)		;    !icanon	; Not exported
(define ttychar/time		10)		;    !icanon	; Not exported

;;; SVR4 & 4.3+BSD
(define ttychar/delete-word	11)		; ^w icanon
(define ttychar/reprint 	12)		; ^r icanon
(define ttychar/literal-next	13)		; ^v iexten
(define ttychar/discard		14)		; ^o iexten
(define ttychar/delayed-suspend	15)		; ^y isig
(define ttychar/eol2		#f)		;    icanon

;;; 4.3+BSD
(define ttychar/status		#f)		; ^t icanon 

;;;  NeXT
(define ttychar/quote		16)		; icanon

;;; Length of control-char string -- *Not Exported*
(define	num-ttychars		17)

;;; Magic "disable feature" tty character
(define disable-tty-char (ascii->char #xff))



;;; Flags controllling input processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  POSIX
(define ttyin/ignore-break		#x0001)	; ignbrk
(define ttyin/interrupt-on-break	#x0002)	; brkint
(define ttyin/ignore-bad-parity-chars	#x0004)	; ignpar
(define ttyin/mark-parity-errors	#x0008)	; parmrk
(define ttyin/check-parity		#x0010)	; inpck
(define ttyin/7bits			#x0020)	; istrip
(define ttyin/nl->cr			#x0040)	; inlcr
(define ttyin/ignore-cr			#x0080)	; igncr
(define ttyin/cr->nl			#x0100)	; icrnl
(define ttyin/output-flow-ctl		#x0200)	; ixon
(define ttyin/input-flow-ctl		#x0400)	; ixoff


;;; SVR4 & 4.3+BSD
(define ttyin/xon-any 		#x0800)	; ixany: Any char restarts after stop
(define ttyin/beep-on-overflow	#x2000)	; imaxbel: queue full => ring bell

;;; SVR4
(define ttyin/lowercase	       #f)	; iuclc: Map upper-case to lower case


;;; Flags controlling output processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  POSIX 
(define	ttyout/enable		#x0001)	; opost: enable output processing

;;; SVR4 & 4.3+BSD
(define ttyout/nl->crnl		#x0002)	; onlcr: map nl to cr-nl

;;; 4.3+BSD
(define ttyout/discard-eot	 #f)	; onoeot
(define ttyout/expand-tabs	 #f)	; oxtabs (NOT xtabs)

;;; SVR4
(define ttyout/cr->nl		 #f)	; ocrnl
(define ttyout/fill-w/del	 #f)	; ofdel
(define ttyout/delay-w/fill-char #f)	; ofill
(define ttyout/uppercase	 #f)	; olcuc
(define ttyout/nl-does-cr	 #f)	; onlret
(define ttyout/no-col0-cr	 #f)	; onocr

;;; Newline delay
(define	ttyout/nl-delay		#x0300)	; mask (nldly)
(define	 ttyout/nl-delay0	#x0000)
(define	 ttyout/nl-delay1	#x0100)	; tty 37
(define	 ttyout/nl-delay2	#x0200)	; vt05 		Non-standard
(define	 ttyout/nl-delay3	#x0300)	;		Non-standard

;;; Horizontal-tab delay
(define	ttyout/tab-delay	#x0c00)	; mask (tabdly)
(define	 ttyout/tab-delay0	#x0000)
(define	 ttyout/tab-delay1	#x0400)	; tty 37 
(define	 ttyout/tab-delay2	#x0800)
(define  ttyout/tab-delayx	#x0c00)	; Expand tabs (xtabs, tab3)

;;; Carriage-return delay
(define	ttyout/cr-delay		#x3000)	; mask (crdly)
(define	 ttyout/cr-delay0	#x0000)
(define	 ttyout/cr-delay1	#x1000)	; tn 300 
(define	 ttyout/cr-delay2	#x2000)	; tty 37 
(define	 ttyout/cr-delay3	#x3000)	; concept 100 

;;; Vertical tab delay 
(define	ttyout/vtab-delay	#x4000)	; mask (vtdly)
(define	 ttyout/vtab-delay0	#x0000)
(define	 ttyout/vtab-delay1	#x4000)	; tty 37 

;;; Backspace delay
(define	ttyout/bs-delay		#x8000)	; mask (bsldy)
(define	 ttyout/bs-delay0	#x0000)
(define	 ttyout/bs-delay1	#x8000)

;;; Form-feed delay -- appears to be rolled into the vertical-tab delay.
(define ttyout/ff-delay		ttyout/vtab-delay)	; mask (ffdly)
(define	 ttyout/ff-delay0	ttyout/vtab-delay0)
(define	 ttyout/ff-delay1	ttyout/vtab-delay1)

(define	ttyout/all-delay
  (bitwise-ior (bitwise-ior (bitwise-ior ttyout/nl-delay ttyout/tab-delay)
			    (bitwise-ior ttyout/cr-delay ttyout/vtab-delay))
	       (bitwise-ior ttyout/bs-delay ttyout/ff-delay)))


;;; Control flags - hacking the serial-line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  POSIX
(define ttyc/char-size		#x0300)	; csize: character size mask 
(define  ttyc/char-size5	#x0000)	; 5 bits (cs5)
(define  ttyc/char-size6	#x0100)	; 6 bits (cs6)
(define  ttyc/char-size7	#x0200)	; 7 bits (cs7)
(define  ttyc/char-size8	#x0300)	; 8 bits (cs8)
(define ttyc/2-stop-bits	#x0400)	; cstopb: Send 2 stop bits.
(define ttyc/enable-read	#x0800)	; cread: Enable receiver.
(define ttyc/enable-parity	#x1000)	; parenb
(define ttyc/odd-parity		#x2000)	; parodd
(define ttyc/hup-on-close	#x4000)	; hupcl: Hang up on last close.
(define ttyc/no-modem-sync	#x8000)	; clocal: Ignore modem lines.

;;;  4.3+BSD
(define	ttyc/ignore-flags	 #x0001); cignore: ignore control flags 
(define ttyc/CTS-output-flow-ctl #f)	; ccts_oflow: CTS flow control of output
(define ttyc/RTS-input-flow-ctl  #f)	; crts_iflow: RTS flow control of input
(define ttyc/carrier-flow-ctl	 #f)	; mdmbuf

;;;  NeXT
(define ttyc/2-stop-bits-when-110-baud	#x010000)   ; stopb110
(define ttyc/parity0			#x20000)    ; par0
(define ttyc/parity1			#x40000)    ; par1


;;; Local flags -- hacking the tty driver / user interface.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  POSIX
(define ttyl/visual-delete    #x000002)	; echoe: Visually erase chars
(define ttyl/echo-delete-line #x000004)	; echok: Echo nl after line kill
(define ttyl/echo	      #x000008)	; echo:  Enable echoing
(define ttyl/echo-nl	      #x000010)	; echonl: Echo nl even if echo is off
(define ttyl/canonical	      #x000020)	; icanon: Canonicalize input
(define ttyl/enable-signals   #x000040)	; isig: Enable ^c, ^z signalling
(define ttyl/extended	      #x000080)	; iexten:  Enable extensions
(define ttyl/ttou-signal      #x400000)	; tostop: SIGTTOU on background output
(define ttyl/no-flush-on-interrupt #x80000000)	; noflsh

;;; SVR4 & 4.3+BSD
(define ttyl/visual-delete-line	#x001)	; echoke: visually erase a line-kill 
(define ttyl/hardcopy-delete	#x200)	; echoprt: visual erase for hardcopy 
(define ttyl/echo-ctl		#x400)	; echoctl: echo control chars as "^X"
(define ttyl/flush-output	  #f)	; flusho: output is being flushed
(define ttyl/reprint-unread-chars #f)	; pendin: retype pending input

;;; 4.3+BSD
(define ttyl/alt-delete-word	#x800)	; altwerase
(define ttyl/no-kernel-status	#f)	; nokerninfo: no kernel status on ^T

;;; SVR4
(define ttyl/case-map #f)	; xcase: canonical upper/lower presentation

;;;  NeXT
(define ttyl/crt-delete		#x00000100) ; visual erase does "\b \b"
(define ttyl/xlcase		#x04000000) ; Vas ist das?
(define ttyl/xeucbksp		#x08000000) ; 'n das?

;;; NOTE: xlcase and xeucbksp are in the NeXT <termios.h>, but don't appear
;;; in the tty(4) or termios(4) man pages. Where are they documented?

;;; Vector of (speed . code) pairs.

(define baud-rates '#((0  . 0)		(1  . 50)	(2  .    75)
		      (3  . 110)	(4  . 134)	(5  .   150)
		      (6  . 200)	(7  . 300)	(8  .   600)
		      (9  . 1200)	(10 . 1800)     (11 .  2400)	
		      (12 . 4800)       (13 . 9600)	(14 .  19200)
		      (15 . 38400)	(14 . exta)     (15 .  extb)
		      (16 . 14400)      (17 . 28800)	(18 .  43200)
		      (19 . 57600)))

;;; tcflush() constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %flush-tty/input  0)	; TCIFLUSH
(define %flush-tty/output 1)	; TCOFLUSH
(define %flush-tty/both   2)	; TCIOFLUSH


;;; tcflow() constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %tcflow/stop-out  1)	; TCOOFF
(define %tcflow/start-out 2)	; TCOON
(define %tcflow/stop-in   3)	; TCIOFF
(define %tcflow/start-in  4)	; TCION


;;; tcsetattr() constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %set-tty-info/now	0)	; TCSANOW   Make change immediately.
(define %set-tty-info/drain	1)	; TCSADRAIN Drain output, then change.
(define %set-tty-info/flush	2)	; TCSAFLUSH Drain output, flush input.
