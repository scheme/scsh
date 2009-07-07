;;; Constant definitions for tty control code (POSIX termios).
;;; Copyright (c) 1999 by Brian Carlstrom.

;;; Special Control Characters 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Indices into the c_cc[] character array.

;;;     Name                    Subscript       Enabled by 
;;;     ----                    ---------       ----------
;;;  POSIX
(define ttychar/eof             4)              ; ^d icanon
(define ttychar/eol             2)              ;    icanon

(define ttychar/delete-char     5)              ; ^? icanon

(define ttychar/delete-line     7)              ; ^u icanon

(define ttychar/interrupt       6)              ; ^c isig
(define ttychar/quit            10)             ; ^\ isig
(define ttychar/suspend         14)             ; ^z isig

(define ttychar/start           12)             ; ^q ixon, ixoff
(define ttychar/stop            13)             ; ^s ixon, ixoff
(define ttychar/min             9)              ;    !icanon    ; Not exported
(define ttychar/time            16)             ;    !icanon    ; Not exported

;;; SVR4 & 4.3+BSD
(define ttychar/eol2            3)              ;    icanon
(define ttychar/delete-word     17)             ; ^w icanon
(define ttychar/reprint         11)             ; ^r icanon
(define ttychar/delayed-suspend #f)             ; ^y isig
(define ttychar/literal-next    8)              ; ^v iexten
(define ttychar/discard         1)              ; ^o iexten

;;; 4.3+BSD
(define ttychar/status          #f)             ; ^t icanon 

;;; Cygwin32
(define ttychar/swtc            15)             ; ???

;;; Length of control-char string -- *Not Exported*
(define num-ttychars            18)

;;; Magic "disable feature" tty character
(define disable-tty-char (ascii->char #xff))    ; _POSIX_VDISABLE

;;; Flags controllling input processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  POSIX
(define ttyin/ignore-break              #o00001)        ; ignbrk
(define ttyin/interrupt-on-break        #o00002)        ; brkint
(define ttyin/ignore-bad-parity-chars   #o00004)        ; ignpar
(define ttyin/mark-parity-errors       #o200000)        ; parmrk
(define ttyin/check-parity              #o00020)        ; inpck
(define ttyin/7bits                     #o00040)        ; istrip
(define ttyin/nl->cr                    #o00100)        ; inlcr
(define ttyin/ignore-cr                 #o00200)        ; igncr
(define ttyin/cr->nl                    #o00400)        ; icrnl
(define ttyin/output-flow-ctl           #o02000)        ; ixon
(define ttyin/input-flow-ctl            #o10000)        ; ixoff

;;; SVR4 & 4.3+BSD
(define ttyin/xon-any         #o100000) ; ixany: Any char restarts after stop
(define ttyin/beep-on-overflow #o00010) ; imaxbel: queue full => ring bell

;;; SVR4
(define ttyin/lowercase        #o40000) ; iuclc: Map upper-case to lower case


;;; Flags controlling output processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  POSIX 
(define ttyout/enable            #o0001) ; opost: enable output processing

;;; SVR4 & 4.3+BSD
(define ttyout/nl->crnl          #o0010) ; onlcr: map nl to cr-nl

;;; 4.3+BSD
(define ttyout/discard-eot       #f)    ; onoeot
(define ttyout/expand-tabs       #f)    ; oxtabs (NOT xtabs)

;;; SVR4
(define ttyout/cr->nl            #o0004) ; ocrnl
(define ttyout/fill-w/del      #o100000) ; ofdel
(define ttyout/delay-w/fill-char #o0100) ; ofill
(define ttyout/uppercase         #o0002) ; olcuc
(define ttyout/nl-does-cr        #o0040) ; onlret
(define ttyout/no-col0-cr        #o0020) ; onocr

;;; Newline delay
(define ttyout/nl-delay         #o1000) ; mask (nldly)
(define  ttyout/nl-delay0       #o0000)
(define  ttyout/nl-delay1       #o1000) ; tty 37 

;;; Horizontal-tab delay
(define ttyout/tab-delay        #o14000) ; mask (tabdly)
(define  ttyout/tab-delay0      #o00000)
(define  ttyout/tab-delay1      #o04000) ; tty 37 
(define  ttyout/tab-delay2      #o01000)
(define  ttyout/tab-delayx      #o14000) ; Expand tabs (xtabs, tab3)

;;; Carriage-return delay
(define ttyout/cr-delay         #o600)  ; mask (crdly)
(define  ttyout/cr-delay0       #o000)
(define  ttyout/cr-delay1       #o200)  ; tn 300 
(define  ttyout/cr-delay2       #o400)  ; tty 37 
(define  ttyout/cr-delay3       #o600)  ; concept 100 

;;; Vertical tab delay 
(define ttyout/vtab-delay       #o20000) ; mask (vtdly)
(define  ttyout/vtab-delay0     #o00000)
(define  ttyout/vtab-delay1     #o20000) ; tty 37 

;;; Backspace delay
(define ttyout/bs-delay         #o2000) ; mask (bsdly)
(define  ttyout/bs-delay0       #o0000)
(define  ttyout/bs-delay1       #o2000)

;;; Form-feed delay
(define ttyout/ff-delay         #o40000) ; mask (ffdly)
(define  ttyout/ff-delay0       #o00000)
(define  ttyout/ff-delay1       #o40000)

(define ttyout/all-delay        #f)

;;; Control flags - hacking the serial-line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  POSIX
(define ttyc/char-size          #o00060) ; csize: character size mask 
(define  ttyc/char-size5        #o00000) ; 5 bits (cs5)
(define  ttyc/char-size6        #o00020) ; 6 bits (cs6)
(define  ttyc/char-size7        #o00040) ; 7 bits (cs7)
(define  ttyc/char-size8        #o00060) ; 8 bits (cs8)
(define ttyc/2-stop-bits        #o00100) ; cstopb: Send 2 stop bits.
(define ttyc/enable-read        #o00200) ; cread: Enable receiver.
(define ttyc/enable-parity      #o00400) ; parenb
(define ttyc/odd-parity         #o01000) ; parodd
(define ttyc/hup-on-close       #o02000) ; hupcl: Hang up on last close.
(define ttyc/no-modem-sync      #o04000) ; clocal: Ignore modem lines.

;;;  4.3+BSD
(define ttyc/ignore-flags       #f)     ; cignore: ignore control flags 
(define ttyc/CTS-output-flow-ctl #f)    ; ccts_oflow: CTS flow control of output
(define ttyc/RTS-input-flow-ctl  #f)    ; crts_iflow: RTS flow control of input
(define ttyc/carrier-flow-ctl    #f)    ; mdmbuf

;;; Local flags -- hacking the tty driver / user interface.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  POSIX
(define ttyl/visual-delete    #x0008)   ; echoe: Visually erase chars
(define ttyl/echo-delete-line #x0010)   ; echok: Echo nl after line kill
(define ttyl/echo             #x0004)   ; echo:  Enable echoing
(define ttyl/echo-nl          #x0020)   ; echonl: Echo nl even if echo is off
(define ttyl/canonical        #x0002)   ; icanon: Canonicalize input
(define ttyl/enable-signals   #x0001)   ; isig: Enable ^c, ^z signalling
(define ttyl/extended         #x0100)   ; iexten:  Enable extensions
(define ttyl/ttou-signal      #x0080)   ; tostop: SIGTTOU on background output
(define ttyl/no-flush-on-interrupt #x0040) ; noflsh

;;; SVR4 & 4.3+BSD
(define ttyl/visual-delete-line #x0400); echoke: visually erase a line-kill 
(define ttyl/hardcopy-delete    #f); echoprt: visual erase for hardcopy 
(define ttyl/echo-ctl           #x0800); echoctl: echo control chars as "^X" 
(define ttyl/flush-output       #x0200); flusho: output is being flushed
(define ttyl/reprint-unread-chars #f); pendin: retype pending input

;;; 4.3+BSD
(define ttyl/alt-delete-word    #f)     ; altwerase
(define ttyl/no-kernel-status   #f)     ; nokerninfo: no kernel status on ^T

;;; SVR4
(define ttyl/case-map #f)       ; xcase: canonical upper/lower presentation

;;; Vector of (speed . code) pairs.

(define baud-rates '#((0  . 0)          (1  . 50)       (2  .    75)
                      (3  . 110)        (4  . 134)      (5  .   150)
                      (6  . 200)        (7  . 300)      (8  .   600)
                      (9  . 1200)       (10 . 1800)     (11 .  2400)    
                      (12 . 4800)       (13 . 9600)     (14 . 19200)
                      (15 . 38400)      (16 . 57600)    (17 .115200)))


;;; tcflush() constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %flush-tty/input  0)    ; TCIFLUSH
(define %flush-tty/output 1)    ; TCOFLUSH
(define %flush-tty/both   2)    ; TCIOFLUSH
(define %flush-tty/flush  3)    ; TCFLSH


;;; tcflow() constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %tcflow/stop-out  0)    ; TCOOFF
(define %tcflow/start-out 1)    ; TCOON
(define %tcflow/stop-in   2)    ; TCIOFF
(define %tcflow/start-in  3)    ; TCION


;;; tcsetattr() constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %set-tty-info/flush     1)      ; TCSAFLUSH Drain output, flush input.
(define %set-tty-info/now       2)      ; TCSANOW   Make change immediately.
(define %set-tty-info/drain     3)      ; TCSADRAIN Drain output, then change.
(define %set-tty-info/dflush    4)      ; TCSADFLUSH
