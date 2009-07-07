;;; Signal constant definitions for Linux
;;; Copyright (c) 1994 by Olin Shivers. See file COPYING.
;;; Copyright (c) 1994 by Brian D. Carlstrom.
 
(define-enum-constants signal
  ;; POSIX
  (hup		 1)
  (int		 2)
  (quit		 3)
  (ill		 4)
  (trap		 5)
  (abrt		 6)
  (iot		 6)
  (bus		 7)
  (fpe		 8)
  (kill		 9)
  (usr1		10)
  (segv		11)
  (usr2		12)
  (pipe		13)
  (alrm		14)
  (term		15)
  (stkflt	16)
  (chld		17)
  (cld		17)			;compat
  (cont		18)
  (stop		19)
  (tstp		20)
  (ttin		21)
  (ttou		22)
  (urg		23)
  (xcpu		24)
  (xfsz		25)
  (vtalrm	26)
  (prof		27)
  (winch	28)
  (io		29)
  (poll		29)
  (pwr		30)
  (unused	31)
  )

(define signals-ignored-by-default
  (list signal/chld signal/cont signal/winch))

