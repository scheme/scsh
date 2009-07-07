;;; OS-dependent time stuff
;;; Copyright (c) 1995 by Olin Shivers. See file COPYING.

;;; This suffices for BSD systems with the gettimeofday()
;;; microsecond-resolution timer.

(define (ticks/sec) 1000000) ; usec

