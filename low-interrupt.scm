; Copyright (c) 1999-2001 by Martin Gasbichler. See file COPYING.

(define-enumeration low-interrupt
  (
;;; just like the VM:
   alarm
   keyboard
   post-gc
   i/o-completion
;;;; os-signal is multiplexed:
   chld 
   cont
   hup
   quit
   term
   tstp
   usr1
   usr2
   info
   io
   poll
   prof
   pwr
   urg
   vtalrm
   winch
   xcpu
   xfsz
   ))


(define number-of-interrupts
  low-interrupt-count)

(define interrupt/alarm         (enum low-interrupt alarm))
(define interrupt/keyboard	(enum low-interrupt keyboard))
(define interrupt/post-gc       (enum low-interrupt post-gc))
(define interrupt/i/o-completion        (enum low-interrupt i/o-completion))
(define interrupt/chld		(enum low-interrupt chld))
(define interrupt/cont		(enum low-interrupt cont))
(define interrupt/hup		(enum low-interrupt hup))
(define interrupt/quit		(enum low-interrupt quit))
(define interrupt/term		(enum low-interrupt term))
(define interrupt/tstp		(enum low-interrupt tstp))
(define interrupt/usr1		(enum low-interrupt usr1))
(define interrupt/usr2		(enum low-interrupt usr2))
(define interrupt/info		(enum low-interrupt info))
(define interrupt/io		(enum low-interrupt io))
(define interrupt/poll		(enum low-interrupt poll))
(define interrupt/prof		(enum low-interrupt prof))
(define interrupt/pwr		(enum low-interrupt pwr))
(define interrupt/urg		(enum low-interrupt urg))
(define interrupt/vtalrm	(enum low-interrupt vtalrm))
(define interrupt/winch		(enum low-interrupt winch))
(define interrupt/xcpu		(enum low-interrupt xcpu))
(define interrupt/xfsz		(enum low-interrupt xfsz))

(define interrupt/int	interrupt/keyboard)
(define interrupt/alrm	interrupt/alarm)

(define (interrupt-set . interrupts)
  (let lp ((ints interrupts) (ans 0))
    (if (pair? ints)
	(lp (cdr ints) (bitwise-ior ans (arithmetic-shift 1 (car ints) )))
	ans)))


(define (interrupt-in-set? int set)
  (not (zero? (bitwise-and (arithmetic-shift 1 int) set))))

(define (insert-interrupt int set)
  (bitwise-ior (arithmetic-shift 1 int) set))

(define (remove-interrupt int set)
  (if (interrupt-in-set? int set)
      (bitwise-xor (arithmetic-shift 1 int) set)
      set))

(define full-interrupt-set
  (let lp ((ans 0) (count (- number-of-interrupts 1)))
    (if (< count 0)
	ans
	(lp (insert-interrupt count ans) (- count 1)))))
