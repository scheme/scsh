; Copyright (c) 1999-2001 by Martin Gasbichler. See file COPYING.

;;; Extend the functions of the RTS

(define (next-sigevent pre-event type)
  (if (not (sigevent? pre-event))
      (error "pre-event is not an event"))
  (rts-next-sigevent pre-event type eq?))

(define (next-sigevent-set pre-event set)
  (if (not (sigevent? pre-event))
      (error "pre-event is not an event"))
  (rts-next-sigevent pre-event set interrupt-in-set?))

(define (next-sigevent/no-wait pre-event type)
  (if (not (sigevent? pre-event))
      (error "pre-event is not an event"))
  (rts-next-sigevent/no-wait pre-event type eq?))

(define (next-sigevent-set/no-wait set pre-event)
  (if (not (sigevent? pre-event))
      (error "pre-event is not an event"))
  (rts-next-sigevent/no-wait pre-event set interrupt-in-set?))

