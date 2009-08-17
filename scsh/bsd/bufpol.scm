;;; Flags that control buffering policy.
;;; Copyright (c) 1993 by Olin Shivers.
;;; Copyright (c) 1995 by Brian D. Carlstrom.

;;; These are for the SET-PORT-BUFFERING procedure, essentially a Scheme
;;; analog of the setbuf(3S) stdio call. We use the actual stdio values.
;;; These constants are not likely to change from stdio lib to stdio lib,
;;; but you need to check when you do a port.

(define-enum-constants bufpol
  (block	0)	; _IOFBF
  (line		1)	; _IOLBF
  (none		2))	; _IONBF
