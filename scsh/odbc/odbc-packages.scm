(define-structure low-odbc low-odbc-interface
  (open
   scheme external-calls scsh-utilities
   (subset define-record-types (define-record-discloser))
   srfi-9
   conditions signals)
  (files odbc))

(define-structure low-odbc-constants low-odbc-constants-interface
  (open scheme)
  (files
   odbc-constants))
