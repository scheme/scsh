(define-structure low-odbc low-odbc-interface
  (open
   scheme external-calls scsh-utilities
   define-record-types
   conditions signals)
  (files odbc))

(define-structure low-odbc-constants low-odbc-constants-interface
  (open scheme)
  (files
   odbc-constants))
