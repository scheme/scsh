;;; CPP Lib
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Character->Character Partial functions
 
;;; Many of these types are pretty weak, but there is no way to
;;; specify that a parameter must be a particular record type.
;;; Every little bit helps, though.

(define-interface ccp-lib-interface
  (export
   ;; ccp? x -> boolean
   (ccp? (proc (:value) :boolean))

   ;; ccp-domain ccp -> char-set
   (ccp-domain (proc (:value) :value)) ; Not very informative.

   ;; ccp-copy ccp -> ccp
   (ccp-copy (proc (:value) :value))

   ;; ccp=  ccp1 ccp2 ...
   ;; ccp<= ccp1 ccp2 ...
   ((ccp= ccp<=) (proc (&rest :value) :boolean)) ; Not very informative.

   ;; ccp-fold kons knil ccp -> value
   (ccp-fold (proc ((proc (:char :char :value) :value) :value :value) :value))
   
   ;; ccp-for-each proc ccp
   (ccp-for-each (proc ((proc (:char :char) :values)) :unspecific))

   ;; ccp->alist ccp -> alist
   (ccp->alist (proc (:value) :value))

   ;; ccp-restrict  ccp cset -> ccp
   ;; ccp-restrict! ccp cset -> ccp
   ((ccp-restrict ccp-restrict!) (proc (:value :value) :value))

   ;; ccp-adjoin  ccp from-char1 to-char1 ... -> ccp
   ;; ccp-adjoin! ccp from-char1 to-char1 ... -> ccp
   ;; ccp-delete  ccp from-char1 ... -> ccp
   ;; ccp-delete! ccp from-char1 ... -> ccp
   ((ccp-adjoin ccp-adjoin!) (proc (:value &rest :char) :value))
   ((ccp-delete ccp-delete!) (proc (:value &rest :char) :value))

   ;; ccp-extend  ccp1 ... -> ccp
   ;; ccp-extend! ccp1 ... -> ccp
   ((ccp-extend ccp-extend!) (proc (&rest :value) :value))

   ;; ccp-compose ccp1 ... -> ccp
   (ccp-compose (proc (&rest :value) :value))

   ;; alist->ccp  char/char-alist [ccp] -> ccp
   ;; alist->ccp! char/char-alist [ccp] -> ccp
   ((alist->ccp alist->ccp!) (proc (:value &opt :value) :value))

   ;; proc->ccp  proc [domain ccp] -> ccp
   ;; proc->ccp! proc [domain ccp] -> ccp
   ((proc->ccp proc->ccp!) (proc ((proc (:char) :char) &opt :value :value)
				 :value))

   ;; constant-ccp  char [domain ccp] -> ccp
   ;; constant-ccp! char  domain ccp  -> ccp
   ((constant-ccp constant-ccp!) (proc (:char &opt :value :value) :value))

   ;; ccp/mappings from1 to1 ... -> ccp
   ;; extend-ccp/mappings  ccp from1 to1 ... -> ccp
   ;; extend-ccp/mappings! ccp from1 to1 ... -> ccp
   (ccp/mappings (proc (&rest :value) :value))
   ((extend-ccp/mappings extend-ccp/mappings!)
    (proc (:value &rest :value) :value))

   ;; construct-ccp  ccp elt ... -> ccp
   ;; construct-ccp! ccp elt ... -> ccp
   ((construct-ccp construct-ccp!) (proc (:value &rest :value) :value))

   ;; ccp-unfold p f g seed -> ccp
   (ccp-unfold (proc ((proc (:value) :boolean)
		      (procedure :value (some-values :char :char))
		      (proc (:value) :value)
		      :value)
		     :value))

   ;; tr       ccp string [start end] -> string
   ;; ccp-map  ccp string [start end] -> string
   ;; ccp-map! ccp string [start end]
   ;; ccp-app ccp char -> char or false
   ((tr ccp-map)
    (proc (:value :string &opt :exact-integer :exact-integer) :string))
   (ccp-map! (proc (:value :string &opt :exact-integer :exact-integer) :unspecific))
   (ccp-app (proc (:value :char) :value))

   ;; Primitive CCP's.
   ccp:0 ccp:1 ccp:upcase ccp:downcase
   ))

(define-structure ccp-lib ccp-lib-interface
  (open srfi-14
	ascii
	defrec-package
	srfi-13
	let-opt
	receiving
	(subset srfi-1 (every fold))
	error-package
	scheme)
  (files ccp)
  (optimize auto-integrate))
