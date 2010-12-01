; Copyright (c) 1993-2008 by Richard Kelsey and Jonathan Rees.

; Functional red-black search trees

; See Red-Black Trees in a Functional Setting by Chris
; Okasaki. Journal of Functional Programming, 9(4):471-477, July 1999.
; (variant from Stefan Kahrs)

; Red-black trees are binary search trees obeying two key invariants:
; - Any path from a root node to a leaf node contains the same number
;   of black nodes.
; - Red nodes always have black children.

(define-record-type search-tree :search-tree
  (really-make-search-tree el= el< tree)
  search-tree?
  (el= search-tree-el=)
  (el< search-tree-el<)
  (tree search-tree-tree))

(define-record-discloser :search-tree
  (lambda (r)
    (list 'search-tree (search-tree-tree r))))

(define-record-type node :node
  (make-node red? left key value right)
  node?
  (red? node-red?)
  (left node-left)
  (key node-key)
  (value node-value)
  (right node-right))

(define-record-discloser :node
  (lambda (r)
    (list 'node
	  (if (node-red? r)
	      'red
	      'black)
	  (node-left r)
	  (node-key r)
	  (node-right r))))

(define (make-search-tree = <)
  (really-make-search-tree = < #f))

; returns invalid subtree or #f
(define (invalid-search-tree st)
  (let ((el= (search-tree-el= st))
	(el< (search-tree-el< st))
	(t (search-tree-tree st)))

    (call-with-current-continuation
     (lambda (exit)

       (define (count-blacks t)
	 (if (not t)
	     1 ; empty nodes are black
	     (let ((blacks-left (count-blacks (node-left t)))
		   (blacks-right (count-blacks (node-right t))))
	       (if (not (= blacks-left blacks-right))
		   (exit (cons 'invalid-black-count t)))
	       (if (node-red? t)
		   blacks-left
		   (+ 1 blacks-left)))))

       (define (check-children t)
	 (if t
	     (begin
	       (if (node-red? t)
		   (begin
		     (cond
		      ((node-left t)
		       => (lambda (left)
			    (if (node-red? left)
				(exit (cons 'invalid-children t))))))
		     (cond
		      ((node-right t)
		       => (lambda (right)
			    (if (node-red? right)
				(exit (cons 'invalid-children t))))))))
	       (check-children (node-left t))
	       (check-children (node-right t)))))

       (check-children t)
       (count-blacks t)
       #f))))

(define (search-tree-transform st proc)
  (let ((el= (search-tree-el= st))
	(el< (search-tree-el< st)))
    (really-make-search-tree
     el= el<
     (proc el= el< (search-tree-tree st)))))

(define (search-tree-ref st key)
  (let ((el= (search-tree-el= st))
	(el< (search-tree-el< st)))
    (let loop ((t (search-tree-tree st)))
      (and t
	   (let ((t-key (node-key t)))
	     (cond
	      ((el< key t-key)
	       (loop (node-left t)))
	      ((el= key t-key)
	       (node-value t))
	      (else
	       (loop (node-right t)))))))))

(define (search-tree-insert st key val)
  (search-tree-transform
   st
   (lambda (el= el< t)
     (make-black
      (let insert ((t t))
	(if (not t)
	    (make-node #t #f key val #f)
	    (let ((t-key (node-key t))
		  (t-val (node-value t))
		  (left (node-left t))
		  (right (node-right t)))
	      (if (node-red? t)
		  (cond
		   ((el< key t-key)
		    (make-node #t (insert left) t-key t-val right))
		   ((el= key t-key) t)
		   (else
		    (make-node #t left t-key t-val (insert right))))
		  (cond
		   ((el< key t-key)
		    (balance (insert left) t-key t-val right))
		   ((el= key t-key) t)
		   (else
		    (balance left t-key t-val (insert right))))))))))))

(define (make-black node)
  (make-node #f
	     (node-left node)
	     (node-key node) (node-value node)
	     (node-right node)))

(define (balance left key val right)
  (or (and left (node-red? left)
	   (if (and right (node-red? right))
	       (make-node #t
			  (make-black left) key val (make-black right))
	       (let ((lleft (node-left left)))
		 (if (and lleft (node-red? lleft))
		     (make-node #t
				;; #### (make-black lleft)
				(make-node #f
					   (node-left lleft)
					   (node-key lleft) (node-value lleft)
					   (node-right lleft))
				(node-key left) (node-value left)
				(make-node #f
					   (node-right left)
					   key val
					   right))
		     (let ((rleft (node-right left)))
		       (if (and rleft (node-red? rleft))
			   (make-node #t
				      (make-node #f
						 lleft
						 (node-key left) (node-value left)
						 (node-left rleft))
				      (node-key rleft) (node-value rleft)
				      (make-node #f
						 (node-right rleft)
						 key val
						 right))
			   #f))))))
      (and right (node-red? right)
	   (let ((lright (node-left right)))
	     (if (and lright (node-red? lright))
		 (make-node #t
			    (make-node #f
				       left
				       key val
				       (node-left lright))
			    (node-key lright) (node-value lright)
			    (make-node #f
				       (node-right lright)
				       (node-key right) (node-value right)
				       (node-right right)))
		 (let ((rright (node-right right)))
		   (if (and rright (node-red? rright))
		       (make-node #t
				  (make-node #f
					     left
					     key val
					     (node-left right))
				  (node-key right) (node-value right)
				  ;; #### make-black rright
				  (make-node #f
					     (node-left rright)
					     (node-key rright) (node-value rright)
					     (node-right rright)))
		       #f)))))
      (make-node #f left key val right)))

(define (search-tree-delete st key)
  (search-tree-transform
   st
   (lambda (el= el< t)

     (define (delete t)
       (if (not t)
	   #f
	   (let ((t-key (node-key t))
		 (t-val (node-value t))
		 (left (node-left t))
		 (right (node-right t)))
	     (cond
	      ((el< key t-key)
	       (delete-form-left left t-key t-val right))
	      ((el= key t-key)
	       (tree-append left right))
	      (else
	       (delete-form-right left t-key t-val right))))))

     (define (delete-form-left left key val right)
       (if (and left
		(not (node-red? left)))
	   (balance-left (delete left) key val right)
	   (make-node #t (delete left) key val right)))

     (define (delete-form-right left key val right)
       (if (and right
		(not (node-red? right)))
	   (balance-right left key val (delete right))
	   (make-node #t left key val (delete right))))

     (cond
      ((delete t) => make-black)
      (else #f)))))

(define (balance-left left key val right)
  (cond
   ((and left
	 (node-red? left))
    (make-node #t (make-black left) key val right))
   ((not (node-red? right))
    (balance left key val (make-red right)))
   (else
    (let ((lright (node-left right)))
      ;; (assert (not (node-red? lright)))
      (make-node #t
		 (make-node #f left key val (node-left lright))
		 (node-key lright) (node-value lright)
		 (balance (node-right lright)
			  (node-key right) (node-value right)
			  (make-red (node-right right))))))))

(define (balance-right left key val right)
  (cond
   ((and right
	 (node-red? right))
    (make-node #t left key val (make-black right)))
   ((not (node-red? left))
    (balance (make-red left) key val right))
   (else
    (let ((rleft (node-right left)))
      ;; (assert (not (node-red? rleft)))
      (make-node #t
		 (balance (make-red (node-left left))
			  (node-key left) (node-value left)
			  (node-left rleft))
		 (node-key rleft) (node-value rleft)
		 (make-node #f
			    (node-right rleft)
			    key val
			    right))))))

(define (tree-append t1 t2)
  (cond
   ((not t1) t2)
   ((not t2) t1)
   ((and (node-red? t1) (node-red? t2))
    (let ((a (tree-append (node-right t1) (node-left t2))))
      (if (and a (node-red? a))
	  (make-node #t
		     (make-node #t (node-left t1) (node-key t1) (node-value t1) (node-left a))
		     (node-key a) (node-value a)
		     (make-node #t (node-right a) (node-key t2) (node-value t2) (node-right t2)))
	  (make-node #t
		     (node-left t1)
		     (node-key t1) (node-value t1)
		     (make-node #t a (node-key t2) (node-value t2) (node-right t2))))))
   ((and (not (node-red? t1)) (not (node-red? t2)))
    (let ((a (tree-append (node-right t1) (node-left t2))))
      (if (and a (node-red? a))
	  (make-node #t
		     (make-node #f (node-left t1) (node-key t1) (node-value t1) (node-left a))
		     (node-key a) (node-value a)
		     (make-node #f (node-right a) (node-key t2) (node-value t2) (node-right t2)))
	  (balance-left (node-left t1)
			(node-key t1) (node-value t1)
			(make-node #f a (node-key t2) (node-value t2) (node-right t2))))))
   ((node-red? t2)
    (make-node #t
	       (tree-append t1 (node-left t2))
	       (node-key t2) (node-value t2)
	       (node-right t2)))
   ((node-red? t1)
    (make-node #t
	       (node-left t1)
	       (node-key t1) (node-value t2)
	       (tree-append (node-right t1) t2)))
   (else
    (assertion-violation 'tree-append "this cannot happen" t1 t2))))

(define (make-red node)
  (make-node #t
	     (node-left node)
	     (node-key node) (node-value node)
	     (node-right node)))

(define (search-tree-walk proc st)
  (let recur ((t (search-tree-tree st)))
    (if t
	(begin
	  (recur (node-left t))
	  (proc (node-key t) (node-value t))
	  (recur (node-right t))))))
