;Thread nomenclature for scsh.

(define fork/thread (structure-ref thread-fluids fork-thread))

(define fork/process (structure-ref scsh-level-0 fork))

(define wait/thread (structure-ref threads-internal wait-for-event))

(define wait/process (structure-ref scsh-level-0 wait))
