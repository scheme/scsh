(import-lambda-definition-2 %gethostname () "scm_gethostname")

(define (system-name)
  (byte-vector->string (%gethostname)))

(define-record-type :uname
  (make-uname os-name node-name release version machine)
  uname?
  (os-name uname:os-name)
  (node-name uname:node-name)
  (release uname:release)
  (version uname:version)
  (machine uname:machine))

(define (uname)
  (make-uname (os-name)
              (os-node-name)
              (os-release-name)
              (os-version-name)
              (machine-name)))
