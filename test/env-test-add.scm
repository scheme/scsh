; definitions

(define alist
  '(("Test-EDITOR" . "MyEditor")
    ("Test-TERM"   . "SuperScsh")
    ("Test-EDITOR" . "HerEditor")))
(define env-alist-alist
  '(("TEST-PATH" . '("Path1" "Path2" "Path3"))))

(define number-list '("Eins" "Zwei" "Vier" "Eins" "Zwei" "Vier"))


; adds tests

(add-test! 'setenv 'env setenv-test "Test-Var" "Hello!")
;(add-test! 'getenv 'env getenv-test "Test-Var" "Hallihallo!")   ; same as setenv-test
(add-test! 'env->alist 'env env->alist-test "env->alist-test-var" "env->alist-test-val")

; COMMENTED OUT, since recent version of scsh produces an exception if
; alist contains string-lists as values. Nevertheless the manual
; says, stringlists are allowed as values (p.73)
;(add-test! 'alist->env 'env alist->env-test (cons '("String-list" . ("String1" "String2" "String3")) alist))
(add-test! 'alist-delete 'env alist-delete-test "Test-EDITOR" alist)
(add-test! 'alist-update 'env alist-update-test "Test-EDITOR" "HisEditor" alist)

(add-test! 'alist-compress 'env alist-compress-test alist)
(add-test! 'with-env* 'env with-env*-test alist)
(add-test! 'with-total-env* 'env with-total-env*-test alist)
(add-test! 'home-directory 'env home-directory-test)
(add-test! 'exec-path-list 'env exec-path-list-test)
(add-test! 'add-before-infix 'env add-before-test "Drei" "Vier" number-list)
(add-test! 'add-before-suffix 'env add-before-test "Fünf" "Sechs" number-list)
(add-test! 'add-after-infix 'env add-after-test "Drei" "Zwei" number-list)
(add-test! 'add-after-prefix 'env add-after-test "Null" "Null" number-list)
(add-test! 'add-after-prefix 'env add-after-test "Drei" "Zwei" number-list)
