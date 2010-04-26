(import-lambda-definition-2 %set-cloexec (fd val) "set_cloexec")

(import-lambda-definition-2 %close-fdes (fd) "scsh_close")

(import-lambda-definition-2 %dup (fd) "scsh_dup")

(import-lambda-definition-2 %dup2 (fd-from fd-to) "scsh_dup2")

(import-lambda-definition-2 %fd-seek (fd offset whence) "scsh_lseek")

(import-lambda-definition-2 %char-ready-fdes? (fd) "char_ready_fdes")

(import-lambda-definition-2 %open-raw (path flags mode) "scsh_open")

(define/vector-args %open %open-raw (path) flags mode)

(import-lambda-definition-2 %pipe-fdes () "scheme_pipe")

(import-lambda-definition-2 %truncate-file-raw (path length) "scsh_truncate")

(define/vector-args %truncate-file %truncate-file-raw (path) length)

(import-lambda-definition-2 %truncate-fdes (path length) "scsh_ftruncate")

(define %create-symlink create-symbolic-link)

(import-lambda-definition-2 %set-file-mode-raw (path mode) "scsh_chmod")

(define/vector-args %set-file-mode %set-file-mode-raw (path) mode)

(import-lambda-definition-2 %set-fdes-mode (path mode) "scsh_fchmod")

(import-lambda-definition-2 %set-file-uid&gid-raw (path uid gid) "scsh_chown")

(define/vector-args %set-file-uid&gid %set-file-uid&gid-raw (path) uid gid)

(import-lambda-definition-2 %set-fdes-uid&gid (fd uid gid) "scsh_fchown")

(import-lambda-definition-2 %utime-raw (path ac m) "scm_utime")

(define/vector-args %utime %utime-raw (path) ac m)

(import-lambda-definition-2 %utime-now-raw (path) "scm_utime_now")

(define/vector-args %utime-now %utime-now-raw (path))

(import-lambda-definition-2 %stat-file-raw (path data chase?) "scheme_stat")

(define/vector-args %stat-file %stat-file-raw (path) data chase?)

(import-lambda-definition-2 %stat-fdes (fd data) "scheme_fstat")

(import-lambda-definition-2 %sync-file (fd) "scsh_fsync")

;;; Uses real uid and gid, not effective. I don't use this anywhere.

(import-lambda-definition-2 %file-ruid-access-not-raw? (path perms) "scsh_access")

(define/vector-args %file-ruid-access-not? %file-ruid-access-not-raw? (path) perms)

;;; Amazingly bogus syscall -- doesn't *actually* sync the filesys.
(import-lambda-definition-2 %sync-file-system (fd) "scsh_fsync")
