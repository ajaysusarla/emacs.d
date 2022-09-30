;;; package --- Summary
;;; Commentary:
;;; Code:

(defvar homedir-base-dir)

(if (eq system-type 'darwin)
    (setq homedir-base-dir "/Users/")  ; if this is MacOS
    (setq homedir-base-dir "/home/")  ; for other unixes
    )
(defvar autosave-dir
  (concat homedir-base-dir (user-login-name) "/.emacs.d/emacs_autosaves/"))

(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name
             (concat "#%" (buffer-name) "#")))))


;; Backups:
;; Put backup files (ie foo~) in one place. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat homedir-base-dir (user-login-name) "/.emacs.d/emacs_backups/"))
(setq backup-directory-alist (list (cons "." backup-dir)))

;;; autosave_and_backup.conf.el ends here
