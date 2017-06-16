;;; init.el
;;


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cl)
(require 'package) ;; Install package
(require 'python)


;; Disable startup message
(setq inhibit-startup-message t)
;; Don't display scratch message
(setq initial-scratch-message nil)

;; Load path
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))

;; Generate autoload
(let ((generated-autoload-file "~/.emacs.d/saps-autoloads.el"))
  (update-directory-autoloads "~/.emacs.d")
  (load generated-autoload-file)
  (let ((buf (get-file-buffer generated-autoload-file)))
    (when buf (kill-buffer buf))))

(defconst *is-a-mac* (eq system-type 'darwin))

(column-number-mode 1) ;; Show column number at the bottom
(menu-bar-mode -1)     ;; Turn of the menu
(tool-bar-mode -1)     ;; Turn off the toolbar
(scroll-bar-mode -1)   ;; Turn off the scrollbar
(setq scroll-step 1)

(set-default 'indicate-buffer-boundaries '((up . nil) (down . nil) (t . left)))

(defvar saps:projects-directory (expand-file-name "~/Projects")
  "My Projects directory.")

;; Background and Foreground colour
(set-background-color "black")
(set-foreground-color "white")

;; Me
(setq user-full-name "Parthasarathi Susarla")
(setq user-mail-address "ajaysusarla@gmail.com")

(setq x-select-enable-clipboard t) ;; Clipboard
(setq jit-lock-stealth-time nil)   ;; reduce polling

;; Initialisations
(defvar init-dir
  (concat user-emacs-directory "init"))
(dolist (file (directory-files init-dir))
  (when (string-match (format "^\\(.+\\)\\.conf\\.el$") file)
    (load (concat init-dir "/" file))))

;; Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(defalias 'yes-or-no-p 'y-or-n-p)

;; My Customisation
(require 'saps-keybindings)
(require 'saps-server)
(require 'saps-coding)
(require 'saps-desktop-session)
(require 'saps-font-lock)
(require 'ipython)

;; Font
(set-face-attribute 'default (selected-frame) :height 100)
(setq frame-title-format '("" invocation-name ": %b"))
(set-frame-font "DejaVu Sans Mono-12")

;;column-marker - to show the 80 column mark for C source
(require 'column-marker)
(add-hook 'c-mode-common-hook
          (lambda ()
            (interactive)
            (column-marker-1 80)))


;; always use spaces to indent, no tab
(set-default 'indent-tabs-mode nil)

;; Show matching parentheses
(show-paren-mode t)

(setq sentence-end-double-space nil)

;; Disable annoying audio sound
(setq ring-bell-function #'ignore)
