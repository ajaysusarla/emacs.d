;;; package --- package initialise
;;; Commentary:
;;;  init.el - says it all

(require 'cl)
(require 'package) ;; Install package
(require 'python)
(require 'haskell)


;;; Code:

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
(setq user-mail-address "mail@spartha.org")

(setq select-enable-clipboard t) ;; Clipboard
(setq jit-lock-stealth-time nil) ;; reduce polling

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
(set-frame-font "DejaVu Sans Mono-18")

;;column-marker - to show the 80 column mark for C source
(require 'column-marker)
(add-hook 'c-mode-common-hook
          (lambda ()
            (interactive)
            (column-marker-1 80)))

;; complete anything
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

;; Go - lsp-mode
;; Set up before-save hooks to format buffer and add/delete imports.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Start LSP Mode and YASnippet mode
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)

;; always use spaces to indent, no tab
(set-default 'indent-tabs-mode nil)

;; Show matching parentheses
(show-paren-mode t)

(setq sentence-end-double-space nil)

;; Disable annoying audio sound
(setq ring-bell-function #'ignore)
(put 'upcase-region 'disabled nil)

;; Auto refresh buffers
(global-auto-revert-mode t)

;; Display line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;;; Auto indent YAML files
(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;; Tide mode for typescript support
(use-package tide :ensure t)
(use-package company :ensure t)
(use-package flycheck :ensure t)

  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)

  (add-hook 'typescript-mode-hook #'setup-tide-mode)

(provide 'init)
;;; init.el ends here
