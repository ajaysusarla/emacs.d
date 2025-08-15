;;; core.el --- Core Emacs settings
;;; Commentary:
;;; Basic Emacs configuration and sensible defaults

;;; Code:

;; Basic settings
(setq-default
 indent-tabs-mode nil                ; Use spaces instead of tabs
 tab-width 4                         ; Tab width
 fill-column 80                      ; Line length
 sentence-end-double-space nil       ; Single space after sentences
 require-final-newline t             ; Always end files with newline
 truncate-lines nil                  ; Wrap long lines
 auto-save-default t                 ; Auto-save files
 make-backup-files t)                ; Create backup files

;; Startup optimizations
(setq inhibit-startup-message t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)  ; Start in fundamental-mode for blank screen

;; File handling
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t))
      delete-old-versions t
      version-control t
      kept-new-versions 6
      kept-old-versions 2)

;; Create backup directories
(make-directory (expand-file-name "backups" user-emacs-directory) t)
(make-directory (expand-file-name "auto-saves" user-emacs-directory) t)

;; Clipboard integration
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t)

;; Better defaults
(setq ring-bell-function 'ignore        ; No audio bell
      use-dialog-box nil                ; Use minibuffer for prompts
      confirm-kill-emacs 'yes-or-no-p   ; Confirm before exit
      help-window-select t              ; Focus help windows
      scroll-conservatively 10000       ; Smooth scrolling
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;; Font-lock (syntax highlighting) configuration
(setq font-lock-maximum-decoration t)  ; Maximum syntax highlighting
(setq font-lock-maximum-size nil)      ; No size limit for font-lock

;; Enable useful features
(global-auto-revert-mode 1)           ; Auto-refresh buffers
(savehist-mode 1)                     ; Save minibuffer history
(save-place-mode 1)                   ; Remember cursor position
(recentf-mode 1)                      ; Track recent files
(delete-selection-mode 1)             ; Replace selected text
(column-number-mode 1)                ; Show column numbers
(show-paren-mode 1)                   ; Highlight matching parentheses
(electric-pair-mode 1)                ; Auto-pair brackets
(global-font-lock-mode 1)             ; Enable syntax highlighting globally

;; Line numbers for programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Recentf settings
(setq recentf-max-saved-items 100
      recentf-max-menu-items 15)

;; Save place settings
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))

;; Savehist settings
(setq savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring)
      savehist-file (expand-file-name "savehist" user-emacs-directory))

;; Uniquify buffer names
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-ignore-buffers-re "^\\*")

;; Mouse settings (from your current config)
(setq mouse-yank-at-point t)

;; Better handling of large files
(setq large-file-warning-threshold (* 50 1024 1024)) ; 50MB

;; Disable warnings for some commands
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; UTF-8 everywhere
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Better abbreviation handling
(setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))

(provide 'core)
;;; core.el ends here
