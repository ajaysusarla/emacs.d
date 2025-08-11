;;; init.el --- Modern Emacs Configuration
;;; Commentary:
;;; A clean, performant Emacs setup focused on Go, C/C++, Rust, TypeScript development

;;; Code:

;; Performance: increase read buffer size for LSP
(setq read-process-output-max (* 1024 1024)) ; 1MB

;; Add config directory to load path
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "config/languages" user-emacs-directory))

;; Bootstrap package system and install required packages
(load (expand-file-name "package-bootstrap.el" user-emacs-directory))

;; Add config directory to load path
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; Bootstrap package system and install required packages
(load (expand-file-name "package-bootstrap.el" user-emacs-directory))

;; Personal information (from your current config)
(setq user-full-name "Parthasarathi Susarla"
      user-mail-address "mail@spartha.org")

;; Custom file location
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Load configuration modules
(require 'core)
(require 'ui)
(require 'completion)
(require 'project-management)
(require 'programming)
(require 'keybindings)
(require 'platform)

;; Load language-specific configurations
(require 'lang-c)
(require 'lang-go)
(require 'lang-rust)
(require 'lang-typescript)
(require 'lang-shell)
(require 'lang-org)

;; Restore GC settings after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)))

;; Display startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(provide 'init)
;;; init.el ends here
