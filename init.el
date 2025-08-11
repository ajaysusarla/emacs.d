;;; init.el --- Modern Emacs Configuration
;;; Commentary:
;;; A clean, performant Emacs setup focused on Go, C/C++, Rust, TypeScript development

;;; Code:

;; Performance: increase read buffer size for LSP
(setq read-process-output-max (* 1024 1024)) ; 1MB

;; Package management setup
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/packages/")))

(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-expand-minimally t
      use-package-compute-statistics t)

;; Add config directory to load path
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

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
