;;; package-bootstrap.el --- Ensure all required packages are installed
;;; Commentary:
;;; This file ensures all packages used in the configuration are installed

;;; Code:

;; Package management setup
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/packages/")))

;; Don't call package-initialize here since it's called in early-init.el

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; List of all packages used in this configuration
(defvar my/required-packages
  '(    ;; Core completion framework
    vertico
    orderless
    marginalia
    consult
    embark
    embark-consult
    corfu
    cape
    which-key

    ;; Company (fallback completion)
    company
    company-c-headers
    company-go
    company-web

    ;; Snippets
    yasnippet
    yasnippet-snippets

    ;; Project management
    projectile
    magit
    git-gutter
    git-gutter-fringe
    git-timemachine
    git-link
    forge
    deadgrep
    treemacs
    treemacs-projectile
    treemacs-magit
    ibuffer-projectile
    perspective
    ace-window

    ;; Programming tools
    lsp-mode
    lsp-ui
    flycheck
    flycheck-rust
    flycheck-golangci-lint
    format-all
    multiple-cursors
    expand-region
    comment-dwim-2
    smartparens
    origami
    dumb-jump
    editorconfig
    helpful
    aggressive-indent
    highlight-indentation
    wgrep

    ;; Language-specific packages
    ;; Go
    go-mode
    go-eldoc
    go-guru
    go-snippets
    gotest
    goimports
    go-add-tags
    go-fill-struct

    ;; Rust
    rustic
    cargo
    toml-mode
    rust-playground
    rust-snippets

    ;; C/C++
    modern-cpp-font-lock
    cmake-mode
    cmake-ide
    clang-format

    ;; TypeScript/JavaScript
    typescript-mode
    tide
    web-mode
    js2-mode
    json-mode
    prettier-js
    add-node-modules-path
    npm-mode
    company-web
    indium
    react-snippets
    emmet-mode
    auto-rename-tag
    lsp-tailwindcss

    ;; Shell
    fish-mode
    flymake-shellcheck

    ;; Other languages
    yaml-mode
    dockerfile-mode

    ;; Debugging
    dap-mode

    ;; UI and themes
    doom-themes
    doom-modeline
    all-the-icons
    rainbow-delimiters
    hl-line
    whitespace
    hl-todo
    display-fill-column-indicator
    good-scroll
    visual-regexp
    auto-dim-other-buffers
    ;; dashboard  ; Commented out for blank startup screen

    ;; Platform-specific
    exec-path-from-shell)
  "List of all packages required by this configuration.")

;; Function to install missing packages
(defun my/install-missing-packages ()
  "Install any missing packages from the required packages list."
  (interactive)
  (let ((missing-packages '()))
    (dolist (package my/required-packages)
      (unless (package-installed-p package)
        (push package missing-packages)))
    
    (when missing-packages
      (message "Installing missing packages: %s" missing-packages)
      (package-refresh-contents)
      (dolist (package missing-packages)
        (condition-case err
            (package-install package)
          (error (message "Failed to install %s: %s" package err)))))))

;; Function to check package status
(defun my/check-package-status ()
  "Check which packages are installed/missing."
  (interactive)
  (let ((installed '())
        (missing '()))
    (dolist (package my/required-packages)
      (if (package-installed-p package)
          (push package installed)
        (push package missing)))
    
    (with-current-buffer (get-buffer-create "*Package Status*")
      (erase-buffer)
      (insert "# Package Status Report\n\n")
      (insert (format "Total packages: %d\n" (length my/required-packages)))
      (insert (format "Installed: %d\n" (length installed)))
      (insert (format "Missing: %d\n\n" (length missing)))
      
      (when missing
        (insert "## Missing Packages:\n")
        (dolist (pkg missing)
          (insert (format "- %s\n" pkg)))
        (insert "\n"))
      
      (insert "## Installed Packages:\n")
      (dolist (pkg installed)
        (insert (format "- %s\n" pkg)))
      
      (display-buffer (current-buffer)))))

;; Function to update all packages
(defun my/update-all-packages ()
  "Update all installed packages."
  (interactive)
  (package-refresh-contents)
  (let ((upgradeable (package-menu--find-upgrades)))
    (if upgradeable
        (progn
          (message "Updating %d packages..." (length upgradeable))
          (dolist (pkg upgradeable)
            (package-install (car pkg))))
      (message "All packages are up to date"))))

;; Install missing packages on first load
(my/install-missing-packages)

(provide 'package-bootstrap)
;;; package-bootstrap.el ends here
