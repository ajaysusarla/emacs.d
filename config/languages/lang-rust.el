;;; lang-rust.el --- Rust language configuration
;;; Commentary:
;;; Configuration for Rust development (enhanced from your existing config)

;;; Code:

;; Rustic mode (enhanced Rust mode)
(use-package rustic
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance)
              ("C-c C-c t" . rustic-cargo-test)
              ("C-c C-c C-t" . rustic-cargo-current-test)
              ("C-c C-c b" . rustic-cargo-build)
              ("C-c C-c r" . rustic-cargo-run)
              ("C-c C-c c" . rustic-cargo-check))
  :config
  ;; Format on save
  (setq rustic-format-on-save t
        rustic-lsp-client 'lsp-mode
        rustic-indent-offset 4)

  ;; Use rust-analyzer
  (setq rustic-analyzer-command '("rust-analyzer"))

  ;; Cargo integration
  (setq rustic-cargo-use-last-stored-arguments t
        rustic-default-clippy-arguments "--all-features")

  ;; Hook for buffer-local settings
  (add-hook 'rustic-mode-hook #'my/rustic-mode-hook))

(defun my/rustic-mode-hook ()
  "Custom hook for rustic-mode."
  ;; Prevent query for saving when running Cargo commands
  (when buffer-file-name
    (setq-local buffer-save-without-query t))

  ;; Enable features
  (electric-pair-local-mode 1)
  (subword-mode 1))

;; LSP configuration for Rust
(with-eval-after-load 'lsp-mode
  ;; Rust-analyzer settings
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil
        lsp-rust-analyzer-display-closure-return-type-hints t
        lsp-rust-analyzer-display-parameter-hints nil
        lsp-rust-analyzer-display-reborrow-hints nil
        lsp-eldoc-render-all t
        lsp-idle-delay 0.6)

  ;; Enable rust-analyzer features
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; Enhanced LSP UI for Rust
(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'bottom))

;; Flycheck for Rust
(use-package flycheck-rust
  :after flycheck
  :hook (flycheck-mode . flycheck-rust-setup))

;; Cargo mode for managing Rust projects
(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  :config
  (setq cargo-process--command-flags '("--color=always")))

;; TOML support for Cargo.toml
(use-package toml-mode
  :mode "\\.toml\\'")

;; Debugging with dap-mode (from your existing config)
(when (executable-find "lldb-mi")
  (use-package dap-mode
    :config
    (dap-ui-mode)
    (dap-ui-controls-mode 1)

    (require 'dap-lldb)
    (require 'dap-gdb-lldb)
    
    ;; Install .extension/vscode
    (dap-gdb-lldb-setup)

    (dap-register-debug-template
     "Rust::LLDB Run Configuration"
     (list :type "lldb"
           :request "launch"
           :name "LLDB::Run"
           :gdbpath "rust-lldb"
           ;; uncomment if lldb-mi is not in PATH
           ;; :lldbmipath "path/to/lldb-mi"
           ))))

;; Rust playground for quick experiments
(use-package rust-playground
  :bind (:map rust-mode-map
              ("C-c p" . rust-playground)))

;; Racer (alternative completion, if not using LSP)
(use-package racer
  :disabled t  ; Disabled since we're using LSP
  :hook (rust-mode . racer-mode)
  :config
  (setq racer-rust-src-path
        (concat (string-trim
                 (shell-command-to-string "rustc --print sysroot"))
                "/lib/rustlib/src/rust/library")))

;; Company backend for Rust (if not using LSP completion)
(use-package company-racer
  :disabled t  ; Disabled since we're using LSP
  :after company
  :config
  (add-to-list 'company-backends 'company-racer))

;; Additional Rust utilities
(defun my/rust-new-project (name)
  "Create a new Rust project."
  (interactive "sProject name: ")
  (let ((dir (read-directory-name "Directory: " "~/Projects/")))
    (shell-command (format "cd %s && cargo new %s" dir name))
    (find-file (format "%s%s/src/main.rs" dir name))))

(defun my/rust-add-dependency (crate)
  "Add a dependency to Cargo.toml."
  (interactive "sCrate name: ")
  (shell-command (format "cargo add %s" crate))
  (revert-buffer t t))

(defun my/rust-update-dependencies ()
  "Update all dependencies."
  (interactive)
  (compile "cargo update"))

(defun my/rust-build-release ()
  "Build in release mode."
  (interactive)
  (compile "cargo build --release"))

(defun my/rust-run-release ()
  "Run in release mode."
  (interactive)
  (compile "cargo run --release"))

(defun my/rust-bench ()
  "Run benchmarks."
  (interactive)
  (compile "cargo bench"))

(defun my/rust-doc ()
  "Generate and open documentation."
  (interactive)
  (compile "cargo doc --open"))

(defun my/rust-clippy-fix ()
  "Run clippy with automatic fixes."
  (interactive)
  (compile "cargo clippy --fix --allow-dirty --allow-staged"))

;; Enhanced Rust mode hooks
(add-hook 'rust-mode-hook
          (lambda ()
            ;; Local keybindings
            (local-set-key (kbd "C-c n") 'my/rust-new-project)
            (local-set-key (kbd "C-c a") 'my/rust-add-dependency)
            (local-set-key (kbd "C-c u") 'my/rust-update-dependencies)
            (local-set-key (kbd "C-c R") 'my/rust-run-release)
            (local-set-key (kbd "C-c B") 'my/rust-build-release)
            (local-set-key (kbd "C-c b") 'my/rust-bench)
            (local-set-key (kbd "C-c d") 'my/rust-doc)
            (local-set-key (kbd "C-c f") 'my/rust-clippy-fix)))

;; Rust-specific snippets
(use-package rust-snippets
  :after yasnippet)

;; Format Rust code on save (alternative to rustic's built-in)
(defun my/rust-format-buffer ()
  "Format the current Rust buffer using rustfmt."
  (interactive)
  (when (eq major-mode 'rust-mode)
    (shell-command-on-region
     (point-min) (point-max)
     "rustfmt"
     (current-buffer) t)))

;; Project-specific Rust configuration
(defun my/setup-rust-project ()
  "Set up Rust project-specific settings."
  (interactive)
  (let ((project-root (projectile-project-root)))
    (when project-root
      ;; Check for Cargo.toml
      (when (file-exists-p (concat project-root "Cargo.toml"))
        ;; Set project-specific compilation command
        (setq-local compile-command "cargo build")

        ;; Enable rust-analyzer if available
        (when (executable-find "rust-analyzer")
          (lsp))))))

;; Integration with project.el
(defun my/rust-project-test-all ()
  "Run all tests in the current Rust project."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "cargo test")))

(defun my/rust-project-check ()
  "Check the current Rust project."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "cargo check")))

;; Rust tool installation helper
(defun my/install-rust-tools ()
  "Install useful Rust tools."
  (interactive)
  (let ((tools '("cargo-edit"
                 "cargo-watch"
                 "cargo-expand"
                 "cargo-tree"
                 "cargo-outdated"
                 "cargo-audit"
                 "cargo-deny"
                 "cargo-udeps")))
    (dolist (tool tools)
      (compile (format "cargo install %s" tool)))))

(provide 'lang-rust)
;;; lang-rust.el ends here
