;;; lang-go.el --- Go language configuration
;;; Commentary:
;;; Configuration for Go development with LSP and additional tools

;;; Code:

;; Go mode
(use-package go-mode
  :mode "\\.go\\'"
  :hook ((go-mode . lsp-deferred)
         (go-mode . (lambda ()
                      (setq tab-width 4)
                      (setq indent-tabs-mode t) ; Go uses tabs
                      ;; Format and organize imports on save
                      (add-hook 'before-save-hook #'lsp-format-buffer t t)
                      (add-hook 'before-save-hook #'lsp-organize-imports t t))))
  :config
  (setq gofmt-command "goimports")
  
  ;; Go-specific keybindings
  :bind (:map go-mode-map
              ("C-c C-f" . gofmt)
              ("C-c C-i" . go-goto-imports)
              ("C-c C-d" . godoc-at-point)
              ("C-c C-r" . go-remove-unused-imports)
              ("C-c C-t" . go-test-current-test)
              ("C-c C-p" . go-test-current-project)
              ("C-c C-c" . go-run)))

;; Go eldoc
(use-package go-eldoc
  :hook (go-mode . go-eldoc-setup))

;; Go test integration
(use-package gotest
  :bind (:map go-mode-map
              ("C-c t f" . go-test-current-file)
              ("C-c t t" . go-test-current-test)
              ("C-c t p" . go-test-current-project)
              ("C-c t b" . go-test-current-benchmark)
              ("C-c t c" . go-test-current-coverage)))

;; LSP configuration for Go
(with-eval-after-load 'lsp-mode
  ;; gopls configuration
  (setq lsp-gopls-staticcheck t
        lsp-gopls-complete-unimported t
        lsp-gopls-deep-completion t
        lsp-gopls-use-placeholders t
        lsp-gopls-server-args '("-debug" ":0"))
  
  ;; Go-specific LSP settings
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)
     ("gopls.usePlaceholders" t t))))

;; Debugging with dlv (Delve)
(use-package dap-mode
  :config
  (require 'dap-dlv-go)
  
  ;; Go debug templates
  (dap-register-debug-template "Go Debug"
                               (list :type "go"
                                     :request "launch"
                                     :name "Launch Package"
                                     :mode "auto"
                                     :program nil
                                     :buildFlags nil
                                     :args nil
                                     :env nil
                                     :envFile nil)))

;; Go-specific compilation and running
(defun my/go-run-main ()
  "Run the main Go file or package."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (string-match "main\\.go$" file)
        (compile (format "go run %s" file))
      (compile "go run ."))))

(defun my/go-build-project ()
  "Build the current Go project."
  (interactive)
  (compile "go build ."))

(defun my/go-test-verbose ()
  "Run Go tests with verbose output."
  (interactive)
  (compile "go test -v ./..."))

(defun my/go-mod-tidy ()
  "Run go mod tidy."
  (interactive)
  (compile "go mod tidy"))

(defun my/go-mod-vendor ()
  "Run go mod vendor."
  (interactive)
  (compile "go mod vendor"))

;; Go tool integration
(defun my/go-install-tools ()
  "Install useful Go tools."
  (interactive)
  (let ((tools '("golang.org/x/tools/gopls@latest"
                 "github.com/go-delve/delve/cmd/dlv@latest"
                 "honnef.co/go/tools/cmd/staticcheck@latest"
                 "github.com/fatih/gomodifytags@latest"
                 "github.com/josharian/impl@latest"
                 "golang.org/x/tools/cmd/goimports@latest")))
    (dolist (tool tools)
      (compile (format "go install %s" tool)))))

;; Go work with modules
(defun my/go-work-init ()
  "Initialize Go workspace."
  (interactive)
  (compile "go work init"))

(defun my/go-work-use (dir)
  "Add directory to Go workspace."
  (interactive "DDirectory: ")
  (compile (format "go work use %s" dir)))

;; Enhanced Go mode hooks
(add-hook 'go-mode-hook
          (lambda ()
            ;; Customize indentation
            (setq tab-width 4)
            (setq indent-tabs-mode t)
            
            ;; Enable features
            (subword-mode 1)
            (electric-pair-local-mode 1)
            
            ;; Local keybindings
            (local-set-key (kbd "C-c r") 'my/go-run-main)
            (local-set-key (kbd "C-c b") 'my/go-build-project)
            (local-set-key (kbd "C-c t v") 'my/go-test-verbose)
            (local-set-key (kbd "C-c m t") 'my/go-mod-tidy)
            (local-set-key (kbd "C-c m v") 'my/go-mod-vendor)))

;; Flycheck configuration for Go
(use-package flycheck-golangci-lint
  :hook (go-mode . flycheck-golangci-lint-setup)
  :config
  (setq flycheck-golangci-lint-tests t
        flycheck-golangci-lint-fast t))

;; Go template support
(use-package go-template-mode
  :mode "\\.gotmpl\\'")

;; Additional Go utilities
(use-package go-add-tags
  :bind (:map go-mode-map
              ("C-c t a" . go-add-tags)))

(use-package go-fill-struct
  :bind (:map go-mode-map
              ("C-c f s" . go-fill-struct)))

;; Project-specific Go configuration
(defun my/setup-go-project ()
  "Set up Go project-specific settings."
  (interactive)
  (let ((project-root (projectile-project-root)))
    (when project-root
      ;; Set GOPATH if needed
      (when (file-exists-p (concat project-root "vendor/"))
        (setenv "GO111MODULE" "on"))
      
      ;; Project-specific build tags
      (dir-locals-set-class-variables
       'go-project
       '((go-mode . ((go-test-args . "-race -v"))))))))

(provide 'lang-go)
;;; lang-go.el ends here
