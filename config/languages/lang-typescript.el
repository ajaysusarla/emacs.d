;;; lang-typescript.el --- TypeScript language configuration
;;; Commentary:
;;; Configuration for TypeScript development with Tide

;;; Code:

;; TypeScript mode
(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :config
  (setq typescript-indent-level 2))

;; Tide - TypeScript Interactive Development Environment
(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :config
  (setq tide-completion-ignore-case t
        tide-always-show-documentation t
        tide-server-max-response-length 524288)
  
  :bind (:map tide-mode-map
              ("C-c d" . tide-documentation-at-point)
              ("C-c r" . tide-rename-symbol)
              ("C-c o" . tide-organize-imports)
              ("C-c f" . tide-format)
              ("C-c i" . tide-add-tslint-disable-next-line)))

;; Setup function for Tide
(defun setup-tide-mode ()
  "Set up Tide mode for TypeScript."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; Web mode for TypeScript JSX
(use-package web-mode
  :mode (("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-attr-value-indent-offset 2)
  
  ;; Enable TypeScript in web-mode for .tsx files
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode)))))

;; JS2 mode for JavaScript
(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2
        js2-bounce-indent-p t))

;; JSON mode
(use-package json-mode
  :mode "\\.json\\'"
  :config
  (setq json-reformat:indent-width 2))

;; Prettier for code formatting
(use-package prettier-js
  :hook ((typescript-mode . prettier-js-mode)
         (js2-mode . prettier-js-mode)
         (web-mode . prettier-js-mode))
  :config
  (setq prettier-js-args '("--single-quote"
                          "--trailing-comma" "es5"
                          "--semi")))

;; ESLint integration
(use-package flycheck
  :config
  ;; TypeScript ESLint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; Add node_modules/.bin to PATH for project-local tools
(use-package add-node-modules-path
  :hook ((typescript-mode . add-node-modules-path)
         (js2-mode . add-node-modules-path)
         (web-mode . add-node-modules-path)))

;; NPM mode for package.json
(use-package npm-mode
  :hook (typescript-mode . npm-mode))

;; YAML mode for configuration files
(use-package yaml-mode
  :mode (("\\.ya?ml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode))
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; Company backend for TypeScript
(use-package company-web
  :after company
  :config
  (add-to-list 'company-backends 'company-web-html))

;; Indium for JavaScript debugging (optional)
(use-package indium
  :hook ((js2-mode . indium-interaction-mode)
         (typescript-mode . indium-interaction-mode)))

;; TypeScript project utilities
(defun my/typescript-new-project (name)
  "Create a new TypeScript project."
  (interactive "sProject name: ")
  (let ((dir (read-directory-name "Directory: " "~/Projects/")))
    (shell-command (format "cd %s && npx create-react-app %s --template typescript" dir name))
    (find-file (format "%s%s/src/App.tsx" dir name))))

(defun my/typescript-install-types (package)
  "Install TypeScript types for a package."
  (interactive "sPackage name: ")
  (compile (format "npm install --save-dev @types/%s" package)))

(defun my/typescript-check-types ()
  "Run TypeScript type checking."
  (interactive)
  (compile "npx tsc --noEmit"))

(defun my/typescript-build ()
  "Build TypeScript project."
  (interactive)
  (compile "npm run build"))

(defun my/typescript-start-dev ()
  "Start development server."
  (interactive)
  (compile "npm start"))

(defun my/typescript-run-tests ()
  "Run TypeScript tests."
  (interactive)
  (compile "npm test"))

(defun my/typescript-lint ()
  "Run ESLint on TypeScript files."
  (interactive)
  (compile "npx eslint . --ext .ts,.tsx"))

(defun my/typescript-lint-fix ()
  "Run ESLint with automatic fixes."
  (interactive)
  (compile "npx eslint . --ext .ts,.tsx --fix"))

;; Keybindings for TypeScript development
(add-hook 'typescript-mode-hook
          (lambda ()
            ;; Local keybindings
            (local-set-key (kbd "C-c n") 'my/typescript-new-project)
            (local-set-key (kbd "C-c i") 'my/typescript-install-types)
            (local-set-key (kbd "C-c t") 'my/typescript-check-types)
            (local-set-key (kbd "C-c b") 'my/typescript-build)
            (local-set-key (kbd "C-c s") 'my/typescript-start-dev)
            (local-set-key (kbd "C-c T") 'my/typescript-run-tests)
            (local-set-key (kbd "C-c l") 'my/typescript-lint)
            (local-set-key (kbd "C-c L") 'my/typescript-lint-fix)))

;; React snippets - using yasnippet-snippets instead
;; (use-package react-snippets
;;   :after yasnippet)

;; Emmet for HTML/JSX expansion
(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (typescript-mode . emmet-mode))
  :config
  (setq emmet-expand-jsx-className? t))

;; Auto-rename JSX tags - custom implementation since package doesn't exist
(defun my/auto-rename-jsx-tag ()
  "Simple auto-rename for JSX tags."
  (when (and (derived-mode-p 'web-mode 'typescript-mode)
             (looking-back "<\\([a-zA-Z][a-zA-Z0-9]*\\)" (line-beginning-position)))
    (let ((tag-name (match-string 1)))
      (save-excursion
        (when (search-forward (concat "</" tag-name ">") (line-end-position) t)
          (replace-match (concat "</" tag-name ">")))))))

;; Add to typescript and web-mode hooks
(add-hook 'typescript-mode-hook
          (lambda ()
            (add-hook 'post-self-insert-hook 'my/auto-rename-jsx-tag nil t)))
(add-hook 'web-mode-hook
          (lambda ()
            (add-hook 'post-self-insert-hook 'my/auto-rename-jsx-tag nil t)))

;; Project-specific TypeScript configuration
(defun my/setup-typescript-project ()
  "Set up TypeScript project-specific settings."
  (interactive)
  (let ((project-root (projectile-project-root)))
    (when project-root
      ;; Check for package.json
      (when (file-exists-p (concat project-root "package.json"))
        ;; Set project-specific compilation commands
        (setq-local compile-command "npm run build")
        
        ;; Check for tsconfig.json
        (when (file-exists-p (concat project-root "tsconfig.json"))
          ;; Enable TypeScript-specific features
          (tide-setup))))))

;; Integration with LSP mode (alternative to Tide)
(use-package lsp-mode
  :hook ((typescript-mode . lsp-deferred))
  :config
  ;; TypeScript language server configuration
  (setq lsp-typescript-preferences-quote-style "single"
        lsp-typescript-preferences-import-module-specifier "relative"))

;; Deno support (if using Deno instead of Node.js)
(use-package deno-fmt
  :hook ((typescript-mode . deno-fmt-mode)
         (js2-mode . deno-fmt-mode)))

;; Package.json helper
(defun my/open-package-json ()
  "Open package.json in the current project."
  (interactive)
  (let ((package-json (concat (projectile-project-root) "package.json")))
    (if (file-exists-p package-json)
        (find-file package-json)
      (message "No package.json found in project root"))))

;; Tailwind CSS support
(use-package lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(provide 'lang-typescript)
;;; lang-typescript.el ends here
