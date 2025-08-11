;;; programming.el --- General programming configuration
;;; Commentary:
;;; LSP, syntax checking, and general programming tools

;;; Code:

;; Language Server Protocol
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-prefer-flymake nil
        lsp-idle-delay 0.6
        lsp-file-watch-threshold 2000
        lsp-headerline-breadcrumb-enable t
        lsp-enable-symbol-highlighting t
        lsp-signature-auto-activate t
        lsp-signature-render-documentation t
        lsp-completion-provider :none  ; Use corfu instead
        lsp-eldoc-enable-hover t
        lsp-modeline-code-actions-enable t
        lsp-modeline-diagnostics-enable t
        lsp-log-io nil)  ; Disable for performance

  ;; Performance optimizations
  (setq lsp-use-plists t)

  :bind (:map lsp-mode-map
              ("C-c l r" . lsp-rename)
              ("C-c l f" . lsp-format-buffer)
              ("C-c l a" . lsp-execute-code-action)
              ("C-c l d" . lsp-find-definition)
              ("C-c l R" . lsp-find-references)
              ("C-c l i" . lsp-find-implementation)
              ("C-c l t" . lsp-find-type-definition)
              ("C-c l s" . lsp-signature-activate)
              ("C-c l h" . lsp-describe-thing-at-point)))

;; LSP UI enhancements
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-max-width 80
        lsp-ui-doc-max-height 20
        lsp-ui-doc-delay 1.0
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'line
        lsp-ui-peek-enable t
        lsp-ui-peek-show-directory t)
  :bind (:map lsp-ui-mode-map
              ("C-c l ." . lsp-ui-peek-find-definitions)
              ("C-c l ?" . lsp-ui-peek-find-references)
              ("C-c l i" . lsp-ui-peek-find-implementation)
              ("M-i" . lsp-ui-doc-focus-frame)))

;; Syntax checking
(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq flycheck-display-errors-delay 0.3
        flycheck-idle-change-delay 1.0
        flycheck-indication-mode 'right-fringe)
  :bind (:map flycheck-mode-map
              ("C-c ! l" . flycheck-list-errors)
              ("C-c ! n" . flycheck-next-error)
              ("C-c ! p" . flycheck-previous-error)))

;; Tree-sitter for better syntax highlighting (Emacs 29+)
(when (treesit-available-p)
  (use-package treesit
    :ensure nil
    :config
    ;; Add treesit modes to auto-mode-alist
    (setq major-mode-remap-alist
          '((c-mode . c-ts-mode)
            (c++-mode . c++-ts-mode)
            (go-mode . go-ts-mode)
            (rust-mode . rust-ts-mode)
            (typescript-mode . typescript-ts-mode)
            (js-mode . js-ts-mode)
            (css-mode . css-ts-mode)
            (json-mode . json-ts-mode)
            (yaml-mode . yaml-ts-mode)))))

;; Code formatting
(use-package format-all
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq format-all-show-errors 'warnings))

;; Multiple cursors for editing
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; Expand region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Commenting
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; Parentheses handling
(use-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (setq sp-hybrid-kill-entire-symbol nil)
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-d" . sp-down-sexp)
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("M-[" . sp-backward-unwrap-sexp)
              ("M-]" . sp-unwrap-sexp)))

;; Code folding
(use-package origami
  :hook (prog-mode . origami-mode)
  :bind (:map origami-mode-map
              ("C-c f" . origami-recursively-toggle-node)
              ("C-c F" . origami-toggle-all-nodes)))

;; Dumb jump for navigation fallback
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'completing-read
        dumb-jump-prefer-searcher 'rg))

;; Editorconfig support
(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;; Better help for programming
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-c C-d" . helpful-at-point)))

;; Aggressive indentation for some modes
(use-package aggressive-indent
  :hook ((lisp-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         (scheme-mode . aggressive-indent-mode)))

;; Highlight indentation
(use-package highlight-indentation
  :hook ((yaml-mode . highlight-indentation-mode)
         (python-mode . highlight-indentation-mode)))

;; Company for completion (fallback)
(use-package company
  :diminish company-mode
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 2
        company-show-numbers t
        company-tooltip-align-annotations t
        company-tooltip-limit 12
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("TAB" . company-complete-common-or-cycle)
              ("<tab>" . company-complete-common-or-cycle)
              ("C-d" . company-show-doc-buffer)))

;; Programming mode hooks
(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)
            (electric-pair-local-mode 1)
            (hs-minor-mode 1)))

;; Compilation mode improvements
(use-package compile
  :ensure nil
  :config
  (setq compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error))

;; Better grep
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

(provide 'programming)
;;; programming.el ends here
