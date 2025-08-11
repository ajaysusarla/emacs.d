;;; completion.el --- Modern completion framework
;;; Commentary:
;;; Vertico, Consult, Marginalia, and Orderless for modern completion

;;; Code:

;; Vertico - vertical completion UI
(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t
        vertico-count 20))

;; Orderless - flexible completion style
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Marginalia - annotations for completions
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Consult - search and navigation commands
(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Register consult sources for 'consult-buffer'
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Use consult for register commands
  (advice-add #'register-preview :override #'consult-register-window)

  :config
  ;; Configure consult
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  ;; Configure narrowing
  (setq consult-narrow-key "<"))

;; Embark - act on things at point
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  ;; Replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Embark-Consult integration
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Corfu - in-buffer completion
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match nil)
  (corfu-preview-current nil)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  (corfu-scroll-margin 5)
  :init
  (global-corfu-mode))

;; Cape - completion functions
(use-package cape
  :init
  ;; Add completion functions to completion-at-point-functions
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  :config
  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

;; Which-key - show available keybindings
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1.0
        which-key-idle-secondary-delay 0.05))

;; Snippets
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory))))

(use-package yasnippet-snippets
  :after yasnippet)

;; Company (fallback for modes that don't work well with corfu)
(use-package company
  :diminish company-mode
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-show-numbers t
        company-tooltip-align-annotations t
        company-tooltip-limit 12))

(provide 'completion)
;;; completion.el ends here
