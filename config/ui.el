;;; ui.el --- UI and appearance configuration
;;; Commentary:
;;; Font, theme, and visual elements configuration

;;; Code:

;; Disable GUI elements
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Frame title
(setq frame-title-format '("Emacs - %b"))

;; Font configuration (from your current config, modernised)
(defun setup-fonts ()
  "Set up fonts for different platforms."
  (when (display-graphic-p)
    (cond
     ;; macOS
     ((eq system-type 'darwin)
      (set-face-attribute 'default nil
                          :font "SF Mono"
                          :height 140))
     ;; Linux
     ((eq system-type 'gnu/linux)
      (set-face-attribute 'default nil
                          :font "DejaVu Sans Mono"
                          :height 120))
     ;; Fallback
     (t
      (set-face-attribute 'default nil
                          :font "monospace"
                          :height 120)))))

(add-hook 'after-init-hook #'setup-fonts)

;; Theme configuration
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Enhanced modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-project-detection 'projectile
        doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-minor-modes nil))

;; Icons (required for doom-modeline)
(use-package all-the-icons
  :if (display-graphic-p))

;; Rainbow delimiters for better bracket visibility
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight current line
(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode 1))

;; Better whitespace visualization
(use-package whitespace
  :ensure nil
  :config
  (setq whitespace-style '(face trailing tabs tab-mark))
  (setq whitespace-display-mappings
        '((tab-mark 9 [124 9] [92 9]))) ; Display tabs as |
  :hook (prog-mode . whitespace-mode))

;; Show TODO/FIXME keywords (from your current config)
(use-package hl-todo
  :config
  (global-hl-todo-mode 1)
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("STUB"   . "#1E90FF")
          ("HACK"   . "#FF4500")
          ("XXX"    . "#FF0000")
          ("NOTE"   . "#008B8B"))))

;; Pulse line on navigation
(use-package pulse
  :ensure nil
  :config
  (defun pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  (dolist (command '(scroll-up-command scroll-down-command
                     recenter-top-bottom other-window))
    (advice-add command :after #'pulse-line)))

;; Column marker (from your current config, modernised)
(use-package display-fill-column-indicator
  :ensure nil
  :hook ((prog-mode . display-fill-column-indicator-mode)
         (text-mode . display-fill-column-indicator-mode))
  :config
  (setq display-fill-column-indicator-column 80))

;; Better window dividers
(use-package frame
  :ensure nil
  :config
  (setq window-divider-default-right-width 1
        window-divider-default-bottom-width 1)
  (window-divider-mode 1))

;; Smooth scrolling
(use-package good-scroll
  :if window-system
  :config
  (good-scroll-mode 1))

;; Visual regex
(use-package visual-regexp
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)))

;; Dim inactive buffers
(use-package auto-dim-other-buffers
  :config
  (auto-dim-other-buffers-mode 1))

;; Dashboard for startup
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-items '((recents  . 10)
                         (projects . 10)
                         (bookmarks . 5))
        dashboard-set-heading-icons t
        dashboard-set-file-icons t))

(provide 'ui)
;;; ui.el ends here
