;;; project-management.el --- Project and version control configuration
;;; Commentary:
;;; Projectile, Magit, and related tools for managing multiple codebases

;;; Code:

;; Projectile - project management
(use-package projectile
  :diminish projectile-mode
  :init
  (projectile-mode +1)
  :config
  (setq projectile-completion-system 'default
        projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-sort-order 'recentf
        projectile-switch-project-action #'projectile-dired
        projectile-project-search-path '("~/Projects" "~/Work"))
  :bind-keymap
  ("C-c p" . projectile-command-map))
  ;; Note: Individual bindings removed as they conflict with the keymap

;; Magit - Git interface
(use-package magit
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch)
   ("C-c g c" . magit-clone)
   ("C-c g f" . magit-fetch)
   ("C-c g p" . magit-push))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        magit-repository-directories '(("~/Projects" . 2)
                                      ("~/Work" . 2))
        magit-save-repository-buffers 'dontask))

;; Git gutter - show changes in fringe
(use-package git-gutter
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode +1)
  (setq git-gutter:update-interval 0.02))

;; Git gutter fringe
(use-package git-gutter-fringe
  :after git-gutter
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;; Git time machine - browse Git history
(use-package git-timemachine
  :bind ("C-c g t" . git-timemachine))

;; Git link - generate links to Git hosting
(use-package git-link
  :bind ("C-c g l" . git-link)
  :config
  (setq git-link-open-in-browser t))

;; Forge - GitHub/GitLab integration
(use-package forge
  :after magit
  :config
  (setq forge-topic-list-limit '(100 . 0)))

;; Enhanced search across projects
(use-package deadgrep
  :bind ("C-c h" . deadgrep)
  :config
  (setq deadgrep-max-line-length 500))

;; File tree navigation
(use-package treemacs
  :defer t
  :bind
  (("M-0" . treemacs-select-window)
   ("C-x t t" . treemacs)
   ("C-x t B" . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag))
  :config
  (setq treemacs-width 30
        treemacs-follow-mode t
        treemacs-filewatch-mode t
        treemacs-fringe-indicator-mode 'always
        treemacs-git-mode 'deferred))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

;; Better buffer management for multiple projects
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("Dired" (mode . dired-mode))
           ("Org" (mode . org-mode))
           ("Programming" (derived-mode . prog-mode))
           ("Magit" (or (mode . magit-status-mode)
                       (mode . magit-log-mode)
                       (mode . magit-diff-mode)))
           ("Help" (or (name . "\\*Help\\*")
                      (name . "\\*Apropos\\*")
                      (name . "\\*info\\*"))))))
  
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default"))))

;; Workspace management with Perspective
(use-package perspective
  :bind
  (("C-x k" . persp-kill-buffer*)
   ("C-x x s" . persp-switch)
   ("C-x x k" . persp-kill)
   ("C-x x n" . persp-next)
   ("C-x x p" . persp-prev))
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))
  :init
  (persp-mode))

;; Enhanced window management
(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-dispatch-always t))

;; Winner mode for window configuration undo/redo
(use-package winner
  :ensure nil
  :config
  (winner-mode 1))

;; Bookmark enhancement
(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-save-flag 1
        bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory)))

;; Session management
(use-package desktop
  :ensure nil
  :config
  (setq desktop-dirname user-emacs-directory
        desktop-base-file-name "emacs.desktop"
        desktop-base-lock-name "lock"
        desktop-path (list desktop-dirname)
        desktop-save t
        desktop-auto-save-timeout 300
        desktop-restore-frames nil)
  (desktop-save-mode 1))

;; Buffer switching enhancement
(use-package bs
  :ensure nil
  :bind ("C-x C-b" . bs-show)
  :config
  (setq bs-configurations
        '(("files" "^\\*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last))))

(provide 'project-management)
;;; project-management.el ends here
