;;; config/languages/lang-org.el --- Org mode configuration
;;; Commentary:
;;; Org mode setup for future use

;;; Code:

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-directory "~/org"
        org-default-notes-file (concat org-directory "/notes.org")
        org-agenda-files (list org-directory)
        org-capture-templates
        '(("t" "Todo" entry (file+headline "tasks.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("n" "Note" entry (file "notes.org")
           "* %?\nEntered on %U\n  %i\n  %a")))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)))

(provide 'lang-org)
