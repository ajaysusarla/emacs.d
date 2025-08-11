;;; config/languages/lang-shell.el --- Shell scripting configuration
;;; Commentary:
;;; Configuration for shell script development

;;; Code:

(use-package sh-script
  :ensure nil
  :mode (("\\.sh\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode)
         ("\\.zsh\\'" . sh-mode)
         ("\\.fish\\'" . fish-mode)
         ("bashrc\\'" . sh-mode)
         ("zshrc\\'" . sh-mode))
  :config
  (setq sh-basic-offset 2
        sh-indentation 2))

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :hook (sh-mode . flymake-shellcheck-load))

(use-package fish-mode
  :mode "\\.fish\\'")

(provide 'lang-shell)
