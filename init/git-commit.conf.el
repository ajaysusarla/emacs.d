;; Disable toggle-save-place
(add-hook 'git-commit-mode-hook
          (lambda () (toggle-save-place 0)))
