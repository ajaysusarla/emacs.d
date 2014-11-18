(add-hook 'text-mode-hook
          (defun saps:text-mode-hook ()
            (unless (eq major-mode 'org-mode)
              (saps:font-lock-add-hack-keywords))
            (auto-fill-mode 1)
            (use-hard-newlines 1 'never)))
