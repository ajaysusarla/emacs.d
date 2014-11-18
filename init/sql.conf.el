(add-hook 'sql-mode-hook
          (defun saps:sql-mode-set-sql-product ()
            (sql-set-product 'postgres)))
