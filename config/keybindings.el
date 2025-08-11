;;; config/keybindings.el --- Custom keybindings
;;; Commentary:
;;; Global keybindings and customizations

;;; Code:

;; Basic navigation and editing (from your saps-keybindings.el)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-r") 'replace-string)
(global-set-key (kbd "M-R") 'replace-regexp)
(global-set-key (kbd "M-p") 'compile)
(global-set-key (kbd "M-s") 'gdb)
(global-set-key (kbd "M-_") 'undo)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Buffer and window management
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-/") 'hippie-expand)

;; Custom functions
(global-set-key (kbd "C-c e")
                (defun my/edit-init-file ()
                  "Edit init.el"
                  (interactive)
                  (find-file (locate-user-emacs-file "init.el"))))

(global-set-key (kbd "C-c q")
                (defun my/quit-emacs ()
                  "Quit Emacs with confirmation"
                  (interactive)
                  (if (yes-or-no-p "Quit Emacs? ")
                      (save-buffers-kill-emacs)
                    (message "Cancelled"))))

;; Terminal
(global-set-key (kbd "C-c t") 'eshell)

;; Useful toggles
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)

(provide 'keybindings)
