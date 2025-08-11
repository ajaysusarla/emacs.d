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
                  "Quit Emacs with confirmation, handling server mode appropriately"
                  (interactive)
                  (if (daemonp)
                      (if (yes-or-no-p "Kill Emacs daemon? ")
                          (kill-emacs)
                        (delete-frame))
                    (if (yes-or-no-p "Quit Emacs? ")
                        (save-buffers-kill-emacs)
                      (message "Cancelled")))))

;; Server mode utilities
(global-set-key (kbd "C-c s s") 
                (defun my/start-server ()
                  "Start Emacs server if not running"
                  (interactive)
                  (if (server-running-p)
                      (message "Server already running")
                    (server-start)
                    (message "Server started"))))

(global-set-key (kbd "C-c s k")
                (defun my/kill-server ()
                  "Kill Emacs server"
                  (interactive)
                  (if (server-running-p)
                      (if (yes-or-no-p "Kill Emacs server? ")
                          (kill-emacs)
                        (message "Cancelled"))
                    (message "Server not running"))))

(global-set-key (kbd "C-c s r")
                (defun my/restart-server ()
                  "Restart Emacs server"
                  (interactive)
                  (when (server-running-p)
                    (server-stop))
                  (server-start)
                  (message "Server restarted")))

;; Terminal
(global-set-key (kbd "C-c t") 'eshell)

;; Useful toggles
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)

(provide 'keybindings)
