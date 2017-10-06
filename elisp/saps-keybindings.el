;; saps-keybindings.el

(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-r" 'replace-string)
(global-set-key "\M-R" 'replace-regexp)
(global-set-key "\M-p" 'compile)
(global-set-key "\M-s" 'gdb)
(global-set-key "\M-_" 'undo)
(global-set-key "\C-ccr" 'revert-buffer)

(global-set-key "\C-cce"
		(defun saps:edit-dot-emacs ()
		  "Edit ~/.emacs.d/init.el"
		  (interactive)
		  (find-file (locate-user-emacs-file "init.el"))))

(global-set-key "\C-cq"
                (defun saps:quit-emacs ()
                  (interactive)
                  (if (yes-or-no-p "Quit emacs? ")
                      (save-buffers-kill-emacs)
                    (message "Ciao!"))))

(global-set-key "\C-xm" 'compose-mail-other-window)

(global-set-key "\C-x\C-b" 'ibuffer)

(global-set-key "\M-/" 'hippie-expand)

(global-set-key (kbd "<f3>") 'flymake-popup-current-error-menu)

(provide 'saps-keybindings)
