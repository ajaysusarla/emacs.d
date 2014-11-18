;; saps-desktop-session.el

;;;http://www.emacswiki.org/emacs/DeskTop#toc5
;;; Save sessions
;; use only one desktop
(setq desktop-dirname "~/.emacs.d/emacs_desktop")
(setq desktop-path (list desktop-dirname))
(setq desktop-base-file-name "emacs.desktop")

;; remove desktop after it's been read
(add-hook 'desktop-after-read-hook
	  '(lambda ()
	     ;; desktop-remove clears desktop-dirname
	     (setq desktop-dirname-tmp desktop-dirname)
	     (desktop-remove)
	     (setq desktop-dirname desktop-dirname-tmp)))

(defun saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; use session-restore to restore the desktop manually
(defun saps:session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read)
    (message "No desktop found.")))

;; use session-save to save the desktop manually
(defun saps:session-save ()
  "Save an emacs session."
  (interactive)
  (if (saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
	  (desktop-save-in-desktop-dir)
	(message "Session not saved."))
  (desktop-save-in-desktop-dir)))

;; ask user whether to restore desktop at start-up
(add-hook 'after-init-hook
	  '(lambda ()
	     (if (saved-session)
		 (if (y-or-n-p "Restore desktop? ")
		     (saps:session-restore)))))


(global-set-key "\C-cs" 'saps:session-save)
(global-set-key "\C-cr" 'saps:session-restore)

(provide 'saps-desktop-session)
