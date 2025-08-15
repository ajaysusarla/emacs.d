;;; offline-mode.el --- Configure Emacs for offline package management
;;; Commentary:
;;; Completely disable network package lookups for fast startup

;;; Code:

;; EMERGENCY OFFLINE MODE
;; Use this if you want to completely disable all network package activity

(defun my/enable-offline-mode ()
  "Enable complete offline mode - no network package lookups."
  (interactive)
  
  ;; Disable package archives
  (setq package-archives nil)
  
  ;; Disable package signature checking
  (setq package-check-signature nil)
  
  ;; Disable automatic package loading
  (setq package-enable-at-startup nil)
  
  ;; Override package-refresh-contents to do nothing
  (advice-add 'package-refresh-contents :override (lambda (&rest args) 
                                                    (message "Package refresh disabled in offline mode")))
  
  ;; Override package-install to warn instead of trying to install
  (advice-add 'package-install :around 
              (lambda (orig-fun package &rest args)
                (if (package-installed-p package)
                    (apply orig-fun package args)
                  (message "Warning: Package %s not available in offline mode" package))))
  
  (message "Offline mode enabled - no network package lookups"))

(defun my/disable-offline-mode ()
  "Disable offline mode and restore normal package behavior."
  (interactive)
  
  ;; Restore package archives
  (setq package-archives
        '(("melpa" . "https://melpa.org/packages/")
          ("gnu" . "https://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/packages/")))
  
  ;; Remove advice
  (advice-remove 'package-refresh-contents (lambda (&rest args) 
                                             (message "Package refresh disabled in offline mode")))
  (advice-remove 'package-install (lambda (orig-fun package &rest args)
                                    (if (package-installed-p package)
                                        (apply orig-fun package args)
                                      (message "Warning: Package %s not available in offline mode" package))))
  
  (message "Offline mode disabled - network package access restored"))

;; Quick network test
(defun my/test-package-network ()
  "Test if package repositories are reachable."
  (interactive)
  (let ((start-time (current-time)))
    (message "Testing network connectivity to package repositories...")
    
    (condition-case err
        (progn
          (url-retrieve-synchronously "https://melpa.org/packages/archive-contents" nil nil 5)
          (message "✅ MELPA reachable in %.2f seconds" 
                   (float-time (time-subtract (current-time) start-time))))
      (error 
       (message "❌ MELPA unreachable: %s" err)))
    
    (setq start-time (current-time))
    (condition-case err
        (progn
          (url-retrieve-synchronously "https://elpa.gnu.org/packages/archive-contents" nil nil 5)
          (message "✅ GNU ELPA reachable in %.2f seconds" 
                   (float-time (time-subtract (current-time) start-time))))
      (error 
       (message "❌ GNU ELPA unreachable: %s" err)))))

;; One-time package setup function
(defun my/setup-packages-once ()
  "One-time package installation setup."
  (interactive)
  (message "Setting up packages for the first time...")
  (let ((start-time (current-time)))
    
    ;; Test connectivity first
    (my/test-package-network)
    
    ;; Refresh and install
    (package-refresh-contents)
    (my/install-missing-packages)
    
    ;; Switch to offline mode
    (my/enable-offline-mode)
    
    (message "Package setup completed in %.2f seconds. Offline mode enabled for future startups." 
             (float-time (time-subtract (current-time) start-time)))))

;; Keybindings
(global-set-key (kbd "C-c P o") 'my/enable-offline-mode)
(global-set-key (kbd "C-c P n") 'my/disable-offline-mode)
(global-set-key (kbd "C-c P t") 'my/test-package-network)
(global-set-key (kbd "C-c P s") 'my/setup-packages-once)
(global-set-key (kbd "C-c P i") 'my/install-missing-packages)

(provide 'offline-mode)
;;; offline-mode.el ends here
