;;; config/platform.el --- Platform-specific configuration
;;; Commentary:
;;; macOS and Linux specific settings

;;; Code:

;; Server mode configuration (from your original saps-server.el)
(use-package server
  :ensure nil
  :config
  ;; Start server if not running as daemon and not already running
  (when (and (not (daemonp))
             (not (server-running-p)))
    (server-start))
  
  ;; In server mode, C-x C-c should delete frame, not kill Emacs
  (when (or (daemonp) (server-running-p))
    (global-set-key (kbd "C-x C-c") 'delete-frame)))

;; macOS specific settings
(when (eq system-type 'darwin)
  ;; Modifier keys - Use Alt (Option) as Meta instead of Command
  (setq mac-option-modifier 'meta        ; Alt/Option key becomes Meta
        mac-command-modifier 'super      ; Command key becomes Super (less used)
        ns-function-modifier 'hyper)     ; Function key becomes Hyper
  
  ;; Function to toggle modifier key setup
  (defun my/toggle-mac-modifiers ()
    "Toggle between Alt-as-Meta and Command-as-Meta setups."
    (interactive)
    (if (eq mac-option-modifier 'meta)
        (progn
          (setq mac-command-modifier 'meta
                mac-option-modifier 'super)
          (message "Switched to Command-as-Meta (default macOS Emacs style)"))
      (progn
        (setq mac-option-modifier 'meta
              mac-command-modifier 'super)
        (message "Switched to Alt-as-Meta (PC-style)"))))
  
  ;; Bind to a key for easy switching
  (global-set-key (kbd "C-c m") 'my/toggle-mac-modifiers)
  
  ;; Mouse wheel
  (setq mouse-wheel-progressive-speed nil
        mouse-wheel-scroll-amount '(2))
  
  ;; Exec path from shell
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize))
  
  ;; Notifications
  (defun my/xml-unescape-string (string)
    (with-temp-buffer
      (insert string)
      (dolist (substitution '(("&amp;" . "&")
                              ("&lt;" . "<")
                              ("&gt;". ">")
                              ("&apos;" . "'")
                              ("&quot;" . "\"")))
        (goto-char (point-min))
        (while (search-forward (car substitution) nil t)
          (replace-match (cdr substitution) t t nil)))
      (buffer-string)))

  (defun notifications-notify (&rest params)
    (let ((title (plist-get params :title))
          (body (plist-get params :body)))
      (start-process "terminal-notifier" nil
                     "terminal-notifier"
                     "-message" (my/xml-unescape-string body)
                     "-title" (my/xml-unescape-string title)
                     "-activate" "org.gnu.Emacs"
                     "-sender" "org.gnu.Emacs"))))

;; Linux specific settings
(when (eq system-type 'gnu/linux)
  ;; Use system notifications
  (use-package notifications
    :ensure nil))

;; Both platforms
(setq x-select-enable-primary t)

(provide 'platform)
