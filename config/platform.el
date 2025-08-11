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
  ;; Modifier keys
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        ns-function-modifier 'hyper)
  
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
