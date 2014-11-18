(when (string-equal system-type "darwin")
  (defun saps:xml-unescape-string (string)
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
                     "-message" (saps:xml-unescape-string body)
                     "-title" (saps:xml-unescape-string title)
                     "-activate" "org.gnu.Emacs"
                     "-sender" "org.gnu.Emacs"))))

