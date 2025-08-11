;;; debug-loadpath.el --- Debug load path issues
;;; Commentary:
;;; Helper to debug why files can't be loaded

;;; Code:

(defun debug-load-path ()
  "Debug load path and file location issues."
  (interactive)
  (with-current-buffer (get-buffer-create "*Load Path Debug*")
    (erase-buffer)
    (insert "=== Load Path Debug Information ===\n\n")
    
    ;; Show current load path
    (insert "Current load-path:\n")
    (dolist (path load-path)
      (insert (format "  %s\n" path)))
    (insert "\n")
    
    ;; Check config directories
    (let ((config-dir (expand-file-name "config" user-emacs-directory))
          (languages-dir (expand-file-name "config/languages" user-emacs-directory)))
      
      (insert (format "user-emacs-directory: %s\n" user-emacs-directory))
      (insert (format "config directory: %s (exists: %s)\n" 
                      config-dir (if (file-directory-p config-dir) "YES" "NO")))
      (insert (format "languages directory: %s (exists: %s)\n" 
                      languages-dir (if (file-directory-p languages-dir) "YES" "NO")))
      (insert "\n")
      
      ;; Check specific files
      (let ((lang-files '("lang-c.el" "lang-go.el" "lang-rust.el" 
                          "lang-typescript.el" "lang-shell.el" "lang-org.el")))
        (insert "Language configuration files:\n")
        (dolist (file lang-files)
          (let ((full-path (expand-file-name file languages-dir)))
            (insert (format "  %s: %s (exists: %s)\n" 
                            file full-path (if (file-exists-p full-path) "YES" "NO")))))
        (insert "\n"))
      
      ;; Check if files can be located
      (insert "File location test:\n")
      (dolist (file '("lang-c" "core" "ui" "completion"))
        (let ((found (locate-library file)))
          (insert (format "  %s: %s\n" file (if found found "NOT FOUND")))))
      (insert "\n")
      
      ;; Check permissions
      (insert "Directory permissions:\n")
      (insert (format "  %s: %s\n" config-dir 
                      (if (file-readable-p config-dir) "readable" "NOT readable")))
      (insert (format "  %s: %s\n" languages-dir 
                      (if (file-readable-p languages-dir) "readable" "NOT readable"))))
    
    (display-buffer (current-buffer))))

;; Safer require function with debugging
(defun safe-require (feature &optional filename)
  "Safely require FEATURE with debugging information."
  (condition-case err
      (require feature filename)
    (error
     (message "Failed to load %s: %s" feature err)
     (debug-load-path)
     nil)))

;; Test function to check all language files
(defun test-language-files ()
  "Test loading all language configuration files."
  (interactive)
  (let ((lang-files '(lang-c lang-go lang-rust lang-typescript lang-shell lang-org))
        (results '()))
    
    (dolist (file lang-files)
      (condition-case err
          (progn
            (require file)
            (push (cons file "OK") results))
        (error
         (push (cons file (format "ERROR: %s" err)) results))))
    
    (with-current-buffer (get-buffer-create "*Language Files Test*")
      (erase-buffer)
      (insert "=== Language Files Loading Test ===\n\n")
      (dolist (result (reverse results))
        (insert (format "%s: %s\n" (car result) (cdr result))))
      (display-buffer (current-buffer)))))

(provide 'debug-loadpath)
;;; debug-loadpath.el ends here
