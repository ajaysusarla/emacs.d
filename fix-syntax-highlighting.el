;;; fix-syntax-highlighting.el --- Force enable syntax highlighting
;;; Commentary:
;;; Run this to diagnose and fix syntax highlighting issues

;;; Code:

;; Step 1: Check what's broken
(defun fix-syntax-step1-diagnose ()
  "Diagnose syntax highlighting issues."
  (interactive)
  (let ((results '()))
    ;; Check font-lock
    (push (format "global-font-lock-mode: %s" global-font-lock-mode) results)
    (push (format "font-lock-mode (current buffer): %s" font-lock-mode) results)
    (push (format "major-mode: %s" major-mode) results)
    
    ;; Check theme
    (push (format "enabled-themes: %s" custom-enabled-themes) results)
    
    ;; Check if faces are defined
    (push (format "font-lock-keyword-face defined: %s" 
                  (facep 'font-lock-keyword-face)) results)
    
    ;; Check if doom-themes loaded
    (push (format "doom-themes loaded: %s" (featurep 'doom-themes)) results)
    
    (message "Diagnosis: %s" (string-join results " | "))))

;; Step 2: Force enable everything
(defun fix-syntax-step2-force-enable ()
  "Force enable syntax highlighting."
  (interactive)
  ;; Enable font-lock globally
  (global-font-lock-mode 1)
  
  ;; Enable in current buffer
  (font-lock-mode 1)
  
  ;; Set maximum decoration
  (setq font-lock-maximum-decoration t)
  
  ;; Re-fontify current buffer
  (when (derived-mode-p 'prog-mode)
    (font-lock-fontify-buffer))
  
  (message "Forced font-lock enable - check if highlighting appears"))

;; Step 3: Load a simple theme if doom-themes isn't working
(defun fix-syntax-step3-simple-theme ()
  "Load a simple theme that definitely has syntax highlighting."
  (interactive)
  ;; Disable any current themes
  (mapc #'disable-theme custom-enabled-themes)
  
  ;; Load a built-in theme
  (load-theme 'tango-dark t)
  
  ;; Re-fontify
  (font-lock-fontify-buffer)
  
  (message "Loaded tango-dark theme - check for syntax highlighting"))

;; Step 4: Manually define C syntax highlighting
(defun fix-syntax-step4-manual-c-highlighting ()
  "Manually set up C/C++ syntax highlighting."
  (interactive)
  (when (derived-mode-p 'c-mode 'c++-mode)
    ;; Clear existing keywords
    (setq font-lock-keywords nil)
    
    ;; Add basic C/C++ keywords manually
    (font-lock-add-keywords
     nil
     '(;; Keywords
       ("\\<\\(auto\\|break\\|case\\|char\\|const\\|continue\\|default\\|do\\|double\\|else\\|enum\\|extern\\|float\\|for\\|goto\\|if\\|inline\\|int\\|long\\|register\\|restrict\\|return\\|short\\|signed\\|sizeof\\|static\\|struct\\|switch\\|typedef\\|union\\|unsigned\\|void\\|volatile\\|while\\)\\>" . font-lock-keyword-face)
       
       ;; C++ specific keywords
       ("\\<\\(class\\|namespace\\|template\\|typename\\|public\\|private\\|protected\\|virtual\\|override\\|final\\|constexpr\\|noexcept\\|nullptr\\|bool\\|true\\|false\\)\\>" . font-lock-keyword-face)
       
       ;; Types
       ("\\<\\([A-Z][a-zA-Z0-9_]*\\|[a-z_][a-zA-Z0-9_]*_t\\)\\>" . font-lock-type-face)
       
       ;; Function names
       ("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*(" 1 font-lock-function-name-face)
       
       ;; Constants
       ("\\<\\(NULL\\|[A-Z_][A-Z0-9_]*\\)\\>" . font-lock-constant-face)
       
       ;; Strings
       ("\".*?\"" . font-lock-string-face)
       
       ;; Comments
       ("//.*$" . font-lock-comment-face)
       ("/\\*.*?\\*/" . font-lock-comment-face)
       
       ;; Preprocessor
       ("^#.*$" . font-lock-preprocessor-face)))
    
    ;; Re-fontify
    (font-lock-mode -1)
    (font-lock-mode 1)
    (font-lock-fontify-buffer)
    
    (message "Manual C/C++ highlighting applied - check for colors")))

;; All-in-one fix function
(defun fix-syntax-highlighting-all ()
  "Run all syntax highlighting fixes in sequence."
  (interactive)
  (fix-syntax-step1-diagnose)
  (sit-for 1)
  (fix-syntax-step2-force-enable)
  (sit-for 1)
  (fix-syntax-step3-simple-theme)
  (sit-for 1)
  (fix-syntax-step4-manual-c-highlighting)
  (message "All fixes applied! Check for syntax highlighting now."))

;; Simple test without any packages
(defun test-basic-highlighting ()
  "Test basic Emacs highlighting without any packages."
  (interactive)
  (with-current-buffer (get-buffer-create "*Basic Highlighting Test*")
    (erase-buffer)
    (emacs-lisp-mode)  ; Use Emacs Lisp mode which should always work
    (insert ";; This is a comment\n")
    (insert "(defun test-function (arg)\n")
    (insert "  \"This is a docstring\"\n")
    (insert "  (let ((variable 'symbol))\n")
    (insert "    (message \"Hello %s\" arg)))\n")
    (insert "\n")
    (insert "(setq my-variable 42)\n")
    (font-lock-fontify-buffer)
    (display-buffer (current-buffer))
    (message "Check *Basic Highlighting Test* - do you see colors in Emacs Lisp?")))

;; Keybindings for easy access
(global-set-key (kbd "C-c f 1") 'fix-syntax-step1-diagnose)
(global-set-key (kbd "C-c f 2") 'fix-syntax-step2-force-enable)
(global-set-key (kbd "C-c f 3") 'fix-syntax-step3-simple-theme)
(global-set-key (kbd "C-c f 4") 'fix-syntax-step4-manual-c-highlighting)
(global-set-key (kbd "C-c f a") 'fix-syntax-highlighting-all)
(global-set-key (kbd "C-c f t") 'test-basic-highlighting)

(message "Syntax highlighting fix functions loaded. Try C-c f a to run all fixes.")
