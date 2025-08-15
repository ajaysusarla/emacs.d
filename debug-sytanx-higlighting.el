;;; Debug syntax highlighting
;;; Add this to your config or run directly

(defun my/debug-syntax-highlighting ()
  "Debug syntax highlighting and font-lock issues."
  (interactive)
  (with-current-buffer (get-buffer-create "*Syntax Debug*")
    (erase-buffer)
    (insert "=== Syntax Highlighting Debug ===\n\n")
    
    ;; Current buffer info
    (insert (format "Current buffer: %s\n" (buffer-name (other-buffer))))
    (insert (format "Major mode: %s\n" (with-current-buffer (other-buffer) major-mode)))
    (insert (format "Font-lock mode: %s\n" (with-current-buffer (other-buffer) font-lock-mode)))
    (insert (format "Global font-lock: %s\n" global-font-lock-mode))
    
    ;; Theme and face info
    (insert (format "\nCurrent theme: %s\n" (car custom-enabled-themes)))
    (insert (format "Font-lock faces available: %s\n" 
                    (mapcar 'symbol-name 
                            '(font-lock-keyword-face 
                              font-lock-type-face 
                              font-lock-function-name-face
                              font-lock-variable-name-face
                              font-lock-constant-face
                              font-lock-comment-face
                              font-lock-string-face))))
    
    ;; Test faces
    (insert "\nFace test:\n")
    (insert (propertize "keyword" 'face 'font-lock-keyword-face) " ")
    (insert (propertize "type" 'face 'font-lock-type-face) " ")
    (insert (propertize "function" 'face 'font-lock-function-name-face) " ")
    (insert (propertize "variable" 'face 'font-lock-variable-name-face) " ")
    (insert (propertize "constant" 'face 'font-lock-constant-face) " ")
    (insert (propertize "comment" 'face 'font-lock-comment-face) " ")
    (insert (propertize "string" 'face 'font-lock-string-face) "\n")
    
    ;; Font-lock keywords for current mode
    (let ((mode (with-current-buffer (other-buffer) major-mode)))
      (when (memq mode '(c-mode c++-mode))
        (insert (format "\nFont-lock keywords for %s:\n" mode))
        (insert (format "Keywords defined: %s\n" 
                        (> (length (symbol-value 
                                    (intern (concat (symbol-name mode) "-font-lock-keywords")))) 0)))))
    
    ;; Quick fix suggestions
    (insert "\nQuick fixes to try:\n")
    (insert "1. M-x font-lock-mode (toggle font-lock)\n")
    (insert "2. M-x global-font-lock-mode (toggle globally)\n") 
    (insert "3. M-x font-lock-fontify-buffer (re-fontify)\n")
    (insert "4. Check if doom-themes is loaded properly\n")
    (insert "5. Try: (font-lock-add-keywords nil nil 'set)\n")
    
    (display-buffer (current-buffer))))

;; Quick test function for C/C++ 
(defun my/test-c-syntax ()
  "Test C/C++ syntax highlighting with sample code."
  (interactive)
  (with-current-buffer (get-buffer-create "*C Syntax Test*")
    (erase-buffer)
    (c++-mode)
    (insert "#include <iostream>\n")
    (insert "#include <vector>\n")
    (insert "\n")
    (insert "// This is a comment\n")
    (insert "/* Multi-line comment */\n")
    (insert "\n")
    (insert "class TestClass {\n")
    (insert "public:\n")
    (insert "    int member_variable;\n")
    (insert "    \n")
    (insert "    TestClass() : member_variable(42) {}\n")
    (insert "    \n")
    (insert "    virtual void test_function() {\n")
    (insert "        const std::string message = \"Hello, World!\";\n")
    (insert "        std::cout << message << std::endl;\n")
    (insert "        \n")
    (insert "        for (auto i = 0; i < 10; ++i) {\n")
    (insert "            if (i % 2 == 0) {\n")
    (insert "                continue;\n")
    (insert "            }\n")
    (insert "        }\n")
    (insert "    }\n")
    (insert "};\n")
    (insert "\n")
    (insert "int main() {\n")
    (insert "    TestClass* obj = new TestClass();\n")
    (insert "    obj->test_function();\n")
    (insert "    delete obj;\n")
    (insert "    return 0;\n")
    (insert "}\n")
    
    (font-lock-fontify-buffer)
    (goto-char (point-min))
    (display-buffer (current-buffer))
    (message "Check *C Syntax Test* buffer - do you see syntax highlighting?")))

;; Add to keybindings
(global-set-key (kbd "C-c d s") 'my/debug-syntax-highlighting)
(global-set-key (kbd "C-c d c") 'my/test-c-syntax)
