;;; lang-c.el --- C/C++ language configuration
;;; Commentary:
;;; Configuration for C and C++ development with LSP

;;; Code:

;; C/C++ mode configuration
(use-package cc-mode
  :ensure nil
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.cxx\\'" . c++-mode)
         ("\\.cc\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.hxx\\'" . c++-mode))
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (c-mode-common . (lambda ()
                            ;; Enable font-lock mode for syntax highlighting
                            (font-lock-mode 1)
                            ;; Your custom styles from saps-coding.el
                            (c-set-style "linux")
                            (setq c-basic-offset 4)
                            (setq indent-tabs-mode nil))))
  :config
  ;; Custom C styles (adapted from your saps-coding.el)
  (c-add-style "saps"
               '("gnu"
                 (c-offsets-alist
                  (block-open . 0)
                  (block-close . 0)
                  (substatement-open . 0)
                  (case-label . +)
                  (func-decl-cont . 0)
                  (inline-open . 0))
                 (c-hanging-braces-alist
                  (brace-list-close nil)
                  (defun-open before after)
                  (defun-close after)
                  (class-open before after)
                  (class-close before)
                  (substatement-open after before)
                  (substatement-close after))
                 (c-block-comment-prefix . "* ")
                 (c-echo-syntactic-information-p . t)
                 (c-basic-offset . 8)))
  
  (c-add-style "saps-gstreamer"
               '("k&r"
                 (c-offsets-alist
                  (block-open . 0)
                  (block-close . 0)
                  (substatement-open . 0)
                  (case-label . +)
                  (func-decl-cont . 0)
                  (inline-open . 0))
                 (c-hanging-braces-alist
                  (brace-list-close nil)
                  (defun-open before after)
                  (defun-close after)
                  (class-open before after)
                  (class-close before)
                  (substatement-open after before)
                  (substatement-close after))
                 (c-block-comment-prefix . "* ")
                 (c-echo-syntactic-information-p . t)
                 (c-basic-offset . 2)))
  
  ;; Enhanced font-lock for C/C++
  (font-lock-add-keywords 'c-mode
                          '(("\\<\\(auto\\|register\\|static\\|extern\\|typedef\\)\\>" . font-lock-keyword-face)
                            ("\\<\\(const\\|volatile\\|restrict\\)\\>" . font-lock-type-face)
                            ("\\<\\(void\\|char\\|short\\|int\\|long\\|float\\|double\\|signed\\|unsigned\\)\\>" . font-lock-type-face)
                            ("\\<\\(struct\\|union\\|enum\\)\\>" . font-lock-keyword-face)
                            ("\\<.+_t\\>" . font-lock-type-face)
                            ("\\<\\(true\\|false\\|NULL\\)\\>" . font-lock-constant-face)))
  
  (font-lock-add-keywords 'c++-mode
                          '(("\\<\\(auto\\|register\\|static\\|extern\\|typedef\\|mutable\\)\\>" . font-lock-keyword-face)
                            ("\\<\\(const\\|volatile\\|restrict\\|constexpr\\|noexcept\\)\\>" . font-lock-type-face)
                            ("\\<\\(void\\|char\\|short\\|int\\|long\\|float\\|double\\|signed\\|unsigned\\|bool\\)\\>" . font-lock-type-face)
                            ("\\<\\(class\\|struct\\|union\\|enum\\|namespace\\|template\\|typename\\)\\>" . font-lock-keyword-face)
                            ("\\<\\(public\\|private\\|protected\\|virtual\\|override\\|final\\)\\>" . font-lock-keyword-face)
                            ("\\<\\(nullptr\\|true\\|false\\|NULL\\)\\>" . font-lock-constant-face)
                            ("\\<.+_t\\>" . font-lock-type-face)))
  
  ;; Keybindings
  :bind (:map c-mode-base-map
              ("RET" . newline-and-indent)
              ("C-c c" . compile)
              ("C-c C-c" . recompile)))

;; Modern C++ font lock
(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

;; CMake support
(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;; CMake IDE integration
(use-package cmake-ide
  :after projectile
  :hook (c++-mode . my/cmake-ide-find-project)
  :preface
  (defun my/cmake-ide-find-project ()
    "Find and set up cmake-ide for the current project."
    (with-eval-after-load 'projectile
      (setq cmake-ide-project-dir (projectile-project-root))
      (setq cmake-ide-build-dir (concat cmake-ide-project-dir "build"))
      (cmake-ide-load-db)))
  :config
  (cmake-ide-setup))

;; Clang-format integration
(use-package clang-format
  :bind (:map c-mode-base-map
              ("C-c i" . clang-format-region)
              ("C-c u" . clang-format-buffer)))

;; GDB integration
(use-package gdb-mi
  :ensure nil
  :config
  (setq gdb-many-windows t
        gdb-show-main t))

;; Debugging with dap-mode
(use-package dap-mode
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)
  
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  
  ;; C/C++ debug templates
  (dap-register-debug-template "LLDB::Run"
                               (list :type "lldb"
                                     :request "launch"
                                     :name "LLDB::Run"
                                     :gdbpath "lldb"
                                     :target nil
                                     :cwd nil))
  
  :bind (:map dap-mode-map
              ("<f12>" . dap-debug)
              ("<f8>" . dap-continue)
              ("<f9>" . dap-next)
              ("<f11>" . dap-step-in)
              ("<f23>" . dap-step-out) ; Shift+F11
              ("C-<f11>" . dap-disconnect)
              ("C-c C-d" . dap-hydra)))

;; Company backends for C/C++
(use-package company-c-headers
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers))

;; Flycheck C/C++ configuration
(use-package flycheck
  :config
  (setq flycheck-clang-standard-library "libc++"
        flycheck-clang-language-standard "c++17"))

;; LSP-specific configuration for C/C++
(with-eval-after-load 'lsp-mode
  ;; clangd configuration
  (setq lsp-clients-clangd-args '("-j=4"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--header-insertion=never"
                                  "--header-insertion-decorators=0"))
  
  ;; Enable clangd
  (add-to-list 'lsp-language-id-configuration '(c-mode . "c"))
  (add-to-list 'lsp-language-id-configuration '(c++-mode . "cpp")))

;; Project-specific compilation commands
(defun my/c-mode-compile-project ()
  "Compile the current C/C++ project."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (build-dir (concat project-root "build/"))
         (compile-command
          (cond
           ((file-exists-p (concat project-root "CMakeLists.txt"))
            (format "cd %s && cmake -B build -S . && cmake --build build" project-root))
           ((file-exists-p (concat project-root "Makefile"))
            (format "cd %s && make" project-root))
           (t
            (format "cd %s && gcc -o output *.c" project-root)))))
    (compile compile-command)))

;; Keybinding for project compilation
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c C-p") 'my/c-mode-compile-project)))

(provide 'lang-c)
;;; lang-c.el ends here
