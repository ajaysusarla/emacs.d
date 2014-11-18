;; saps-coding.el
(defcustom saps:programming-language-major-modes
  '(prog-mode
    lua-mode
    cmake-mode
    tex-mode
    css-mode
    nxml-mode
    diff-mode
    haskell-mode
    sql-mode
    rst-mode)
  "What I consider programming languages.")

(defun saps:customize-prog-mode-common ()
  (saps:font-lock-add-hack-keywords)
  (rainbow-mode 1)
  (setq show-trailing-whitespace t)
  (flyspell-prog-mode))

(defun saps:customize-programming-language-mode ()
  (saps:customize-prog-mode-common)
  (flymake-mode 1))

(dolist (mode saps:programming-language-major-modes)
  (add-hook
   (intern (concat (symbol-name mode) "-hook"))
   'saps:customize-programming-language-mode))

(add-hook 'sgml-mode-hook 'saps:customize-prog-mode-common)

(semantic-mode 1) ;;

;; CC mode
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
                (defun-close after)
                (class-open before after)
                (class-close before)
                (substatement-open after before)
                (substatement-close after))
               (c-block-comment-prefx . "* ")
               (c-echo-syntactic-information-p . t)
               (c-basic-offset . 8)))
(setq-default c-hanging-semi&comma-criteria nil)

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
                (defun-close after)
                (class-open before after)
                (class-close before)
                (substatement-open after before)
                (substatement-close after))
               (c-block-comment-prefx . "* ")
               (c-echo-syntactic-information-p . t)
               (c-basic-offset . 2)))

(font-lock-add-keywords 'c-mode
                        '(("\\<.+_t\\>" . font-lock-type-face)
                          ("\\<bool\\>" . font-lock-type-face)
                          ("\\<foreach\\>" . font-lock-keyword-face)))

;; Use Linux kernel coding style
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (fboundp 'doxymacs-mode)
              (doxymacs-mode 1))
            (c-set-style "linux")))

(add-hook 'c-initialization-hook
          (lambda ()
            (define-key c-mode-base-map (kbd "RET")
              'newline-and-indent)))


(provide 'saps-coding)
