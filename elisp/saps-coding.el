;; saps-coding.el
(defcustom saps:programming-language-major-modes
  '(prog-mode
    lua-mode
    cmake-mode
    cperl-mode
    tex-mode
    css-mode
    nxml-mode
    diff-mode
    haskell-mode
    rust-mode
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
  (global-flycheck-mode))

(dolist (mode saps:programming-language-major-modes)
  (add-hook
   (intern (concat (symbol-name mode) "-hook"))
   'saps:customize-programming-language-mode))

(add-hook 'sgml-mode-hook 'saps:customize-prog-mode-common)
;; (add-hook 'haskell-mode-hook #'flycheck-haskell-setup)
;; (setq-default flycheck-disabled-checkers '(haskell-stack-ghc))

(semantic-mode 1) ;;

;; Perl mode
(defalias 'perl-mode 'cperl-mode)
;;(setq cperl-indent-level  2)
;;(cperl-set-style "GNU")

(defun runperltidy ()
  "Run perltidy on the current buffer or selected region."
  (interactive)
  (save-excursion
    (unless mark-active (mark-defun))
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)))

(global-set-key "\C-ct" 'runperltidy)

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

(c-add-style "nicta"
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
               (c-basic-offset . 4)))

(c-add-style "fm"
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
               (c-basic-offset . 4)))


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


;; To change c style use C-c .
(provide 'saps-coding)
