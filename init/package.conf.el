;;; package --- Packages to install
;;; Commentary:
;;; Code:

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(dolist (package '(oauth2
		   browse-kill-ring
		   clojure-mode
		   multi-term
		   auto-dim-other-buffers
		   htmlize
		   lua-mode
		   git-gutter
		   git-auto-commit-mode
		   tox
		   python
		   pylint
		   paredit
		   paredit-everywhere
		   git-commit
		   offlineimap
		   rust-mode
		   cmake-mode
		   sml-mode
                   flycheck
                   flycheck-haskell
		   rainbow-mode
                   hindent
                   magit
                   go-mode
                   lsp-mode
                   company
                   yaml-mode
                   indent-tools
                   lsp-ui
                   yasnippet
                   yasnippet-snippets
                   wanderlust
                   use-package
                   rustic
                   tide))
  (unless (package-installed-p package)
    (package-install package)))

(provide 'package.conf)
;;; package.conf.el ends here
