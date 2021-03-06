(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

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
		   flymake-easy
                   flymake-python-pyflakes
                   flymake-perlcritic
		   git-commit
		   offlineimap
		   rust-mode
		   cmake-mode
		   sml-mode
		   rainbow-mode))
  (unless (package-installed-p package)
    (package-install package)))
