;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Semantic.html
(require 'semantic)
(global-semantic-stickyfunc-mode 1)
(global-semantic-idle-summary-mode 1)

(eval-after-load "semantic/db"
  '(add-to-list 'semanticdb-project-root-functions
		(defun saps:semanticdb-is-project-root (directory-name)
		  (save-match-data
		    (when (string-match
			   (concat "^\\(" (regexp-quote (expand-file-name saps:projects-directory)) "/[^/]+\\)")
			   directory-name)
		      (match-string 1 directory-name))))))

(setq semantic-idle-summary-function
      (defun saps:semantic-format-tag-concise-or-full (tag &optional parent color)
        "Return the full prototype if we have enough size in the echo area to print it.
Otherwise, return a concise tag."
        (let ((ret (semantic-format-tag-prototype-default tag parent color)))
          (if (> (length ret)
                 (frame-width))
              (semantic-format-tag-concise-prototype-default tag parent color)
            ret))))
