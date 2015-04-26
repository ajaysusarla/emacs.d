(add-to-list 'completion-styles 'substring t)

(setq savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring)) ;; Save kill ring as well
(setq savehist-file "~/.emacs.d/history/savehist") ;; History will be saved here
(savehist-mode t) ;; Enable history

