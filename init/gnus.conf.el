(require 'saps-email)

;; Spam needs to move to SPAM
(require 'spam)

;; Load libraries
(load-library "smtpmail")
(load-library "message")
(load-library "gnus-group")

(setq gnus-novice-user nil)

(setq gnus-spam-process-destinations
      '(("." "spam")))

(setq gnus-agents nil) ; disable agent


(setq gnus-ignored-newsgroups "")

;; This is to show all the [Gmail] system folders
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\(\\|$\\)\\|^[\"]\"[#'()]")


;;summary format
(setq gnus-summary-line-format (concat
                                "%*%5{%U%R%z%}"
                                "%4{|%}"
                                "%2{%-10&user-date;%}"
                                "%4{|%}"
                                "%2{ %}%(%-24,24n"
                                "%4{|%}"
                                "%2{%5i%}"
                                "%4{|%}"
                                "%2{%6k %}%)"
                                "%4{|%}"
                                "%2{ %}%3{%B%}%1{%s%}\n"))

;; Wrap at 80 cols.
(add-hook 'message-mode-hook
          '(lambda()
             (turn-on-auto-fill)
             (setq fill-column 80)))

;; gnus - no server
;; http://www.emacswiki.org/emacs/GnusNiftyTricks#toc5
;; use "3 g" to refresh in groups buffer
(setq gnus-read-active-file 'some)


;; My offlineimap and dovecot setup
(setq gnus-select-method '(nnimap "Mail" (nnimap-stream shell)))
(setq imap-shell-program "MAIL=maildir:$HOME/Maildir /usr/lib/dovecot/imap")

;; Display date in local timezone
(setq gnus-treat-date-local 'head)

;; don't bugger me with dribbles
(setq gnus-always-read-dribble-file t)




;; gnus-score
(setq gnus-home-score-file "~/.gnus.score")

;; use mini window
(setq gnus-use-full-window nil)

;; enable imap log
(setq imap-log nil)
