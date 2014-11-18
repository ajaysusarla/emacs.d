(require 'saps-email)

;; Don't include me in To/Cc when replying
(setq message-dont-reply-to-names (concat "\\("
                                          saps:email-addresses-regexp
                                          "\\)"))

(setq message-confirm-send t) ;; Confirm before sending message
(setq message-kill-buffer-on-exit t) ;; kill buffer when message is sent
(setq message-elide-ellipsis "\n[…]\n\n")
(setq message-citation-line-function 'message-insert-formatted-citation-line)
(add-hook 'message-mode-hook 'footnote-mode)
(add-hook 'message-mode-hook 'turn-on-flyspell)
(setq message-subscribed-address-functions '(gnus-find-subscribed-addresses))


(defvar saps:message-signatures
  '("EnTRopY always gets you in the end."
    "May the source be with you.")
  "Signatures for messages.")


(setq message-signature '
      (concat "Parthasarathi Susarla\n"
              (nth (random (length saps:message-signatures))
                   saps:message-signatures)))

(setq message-send-mail-function 'message-smtpmail-send-it)

