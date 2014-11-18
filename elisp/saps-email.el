(defvar saps:email-addresses
  '("ajaysusarla@gmail\\.com")
  "Regexp of my email addresses.")

(defvar saps:email-addresses-regexp
  (concat "^\\("
          (mapconcat 'identity saps:email-addresses "\\]")
          "\\)$"))

(provide 'saps-email)
