;;; early-init.el --- Early initialization for Emacs 27+
;;; Commentary:
;;; This file is loaded before init.el and package.el

;;; Code:

;; Performance optimizations during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; CRITICAL: Disable network package operations for fast startup
(setq package-enable-at-startup nil)
(setq package-check-signature nil)

;; Disable GUI elements early to prevent flashing
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Prevent frame resizing when font changes
(setq frame-inhibit-implied-resize t)

;; Reduce startup file handler overhead
(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Disable some expensive features during startup
(setq site-run-file nil)

;; Initialize packages WITHOUT network refresh
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/packages/")))

;; Initialize packages but DON'T refresh from network
(package-initialize)

;; Native compilation settings (Emacs 28+)
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-deferred-compilation t))

;;; early-init.el ends here
