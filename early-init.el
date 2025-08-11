;;; early-init.el --- Early initialization for Emacs 27+
;;; Commentary:
;;; This file is loaded before init.el and package.el

;;; Code:

;; Performance optimizations during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Prevent package.el loading packages before init.el
(setq package-enable-at-startup nil)

;; Disable GUI elements early to prevent flashing
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Prevent frame resizing when font changes
(setq frame-inhibit-implied-resize t)

;; Initialize packages early
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/packages/")))
(package-initialize)

;; Native compilation settings (Emacs 28+)
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-deferred-compilation t))

;;; early-init.el ends here
