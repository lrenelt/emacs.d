;;; init-local.el --- custom settings
;;; Commentary:
;;; Code:

;;; required for org-mode?
(eval-after-load "org"
  '(require 'ox-gfm nil t))

;;; add YASnippet
(add-to-list 'load-path
             "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;;; enable irony-mode for c-files
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cbd-autosetup-compile-options)

;;; include DUNE macros
(require 'init-dune nil t)

(provide 'init-local)
