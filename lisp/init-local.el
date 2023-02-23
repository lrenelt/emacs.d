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


(setq dune-directory "~/dune/")

(defun dune-test ()
  (interactive)
  "Simple test function."
  (when (yes-or-no-p "Hello?")
    (message dune-directory)))


;;; dune commands
(defun dune-configure ()
  (interactive)
  (start-process
   "dune-configure"
   "*dune-configure*"
   "./dune-common/bin/dunecontrol"
   "--opts=ultraweak.opts --only=dune-ultraweak"))

(defun dune-compile ()
  (interactive)
  (setq target (read-string "Input target name: "))
  ())

(provide 'init-local)
