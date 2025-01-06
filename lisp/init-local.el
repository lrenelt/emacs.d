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

;; use cleveref in LaTeX
(add-hook 'LaTeX-mode-hook
          (lambda nil
            (setq reftex-ref-style-default-list '("Default" "Cleveref"))
            )
          )


;;; include DUNE macros
(require 'init-dune nil t)

;;; load reftex in latex mode
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-shell-escape nil)
(eval-after-load "tex"
  '(add-to-list 'TeX-expand-list
                '("%(shellesc)"
                  (lambda nil
                    (if
                        (eq TeX-shell-escape t)
                        "-shell-escape" ""))
                  )))

(provide 'init-local)
