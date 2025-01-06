;; -*- lexical-binding: t; -*-
;;; init-dune.el --- custom DUNE utility functions
;;; Commentary:
;;; Code:

(defcustom dune-directory "~/dune/" "Path where the DUNE repositories are located")
(defvar lastUsedOptsFile nil)
(defvar defaultOptsFile "default.opts")
(defvar lastCompiledModule nil)

(defun isOptsFile (candidate)
  (or (string-match ".*.opts$" candidate)
      (directory-name-p candidate))
  )

;;; run configure via dunecontrol
(defun dune-configure ()
  (interactive)
  (let ((default-directory dune-directory)
        (optsFile
         (read-file-name
          "Choose the opts-file: " dune-directory defaultOptsFile nil lastUsedOptsFile 'isOptsFile))
        (moduleName
         (read-string
          "DUNE-module to configure: " lastCompiledModule))
        )
    )
  (setq lastUsedOptsFile optsFile)
  (setq lastCompiledModule moduleName)
  (async-shell-command
   (format "dunecontrol --opts=%s --module=%s configure"
           defaultOptsFile moduleName
           )
   )
  )

;;; determine build directory from opts file
(defun dune-find-builddir (optsFile)
  (let ((default-directory dune-directory))
    (setq builddir
          (substring
           (shell-command-to-string
            (concat "grep -i BUILDDIR " optsFile)) 0 -1))
    (setq builddir
          (if (string-match "BUILDDIR *= *" builddir)
              (replace-match "" nil nil builddir)
            (read-directory-name
             "Could not determine build directory from opts-file. Please specify manually: "
             dune-directory)
            )
          )
    )
  (file-name-as-directory builddir)
  )

;;; delete build directory and fully compile PDELab
(defun dune-recompile-pdelab ()
  (interactive)
  (let ((default-directory dune-directory)))
  (setq
   optsFile
   (read-file-name
    "Choose the opts-file: " dune-directory defaultOptsFile nil lastUsedOptsFile 'isOptsFile))
  (setq lastUsedOptsFile optsFile)
  (shell-command
   (format "rm -r %s" (dune-find-builddir optsFile))
   )
  (async-shell-command
   (format "dunecontrol --opts=%s --module=dune-pdelab all"
           optsFile)
   )
  )

(defvar lastTarget "")

;;; compile the specified target and potentially execute
(defun dune-compile ()
  (interactive)
  (defun runConfigure (optsFile moduleName)
    (let ((default-directory dune-directory))
      (display-buffer
       (get-buffer-create "*dunecontrol*")
       '((display-buffer-reuse-window display-buffer-pop-up-window)
         (reusable-frames . visible)))
      (with-current-buffer "*dunecontrol*"
        (eshell-mode)
        ;;;(eshell-show-maximum-output)
        )
      (start-process-shell-command
       "configure" "*dunecontrol*"
       (format "dunecontrol --opts=%s --only=%s configure"
               optsFile moduleName))))
  (defun runCompile (optsFile moduleName targetName)
    (let ((default-directory dune-directory))
      (display-buffer
       (get-buffer-create "*dunecontrol*")
       '((display-buffer-reuse-window display-buffer-pop-up-window)
         (reusable-frames . visible)))
      (with-current-buffer "*dunecontrol*"
        (eshell-mode)
        ;;;(eshell-show-maximum-output)
        )
      (start-process-shell-command
       "make" "*dunecontrol*"
       (format "dunecontrol --opts=%s --only=%s make %s"
               optsFile moduleName targetName))))
  (let ((default-directory dune-directory))
    (setq lastUsedOptsFile
          (file-name-nondirectory
           (read-file-name
            "Choose the opts-file: " dune-directory defaultOptsFile nil "" 'isOptsFile)))
    (setq lastTarget
          (read-string
           "Compilation target: " lastTarget))
    (setq lastCompiledModule
          (read-string
           "Compile in the following module? " lastCompiledModule))
    (set-process-sentinel
     (runConfigure lastUsedOptsFile lastCompiledModule)
     '(lambda (p e) (when (= 0 (process-exit-status p))
                      (set-process-sentinel
                       (runCompile lastUsedOptsFile lastCompiledModule lastTarget)
                       '(lambda (p e) (when (= 0 (process-exit-status p))
                                        (when (yes-or-no-p
                                               "Compilation finished. Also run compiled executable?")
                                          (dune-execute lastUsedOptsFile lastTarget))))))))

    ))

;;; execute target in the build directory
(defun dune-execute (optsFile targetName)
  (let ((default-directory dune-directory)
        (builddir (dune-find-builddir optsFile)))
    (setq targetPath
          (substring
           (shell-command-to-string
            (format "find %s -name %s" builddir targetName)) 0 (- -1 (length targetName))))
    (display-buffer
     (get-buffer-create "*dunecontrol*")
     '((display-buffer-reuse-window display-buffer-pop-up-window)
       (reusable-frames . visible)))
    (with-current-buffer "*dunecontrol*"
      (eshell-mode)
      ;;;(eshell-show-maximum-output)
      )                                   ;(
    (start-process-shell-command
     "run" "*dunecontrol*"
     (format "cd %s && ./%s" targetPath targetName))))

;;; interactive version
(defun dune-run-executable ()
  (interactive)
  (unless lastUsedOptsFile
    (setq lastUsedOptsFile
          (file-name-nondirectory
           (read-file-name
            "Choose the opts-file: " dune-directory defaultOptsFile nil "" 'isOptsFile))))
  (unless lastTarget
    (setq lastTarget
          (read-string
           "Target name: " lastTarget)))
  (dune-execute lastUsedOptsFile lastTarget)
  )

(provide 'init-dune)
