;;; init-dune.el --- custom DUNE utility functions
;;; Commentary:
;;; Code:

(defcustom dune-directory "~/dune/" "Path where the DUNE repositories are located")
(defvar defaultOptsFile "default.opts")
(defvar lastCompiledModule "")

;;; run configure via dunecontrol
(defun dune-configure ()
  (interactive)
  (let ((default-directory dune-directory)
        (optsFile
         (read-string
          "opts-file to use: " defaultOptsFile))
        (moduleName
         (read-string
          "DUNE-module to configure: " lastCompiledModule))
        )
    )
  (setq lastCompiledModule moduleName)
  (async-shell-command
   (format "dunecontrol --opts=%s --module=%s configure"
           defaultOptsFile moduleName
           )
   )
  )

;;; determine build directory from opts file
(defun dune-find-builddir (optsFile)
  (let ((default-directory dune-directory)))
  (setq builddir
        (substring
         (shell-command-to-string
          (concat "grep -i BUILDDIR " optsFile)) 0 -1)
        )
  (if (string-match "BUILDDIR *= *" builddir)
      (setq builddir (replace-match "" nil nil builddir))
    (message
     (concat "Could not determine build directory from opts-file " optsFile) ". Please specify it via BUILDDIR=..."))
  (file-name-as-directory builddir)
  )

;;; delete build directory and fully compile PDELab
(defun dune-recompile-pdelab ()
  (interactive)
  (let ((default-directory dune-directory)))
  (setq
   optsFile
   (read-string
    "opts-file to use: " defaultOptsFile))
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
  (let (
        (default-directory dune-directory)
        (targetName
         (read-string
          "Compilation target: " lastTarget))
        (optsFile
         (read-string
          "opts-file to use: " defaultOptsFile))
        (moduleName
         (read-string
          "Compile in the following module? " lastCompiledModule))
        (doExecute
         (yes-or-no-p "Also run compiled executable?"))
        )
    )
  (setq lastTarget targetName)
  (setq lastCompiledModule moduleName)
  (shell-command
   (format "dunecontrol --opts=%s --only=%s configure"
           optsFile moduleName))
  (shell-command
   (format "dunecontrol --opts=%s --only=%s make %s"
           optsFile moduleName targetName))
  (when doExecute
    (dune-execute optsFile targetName))
  )

;;; execute target in the build directory
(defun dune-execute (optsFile targetName)
  (let (
        (default-directory dune-directory)
        (builddir (dune-find-builddir optsFile))
        )
    )
  (setq targetPath
        (substring
         (shell-command-to-string
          (format "find %s -name %s" builddir targetName)) 0 (- -1 (length targetName))))
  (async-shell-command
   (format "cd %s && ./%s" targetPath targetName))
  )

(provide 'init-dune)
