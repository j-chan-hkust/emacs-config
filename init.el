;;; init.el --- Bootstrap for literate configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file loads org-mode and tangles the literate configuration from init.org
;; The actual configuration lives in init.org and is tangled to config.el

;;; Code:

;; Load org-mode (required for tangling)
(require 'org)

;; Tangle and load the configuration
(let* ((init-dir user-emacs-directory)
       (init-org (expand-file-name "init.org" init-dir))
       (config-el (expand-file-name "config.el" init-dir))
       (local-config (expand-file-name "local-config.el" init-dir)))

  ;; Tangle the org file to generate config.el
  (when (file-exists-p init-org)
    (message "Tangling init.org...")
    (org-babel-tangle-file init-org config-el "emacs-lisp")

    ;; Load local configuration FIRST (sets working directory)
    (when (file-exists-p local-config)
      (message "Loading local configuration...")
      (load-file local-config))

    ;; Load tangled configuration
    (message "Loading tangled configuration...")
    (load-file config-el)

    ;; Byte-compile for better performance
    (message "Byte-compiling config.el...")
    (byte-compile-file config-el)))

;;; init.el ends here
