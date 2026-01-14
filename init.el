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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.15))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.05))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
