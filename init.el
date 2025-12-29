;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Set working directory to org-mode files location
(cd "/Users/jungchan/Library/Mobile Documents/iCloud~md~obsidian/Documents/org-mode/org/")

;; Setup use-package just in case everything isn't already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
(use-package org
  :pin gnu)

;; Setup org-superstar
(unless (package-installed-p 'org-superstar)
  (package-refresh-contents)
  (package-install 'org-superstar))

(eval-when-compile
  (require 'org-superstar))

(unless (package-installed-p 'jinx)
  (package-refresh-contents)
  (package-install 'jinx))

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$"   . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (setq jinx-languages "en_US"))

(unless (package-installed-p 'olivetti)
  (package-refresh-contents)
  (package-install 'olivetti))

(use-package olivetti
  :bind ("C-c o" . olivetti-mode))

(add-hook 'olivetti-mode-on-hook (lambda () (olivetti-set-width 100)))

(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;; Set default font
(set-face-attribute 'default nil :font "Menlo-14")

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

;; open emacs in full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Must do this so the agenda knows where to look for my files
(setq org-agenda-files '("."
			 "projects"))

;; Agenda always takes over current window
(setq org-agenda-window-setup 'only-window)

;; Configure refile targets
(setq org-refile-targets '(
                           ("todo.org" :maxlevel . 3)
                           ("thoughts.org" :maxlevel . 3)
                           ("questions.org" :maxlevel . 3)
                           ("trash.org" :maxlevel . 3)
                           (org-agenda-files :maxlevel . 3)))

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; When a TODO is set to a done state, record a timestamp
(setq org-log-done 'time)

;; Follow the links
(setq org-return-follows-link  t)

;; Associate all org files with org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Make the indentation look nicer
(add-hook 'org-mode-hook 'org-indent-mode)

;; Remap the change priority keys to use the UP or DOWN key
(define-key org-mode-map (kbd "C-c <up>") 'org-priority-up)
(define-key org-mode-map (kbd "C-c <down>") 'org-priority-down)

;; Shortcuts for viewing the agenda
(define-key global-map "\C-ca" 'org-agenda)

;; When you want to change the level of an org item, use SMR
(define-key org-mode-map (kbd "C-c C-g C-r") 'org-shiftmetaright)

;; Hide the markers so you just see bold text as BOLD-TEXT and not *BOLD-TEXT*
(setq org-hide-emphasis-markers t)

;; Wrap the lines in org mode so that things are easier to read
(add-hook 'org-mode-hook 'visual-line-mode)

;; Tags
(setq org-tag-alist '(
                      ;; Contexts
                      (:startgroup . nil)
                      ("@work" . ?w)
                      ("@personal" . ?p)                    
                      (:endgroup . nil)
		      ))

;; Tag colors
(setq org-tag-faces
      '(
        ("work"  . (:foreground "royalblue1" :weight bold))
        ("personal"   . (:foreground "sienna"    :weight bold))
        ))

(find-file ".")

;; Set a specific location for archived tasks
(setq org-archive-location "archive/archive.org::* Archived Tasks")

;; Todo States - "@" means prompt for a note upon entering the state, "!" means make a timestamp when you enter the state, "/!" means make a timestamp when leaving the state
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@!)" )
        ))

;; TODO colors
(setq org-todo-keyword-faces
      '(
        ("TODO" . (:foreground "firebrick3" :weight bold))
        ("WAITING" . (:foreground "DarkOrange2" :weight bold))
        ("DONE" . (:foreground "forest green" :weight bold))
        ("CANCELLED" . (:foreground "slate gray" :weight bold))
        ))


;; === AUTO-SAVE: Constantly save changes to disk ===
;; Auto-save visited files every 10 seconds of idle time
(auto-save-visited-mode 10)
(setq auto-save-visited-interval 10)  ; Very frequent saves for iCloud sync

;; Also save all org buffers after agenda operations
(defun my/save-all-org-buffers (&rest _)
  "Save all org-mode buffers without prompting."
  (save-some-buffers t (lambda () (derived-mode-p 'org-mode))))

(add-hook 'org-agenda-after-show-hook #'my/save-all-org-buffers)
(advice-add 'org-agenda-todo :after #'my/save-all-org-buffers)
(advice-add 'org-agenda-priority :after #'my/save-all-org-buffers)
(advice-add 'org-agenda-set-tags :after #'my/save-all-org-buffers)
(advice-add 'org-agenda-schedule :after #'my/save-all-org-buffers)
(advice-add 'org-agenda-deadline :after #'my/save-all-org-buffers)
(advice-add 'org-agenda-refile :after #'my/save-all-org-buffers)

;; === AUTO-REVERT: Reload files when they change on disk ===
;; Enable global auto-revert for all buffers
(global-auto-revert-mode 5)

;; Make auto-revert very responsive for org files
(setq auto-revert-interval 5)  ; Check every 5 seconds
(setq auto-revert-check-vc-info nil)  ; Skip VC checks (i.e. git stuff) for performance

;; Also revert non-file buffers like dired
(setq global-auto-revert-non-file-buffers t)

;; custom skip function that checks inherited tags
(defun my/skip-if-has-tag (tag)
  "Skip entries that have TAG (including inherited tags)."
  (let ((tags (org-get-tags)))
    (when (member tag tags)
      (or (outline-next-heading)
          (point-max)))))

(defun my/skip-if-has-work-tag ()
  "Skip entries tagged with @work (including inherited)."
  (my/skip-if-has-tag "@work"))

(defun my/skip-if-has-personal-tag ()
  "Skip entries tagged with @personal (including inherited)."
  (my/skip-if-has-tag "@personal"))

;; custom agendas
(setq org-agenda-custom-commands
      '(("w" "Work Tasks"
         ((agenda ""
                  ((org-agenda-skip-function 'my/skip-if-has-personal-tag)))
          (tags-todo "-@personal"
                     ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))))
        ("p" "Personal Tasks"
         ((agenda ""
                  ((org-agenda-skip-function 'my/skip-if-has-work-tag)))
          (tags-todo "-@work"
                     ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))))))

;; turning on line highlighting, and defining custom color
(global-hl-line-mode 1)
(set-face-background 'hl-line "azure1")

;; custom refile function to move todos to todo.org
(defun refile-todos-to-todo-file ()
  "Refile all TODO items from current buffer to todo.org under 'todos' heading."
  (interactive)
  (unless (file-exists-p "todo.org")
    (error "Target file todo.org not found! Update the path in this function"))
  (let ((org-refile-targets '(("todo.org" :regexp . "^\\*+ Todos\\>")))
        (count 0))
    (save-excursion
      (while (progn
               (goto-char (point-min))
               (re-search-forward "^\\*+ TODO " nil t))
        (forward-line 0)  ; Ensure we're at headline start
        (org-refile nil nil (org-refile-get-location))
        (cl-incf count)))
    (message "Successfully refiled %d TODO items!" count)))

(global-set-key (kbd "C-c r") 'refile-todos-to-todo-file)

;; custom function to quickly add notes
(defun quick-note ()
  "Open notes.org, create a newline, and insert **."
  (interactive)
  (find-file "./notes.org")
  (goto-char (point-max))
  (insert "\n** "))

(global-set-key (kbd "s-SPC") 'quick-note)

;; show indenting in agenda view
(setq org-tags-match-list-sublevels 'indented)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
