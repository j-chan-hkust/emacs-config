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

(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(load-theme 'modus-operandi :no-confirm)

(set-face-attribute 'default nil :height 130)

(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.15))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.05))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
  (set-face-attribute 'default nil :height 130)
)

;; open emacs in full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Must do this so the agenda knows where to look for my files
(setq org-agenda-files '("/Users/jungchan/Library/Mobile Documents/iCloud~md~obsidian/Documents/org-mode/org/"
			 "/Users/jungchan/Library/Mobile Documents/iCloud~md~obsidian/Documents/org-mode/org/projects"))

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

;; Shortcuts for storing links, viewing the agenda, and starting a capture
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

;; When you want to change the level of an org item, use SMR
(define-key org-mode-map (kbd "C-c C-g C-r") 'org-shiftmetaright)

;; Hide the markers so you just see bold text as BOLD-TEXT and not *BOLD-TEXT*
(setq org-hide-emphasis-markers t)

;; Wrap the lines in org mode so that things are easier to read
(add-hook 'org-mode-hook 'visual-line-mode)

(setq org-capture-templates
      '(
        ("n" "Note"
         entry (file+headline "/Users/jungchan/Library/Mobile Documents/iCloud~md~obsidian/Documents/org-mode/org/notes.org" "Notes")
         "** %?"
         :empty-lines 0)
        ))

;; Auto-reload notes.org when it changes on disk
(add-hook 'find-file-hook
          (lambda ()
            (when (and buffer-file-name
                       (string-match-p "notes\\.org$" buffer-file-name))
              (auto-revert-mode 1))))

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

(find-file "/Users/jungchan/Library/Mobile Documents/iCloud~md~obsidian/Documents/org-mode/org/")

;; Set a specific location for archived tasks
(setq org-archive-location "/Users/jungchan/Library/Mobile Documents/iCloud~md~obsidian/Documents/org-mode/org/archive.org::* Archived Tasks")

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

;; custom agendas
(setq org-agenda-custom-commands
      '(("w" "Work Tasks" ((agenda "") (tags-todo "-@personal")))
        ("p" "Personal Tasks" ((agenda "") (tags-todo "-@work")))))

;; turning on line highlighting, and defining custom color
(global-hl-line-mode 1)
(set-face-background 'hl-line "azure1")

;; custom refile function to move todos to todo.org 
(defun refile-todos-to-todo-file ()
  "Refile all TODO items from current buffer to todo.org under 'todos' heading."
  (interactive)
  (unless (file-exists-p "/Users/jungchan/Library/Mobile Documents/iCloud~md~obsidian/Documents/org-mode/org/todo.org")
    (error "Target file todo.org not found! Update the path in this function"))
  (let ((org-refile-targets '(("/Users/jungchan/Library/Mobile Documents/iCloud~md~obsidian/Documents/org-mode/org/todo.org" :regexp . "^\\*+ Todos\\>")))
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

;; show indenting in agenda view
(setq org-tags-match-list-sublevels 'indented)
