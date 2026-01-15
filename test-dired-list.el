;; Test script to verify dired with list of files
(let* ((proj-dir (expand-file-name "projects" user-org-directory))
       (org-files (directory-files proj-dir nil "\.org$"))
       (dired-list (cons proj-dir org-files)))
  (message "Directory: %s" proj-dir)
  (message "Files: %s" org-files)
  (message "Dired List: %s" dired-list))
