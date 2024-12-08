;;; Defines a menu of common locations for me to quickly jump to: home
;;; directory, downloads, key org files, programming projects... etc.

(defvar shortcuts-core-shortcuts
  '(("Home" . "~")
    ("Downloads" . "~/Downloads")
    ("Documents" . "~/Documents")
    ("Programming" . "~/Programming"))
  "Shortcuts suggested from `shortcuts-core'.

Entries are pairs (NAME . PATH) where PATH is the path to jump to
and NAME is a display name to show in the menu.")

(defun shortcuts-core ()
  "Return “core” shortcuts like ~, ~/Downloads... etc as set in
`shortcuts-core-shortcuts'."
  shortcuts-core-shortcuts)

(defun shortcuts-programming-projects ()
  "Return a list of directories in ~/Programming."
  (let ((files (cddr (directory-files "~/Programming"))))
    (mapcar (lambda (f) (cons f (format "~/Programming/%s" f))) files)))

(defun is-git-directory (dir)
  "Is the given path a git repository? Checks that the directory
exists and contains a .git subdirectory."
  (file-exists-p (expand-file-name ".git" dir)))

(defun shortcuts-org-agenda-files ()
  "Return shortcuts for every configured Org agenda file."
  (mapcar (lambda (f) (cons (file-name-nondirectory f) f)) org-agenda-files))

(defcustom shortcuts-sources '(shortcuts-core shortcuts-programming-projects)
  "Sources for shortcut locations to jump to.

This should be a list where each entry is a function that takes
no arguments and returns a list of (NAME . PATH) pairs where NAME
is a display name and PATH is the corresponding path to jump
to.")

(defun shortcuts ()
  "Return a list with all of the configured shortcut locations I
can jump to."
  (apply #'append (mapcar #'funcall shortcuts-sources)))

;; TODO: different vertico settings for different commands?
;;
;; probably with vertico-multiform or something
;;
;; (defun center-completing-read (prompt completions)
;;   "A version of completing-read that sets Selectrum to display a
;;   prompt in the middle of the current window rather than the
;;   bottom."
;;   (let ((initial-display-action selectrum-display-action))
;;     (setq selectrum-display-action '(display-posframe-center))
;;     (unwind-protect
;;         (completing-read prompt completions)
;;       (setq selectrum-display-action initial-display-action))))

(defun jump-to-shortcut (add-to-kill-ring)
  "Prompt the user with completions for the list of shortcuts,
then jump to the corresponding location.

If called with a numeric argument, add the location to the kill
ring instead of jumping there."
  (interactive "P")
  (let* ((targets (shortcuts))
         (chosen (completing-read "Jump to:" targets))
         (path (cdr (assoc chosen targets))))
    (when path (if add-to-kill-ring (kill-new path) (find-file path)))))
