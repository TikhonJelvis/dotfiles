;;; Defines a menu of common locations for me to quickly jump to: home
;;; directory, downloads, key org files, programming projects... etc.

(defun core-shortcuts ()
  "Return “core” shortcuts like ~, ~/Downloads... etc.

Entries are pairs (NAME . PATH) where PATH is the path to jump to
and NAME is a display name to show in the menu."

  '(("Home" . "~") ("Downloads" . "~/Downloads") ("Documents" . "~/Documents") ("Programming" . "~/Programming")))

(defun programming-projects ()
  "Return a list of directories in ~/Programming."
  (let ((files (cddr (directory-files "~/Programming"))))
    (mapcar (lambda (f) (cons f (format "~/Programming/%s" f))) files)))

(defcustom shortcut-sources '(core-shortcuts programming-projects)
  "Sources for shortcut locations to jump to.

This should be a list where each entry is a function that takes
no arguments and returns a list of (NAME . PATH) pairs where NAME
is a display name and PATH is the corresponding path to jump
to.")

(defun shortcuts ()
  "Return a list with all of the configured shortcut locations I
can jump to."
  (apply #'append (mapcar #'funcall shortcut-sources)))

(defun center-completing-read (prompt completions)
  "A version of completing-read that sets Selectrum to display a
  prompt in the middle of the current window rather than the
  bottom."
  (let ((initial-display-action selectrum-display-action))
    (setq selectrum-display-action '(display-posframe-center))
    (unwind-protect
        (completing-read prompt completions)
      (setq selectrum-display-action initial-display-action))))

(defun jump-to-shortcut ()
  "Prompt the user with completions for the list of shortcuts,
then jump to the corresponding location."
  (interactive)
  (let* ((targets (shortcuts))
         (chosen (center-completing-read "Jump to:" targets))
         (path (cdr (assoc chosen targets))))
    (when path (find-file path))))