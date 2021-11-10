;; Had to pull this out into a file because the Nix Elisp parser
;; didn't like literals like #x2300 :/

;; TODO: Move font stuff to home directory (with home-manager or
;; nix-darwin or whatever...)
(load-file (dotfile "../nixos/base/fonts/icons/characters.el"))

;; Not sure why this needs to be in a hook, but it didn't initialize
;; correctly otherwise.
(defun emoji-fonts-hook (frame)
  (set-fontset-font t 'unicode (face-attribute 'default :family))
  (set-fontset-font t '(#x2300 . #x27e7) "Twitter Color Emoji")
  (set-fontset-font t '(#x27F0 . #x1FAFF) "Twitter Color Emoji")

  (let ((start (string-to-char (cdr (car tikhon-emacs-symbols-font-alist))))
        (end (string-to-char (cdr (car (last tikhon-emacs-symbols-font-alist))))))
    (set-fontset-font t (cons start end) "tikhon-emacs-icons"))

  (set-fontset-font t 'unicode "Symbola" nil 'append))
(add-hook 'after-make-frame-functions #'emoji-fonts-hook)

(defun tikhon-icon (name)
  "Return the character that corresponds to the icon with the
given name from my custom icon font.

The full mapping of names and characters is in
`tikhon-emacs-symbols-font-alist'."
  (cdr (assoc name tikhon-emacs-symbols-font-alist)))

(defun tikhon-icon-candidates ()
    "Return a list of icon names based on
`tikhon-emacs-symbols-font-alist', but with names that include
the character itself (for better selection menu usability)."
  (mapcar
   (lambda (pair)
     (cons (format "%s	%s" (cdr pair) (car pair)) (cdr pair)))
   tikhon-emacs-symbols-font-alist))

(defun tikhon-icon-insert ()
  "Interactively insert a character from my custom symbols font by name."
  (interactive)
  (let* ((candidates (tikhon-icon-candidates))
         (selection (completing-read "Icon: " candidates nil t)))
    (insert (cdr (assoc selection candidates)))))

(defun tikhon-icon-for-path (path)
  "Return an icon character and a color for a given file or directory."
  (if (file-directory-p path)
      (tikhon-icon-for-directory path)
    (tikhon-icon-for-file path)))

(defun tikhon-icon-for-directory (path)
  "Return an icon character for the given directory. This
defaults to  with special logic for a few specific things like
symlinks () and programming projects, as well as directories
with patterns defined in `tikhon-icons-file-name-patterns'."
  (cond
   ((file-symlink-p path) (tikhon-icon "directory-symlink"))
   (t (tikhon-icon "directory"))))
