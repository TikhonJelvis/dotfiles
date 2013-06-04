;; A mode for interacting with gosu programs.
;;
;; This is not a mode for interacting with _inferior gosu processes_; rather, it
;; is a minor mode that enhances the shell with some additional commands I find
;; convenient. It allows you to easily restart the running program, clear the
;; shell or run tests; all this is abstracted into a system involving "profiles"
;; which just let you to control the commands used for both running the program
;; and running tests.
;;
;; The supplied example is for a gosu server that uses Aardvark to both run the
;; actual program ("vark server") and to run the tests ("vark test"); however, 
;; there is nothing binding this to Aardvark, or indeed to Gosu at all. However,
;; since I personally have only used this for Gosu so far,
(defvar gosu-program-mode-map (make-keymap))

(make-variable-buffer-local 'gosu-test-command)
(make-variable-buffer-local 'gosu-program-command)

(define-key gosu-program-mode-map (kbd "C-c C-j") 'gosu-run-program)
(define-key gosu-program-mode-map (kbd "C-c C-l") 'gosu-reset-and-clear)
(define-key gosu-program-mode-map (kbd "C-c C-t") 'gosu-run-tests)
(define-key gosu-program-mode-map (kbd "C-c C-;") 'gosu-program-profile)

(defvar gosu-program-profiles '())

(defun gosu-add-profile (name profile)
  "Adds the given profile with the given name to the list of gosu
  program profiles. If a profile with the given name already
  exists, this does nothing and returns nil."
  (unless (assoc name profile)
    (setq gosu-program-profiles (cons (cons name profile) gosu-program-profiles))))

;; An example profile for a Ronin server:
(gosu-add-profile "server" '((test-command . "vark test")
                             (run-command . "vark server")))
(gosu-add-profile "django" '((test-command . "empy manage.py test")
                             (run-command . "empy manage.py runserver")))
(gosu-add-profile "mysql" '((test-command . "")
                             (run-command . "sudo mysqld_safe")))
(gosu-add-profile "mongo" '((test-command . "")
                             (run-command . "mongod --dbpath ~/Documents/work/EatMetrics/db")))
(gosu-add-profile "empy" '((test-command . "empy manage.py test")
                             (run-command . "empy manage.py shell")))

(defun gosu-program-load-profile (profile)
  (if profile
      (progn (setq gosu-program-command (cdr (assoc 'run-command profile)))
             (setq gosu-test-command (cdr (assoc 'test-command profile))))
    (message "Error! Specified profile does not exist.")))
(defun gosu-program-profile ()
  (interactive)
  (let ((profile-name (read-from-minibuffer "Profile: ")))
    (message "Switching to profile %s." profile-name)
    (sleep-for 0 750)
    (gosu-program-load-profile (assoc profile-name gosu-program-profiles))))
(defun gosu-program-profile-by-name (name)
  (gosu-program-load-profile (assoc name gosu-program-profiles)))
  
(defun gosu-interrupt-and-clear ()
  (interactive)
  (comint-interrupt-subjob)
  (sleep-for 0 100)
  (delete-region (point-min) (point-max)))

(defun gosu-reset-and-clear ()
  (interactive)
  (gosu-interrupt-and-clear)
  (sleep-for 0 100)
  (comint-send-input)
  (save-excursion
    (beginning-of-buffer)
    (delete-blank-lines)
    (delete-blank-lines)))

(defun gosu-run-cmd (cmd msg)
  (interactive)
  (gosu-interrupt-and-clear)
  (comint-simple-send (get-buffer-process (current-buffer)) 
                      (concat "echo '" msg " Command: " cmd "'"))
  (sleep-for 0 50)
  (save-excursion
    (backward-char)
    (beginning-of-line)
    (kill-line))
  (end-of-buffer)
  (message "Sending %s to %s." cmd (get-buffer-process (current-buffer)))
  (comint-simple-send (get-buffer-process (current-buffer)) cmd))

(defun gosu-run-tests ()
  (interactive)
  (gosu-run-cmd gosu-test-command "Running tests..."))

(defun gosu-run-program ()
  (interactive)
  (gosu-run-cmd gosu-program-command "Running..."))

(defun gosu-set-test-command ()
  (interactive)
  (setq gosu-test-command (read-from-minibuffer "New gosu test command: ")))

(defun gosu-set-program ()
  (interactive)
  (setq gosu-program-command (read-from-minibuffer "New gosu program: ")))

;; Sets up the buffer-specific variables to a default value (Ronin commands, in this case).
(defun gosu-program-mode-start ()
  (set 'gosu-test-command  "./run-tests.gsp")
  (set 'gosu-program-command  "../bin/client.gsp -m daemon")
  (message "Process: %s" (get-buffer-process (current-buffer))))

(define-minor-mode gosu-program-mode "Simpler interaction with gosu programs."
  :init-value nil
  :lighter " gsp" 
  :keymap gosu-program-mode-map
  'gosu-program-mode-start)

(defun switch-to-program (name location profile)
  (pop-to-buffer (concat "*" name "*"))
  (unless (eq major-mode 'shell-mode) 
    (shell (current-buffer))
    (sleep-for 0 100)
    (delete-region (point-min) (point-max))
    (comint-simple-send (get-buffer-process (current-buffer)) 
                        (concat "cd " location)))
  (gosu-program-mode 1)
  (gosu-program-load-profile (assoc profile  gosu-program-profiles)))

(defvar example-server-location "~/server")
(defun example-server ()
  (interactive)
  (switch-to-program "server" example-server-location "server"))

(provide 'gosu-program-mode)