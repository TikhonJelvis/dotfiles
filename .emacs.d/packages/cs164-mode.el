;; The easiest way to make an Emacs mode is to use the generic-mode macro.
;; Since I am inherently lazy, this is what I have done here.
;; This was loosely based off of gosu-mode at http://gosu-lang.org/downloads/gosu-mode.el

;; By: Tikhon Jelvis
(require 'generic-x)

(defgroup cs164 nil
  "Customization variables for the glorious cs164 language mode."
  :version "23.3.1")

(defcustom cs164-basic-offset 4
  "This is the default size of one indentation. Code in
blocks (like after an if statement) will be indented by this many
spaces when you press <tab>. This probably won't work if it's
negative."
  :group 'cs164
  :type 'integer)
(defcustom cs164-indent-automatically t
  "If this is t, indents whenever you press RET or type a closing
brace (}). Otherwise behaves as normal."
  :group 'cs164
  :type 'boolean)
(defcustom cs164-clear-shell-output t
  "If this is nil, cs164-run-file and cs164-parse-file will not
clear the old output in the *164* shell buffer."
  :group 'cs164
  :type 'boolean)
(defcustom cs164-base-directory ""
  "This is the directory that contains your main.py file. It
should also contain rparse.py. If it is a blank string, all file
paths will be relative; this means you will only be able to run
.164 files from the same directory as your interpreter."
  :group 'cs164
  :type 'directory)
(defcustom cs164-python-command "python"
  "This is the command used to launch python to run your files."
  :group 'cs164
  :type 'string)
(defcustom cs164-interpreter "main.py" 
  "This is the python file that runs your interpreter."
  :group 'cs164
  :type 'string)
(defcustom cs164-parser "rparse.py"
  "This is the file containing the parser for the language."
  :group 'cs164
  :type 'string)
(defcustom cs164-grammar "cs164a.grm"
  "This file describes the grammar of the language. It is passed
to the parser if you just want the AST of your code."
  :group 'cs164
  :type 'string)

(defun cs164-run-command ()
  (concat cs164-python-command " " cs164-base-directory cs164-interpreter))

(defun cs164-parse-command ()
  (concat cs164-python-command " " cs164-base-directory cs164-parser " -r " cs164-grammar " < "))

(defvar cs164-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "< " table)
    (modify-syntax-entry ?\n "> " table)
    (modify-syntax-entry ?' "\"'" table)
    (modify-syntax-entry ?\" "\"'" table)
    table))

(defvar cs164-mode-map (make-keymap))
(define-key cs164-mode-map (kbd "C-c C-l") 'cs164-run-file)
(define-key cs164-mode-map (kbd "C-c C-p") 'cs164-parse-file)
(define-key cs164-mode-map (kbd "RET") 'cs164-condition-indent)
(define-key cs164-mode-map (kbd "}") 'cs164-electric-brace)

(defun cs164-run-command-on-file (command)
  (let ((file-name (buffer-file-name)))
    (pop-to-buffer "*cs164*")
    (unless (eq major-mode 'shell-mode) 
      (shell (current-buffer)))
    (sleep-for 0 100)
    (if cs164-clear-shell-output (delete-region (point-min) (point-max)))
    (comint-simple-send (get-buffer-process (current-buffer)) 
                        (concat command " " file-name))))

(defun cs164-run-file ()
  "Runs the current 164 file using your interpreter."
  (interactive)
  (cs164-run-command-on-file (cs164-run-command)))

(defun cs164-parse-file ()
  "Passes the current 164 file into rparse.py to get a pretty AST."
  (interactive)
  (cs164-run-command-on-file (cs164-parse-command)))

(defun cs164-condition-indent ()
  "Indents if automatic indentation is on."
  (interactive)
  (newline)
  (if cs164-indent-automatically
      (cs164-indent-line)))

(defun cs164-electric-brace ()
  "Inserts a } and indents if automatic indentation is on."
  (interactive)
  (insert "}")
  (if cs164-indent-automatically
      (progn (if (current-line-matchesp "[ \t]*}[ \t]*")
                 (cs164-indent-line))
             (if (equal (char-after) ?})
                 (forward-char)))))
  
(defun line-matchesp (regexp offset)
  "Return t if line matches regular expression REGEXP.  The 
selected line is chosen by applying OFFSET as a numeric 
increment or decrement away from the current line number.
This function modifies the match data that `match-beginning',
`match-end' and `match-data' access; save and restore the match
data if you want to preserve them."
  (interactive)
  (save-excursion
    (forward-line offset)
    (beginning-of-line)
    (looking-at regexp)))

(defun previous-line-matchesp (regexp)
  "Return t if previous line matches regular expression REGEXP.
This function modifies the match data that `match-beginning',
`match-end' and `match-data' access; save and restore the match
data if you want to preserve them."
  (interactive)
  (line-matchesp regexp -1))

(defun current-line-matchesp (regexp)
  "Return t if current line matches regular expression REGEXP.
This function modifies the match data that `match-beginning',
`match-end' and `match-data' access; save and restore the match
data if you want to preserve them."
  (interactive)
  (line-matchesp regexp 0))

(defun cs164-indent-line ()
  (interactive)
  "Establish a set of conditional cases for the types of lines that
point currently is on, and the associated indentation rules."
  (indent-line-to
   (cond
    ((and
      (previous-line-matchesp "^[ \t]*\\*")
      (current-line-matchesp "^[ \t]*\\*"))
     (save-excursion
       (forward-line -1)
       (current-indentation)))
    ((and
      (previous-line-matchesp "^[ \t]*/\\*")
      (current-line-matchesp "^[ \t]*\\*"))
     (save-excursion
       (forward-line -1)
       (+ (current-indentation) 1)))
    ((and
      (previous-line-matchesp "^[ \t]*\\.")
      (current-line-matchesp "^[ \t]**\\."))
     (save-excursion
       (forward-line -1)
       (current-indentation)))
    ((and
      (not (previous-line-matchesp "^[ \t]*\\."))
      (current-line-matchesp "^[ \t]*\\."))
     (save-excursion
       (forward-line -1)
       (+ (current-indentation) cs164-basic-offset)))
    ((current-line-matchesp "^[ \t]*}")
     (save-excursion
       (beginning-of-line)
       (backward-up-list)
       (current-indentation)))
    (t
     (save-excursion
       (condition-case nil
           (progn
             (beginning-of-line)
             (backward-up-list)
             (+ (current-indentation) cs164-basic-offset))
         (error 0)))))))

;; Set up the actual generic mode
(define-generic-mode 'cs164-mode
  ;; comment-list
  nil
  ;; keyword-list
  '("def"
    "if"
    "else"
    "while"
    "for"
    "in"
    "print"
    "error"
    "ite"
    "lambda"
    "yield"
    "coroutine"
    "resume"
    "native")
  ;; font-lock-list
  '(("\\b\\([0-9]+\\|null\\)\\b" . font-lock-constant-face)
    ("[-+*/!=<>]+" . font-lock-builtin-face)
    ("def \\([_a-zA-Z0-9]+\\)" 1 'font-lock-variable-name-face))
  ;; auto-mode-list
  '(".164\\'")
  ;; function-list
  '((lambda () 
      (set-syntax-table cs164-mode-syntax-table)
      (set (make-local-variable 'indent-line-function) 'cs164-indent-line)
      (use-local-map cs164-mode-map))))

(provide 'cs164-mode)
