;; -*- lexical-binding: t; -*-
;;
;; Zen finder --- simple and efficient fuzzy finder for minimalists
;; Copyright (C) 2021 Hiroyuki Ohsaki.
;; All rights reserved.
;;
;; $Id: zf.el,v 1.179 2021/05/05 10:22:37 ohsaki Exp ohsaki $
;;

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(defvar zf-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "C-n") 'zf-next)
    (define-key map (kbd "<down>") 'zf-next)
    (define-key map (kbd "M-TAB") 'zf-next)
    (define-key map (kbd "C-s") 'zf-next)
    (define-key map (kbd "C-p") 'zf-previous)
    (define-key map (kbd "<up>") 'zf-previous)
    (define-key map (kbd "C-v") '(lambda () (interactive) (zf-next 10)))
    (define-key map (kbd "M-v") '(lambda () (interactive) (zf-next -10)))
    (define-key map (kbd "M-<") '(lambda () (interactive) (zf-next -100000)))
    (define-key map (kbd "M->") '(lambda () (interactive) (zf-next 100000)))
    (define-key map (kbd "TAB") 'zf-accept-if-match)
    (define-key map (kbd "ESC ESC") '(lambda () (interactive) (throw 'zf-exit nil)))
    map)
  "Keymap for `zf' in the minibuffer.")

(defvar zf-find-file-map
  (let ((map (copy-keymap zf-map)))
    (define-key map (kbd "RET") 'zf-select)
    (define-key map (kbd "M-C-h") 'zf-dir-up)
    (define-key map (kbd "~") 'zf-dir-home)
    (define-key map (kbd "/") 'zf-dir-root)
    (define-key map (kbd "&") 'zf-find-this)
    (define-key map (kbd "!") 'zf-open-with)
    (define-key map (kbd "M-d") 'zf-dired)
    (define-key map (kbd "M-n") 'zf-next-history)
    (define-key map (kbd "M-p") 'zf-previous-history)
    map)
  "Keymap for `zf-find-file' in the minibuffer.")

(defgroup zf-face nil
  "Faces for zf-mode."
  :group 'zf
  :group 'faces)

(defface zf-marker-face
  '((t (:background "#808080" :foreground "#ff0000")))
  "Face for cursor indicator.")

(defface zf-cursor-face
  '((t (:background "#808080" :foreground "#40ffc0" :weight bold)))
  "Face for cursor line.")

(defface zf-match-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for showing characters matched.")

(defface zf-highlight-face
  '((t (:background "#0000ff")))
  "Face for highlighted strings.")

(defface zf-directory-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for directories.")

(defface zf-read-only-face
  '((t (:inherit font-lock-type-face)))
  "Face for read-only files.")

(defface zf-ignored-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for non-important items.")

(defvar zf-preview-font-lock-keywords
  '(;; comments
    ("^[#;%].*"  . font-lock-comment-face)
    ;; headers
    ("^.*¶$" . font-lock-function-name-face)
    ("^\\*+ +.*$" . font-lock-function-name-face)
    ;; itemize
    ("^ *[0-9.]+ " . font-lock-type-face)
    ("^ *- .+$" . font-lock-string-face)
    ;; paren
    ("\\[.+?\\]" . font-lock-variable-name-face)
    ("{.+?}" . font-lock-keyword-face)
    ;; symbols
    ("[$()\\]" . font-lock-type-face)
    ("\\(--+\\||\\)" . font-lock-comment-face) ;; table
    ;; constants
    ("[A-Z_]\\{3,\\}" . font-lock-constant-face)
    ;; LaTeX macros
    ("\\\\\\w+" . font-lock-keyword-face)
    ;; string
    ("\".*?\"" . font-lock-string-face)
    ("'.*?'" . font-lock-string-face)
    ("`.*?'" . font-lock-string-face)
    ("‘.*?’" . font-lock-string-face)))

(defvar zf-max-window-height 20
  "The maximum window height of the minibuffer.  Note that
  `zf-enable' overwrites `max-mini-window-height'.")

(defvar zf-xref-regexp-list
  '("\\((defun \\|(defvar \\|(defmacro \\|(defface \\|;;; \\)" ;; elisp
    "\\( *def \\| *class \\|[A-Z_]+ +=\\)"		   ;; python
    "\\( *sub +[^{]+ +{\\|my +\\$[A-Z_]+ +=\\)"		   ;; perl
    "\\(\\*+ +\\|<<\\|#\\+include:\\|#\\+caption:\\|#\\+label:\\)" ;; org
    "\\(\\\\section{\\|\\\\chapter{\\| *\\\\insertfig{\\|\\\\(re)?newcommand{\\)") ;; latex
  "List of regular expressions that match important parts in the
buffer (e.g., function/class/method/variable definitions and
section headers).")

(defvar zf-open-command-pattern-alist
  '(("\\.pdf$" . "mupdf '%s'")
    ("\\.jpg$" . "display '%s'"))
  "Definition of commands to be executed with `zf-open-with'.
  Each elemet in the association list is (REGEXP . COMMAND) where
  REGEXP is the regular expression macthing the file name and the
  COMMAND is the template of the shell command.")

(defvar zf-elisp-info-path "/usr/local/info/elisp.info.gz"
  "The path name of Elisp Reference Manual.")

(defvar zf-recently-used-ignore-regexp "\\(\\.bak\\|~\\)$")


;;; Internal variables
(defvar zf--prompt nil
  "String displayed as a prompt.")

(defvar zf--collection nil
  "List all candidates for matching.  This will be overridden if
  `zf--collection-function' is non-nil.")

(defvar zf--collection-function nil
  "Function to retrieve a list of candidates.")

(defvar zf--matches nil
  "List of matching items.")

(defvar zf--selected 0
  "The index of the selected item in `zf--collection'.")

(defvar zf--predicate-function nil
  "Function to filter candidates.")

(defvar zf--preview-buffer nil
  "Buffer to display the content for preview.")

(defvar zf--preview-function nil
  "Function for previewing the selected element, which will be
invoked with the selected element as the argument.")

(defvar zf--process-filter-func nil
  "Function for handling the output from the preview process.
  This function is invoked with two arguments: the start and the
  end of the region to be processed.")

(defvar zf-min-pattern-length 0
  "The threshold of the pattern length for starting pattern matching.")

(defvar zf--last-pattern nil
  "Store the last input pattern.")

(defvar zf--collection-cache nil
  "Store the element cache.  Each element is (PATTERN
. STRING-LIST).")

(defvar zf--info-elisp-buffer nil
  "Buffer for loading Emacs Lisp Info.")

(defvar zf--find-file-history nil
  "History of selected files in `zf-find-file'.")

(defvar zf--history-index -1
  "The current selection in `zf--find-file-history'.")

;; Variables for memorization (caching)
(defvar zf--exec-commands nil)
(defvar zf--function-functions nil)
(defvar zf--variable-variables nil)
(defvar zf--complete-symbols nil)
(defvar zf--info-elisp-lines nil)
(defvar zf--man-entries nil)

;; (zf-enable)
(defun zf-enable ()
  "Change global keymaps to activate the zf-mode."
  (interactive)
  ;; FIXME: Store the original binding.
  (global-set-key (kbd "C-x C-f") 'zf-find-file)
  (global-set-key (kbd "M-x") 'zf-exec)
  (global-set-key (kbd "C-x b") 'zf-list-buffers)
  (global-set-key (kbd "M-s o") 'zf-occur)
  ;; help-for-help
  (define-key help-map "f" 'zf-function)
  (define-key help-map "v" 'zf-variable)
  ;; dired
  (with-eval-after-load "dired"
    (define-key dired-mode-map (kbd "M-d") 'zf-find-file))
  ;; Local keybindings.
  ;; FIXME: Should not use user-configurable keymap.
  (global-set-key (kbd "C-c M") 'zf-man)
  (global-set-key (kbd "C-c I") 'zf-info-elisp)
  (global-set-key (kbd "C-c j") 'zf-xref)
  (global-set-key (kbd "C-c h") 'zf-org)
  (setq max-mini-window-height zf-max-window-height)
  ;; Load history.
  (setq zf--find-file-history (zf--load-history))
  (add-hook 'kill-emacs-hook 'zf--save-history))

;; (zf-disable)
(defun zf-disable ()
  "Revert global keymaps to deactivate the zf-mode.  The keymaps
is restored to the default keymap of the Emacs."
  (interactive)
  ;; FIXME: Restore the original binding.
  (global-set-key (kbd "C-x C-f") 'find-file)
  (global-set-key (kbd "M-x") 'execute-extended-command)
  (global-set-key (kbd "C-x b") 'switch-to-buffer)
  (global-set-key (kbd "M-s o") 'occur)
  ;; help-for-help
  (define-key help-map "f" 'describe-function)
  (define-key help-map "v" 'describe-variable)
  ;; Local keybindings.
  ;; FIXME: Should restore local keybidings.
  (setq max-mini-window-height 0.25))

(defun zf-mode (arg)
  "Activate or deactivate zf-mode globally.  With positive ARG,
zf-mode is activated.  Otherwise, zf-mode is deactivated."
  (interactive "p")
  (if (> arg 0)
      (zf-enable)
    (zf-disable)))


;;; Macros
(defmacro zf--with-active-minibuffer (&rest body)
  "Execute BODY in the active minibuffer."
  `(let ((win (active-minibuffer-window)))
     (when win
       (with-selected-window win
	 ,@body))))

(defun zf--minibuffer-active-p ()
  "Return non-nill if the current buffer is the active minibuffer."
  (zf--with-active-minibuffer
   (eq (get-buffer-window (current-buffer))
       (active-minibuffer-window))))

;; (zf--as-string 'symbolp)
;; (zf--as-string "abc")
(defmacro zf--as-string (object)
  "Return the string representation of object OBJECT."
  `(if (symbolp ,object)
       (symbol-name ,object)
     ,object))

;; for backward compatibility
(unless (fboundp 'string-empty-p)
  (defun string-empty-p (string)
    (string= string "")))


;;; Commands
;; (zf-next 1)
(defun zf-next (arg)
  "Move the cursor downward.  ARG specifies the number of
movements."
  (interactive "p")
  (let (max-index)
    (setq zf--selected (+ zf--selected arg))
    ;; Bound the cursor movement.
    (setq max-index (- (length zf--matches) 1))
    (setq zf--selected (max 0 (min max-index zf--selected)))
    (zf--update-display)
    (zf--preview)))

;; (zf-previous 1)
(defun zf-previous (arg)
  "Move the cursor upward.  ARG specifies the number of
movements."
  (interactive "p")
  (zf-next (- arg)))

;; (zf-accept-if-match)
(defun zf-accept-if-match ()
  "Exit the minibuffer if any matching item is currently selected."
  (interactive)
  (if (zf--current-item)
      (exit-minibuffer)
    (message "No matching item")))


;;; Commands for zf-find-file
;; (zf-select)
(defun zf-select ()
  "Select the current item and exit the minibuffer in
`zf-find-file'.  If the current pattern is empty, open
`default-directory` with `dired`.  If the current selection is a
directory, visit the directory with `zf-find-file`.  Otherwise,
create a new file or open the existing file."
  (interactive)
  (let ((pattern zf--last-pattern)	
	(dir (zf--current-item)))
    (cond ((or (string-empty-p pattern) ;; Empty pattern?
	       (string= pattern "^"))
	   (throw 'zf-exit `(dired ,default-directory)))
	  ((and dir ;; Directory selected?
		(file-directory-p dir))
	   (throw 'zf-exit `(dired ,dir)))
	  (t
	   (exit-minibuffer)))))

;; (zf-dir-home)
(defun zf-dir-home ()
  "Move to the home directory in `zf-find-file'."
  (interactive)
  (throw 'zf-exit "~/"))

;; (zf-dir-root)
(defun zf-dir-root ()
  "Move to the root directory in `zf-find-file'."
  (interactive)
  (if (eq last-command 'zf-dir-home)
      nil	; Do nothing if the last command is tilde `~'.
    (throw 'zf-exit "/")))

;; (zf-dir-up)
(defun zf-dir-up ()
  "Move to the parent directory in `zf-find-file'."
  (interactive)
  (throw 'zf-exit "../"))

;; (zf-find-this "RCS")
(defun zf-find-this ()
  "Replace the collection with the list of files under the
current working directory."
  (interactive)
  (let* ((dir (zf--current-item)))
    (when (file-directory-p dir)
      (zf--async-load-with-find dir))))

;; (zf-open-with "/home/ohsaki/public_html/papers/Asai07_IN3.pdf")
;; (zf-open-with)
(defun zf-open-with (&optional file)
  "Open the current selection with the external command, which is
determined by the configuration variable
`zf-open-command-pattern-alist'."
  (interactive)
  (unless file
    (setq file (zf--current-item)))
  (let (elem)
    (when (and file
	       (file-readable-p file))
      (setq elem (seq-find
		  (lambda (x)
		    (string-match (car x) file))
		  zf-open-command-pattern-alist))
      (when elem
	(start-process-shell-command
	 "*zf-open-with*" nil (format (cdr elem) file))))))

(defun zf-dired ()
  "Open the current directory with `dired` in `zf-find-file'."
  (interactive)
  (throw 'zf-exit `(dired ,default-directory)))

(defun zf-previous-history (&optional history)
  "Insert the pattern from the history HISTORY.  If HISTORY is
not specifed, `zf--find-file-history' is used."
  (interactive)
  (unless history
    (setq history zf--find-file-history))
  (let (path)
    (setq zf--history-index 
	  (min (1+ zf--history-index) (1- (length history))))
    (setq path (nth zf--history-index history))
    (when (file-readable-p path)
      (zf--put-pattern path))))

(defun zf-next-history (&optional history)
  "Insert the pattern from the history HISTORY.  If HISTORY is
not specifed, `zf--find-file-history' is used."
  (interactive)
  (unless history
    (setq history zf--find-file-history))
  (let (path)
    (setq zf--history-index 
	  (max -1 (1- zf--history-index)))
    (setq path (if (< zf--history-index 0)
		   ""
		 (nth zf--history-index history)))
    (when (file-readable-p path)
      (zf--put-pattern path))))


;; Utililities
;; (zf--buffer-to-strings)
(defun zf--buffer-to-strings ()
  "Split the content of the current buffer and return the list of
strings."
  (split-string (buffer-string) "\n"))

;; (zf--shell-command-to-strings "ls")
(defun zf--shell-command-to-strings (command)
  "Execute the shell command COMMAND and split the output from
the command execution, which will be returned as a list of
strings."
  (split-string (shell-command-to-string command) "\n"))

;; (zf--propertize-regexp "abc")
(defun zf--propertize-regexp (regexp &optional attr face beg end)
  "Change the text property of all strings matching REGEXP in the
current buffer between BEG and END.  The attribute and its value
are specified by ATTR and FACE.  Default values for ATTR, FACE,
BEG, and END are 'face, 'zf-highlight-face, and the beginning and
the end of the buffer."
  (unless attr
    (setq attr 'face))
  (unless face
    (setq face 'zf-highlight-face))
  (unless beg
    (setq beg (point-min)))
  (unless end
    (setq end (point-max)))
  (save-excursion
    (goto-char beg)
    (while (re-search-forward regexp end t)
      (put-text-property (match-beginning 0) (match-end 0)
			 attr face))))

(defun zf--re-search-around (lineno regexp &optional before after)
  "Perform `re-search-forward' with the regular expression REGEXP
in the current buffer at around the line number LINENO.  BEFORE
and AFTER specified the range for search."
  (unless before
    (setq before 3))
  (unless after
    (setq after 3))
  (let (end)
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- lineno))
      (forward-line (- before))
      (setq end (save-excursion
		  (forward-line (+ before after))
		  (point)))
      (re-search-forward regexp end t))))

;; (zf--useful-file-p ".emacs")
;; (zf--useful-file-p "")
;; (zf--useful-file-p "..")
;; (zf--useful-file-p ".emacs_flylint.el")
;; (zf--useful-file-p "RCS/.emacs")
;; (zf--useful-file-p "rcs/.emacs")
(defun zf--useful-file-p (name)
  "Return nil if the path name NAME should be ignored."
  (let ((case-fold-search nil))
    (when name
      (cond ((string-empty-p name)
	     nil)
	    ((member name '("." ".."))
	     nil)
	    ((string-match "\\(RCS\\|_flylint\\)" name)
	     nil)
	    (t
	     t)))))

;; (zf--expand-path "foo")
;; (zf--expand-path "~/foo")
;; (zf--expand-path "/foo/bar")
;; (zf--expand-path "foo/")
;; (zf--expand-path "/foo/../bar")
;; (zf--expand-path "../")
;; (zf--expand-path "foo//")
(defun zf--expand-path (name)
  "Normalize the pathname NAME, which can be either a relative
path name or a absolute path name.  Return an absolute path name.
Doulb slashes `//` are regarded as the root directory."
  (when (stringp name)
    ;; Double slash `//' means the root directory.
    (setq name (replace-regexp-in-string "^.*//" "/" name))
    (expand-file-name
     (if (file-name-absolute-p name)
	 name
       (concat (file-name-as-directory default-directory) name)))))

;; (zf--unexpand-path "/home/ohsaki/bin")
;; (zf--unexpand-path "pub//home/ohsaki/bin")
(defun zf--unexpand-path (path)
  "Replace the home directory with the tilde `~'."
  (replace-regexp-in-string
   (concat "^\\(.+?\\)?" (getenv "HOME")) "~" path))

;; (zf--string-file-key "ab")
;; (zf--string-file-key ".ab")
;; (zf--string-file-key "*zf*")
(defun zf--string-file-key (s)
  "Key function for sorting file names."
  (let ((prefix
	 (cond ((string-match "^[^.~/].*/$" s) ; Directory?
		"\000")
	       ((string-match "^~" s)	; History (home)?
		"\200")
	       ((string-match "^/" s)	; History (others)?
		"\210")
	       ((string-match "^\\.." s) ; Hidden?
		"\220")
	       ((string-match "^\\*" s)	; Hidden buffer?
		"\240" )
	       (t
		""))))
    (concat prefix
	    ;; Remove trailing slash `/'.
	    (replace-regexp-in-string "/$" "" s))))

;; (sort '("a/" ".fo" "a" "~a" "/b") 'zf--file-name-lessp)
;; (sort '("doc-base/" "doc/")'zf--file-name-lessp)
(defun zf--file-name-lessp (s1 s2)
  "Predicate for sorting path names (relative and absolute) and
buffer names."
  (string-lessp (zf--string-file-key s1) (zf--string-file-key s2)))

;; (zf--file-size "~/.emacs")
(defun zf--file-size (path)
  "Return the file size of file PATH.  The file PATH must exist
and be accessible."
  (file-attribute-size (file-attributes path)))

;; (zf--file-modes "~/.emacs")
(defun zf--file-modes (path)
  "Return the permission of file PATH (e.g., -rwxr-xr-x).  The
file PATH must exist and be accessible."
  (if (file-exists-p path)
      (file-attribute-modes (file-attributes path))
    "---------"))

;; (zf--format-pathname "~/.emacs")
;; (zf--format-pathname "RCS")
(defun zf--format-pathname (name)
  "Pretty printer for file name NAME.  Suffix `/' is added if
NAME is a directory.  If NAME is a directory or read-only, its
face is changed to `zf-directory-face' or `zf-read-only-face',
respectively."
  (let ((is-directory (file-directory-p name))
	(modes (zf--file-modes name)))
    (cond (is-directory
	   (propertize (file-name-as-directory name) ; Add trailing slash.
		       'face 'zf-directory-face))
	  ((string-match "^..-." modes)	; Read-only file?
	   (propertize name 'face 'zf-read-only-face))
	  (t
	   name))))

;; (zf--format-buffer-name "~/.emacs")
;; (zf--format-buffer-name "*zf*")
(defun zf--format-buffer-name (name)
  "Pretty printer for buffer name NAME.  Hidden buffers whose
names start with a space will have `zf-ignored-face'."
  (cond ((string-match "^\\*" name)	; Hidden buffer?
	 (propertize name 'face 'zf-ignored-face))
	(t
	 name)))

;; (zf--all-symbols)
(defun zf--all-symbols (&optional predicate)
  "Return all Emacs Lisp symbols in Emacs."
  (unless predicate
    (setq predicate 'identity))
  (let (symbols)
    ;; NOTE: Unlike mapcar, mapatoms will not return results.
    (mapatoms (lambda (x)
		(setq symbols (cons x symbols))))
    (seq-filter predicate symbols)))


;;; History
;; (zf--save-history '("a" "b" "c") "~/.emacs.d/zf-history")
(defun zf--save-history (&optional history file)
  "Save the history HISTORY to the file FILE.  If HISTORY is not
specified, `zf--find-file-history' is used.  If FILE is not
specified, `~/.emacs.d/zf-history' is used."
  (unless history
    (setq history zf--find-file-history))
  (unless file
    (setq file (expand-file-name "~/.emacs.d/zf-history")))
  (when history
    (with-temp-buffer
      (insert (mapconcat 'identity history "\n"))
      (write-region (point-min) (point-max) file))))

;; (zf--load-history "~/.emacs.d/zf-history")
  "Load the history from the file FILE.  Return a list of
strings.  If FILE is not specified, `~/.emacs.d/zf-history' is
used."
(defun zf--load-history (&optional file)
  (unless file
    (setq file (expand-file-name "~/.emacs.d/zf-history")))
  (when (file-readable-p file)
    (seq-filter 'file-readable-p
		(with-temp-buffer
		  (insert-file-contents file)
		  (zf--buffer-to-strings)))))


;; Collection handling
;; (zf--load-collection "abc" (lambda (pattern) '("a" "b")))
;; zf--collection
(defun zf--load-collection (pattern load-function is-async cache)
  "Load a list of items (either strings or symbols) corresponding
to the pattern PATTERN into `zf--collection'.  If the list does
not exist in the cache, it is generated by calling the function
LOAD-FUNCTION.  If IS-ASYNC is not nil, load collection
asynchronously.  CACHE is an alist for storing the collection
cache."
  (let ((cached-item (assoc pattern cache)))
    (if cached-item
	(setq zf--collection (cdr cached-item))
      ;; If not cached, retrieve the collection.
      (if is-async
	  (funcall load-function pattern)
	(setq zf--collection (funcall load-function pattern))
	(setq cached-item (cons pattern zf--collection))
	;; Add the item at the end of list.
	(nconc cache (list cached-item))))))

;; (zf--async-load-collection-with-find "RCS")
;; zf--collection
(defun zf--async-load-with-find (dir)
  "Asynchronously fiill `zf--collection' with the list of files
under the directory DIR."
  (let ((command (format "find '%s' -print 2>/dev/null" dir)))
    (setq zf--collection nil)
    (zf--async-loader command)))

(defun zf--async-loader (command)
  "Start asynchronous process for executing the shell command
COMMAND.  The output from the process is parsed and added at the
end of `zf--collection'."
  (let* ((process-connection-type nil)	; Use pipe instead of pty.
	 (buffer-name "*zf-async*")
	 (buffer (get-buffer buffer-name))
	 process)
    ;; Destroy and re-create the buffer.
    (when (buffer-live-p buffer)
      (let ((kill-buffer-query-functions nil))
	(kill-buffer buffer)))
    (setq buffer (get-buffer-create buffer-name))
    (setq process
	  (start-process-shell-command "zf-async" buffer command))
    (set-process-filter process 'zf--async-loader-filter)
    (set-process-sentinel process
			  (lambda (_proc _event)
			    (zf--update-matches)
			    (zf--update-display)))))

(defun zf--async-loader-filter (proc string)
  "Filter function to process the output STRING from the process
PROC.  STRING is split into lines and stored at the end of
`zf--collection'.  Pattern matching and redisplaying are
performed every 1,000 items addition."
  (when (buffer-live-p (process-buffer proc))
    ;; FIXME: output from the process might not be terminated by the newline
    (dolist (item (split-string string "\n"))
      (setq zf--collection (nconc zf--collection (list item)))
      (when (zerop (% (length zf--collection) 1000))
	(zf--update-matches)
	(zf--update-display)))))

;; (zf--current-item)
(defun zf--current-item (&optional index)
  "Return the currently-selected item in the matches.  If the
index INDEX is nil, return the INDEX-th element instead."
  (unless index
    (setq index zf--selected))
  (nth index zf--matches))


;;; Pattern matching
;; (zf--grep '("abcd" "b" "xxaa")  "a")
(defun zf--grep (list regexp)
  "Pick all items from a list of strings/symbols LIST.  REGEXP is
a regular expression except both period and minus are quoted."
  ;; Quote some regexp characters.
  (let ((quoted (replace-regexp-in-string "\\([.-]\\)" "\\\\\\1" regexp))
	matches x-str)
    (dolist (x list)
      (if (symbolp x)
	  (setq x-str (symbol-name x))
	(setq x-str x))
      (if (string-match quoted x-str)
	  (setq matches (cons x matches))))
    (nreverse matches)))

;; (zf--find-matches '("ab" "a" "ac" "ca" "cb")  "c a")
;; (zf--find-matches '("ab" "a" "ac" "ca" "cb")  "^c a")
;; (zf--find-matches '("c-w3m" "a" ".ac" ".ca" "c.b")  "^w3")
;; (zf--find-matches zf--collection "e")
(defun zf--find-matches (list pattern predicate)
  "Pick all items from the list LIST.  If PREDICATE is specified,
all items in the list LIST, which PREDICATE returns non-nil are
collected.  If PREDICATE is nil, PATTERN is regarded as a list of
regular expressions separated by one or more white spaces, and
items are selected using `zf--grep'."
  (if predicate
      (seq-filter (lambda (item)
		    (funcall predicate pattern item))
		  list)
    (let ((matches list)
	  (regexp-list (split-string pattern " +")))
      (dolist (regexp regexp-list)
        (setq matches (zf--grep matches regexp)))
      matches)))

;; (zf--highlight-pattern "abcde" "a d")
(defun zf--highlight-pattern (string pattern)
  "Highlight all substrings matching the pattern PATTERN in the
string STRING."
  (let ((regexp-list (split-string pattern " +"))
	quoted)
    (dolist (regexp regexp-list)
      (setq quoted (replace-regexp-in-string "\\([.-]\\)" "\\\\\\1" regexp))
      (if (string-match quoted string)
	  (put-text-property
	   (match-beginning 0) (match-end 0)
	   'face 'font-lock-keyword-face
	   string))))
  string)

(defun zf--update-matches ()
  "Update the maches stored in `zf--matches'."
  (setq zf--matches
	(zf--find-matches zf--collection zf--last-pattern
			  zf--predicate-function))
  ;; If the pattern starting with hat (^) yields nothing, removes
  ;; the preceeding hat, and try matching again.
  (when (and (not zf--matches)
	     (string-match "^\\^\\(.+\\)" zf--last-pattern)) ; ^ + Pattern?
    (save-excursion
      (goto-char (minibuffer-prompt-end))
      (delete-char 1))
    (setq zf--matches
	  (zf--find-matches zf--collection zf--last-pattern
			    zf--predicate-function))))


;;; Minibuffer
(defun zf--get-pattern ()
  "Retrieve the current pattern in the minibuffer."
  (car (split-string (minibuffer-contents) "\n")))

(defun zf--put-pattern (string)
  "Overwrite the current pattern in the minibuffer with the
string STRING."
  (zf--with-active-minibuffer
   (goto-char (minibuffer-prompt-end))
   (delete-region (point) (line-end-position))
   (insert string)))

(defvar zf--idle-timer nil)
(defvar zf--pattern-changed nil)
(defvar zf--initial-position nil)

(defun zf--idle-timer-event ()
  "This function is executed when the Emacs is idle.  If the
collection loading function `zf--collection-function' is non-nil,
it is invoked to load the collection.  Matching and display are
updated."
  (let ((is-async (get 'zf--collection-function 'async)))
    (when (and zf--collection-function
	       ;; Update only when the pattern is long enough.
	       (>= (length zf--last-pattern) zf-min-pattern-length))
      (zf--load-collection zf--last-pattern zf--collection-function
			   is-async zf--collection-cache)
      (zf--update-matches)
      (setq zf--selected 0)
      (zf--update-display))))

(defun zf--schedule-load ()
  "Add the timer for invoking `zf--idle-timer-event' to
`timer-idle-list' if it is not in `timer-idle-list' yet."
  (unless (memq zf--idle-timer timer-idle-list)
    (setq zf--idle-timer
	  (run-with-idle-timer 0.5 nil 'zf--idle-timer-event))))

(defun zf-post-command ()
  "The function invoked at every command execution.  If there is
  any change in the input pattern, recalculate the matching
  items and update the display."
  (let (pattern)
    (when (zf--minibuffer-active-p)
      (setq pattern (zf--get-pattern))
      ;; Update only when the pattern is changed.
      (unless (string= pattern zf--last-pattern)
	(setq zf--last-pattern pattern)
	(zf--update-matches)
	(if (not zf--initial-position)
	    (setq zf--selected 0)
	  ;; Use the initial position if specified.
	  (setq zf--selected zf--initial-position
		zf--initial-position nil))
	(zf--update-display)
	(zf--schedule-load)))))


;;; Display
;; (zf--display-header '("a" "b") '("a"))
(defun zf--display-header (collection matches)
  "Display the header line showing the numbers of matches and
candidates."
  (let ((nitems (length collection))
	(nmatches (length matches))
	(dir (propertize default-directory 'face 'zf-highlight-face)))
    (insert (format "  %4d/%4d %s\n"
		    nmatches nitems dir))))

;; (setq zf--last-pattern "a")
;; (zf--display-elems '("a" "b") 0)
(defun zf--display-elems (matches &optional selected)
  "Display all matching items in the minibuffer."
  (let ((height (- zf-max-window-height 2))
	(n 0)
	mark item-str)
    (dolist (item matches)
      ;; Display only around the cursor.
      (when (and (< (- selected (/ height 2)) n)
		 (< n (+ selected height)))
	(setq mark "  "
	      item-str (zf--highlight-pattern (zf--as-string item) zf--last-pattern))
	;; Highlight the line at the cursor.
	(when (= n selected)
	  (setq mark (propertize "> " 'face 'zf-marker-face))
	  (setq item-str (propertize item-str 'face 'zf-cursor-face)))
	(insert (format "%s%s\n" mark item-str)))
      (setq n (1+ n)))))

(defun zf--display (collection matches &optional selected)
  "Redraw the content of the minibufer."
  (save-excursion
    ;; Delete the existing element display.
    (goto-char (minibuffer-prompt-end))
    (goto-char (line-end-position))
    (delete-region (point) (point-max))
    ;; Display the items at the end.
    (insert "\n")
    (zf--display-header collection matches)
    (zf--display-elems matches selected)))

(defun zf--update-display ()
  "Update the content of the minibuffer and preview the current item."
  (when (zf--minibuffer-active-p)
    (zf--with-active-minibuffer
     ;; redisplay the maching items
     (zf--display zf--collection zf--matches zf--selected)
     (zf--preview))))


;;; Preview
(defun zf--init-preview-buffer ()
  "Initialize the buffer for preview."
  (let ((buffer (zf--preview-buffer)))
    (when buffer
      ;; Destroy and re-create the buffer.
      (let ((kill-buffer-query-functions nil))
	(kill-buffer buffer)))
    (setq zf--preview-buffer (get-buffer-create "*zf-preview*"))
    (with-current-buffer zf--preview-buffer
      (zf-preview-mode))))

(defun zf--preview-buffer ()
  "Return the preview buffer.  Return nil if the preview buffer
is not available."
  (let ((buffer zf--preview-buffer))
    (when (and buffer
	       (buffer-live-p buffer))
      buffer)))

;; (zf-preview-mode)
(defun zf-preview-mode ()
  "Major mode for content preview."
  (interactive)
  (visual-line-mode 1)
  (setq font-lock-defaults
	'(zf-preview-font-lock-keywords 'keywords-only nil))
  (font-lock-mode 1))

;; (zf--preview)
(defun zf--preview ()
  "Perfom the preview action specified by the variable
`zf--preview-function' for the currently-selected item."
  (let ((item (zf--current-item)))
    (if (and item
	     zf--preview-function)
	(funcall zf--preview-function item))))

(defun zf-preview-file (name)
  "Asynchronously display the content of the file specified by
NAME in some window."
  (let ((path (zf--expand-path name)))
    (when (and (file-regular-p path)
	       (file-readable-p path))
      (zf-preview-command-output
       ;; Display the first 100 lines for text files.  Otherwise,
       ;; display the first 100 lines of hexadecimal dump.
       (format "if file -L '%s' | grep -q text; \
then head -100 '%s' | nkf -w; \
else hd '%s' | head -100; \
fi"
	       path path path)))))

;; (zf-preview-command-output "ls")
(defun zf-preview-command-output (command &optional filter)
  "Asynchronously display the output from the shell command
COMMAND.  If FILTER is specified, it will be invoked for every
output from the process."
  ;; Arrange preview buffer.
  (let* ((shell-file-name "/bin/sh")
	 (process-connection-type nil)	; Use pipe instead of pty.
	 buffer process)
    (zf--init-preview-buffer)    
    (setq zf--process-filter-func filter)
    (setq buffer (zf--preview-buffer))
    (setq process (start-process-shell-command "zf-preview" buffer command))
    (set-process-filter process 'zf--process-filter)
    (set-process-sentinel process (lambda (_proc _event))) ; Discard `Finished.'.
    (display-buffer buffer
		    '((display-buffer-in-previous-window
                       display-buffer-use-some-window)))))

(defun zf--process-filter (proc string)
  "Process filter for receiving the output from the asynchronous
process.  This code is taken from the Emacs Lisp Reference
Manual."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
	  ;; Highlight strings matching the pattern.
	  (with-silent-modifications
	    (when (>= (length zf--last-pattern) 3)
	      (zf--propertize-regexp
	       zf--last-pattern
	       'font-lock-face 'zf-highlight-face
	       (process-mark proc) (point)))
	    ;; Invoke additional filter is specified.
	    (when zf--process-filter-func
	      (funcall zf--process-filter-func
		       (process-mark proc) (point))))
          (set-marker (process-mark proc) (point)))
	(if moving (goto-char (process-mark proc)))))))

;; (zf-preview-buffer "*scratch")
(defun zf-preview-buffer (name)
  "Display the buffer with NAME in some window."
  (let ((buf (get-buffer name)))
    (when (buffer-live-p buf)
      (display-buffer buf
		      '((display-buffer-in-previous-window
			 display-buffer-use-some-window))))))

(defun zf--preview-goto-line (entry &optional window)
  "Jump to the line matching the entry ENTRY in the window
WINDOW.  The line number needs to be embeded at the beginning of
ENTRY.  If WINDOW is nil, use the window returned from the
function `minibuffer-selected-window'."
  (unless window
    (setq window (minibuffer-selected-window)))
  (let ((lineno (get-text-property 0 'lineno entry)))
    (when (and window
	       lineno)
      (with-selected-window window
	(goto-char (point-min))
	(forward-line (1- lineno))
	(with-silent-modifications
	  (put-text-property (point-min) (point-max) 'font-lock-face nil)
	  ;; Highlight the current line.
	  (put-text-property (line-beginning-position) (line-end-position)
			     'font-lock-face 'zf-cursor-face)
	  ;; Highlight strings matching the pattern.
	  (when (>= (length zf--last-pattern) 3)
	    (zf--propertize-regexp zf--last-pattern
				      'font-lock-face 'zf-highlight-face)))))))


;;; Main
(defun zf--configure-variables (prompt collection preview predicate)
  "Save parameters in global variables."
  (setq zf--prompt prompt)
  (if (listp collection)
      (setq zf--collection collection
	    zf--collection-function nil)
    (setq zf--collection nil
	  zf--collection-function collection))
  (setq zf--preview-function preview)
  (setq zf--predicate-function predicate)
  (setq zf--last-pattern nil)
  (setq zf--collection-cache '("dummy" . nil)))

;; (zf "> " '("a" "b" "c"))
;; (zf "> " '("a" "b" "c") nil nil 'list)
(defun zf (prompt collection &optional initial preview predicate keymap)
  "The main function of the ZF.  Initiate fuzzy finder showing
PROMPT as a prompt with the initial pattern INITIAL.  

List of items is specified by COLLECTION, which is either a list
of strings/symbols or a function.  If COLLECTION is a function,
it will be invoked whenever the input pattern is changed.

PREVIEW is a function for previewing the selected item.

PREDICATE is a function for selecting items for matching.  If
PREDICATE is nil, use the ZF's default matching rule.

KEYMAP is the keyboard map in the minibuffer."
  (unless prompt
    (setq prompt "> "))
  (unless keymap
    (setq keymap zf-map))
  ;; Configure global variables.
  (zf--configure-variables prompt collection preview predicate)
  (add-hook 'post-command-hook 'zf-post-command)
  ;; Force the execution of remove-hook in any case.
  (let ((input
         (unwind-protect
             ;; Receive the exit status if exception.
             (catch 'zf-exit
	       (read-from-minibuffer prompt initial keymap)
	       (or (zf--current-item)
	           zf--last-pattern))   ; Use the input if no match.
           (remove-hook 'post-command-hook 'zf-post-command))))
    input))


;;; File
;; (zf--recently-used-files)
(defun zf--recently-used-files (&optional history)
  "Return a list of recently-used files."
  (unless history
    (setq history zf--find-file-history))
  (seq-filter (lambda (path)
		(not (string-match zf-recently-used-ignore-regexp path)))
	      (mapcar 'zf--unexpand-path history)))

;; (let ((default-directory "/usr/share")) (zf--find-file-1))
(defun zf--find-file-1 ()
  (let ((collec (directory-files "." nil nil 'nosort)))
    ;; Add recently-opened files at the end.
    (setq collec (nconc collec (zf--recently-used-files)))
    (setq collec (seq-filter 'zf--useful-file-p collec))
    (setq collec (mapcar 'zf--format-pathname collec))
    (setq collec (sort collec 'zf--file-name-lessp))
    (setq zf--history-index -1)
    (zf "Find file> " collec "^"
	'zf-preview-file nil zf-find-file-map)))

;; (zf-find-file)
(defun zf-find-file ()
  "Start file browsing from the current directory.  If a file is
selected, open the file with `find-file'.  If the input pattern
does not match with existing files/directories, create a new
file."
  (interactive)
  (let* ((dir default-directory)
	 (default-directory dir)
	 name path)
    ;; Repeat as long as a directory is chosen.
    (catch 'found
      (while t
	(setq  default-directory dir
	       name (zf--find-file-1)
	       path (zf--expand-path name))
	(cond ((listp name)		; S-expression?
	       (eval name)
	       (throw 'found name))
	      ((not (file-directory-p path))
	       (add-to-list 'zf--find-file-history (zf--unexpand-path path))
	       ;; FIXME: Remove the preceeding hat `^'.
	       (find-file path)
	       (throw 'found path))
	      (t
	       ;; Path must be a directory.
	       (setq dir (file-name-as-directory path))))))))


;;; Occur
;; (zf--occur-compose-lines)
;; (zf--occur-compose-lines '(lambda (s) (if (string-match "defun" s) s)))
(defun zf--occur-compose-lines (&optional filter sort-pred)
  "Split the current buffer into lines.  If a filter function
FILTER is specified, FILTER is invoked for every line.  A line is
included only when FILTER returns a non-nil value.  If SORT-PRED
is specified, all lines are sorted using the predicate
SORT-PRED."
  (let ((lines (zf--buffer-to-strings))
	(n 1))
    (dolist (line lines)
      (unless (string-empty-p line)
	;; Embed the line number as a text property.
	(put-text-property 0 (length line) 'lineno n line))
      (setq n (1+ n)))
    ;; Filter candidates if a filter function is specified.
    (when filter
      (setq lines (delete nil (mapcar filter lines))))
    (when sort-pred
      (setq lines (sort lines sort-pred)))
    lines))

;; (zf-occur)
(defun zf-occur (&optional prompt filter)
  "Start fuzzy finder for all lines in the current buffer.
PROMPT specified the prompt shown at the beginning of the
minibuffer.  FILTER is a filter function passed to
`zf--occur-compose-lines'."
  (interactive)
  (unless prompt
    (setq prompt "Occur> "))
  (let ((lines (zf--occur-compose-lines filter))
	(current (line-number-at-pos))
	(init-pos 0)
	lineno)
    ;; Initially select the line around the point.
    (dolist (line lines)
      (setq lineno (get-text-property 0 'lineno line))
      (if (<= (or lineno 0) current)
	  (setq init-pos (1+ init-pos))))
    (setq zf--initial-position init-pos)
    (unwind-protect
	(zf prompt lines nil
	    'zf--preview-goto-line)
      ;; Remove highlights.
      ;; FIXME: Should be invoked as a hook.
      (with-silent-modifications
	(put-text-property (point-min) (point-max)
			   'font-lock-face nil)))))


;;; Xref
(defun zf--xref-filter (string)
  "Return non-nill if a string STRING contains a tag.  The
definition of tags are stored in the variable
`zf-xref-regexp-list'."
  (let ((case-fold-search nil))
    (seq-some
     (lambda (regexp)
       (when (string-match regexp string)
	 string))
     zf-xref-regexp-list)))

(defun zf-xref ()
  "Start fuzzy finder for all tags (e.g., definitions, headers
  and specific keywords) in the current buffer."
  (interactive)
  (zf-occur "Xref> " 'zf--xref-filter))


;;; Elisp manual
(defun zf--info-elisp-filter (line)
  "A filter function to process the string LINE."
  ;; e.g., ` -- Function: append &rest sequences'
  (when (string-match " -- [A-Za-z][A-Za-z ]*: +" line)
    (replace-match "" nil nil line)))

(defun zf--info-elisp-setup ()
  "Return the buffer visiting the Emacs Lisp Info."
  (let ((buffer zf--info-elisp-buffer))
    (unless (buffer-live-p buffer)
      ;; Load info buffer and extract the collection.
      (setq buffer (get-buffer-create "*zf-info-elisp*"))
      (setq zf--info-elisp-buffer buffer)
      ;; Visit elisp info.
      (with-current-buffer buffer
	(insert-file-contents zf-elisp-info-path)
	(zf-preview-mode)
	(setq zf--info-elisp-lines
	      (zf--occur-compose-lines 'zf--info-elisp-filter 'string-lessp))))
    buffer))

(defun zf--info-elisp-preview (item)
  "Jump to the location where the definition of the selected item
ITEM is described in the Emacs Lisp reference manual."
  (let ((win (get-buffer-window zf--info-elisp-buffer)))
    (when win
      (zf--preview-goto-line item win))))

;; (zf-info-elisp)
(defun zf-info-elisp ()
  "Start fuzzy finder for Emacs Lisp functions and variables."
  (interactive)
  (let ((buffer (zf--info-elisp-setup)))
    (display-buffer buffer
		    '((display-buffer-in-previous-window
		       display-buffer-use-some-window)))
    (zf "Elisp> " zf--info-elisp-lines nil
	'zf--info-elisp-preview)
    (switch-to-buffer buffer)
    (recenter)))


;;; Others
;; (zf-exec)
(defun zf-exec ()
  "Replacement of the Emacs' standard `execute-extended-command'.
Prefix argument is not supported in the current version."
  (interactive)
  (let (symbol)
    ;; FIXME: Needs to periodically refresh the cache.
    (unless zf--exec-commands
      ;; Retrieve all function symbols.
      (setq zf--exec-commands
      	    (sort
      	     (zf--all-symbols 'commandp)
      	     'string-lessp)))
    ;; Invoke zf and read symbol to execute.
    (setq symbol (zf "M-x> " zf--exec-commands "^"))
    (when symbol
      (command-execute symbol 'record))))

;; (zf-preview-symbol 'add-to-list)
;; (zf-preview-symbol 'fill-column)
;; (zf-preview-symbol '2C-command)
(defun zf-preview-symbol (symbol)
  "Display the definition of the symbol SYMBOL in some window."
  (let ((name (symbol-name symbol))
	buffer file doc
	usage header desc)
    (zf--init-preview-buffer)
    (setq buffer (zf--preview-buffer))
    (with-current-buffer buffer
      (insert "* " name "\n\n")
      (when (fboundp symbol)
	;; Display function usage and description taken from the
	;; docstring.
	;; FIXME: Import autoload functions.
	(setq doc (documentation symbol))
	(setq usage (help-split-fundoc (or doc "") symbol t))
	(setq header (car usage))
	(setq desc (cdr usage))
	(insert "  " (or header "") "\n\n"
		(or desc "") "\n"))
      (setq doc (documentation-property symbol 'variable-documentation))
      (when doc
	(insert doc "\n\n"))
      ;; Display the definition in the source code.
      (setq file (find-lisp-object-file-name symbol nil))
      (when (stringp file)		; Can be C-source.
	(insert "----------------\n" file ":\n")
	(shell-command
	 (format "zcat '%s.gz' | cat -n | \
sed -n '/(de\\(fun\\|macro\\|subst\\|fine-derived-mode\\|fvar\\|fvar-local\\) %s/,$p'"
		 file name) 'current))
      (goto-char (point-min)))
    (display-buffer buffer
		    '((display-buffer-in-previous-window
                       display-buffer-use-some-window)))))

;; (zf-function)
;; (setq debug-on-signal t)
;; (setq debug-on-signal nil)
(defun zf-function ()
  "Replacement of the Emacs' standard `describe-function'."
  (interactive)
  (let (symbol initial func)
    (unless (and zf--function-functions)
      ;; Retrieve all function symbols.
      (setq zf--function-functions
	    (sort
	     (zf--all-symbols 'fboundp)
	     'string-lessp)))
    (setq symbol (thing-at-point 'symbol)
	  initial (concat "^" symbol)
	  func (zf "Describe function> " zf--function-functions initial
		   'zf-preview-symbol))
    (when func
      (describe-function func))))

;; (zf-variable)
(defun zf-variable ()
  "Replacement of the Emacs' standard `describe-variable'."
  (interactive)
  (let (symbol initial variable)
    (unless zf--variable-variables
      ;; Retrieve all variable symbols.
      (setq zf--variable-variables
	    (sort
	     (zf--all-symbols
	      (lambda (x)
		(and (boundp x)
		     (not (keywordp x))
		     (get x 'variable-documentation))))
	     'string-lessp)))
    (setq symbol (thing-at-point 'symbol)
	  initial (concat "^" symbol)
	  variable (zf "Describe variable> " zf--variable-variables initial
		       'zf-preview-symbol))
    (when variable
      (describe-variable variable))))

;; (zf-list-buffers)
(defun zf-list-buffers ()
  "Replacement of the Emacs' standard `list-buffers'."
  (interactive)
  (let ((buffers (mapcar 'buffer-name (buffer-list)))
	selected)
    ;; Exclude all hidden (internal) buffers.
    (setq buffers (seq-remove
		   (lambda (name)
		     (string-match "^ " name))
		   buffers))
    (setq buffers (sort buffers 'zf--file-name-lessp))
    (setq buffers (mapcar 'zf--format-buffer-name buffers))
    (setq selected (zf "Buffer> " buffers nil
		       'zf-preview-buffer))
    (when selected
      (switch-to-buffer selected))))

;; (zf-complete)
;; (local-set-key (kbd "M-TAB") 'zf-complete) ins
(defun zf-complete ()
  "Start the completion of Emacs Lisp symbol at the point."
  (interactive)
  (let (beg end word initial selected)
    (unless zf--complete-symbols
      ;; Retrieve all symbols.
      (setq zf--complete-symbols
	    (sort
	     (zf--all-symbols
	      (lambda (x)
		(or (fboundp x)
		    (and (boundp x)
			 (not (keywordp x))
			 (get x 'variable-documentation)))))
	     'string-lessp)))
    (setq end (point))
    (setq beg (save-excursion
		(backward-word 1)
		(point)))
    (setq word (buffer-substring-no-properties beg end))
    (setq initial (concat "^" word))
    (setq selected (zf "Complete> " zf--complete-symbols initial))
    (when selected
      (delete-region beg end)
      (insert (zf--as-string selected)))))


;;; Man
;; (zf--man-parse-entry " ls (1) - list directory contents")
;; (zf--man-parse-entry "X11::Xlib (3pm) - Low-level access to the X11 library")
(defun zf--man-parse-entry (line)
  "Parse a string LINE, which is an output from `mak -k'.  Return
a list of the manual entry, the section, and the short
description."
  (when (string-match "^ *\\([^ ]+\\) *(\\([0-9]+.*?\\)) *- *\\(.+\\)" line)
    (list (match-string-no-properties 1 line)
	  (match-string-no-properties 2 line)
	  (match-string-no-properties 3 line))))

;; (zf--man-entry-lessp "ls(2)" "cat(1)")
;; (zf--man-entry-lessp "ls(2)" "ls(1)")
;; (zf--man-entry-lessp "ls(1)" "ls(2)")
(defun zf--man-entry-lessp (s1 s2)
  "Predicate for sorting manual entries.  Entries are sorted by
the section number.  If the section numbers are idential, they
are sorted by the alphabetical order."
  (let ((sec1 (nth 1 (zf--man-parse-entry s1)))
	(sec2 (nth 1 (zf--man-parse-entry s2))))
    (if (string= sec1 sec2)
	(string-lessp s1 s2)
      (string-lessp sec1 sec2))))

;; (zf-man)
(defun zf-man ()
  "Start fuzzy finder of manual pages."
  (interactive)
  (let (entries line entry)
    (unless zf--man-entries
      (setq entries (zf--shell-command-to-strings "man -k ''"))
      ;; reformat the entries obtained from `man -k' for better readability
      (setq zf--man-entries
	    (sort
	     (mapcar
	      (lambda (s)
		(let ((entry (zf--man-parse-entry s)))
		  (format "%-40s - %s"
			  (format "%s (%s)" (nth 0 entry) (nth 1 entry))
			  (nth 2 entry))))
	      entries)
	     'zf--man-entry-lessp)))
    (setq line (zf "Man> " zf--man-entries "^"
                   'zf--man-preview-function))
    (setq entry (zf--man-parse-entry line))
    (let ((Man-notify-method 'pushy))
      (manual-entry (car entry)))))

;; (zf--man-preview-function " ls (1) - list directory contents")
(defun zf--man-preview-function (line)
  "Parse a string LINE and if it contains a valid manual entry,
display the manual page in the preview buffer."
  (let ((entry (zf--man-parse-entry line))
	name sect cmd)
    (when entry
      (setq name (nth 0 entry))
      (setq sect (nth 1 entry))
      (setq cmd (format "man %s '%s' | cat" sect name)) ; Pipe for disabling tty.
      (zf-preview-command-output cmd))))


;;; Org
;; (zf--org-filter "* some header")
;; (zf--org-filter "some text")
(defun zf--org-filter (line)
  (let ((lineno (get-text-property 0 'lineno line))
	(tag "")
	indent output)
    (cond
     ;; Section header?
     ((string-match "^\\(\\*+\\) " line)
      ;; Calcurat the indent level.
      (setq indent (match-string 1 line))
      (setq indent (replace-regexp-in-string "." "  " indent))
      ;; Look for the anchor around the section header.
      (when (zf--re-search-around lineno "<<.+?>>" 0 1)
	(setq tag (match-string 0)))
      (setq line (concat indent line))
      (setq output (format "%-48s\t%s" line tag)))
     ;; Label definition?
     ((string-match "#\\+label: \\(.+\\)" line)
      ;; Replace the entire line with the label.
      (setq line (match-string 1 line))
      ;; Look for the caption around the label.
      (when (zf--re-search-around lineno "#\\+caption: *\\(.+\\)")
	(setq tag (match-string 1)))
      (setq output (format "        %-40s\t%s" line tag))))
    (when output
      (put-text-property 0 1 'lineno lineno output)
      output)))

;; (progn (find-file "~/doc/book/asm/book.org") (zf-org))
;; (zf-org)
(defun zf-org ()
  (interactive)
  (zf-occur "Org> " 'zf--org-filter))

(provide 'zf)
