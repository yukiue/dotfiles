;; -*- Emacs-Lisp -*-
;; 
;; Major mode for editing and running Mathematica program.
;; Copyright (c) 2015-2017, Hiroyuki Ohsaki.
;; All rights reserved.
;; 
;; $Id: math-mode.el,v 1.30 2019/06/23 03:33:12 ohsaki Exp ohsaki $
;; 

;; This code is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; this code, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; $Log: math-mode.el,v $
;; Revision 1.30  2019/06/23 03:33:12  ohsaki
;; *** empty log message ***
;;
;; Revision 1.29  2018/05/03 08:23:14  ohsaki
;; *** empty log message ***
;;
;; Revision 1.28  2017/07/14 15:02:56  ohsaki
;; - revised handling of escape sequences.
;; - adjusted alignment of inline image.
;;
;; Revision 1.27  2017/07/12 01:32:15  ohsaki
;; *** empty log message ***
;;
;; Revision 1.26  2017/07/09 14:22:02  ohsaki
;; - Improved indentation algorithm to take account of nest level and comments.
;; - Changed to open Web pages rather than local files for help files.
;; - Preliminary support for inline LaTeX equation drawing.
;;

(require 'math-syms)

;;; keymap
(defvar math-mode-map (make-sparse-keymap))
(define-key math-mode-map "\C-i" 'math-indent-line)
(define-key math-mode-map "\M-\C-i" 'complete-math-symbol)
(define-key math-mode-map "\C-c\C-c" 'inf-math)
(define-key math-mode-map "\M-\C-m" 'math-send-cell)
(define-key math-mode-map "\M-p" 'math-send-pretty)
(define-key math-mode-map "\C-c?" 'math-help)
(define-key math-mode-map "\C-c\177" 'math-guide)

;;; syntax table
(defvar math-mode-syntax-table (make-syntax-table))

;;; varibales
(defvar math-indent-level 2)
(defvar math-reference-template "https://reference.wolfram.com/language/ref/%s.html")
(defvar math-guide-url "https://reference.wolfram.com/language/")
(defvar math-font-lock-keywords
  '(("(\\*.+?\\*)" . font-lock-comment-face)
    ("\"[^\"]*\"" . font-lock-string-face)
    ("In\\[[0-9]+\\]:=" . font-lock-builtin-face)
    ("Out\\[[0-9]+\\]=" . font-lock-comment-face)
    ("\\([A-Z][0-9A-Za-z]*\\)\\[" (1 font-lock-keyword-face))
    ("\\$[A-Z][0-9A-Za-z]*" . font-lock-variable-name-face)
    ("[A-Z][0-9A-Za-z`]*" . font-lock-constant-face)
    ("[a-z][a-z0-9]*_+" . font-lock-type-face)))

(defun math-mode ()
  "Major mode for editing Mathematica program."  
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'math-mode)
  (setq mode-name "Math")
  (use-local-map math-mode-map)
  (set-syntax-table math-mode-syntax-table)
  ;; local variables
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'math-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "(* ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\*.*\*)")
  (make-local-variable 'comment-end)
  (setq comment-end " *)")
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(math-font-lock-keywords nil nil))
  (run-hooks 'math-mode-hook))

(defun find-math-tag-at-point ()
  "Find a tag name around the current point."
  (skip-chars-forward "0-9A-Za-z`")
  (buffer-substring-no-properties
   (save-excursion
     (skip-chars-backward "0-9A-Za-z`")
     (point))
   (point)))

(defun complete-math-symbol ()
  "Complete the symbol around the point as a Mathematica symbol."
  (interactive)
  (let ((pattern (find-math-tag-at-point))
	beg)
    (or pattern
	(error "Nothing to complete"))
    (search-backward pattern)
    (setq beg (point))
    (forward-char (length pattern))
    (completion-in-region beg (point) math-symbols)))

;; indentation
(defun math-indent-line ()
  "Indent the current line as Mathematica program."
  (interactive)
  (let ((indent (math-calculate-indent)))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to indent))
    (if (< (current-column) indent)
	(skip-chars-forward " \t"))))

(defun math-up-list (arg)
  "Move forward out of one level of parentheses."
  (condition-case nil
      (progn
	(up-list arg)
	t)
    (error nil)))

(defun math-current-nest-level ()
  "Return the depth of parentheses nesting at the current point."
  (let ((level 0))
    (save-excursion
      (while (math-up-list -1)
	(setq level (1+ level))))
    level))
  
(defun math-calculate-indent ()
  "Determine the indentation level at the current line."
  (let ((level (math-current-nest-level)))
    (if (> level 0)
	;; inside the comment; always 3
	(if (save-excursion
	      (math-up-list -1)
	      (looking-at "(\\*"))
	    3
	  ;; in the nest; math-indent-level * level
	  ;; outdent if the line starts with closing parenthes
	  (if (save-excursion
		(beginning-of-line)
		(looking-at " *\\()\\|}\\|]\\)"))
	      (* math-indent-level (1- level))
	    (* math-indent-level level)))
      ;; independent line; always 0
      0)))

(defun math-start-cell ()
  "Move the point to the begining of the current cell."
  (interactive)
  (beginning-of-line)
  (let ((found))
    (while (and (not found)
		(not (bobp)))
      (forward-line -1)
      (when (looking-at "^[ \t]*$")
	(setq found t)
	(forward-line 1)))))

(defun math-end-cell ()
  "Move the point to the end of the current cell."
  (interactive)
  (beginning-of-line)
  (let ((found))
    (while (and (not found)
		(not (eobp)))
      (forward-line 1)
      (when (looking-back "^[ \t]*$" nil)
	(setq found t)
	(forward-char -1)))))

;; interaction with inferior math
(defun math-send-cell ()
  "Send the current cell around the point to the the inferior
Mathematica process."
  (interactive)
  (inf-math-send-string
   (buffer-substring-no-properties
    (save-excursion
      (math-start-cell)
      (point))
    (save-excursion
      (math-end-cell)
      (point)))))

(defun math-send-pretty ()
  "Send `pretty[%]' to the inferior Mathematica process to
graphically display the last result."
  (interactive)
  (inf-math-send-string "pretty[%]"))

(defun math-send-region (beg end)
  (interactive "r")
  (inf-math-send-string
   (buffer-substring-no-properties beg end)))

(defun math-help (str)
  "Display a short help for a Mathematica command STR in an
  inferior Mathematica.  With any prefix argument, visit the
  Mathematica reference page using external Web browser."
  (interactive
   (list (read-string 
	  (if current-prefix-arg
	      "Help (MathBook): " 
	    "Help: ")
	  (find-math-tag-at-point))))
  (if current-prefix-arg
      (let ((url (format math-reference-template str)))
	(start-process "*math-help*" nil "firefox" url))
    (inf-math-send-string (concat "?" str))))

(defun math-guide ()
  "Open the Mathematica guidebook using external Web browser."
  (interactive)
  (start-process "*math-guide*" nil "firefox" math-guide-url))

;; inferior math
(require 'comint)

(defvar inf-math-mode-map (copy-keymap comint-mode-map))
(define-key inf-math-mode-map "\M-\C-m" 'comint-send-input)
(define-key inf-math-mode-map "\M-\C-i" 'complete-math-symbol)

;; varibales
(defvar inf-math-buffer nil)
(defvar inf-math-program "WolframKernel")
(defvar inf-math-remote-host nil)

(defun inf-math-mode ()
  "Major mode for interactig with Mathematica.  You can use all
the comint commands in this buffer."
  (interactive)
  (comint-mode)
  (setq major-mode 'inf-math-mode)
  (setq mode-name "Inferior Mathematica")
  (use-local-map inf-math-mode-map)
  (set-syntax-table math-mode-syntax-table)
  ;; local variables
  (setq comint-process-echoes t)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(math-font-lock-keywords nil nil))
  (add-hook 'comint-preoutput-filter-functions 
    	    'math-preoutput-filter nil t)
  (add-hook 'comint-preoutput-filter-functions 
     	    'math-preoutput-image-filter t t)
  (make-local-variable 'comint-move-point-for-output)
  (setq comint-move-point-for-output t)
  (make-local-variable 'comint-inhibit-carriage-motion)
  (setq comint-inhibit-carriage-motion t)
  (run-hooks 'inf-math-mode-hook))

(defun inf-math ()
  "Start an inferior Mathematica process and display its buffer."
  (interactive)
  (if (buffer-live-p inf-math-buffer)
      (display-buffer inf-math-buffer)
    (setq inf-math-buffer 
	  (if inf-math-remote-host
	      (make-comint "inf-math" "ssh" nil 
			   inf-math-remote-host inf-math-program)
	    (make-comint "inf-math" inf-math-program)))
    (set-buffer inf-math-buffer)
    (inf-math-mode)
    (display-buffer inf-math-buffer)))

(defun math-preoutput-filter (str)
  "A function called via comint-preoutput-filter-functions to
preprocess the output from the Mathematica."
  (while (string-match " \\{80,\\}" str)
    (setq str (replace-match " " nil nil str)))
  ;; remove forward cursor escape sequence
   (while (string-match "\\[[0-9]+C" str)
     (setq str (replace-match "" nil nil str)))
   (while (string-match "" str)
     (setq str (replace-match "" nil nil str)))
  str)

(defun inf-math-display-image (file)
  (when (file-exists-p file)
    (clear-image-cache)
    (let ((image (create-image file)))
      (save-excursion
	(forward-char -1)
	(end-of-line)
	(insert "\n")
	(insert-image image)))))

(defun math-preoutput-image-filter (str)
  (if (string-match "<<\\([^<]+\\)>>" str)
      (let ((file (match-string 1 str)))
	(inf-math-display-image file)
	"")
    str))

(defun inf-math-send-string (str)
  "Send a string STR to the inferior Mathematica process."
  (inf-math)
  (if (region-active-p)
      (setq str (buffer-substring-no-properties (region-beginning)
						(region-end))))
  (with-current-buffer inf-math-buffer
    (goto-char (point-max))
    (insert (inf-math-strip-comment str))
    (comint-send-input)))

(defun inf-math-strip-comment (str)
  "Delete all comments from string STR."
  (while (string-match "(\\*[^\\*]+\\*)[ \t]*\n?" str)
    (setq str (replace-match "" nil nil str)))
  (while (string-match "\n[ \t]*\n" str)
    (setq str (replace-match "\n" nil nil str)))
  str)
