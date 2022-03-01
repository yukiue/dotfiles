;; -*- Emacs-Lisp -*-
;;
;; Format Japanese paragraph for easier reading/editing.
;; Copyright (C) 1997-2005 Hiroyuki Ohsaki.
;; All rights reserved.
;;
;; $Id: format-paragraph.el,v 1.1 2005/02/08 18:05:59 oosaki Exp $
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

;; usage:
;; add the following lines to your ~/.emacs.
;; (autoload 'format-paragraph "format-paragraph" nil t)
;; (global-set-key "\M-p" 'format-paragraph)

(defun format-region-as-paragraph (start end &optional no-space)
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (message "Formatting...")

    ;; concatenate all lines
    (goto-char (point-min))
    (while (not (eobp))
      (end-of-line)
      (kill-line 1)
      ;; insert an extra space not for mingling two ASCII words
      (insert " "))

    ;; delete unnecessary spaces
    (goto-char (point-min))
    (while (re-search-forward
	    "\\([!-~]\\) +\\([!-~]\\)" nil t)
      (replace-match "\\1########\\2")
      (goto-char (nth 1 (match-data 2))))
    (goto-char (point-min))
    (while (re-search-forward " +" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (search-forward "########" nil t)
      (replace-match " "))
    
    (if no-space
	nil
      ;; put space around non-Kanji words
      (goto-char (point-min))
      (while (re-search-forward "\\(\\cj\\) *\\([!-(*-,.-~]\\)" nil t)
	(if (or (string-match "\\(、\\|，\\)" (match-string 1))
		(string-match "\\(。\\|．\\)" (match-string 1)))
	    nil
	  (replace-match "\\1 \\2")))
      (goto-char (point-min))
      (while (re-search-forward "\\([!-')-,.-~]\\) *\\(\\cj\\)" nil t)
	(if (or (string-match "\\(、\\|，\\)" (match-string 2))
		(string-match "\\(。\\|．\\)" (match-string 2)))
	    nil
	  (replace-match "\\1 \\2"))))

    ;; insert newline after punctuation
    (goto-char (point-min))
    (while (not (eobp))
      (skip-chars-forward "^、。?，．")
      ;; fold a longer line than 'fill-prefix
      (while (>= (current-column) fill-column)
	(while (>= (current-column) fill-column)
	  (forward-word -1))
	;; 
 	(skip-chars-backward "0-9A-Za-z\\\\「{([:,\\-~\\$")
	(if (bolp)
	    (progn
	      (skip-chars-forward "^、。?，．")
	      (forward-char 1)))
	(newline)
	(skip-chars-forward "^、。?，．"))
      (or (eobp)
	  (forward-char 1))
      ;; check if the line is too short
      (if (or (and (not (eobp))
		   (eq (char-before) ?、)
		   (<= (current-column) (/ fill-column 4)))
	      ;; or a close paren immediately follows
	      (looking-at " *[])}」]"))
	  nil
	(newline)))

    ;; for a LaTeX document
    (if (eq major-mode 'latex-mode)
	(progn
	  ;; indent every line
	  (goto-char (point-min))
	  (while (not (eobp))
	    (indent-according-to-mode)
	    (forward-line 1))))

    (message "Formatting...done.")))

(defun format-paragraph (no-space)
  "Format japanese paragraph around the point by using
'format-region-as-paragraph." 
  (interactive "P")
  (save-excursion
    (forward-paragraph 1)
    (format-region-as-paragraph (point)
				(save-excursion
				  (backward-paragraph 1)
				  (skip-chars-forward "\n")
				  (point))
				no-space))
    
  ;; adjust cursor position
  (if (eolp)
      (forward-char 1)))