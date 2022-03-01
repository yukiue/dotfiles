;; -*- Emacs-Lisp -*-
;; 
;; A Major-mode for editing and viewing annotations for an image file.
;; Copyright (c) 2018-2019, Hiroyuki Ohsaki.
;; All rights reserved.
;; 
;; $Id: doc-annotate.el,v 1.25 2019/11/09 14:47:58 ohsaki Exp ohsaki $
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

;; Prerequisites:
;;   Emacs, sh, bc, and ImageMagick.
;;
;; Installation:
;;
;; Add these lines to your ~/.emacs.
;;
;; (setq doc-view-scale-internally nil)
;; (add-hook 'doc-view-mode-hook
;;   	  '(lambda ()
;; 	     (local-set-key "c" 'doc-annotate-add-annotation)
;; 	     (local-set-key [mouse-1] 'doc-annotate-add-annotation)))
;; (autoload 'doc-annotate-mode "doc-annotate")
;; (autoload 'doc-annotate-add-annotation "doc-annotate")
;; (add-to-list 'auto-mode-alist '("\\.ant$" . doc-annotate-mode))
;;
;; Download a shell script `doc-annotate-clip' from
;; http://www.lsnl.jp/~ohsaki/software/elisp/doc-annotate-clip, and save it as
;; /usr/local/bin/doc-annotate-clip (or anywhere in your search path).
;; Do not foget to run `chmod +x' to make it executable.

;; Usage:
;;
;; 1. Add the above lines to your ~/.emacs, and restart the Emacs (or
;; reload ~/.emacs with `eval-region').
;; 
;; 2. Open an image file (e.g., paper.pdf) with Emacs.  With recent
;; Emacsen, image files (PDF, PS, and DVI files) are loaded in the
;; DocView mode.  Check the document of the DocView mode for its basic
;; operations.
;;
;; 3. In the DocView mode, move the mouse cursor, and type the 'c' key
;; or click the left mouse button at which you want to record an
;; annotation.  An annotation file corresponding to the image file
;; (e.g., paper.ant) is opened (or newly created if it does not exist)
;; and it pops up in the lower window.  Insert you comment at the
;; point.
;;
;; 4. When you are visiting an annotation file, you can use the
;; following commands.
;;  - Navigate annotations
;;   - doc-annotate-next-annotation (M-p)
;;   - doc-annotate-previous-annotation (M-n)
;; - Display annotation at the point
;;   - doc-annotate-visit-current-annotation (C-cC-c)
;;   - doc-annotate-display-current-clip (C-cC-i)
;; - Close annotation at the point
;;   - doc-annotate-record-done (C-cC-t)

(require 'doc-view)

;;; keymap
(defvar doc-annotate-mode-map (make-sparse-keymap))
(define-key doc-annotate-mode-map "\C-c\C-c" 'doc-annotate-visit-current-annotation)
(define-key doc-annotate-mode-map "\C-c\C-i" 'doc-annotate-display-current-clip)
(define-key doc-annotate-mode-map "\M-n" 'doc-annotate-next-annotation)
(define-key doc-annotate-mode-map "\M-p" 'doc-annotate-previous-annotation)
(define-key doc-annotate-mode-map "\C-c\C-t" 'doc-annotate-record-done)

(defvar doc-annotate-font-lock-keywords
      '(("^ *#.*$" . font-lock-comment-face)
	("DONE" . font-lock-keyword-face)
	("\\[[0-9/]+\\]" . font-lock-string-face)
	("(\\?)" . font-lock-warning-face)))

(defvar-local doc-annotate-image-buffer nil)

(defun doc-annotate-mode ()
  "Major mode for editing and viewing annotations for an image file.

DocAnnotate mode enables to write annotations to any document
(PDF, PS, and DVI files) readable with the DocView mode.

You can use
\\<doc-annotate-mode-map>\\[doc-annotate-visit-current-annotation]
to display the corresponing image in the other window at which
the annotation at the point (i.e., cursor) is recorded.

\\{doc-annotate-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'doc-annotation-mode)
  (setq mode-name "DocAnnotate")
  (use-local-map doc-annotate-mode-map)
  ;; local variables
  (setq doc-annotate-image-buffer
	(find-file-noselect (doc-annotate-image-file))) ;load accompanying image file
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-start-skip)
  (setq comment-end "")
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(doc-annotate-font-lock-keywords nil nil))
  (font-lock-mode 1)
  (run-hooks 'doc-annotation-mode-hook))

; ----------------------------------------------------------------
(defun doc-annotate-image-file ()
  "Return the filename of a corresponding image file."
  (let (file found)
    ;; FIXME: image file type should be configurable
    (dolist (ext '(".pdf" ".ps" ".dvi"))
      (setq file (concat (file-name-sans-extension (buffer-file-name)) ext))
      (if (and (not found)
	       (file-exists-p file))
	  (setq found file)))
    found))

(defun doc-annotate-annotation-file ()
  "Return the filename of corresponding ANT file."
  (concat (file-name-sans-extension (buffer-file-name)) ".ant"))

(defun doc-annotate-convert-to-relative (pos)
  "Convert pixel geometry POS to relative image geometry.  POS
is a cons cell representing (X . Y)."
  (let* ((x (car pos))
	 (y (cdr pos))
	 (edges (window-inside-pixel-edges))
	 (win-width (- (nth 2 edges) (nth 0 edges)))
	 (win-height (- (nth 3 edges) (nth 1 edges)))
	 (imgsize (image-size (doc-view-current-image) t))
	 (img-width (car imgsize))
	 (img-height (cdr imgsize))
	 (hscroll (image-mode-window-get 'hscroll))
	 (vscroll (image-mode-window-get 'vscroll))
	 (xoffset (/ (- win-width img-width) 2))
	 (slice (doc-view-current-slice))
	 rx ry)
    ;; convert frame coordinate (pixel) to window coordinate (pixel)
    (setq x (- x (nth 0 edges)))
    (setq y (- y (nth 1 edges)))
    ;; convert window coordinate (pixel) to image coordinate (pixel)
    (when hscroll
      (setq x (+ x (* hscroll (frame-char-width)))))
    (when vscroll
      (setq y (+ y (* vscroll (frame-char-height)))))
    (when (> xoffset 0)
      ;; if the image is narrower than the window, adjust the origin
      (setq x (- x xoffset)))
    ;; convert image coordinate (pixel) to relative image coordinate (fraction)
    (if slice
	(let ((x1 (nth 0 slice))
	      (y1 (nth 1 slice))
	      (x2 (nth 2 slice))
	      (y2 (nth 3 slice)))
	  (setq rx (/ (+ x1 (float x)) img-width))
	  (setq ry (/ (+ y1 (float y)) img-height)))
      (progn
	(setq rx (/ (float x) img-width))
	(setq ry (/ (float y) img-height))))
    (cons rx ry)))

(defun doc-annotate-convert-to-absolute (relpos)
  "Convert relative image geometry RELPOS to pixel geometry.  POS
is a cons cell representing (RX . RY)."
  (let* ((rx (car relpos))
	 (ry (cdr relpos))
	 (edges (window-inside-pixel-edges))
	 (win-width (- (nth 2 edges) (nth 0 edges)))
	 (win-height (- (nth 3 edges) (nth 1 edges)))
	 (imgsize (image-size (doc-view-current-image) t))
	 (img-width (car imgsize))
	 (img-height (cdr imgsize))
	 (hscroll (image-mode-window-get 'hscroll))
	 (vscroll (image-mode-window-get 'vscroll))
	 (xoffset (/ (- win-width img-width) 2))
	 (slice (doc-view-current-slice))
	 x y)
    ;; convert relative image coordinate (fraction) to image coordinate (pixel)
    (setq x (* (float rx) img-width))
    (setq y (* (float ry) img-height))
    ;; convert image coordinate (pixel) to window coordinate (pixel)
    (if slice
	(let ((x1 (nth 0 slice))
	      (y1 (nth 1 slice))
	      (x2 (nth 2 slice))
	      (y2 (nth 3 slice)))
	  (setq x (- x x1))
	  (setq y (- y y1))))
    (when (> xoffset 0)
      (setq x (+ x xoffset)))
    (when hscroll
      (setq x (- x (* hscroll (frame-char-width)))))
    (when vscroll
      (setq y (- y (* vscroll (frame-char-height)))))
    ;; convert window coordinate (pixel) to frame coordinate (pixel)
    (setq x (+ x (nth 0 edges)))
    (setq y (+ y (nth 1 edges)))
    (cons (round x) (round y))))

; ----------------------------------------------------------------
(defun doc-annotate-display-buffer ()
  "Display the annotation buffer (current buffer) and its
  corresponding image buffer."
  (interactive)
  (let* ((ant-buf (current-buffer))
	 (img-buf doc-annotate-image-buffer))
    (delete-other-windows)
    ;(split-window-vertically -16)
    (split-window-horizontally -80)
    (switch-to-buffer img-buf)
    (switch-to-buffer-other-window ant-buf)))
  
(defun doc-annotate-current-annotation ()
  "Return a list describing the annotation at the point; the
  point at which the annotation starts, the page number, and the
  geometry."
  (save-excursion
    (end-of-line)
    (when (re-search-backward "^#\\+annotation: *\\([0-9.]+\\) +\\([0-9.]+\\) +\\([0-9.]+\\)" nil t)
      (let* ((page (string-to-number (match-string 1)))
	     (x (string-to-number (match-string 2)))
	     (y (string-to-number (match-string 3))))
	(beginning-of-line)
	(list (point) page x y)))))

(defun doc-annotate-next-annotation ()
  "Move to the next annotation."
  (interactive)
  (let ((pnt))
    (save-excursion
      (forward-line 1)
      (when (re-search-forward "^#\\+annotation:" nil t)
	(beginning-of-line)
	(setq pnt (point))))
    (when pnt
      (goto-char pnt)
      (doc-annotate-visit-current-annotation))))

(defun doc-annotate-previous-annotation (&optional count)
  "Move to the previous annotation."
  (interactive)
  (let ((pnt))
    (save-excursion
      (when (re-search-backward "^#\\+annotation:" nil t)
	(setq pnt (point))))
    (when pnt
      (goto-char pnt)
      (doc-annotate-visit-current-annotation))))

(defun doc-annotate-visit-current-annotation ()
  "Parse the annotation at the point and display the
  corresponding image in other window."
  (interactive)
  (let* ((annot (doc-annotate-current-annotation))
	 (pnt (nth 0 annot))
	 (page (nth 1 annot))
	 (rx (nth 2 annot))
	 (ry (nth 3 annot))
	 pos)
    ;; display the image in another window
    (doc-annotate-display-buffer)
    (other-window 1)
    (doc-view-goto-page page)
    (doc-view-fit-width-to-window)
    (image-bob)
    ;; scroll the image so that the annotation is visible
    (let* ((edges (window-inside-pixel-edges)))
      (setq pos (doc-annotate-convert-to-absolute (cons rx ry)))
      (while (> (cdr pos) (nth 3 edges))
	(image-scroll-up)
	(setq pos (doc-annotate-convert-to-absolute (cons rx ry)))))
    ;; redisplay the window
    (sit-for 0)
    (other-window 1)
    ;; FIXME: mouse color sould change temporarily
    (set-frame-parameter (window-frame) 'mouse-color "red")
    (set-mouse-pixel-position (window-frame) (car pos) (cdr pos))))

(defun doc-annotate-display-current-clip ()
  "Display an inline image showing where the annotation at the
point is recorded.  The exact location of the annotation is
pointed by the small red rectangular.  This function executes an
external program `doc-annotate-clip'."
  (interactive)
  (let* ((annot (doc-annotate-current-annotation))
	 (pnt (nth 0 annot))
	 (page (nth 1 annot))
	 (x (nth 2 annot))
	 (y (nth 3 annot))
	 ;; NOTE: the following code depends on how DocView mode works
	 ;; internally --- PNG files are stored as page-<n>.png in the
	 ;; cache directory.
	 (cachedir (with-current-buffer doc-annotate-image-buffer
		     (doc-view--current-cache-dir)))
	 (pngfile (format "%spage-%d.png" cachedir page))
	 (clipfile (format "%s/clip-%d.png" cachedir pnt)))
    (call-process "doc-annotate-clip" nil nil nil 
		  pngfile (number-to-string x) (number-to-string y) clipfile)
    ;; FIXME: probhibit duplicate displays
    ;; FIXME: support clip removal
    (put-image (create-image clipfile)
	       (save-excursion
		 (goto-char pnt)
		 (forward-line -1)
		 (point)))))

(defun doc-annotate-record-done ()
  "Record that the annotation at the point has beeen processed.
Leave a tag (DONE) followed by your login name and the curre time."
  (interactive)
  (let ((lst (doc-annotate-current-annotation)))
    (goto-char (nth 0 lst))
    (forward-line 1)
    (insert (format "DONE -%s [%s]\n"
		    (user-login-name)
		    (format-time-string "%Y/%m/%d")))
    (search-backward (concat "-" (user-login-name)))))

; ----------------------------------------------------------------
(defun doc-annotate-add-annotation ()
  "Open (or create) the corresponding annotation file, and add a
  new annotation for the image under the mouse pointer."
  (interactive)
  (let* ((img-buf (current-buffer))
	 (page (doc-view-current-page))
	 (relpos (doc-annotate-convert-to-relative
		  (cdr (mouse-pixel-position))))
	 (ant-buf (find-file-noselect (doc-annotate-annotation-file))))
    (switch-to-buffer ant-buf)
    (doc-annotate-display-buffer)
    ;; FIXME: keep annotations sorted
    (goto-char (point-max))
    ;; delete space at the bottom
    (delete-region (point)
		   (save-excursion
		     (skip-chars-backward " \t\n")
		     (point)))
    (insert (format "\n\n#+annotation: %d %.3f %.3f -%s [%s]\n" 
		    page (car relpos) (cdr relpos)
		    (user-login-name)
		    (format-time-string "%Y/%m/%d")))))
