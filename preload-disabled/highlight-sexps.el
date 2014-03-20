;;; highlight-sexps.el --- highlight surrounding parentheses
;;
;; Copyright (C) 2011 David Rysdam
;;
;; Author: David Rysdam <david * rysdam org>
;; Version: 0.9.1
;; Keywords: faces, matching, s-expression, sexp
;; URL: http://david.rysdam.org/src/emacs/highlight-sexps.el
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x
;;
;; Based on highlight-parentheses:
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 1.0.1
;; Keywords: faces, matching
;; URL: http://nschum.de/src/emacs/highlight-parentheses/
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Add the following to your .emacs file:
;; (require 'highlight-sexps)
;;
;; Enable `highlight-sexps-mode'.
;;
;; Or also add one or both of the following your .emacs file:
;; (add-hook 'lisp-mode-hook 'highlight-sexps-mode)
;; (add-hook 'emacs-lisp-mode-hook 'highlight-sexps-mode)
;;
;; highlight-sexps-mode supports customization. You will likely find
;; it most useful to customize the hl-sexp-background-colors
;; variable. The number of nexted s-expressions highlighted is
;; determined by the number of colors defined.
;;
;;; Change Log:
;;
;; 2012-01-31 (0.9.1)
;;    lisp-mode namespace collision fixed (but probably not The Right
;;    Way)
;;
;; 2012-01-31 (0.9)
;;    Initial Release.
;;
;;; Code:

(eval-when-compile (require 'cl))

(provide 'highlight-sexps)

(defgroup highlight-sexps nil
  "Highlight the nested s-expressions around point"
  :group 'faces
  :group 'matching)

(defun hl-sexp-set (variable value)
  (set variable value)
  (when (fboundp 'hl-sexp-color-update)
    (hl-sexp-color-update)))

(defcustom hl-sexp-colors 
  nil
  "*List of colors for the highlighted parentheses.
The list starts with the the inside parentheses and moves
outwards."
  :type '(repeat color)
  :set 'hl-sexp-set
  :group 'highlight-sexps)

(defcustom hl-sexp-background-colors
  '("cyan" "white")
  "*List of colors for the background highlighted parentheses.
The list starts with the the inside parentheses and moves
outwards."
  :type '(repeat color)
  :set 'hl-sexp-set
  :group 'highlight-sexps)

(defface hl-sexp-face nil
  "*Face used for highlighting parentheses.
Color attributes might be overriden by `hl-sexp-colors' and
`hl-sexp-background-colors'."
  :group 'highlight-sexps)

(defvar hl-sexp-overlays nil
  "This buffers currently active overlays.")
(make-variable-buffer-local 'hl-sexp-overlays)

(defvar hl-sexp-last-point 0
  "The last point for which parentheses were highlighted.
This is used to prevent analyzing the same context over and
over.")
(make-variable-buffer-local 'hl-sexp-last-point)

(defun hl-sexp-highlight ()
  "Highlight the nested s-expressions around point"
  (unless (= (point) hl-sexp-last-point)
    (setq hl-sexp-last-point (point))
    (let ((overlays hl-sexp-overlays)
		  (sexp-list (hl-sexp-end-points (point) 
										 (length hl-sexp-overlays)))
          pos1 pos2)
	  (condition-case err
		  (while (and overlays sexp-list)
			(let* ((overlay (pop overlays))
				   (sexp (pop sexp-list))
				   (pos1 (car sexp))
				   (pos2 (cadr sexp)))
			  (move-overlay overlay pos1 pos2)))
		(error nil))
      (dolist (ov overlays)
        (move-overlay ov 1 1)))))

;;;###autoload
(define-minor-mode highlight-sexps-mode
  "Minor mode to highlight an expanding set of surrounding s-expressions."
  nil " hl-s" nil
  (if highlight-sexps-mode
      (progn
        (hl-sexp-create-overlays)
        (add-hook 'post-command-hook 'hl-sexp-highlight nil t))
    (mapc 'delete-overlay hl-sexp-overlays)
    (kill-local-variable 'hl-sexp-overlays)
	(kill-local-variable 'hl-sexp-last-point)
    (remove-hook 'post-command-hook 'hl-sexp-highlight t)))

(defun hl-sexp-create-overlays ()
  "Create some 'overlays-in-waiting' with the right priorities and attributes."
  (let* ((fg hl-sexp-colors)
		 (bg hl-sexp-background-colors)
		 (count (max (length fg) (length bg)))
		 (num count)
		 attributes)
    (while (> num 0)
      (setq attributes (face-attr-construct 'hl-sexp-face))
      (when (car fg)
        (setq attributes (plist-put attributes :foreground (car fg))))
      (pop fg)
      (when (car bg)
        (setq attributes (plist-put attributes :background (car bg))))
      (pop bg)
	  (push (make-overlay 0 0) hl-sexp-overlays)
	  (overlay-put (car hl-sexp-overlays) 'face attributes)
	  (overlay-put (car hl-sexp-overlays) 'priority num)
	  (decf num))
	(setq hl-sexp-overlays (nreverse hl-sexp-overlays))))

(defun hl-sexp-color-update ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when hl-sexp-overlays
        (mapc 'delete-overlay hl-sexp-overlays)
        (setq hl-sexp-overlays nil)
        (hl-sexp-create-overlays)
        (let ((hl-sexp-last-point -1)) ;; force update
          (hl-sexp-highlight))))))

(defun hl-sexp-start-of-sexp (pt)
  "Start of the s-expression surrounding PT."
  (save-excursion (cadr (syntax-ppss pt))))

(defun hl-sexp-end-of-sexp (pt)
  "End of s-expression that matches beginning point PT."
  (condition-case nil
	  (scan-sexps pt 1)
	(error nil)))
  
(defun hl-sexp-end-points (pt n)
  "Get beginning and ending points of N depths of s-expressions
surrounding PT."
  (let (results prev next
		(p pt))
	(dotimes (i n (nreverse results))
	  (setq prev (hl-sexp-start-of-sexp p))
	  (when prev
		(setq next (hl-sexp-end-of-sexp prev))
		(when next
		  (push (list prev next) results)
		  (setq p (1- prev)))))))

;;; highlight-sexps.el ends here
