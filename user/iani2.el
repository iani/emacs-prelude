
(load-theme 'solarized-dark)

(set-fontset-font "fontset-default"
                  'japanese-jisx0208
                  '("Hiragino Mincho Pro" . "iso10646-1"))
(set-fontset-font "fontset-default"
                  'greek
 ;; Note: iso10646-1 = Universal Character set (UCS)
 ;; It is compatible to Unicode, in its basic range
                  '("Menlo" . "iso10646-1"))

(maximize-frame) ;; maximize frame on startup
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
(tool-bar-mode -1)

(require 'bookmark+)
(require 'ido)
(require 'imenu+)
(ido-mode t)
(yas-global-mode)

;;; ido-imenu
(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols
          (symbol-list)
          (when (listp symbol-list)
            (dolist (symbol symbol-list)
              (let ((name nil) (position nil))
                (cond
                 ((and (listp symbol) (imenu--subalist-p symbol))
                  (addsymbols symbol))

                 ((listp symbol)
                  (setq name (car symbol))
                  (setq position (cdr symbol)))

                 ((stringp symbol)
                  (setq name symbol)
                  (setq position
                        (get-text-property 1 'org-imenu-marker symbol))))

                (unless (or (null position) (null name))
                  (add-to-list 'symbol-names name)
                  (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols
                (delq nil (mapcar (lambda (symbol)
                                    (if (string-match regexp symbol) symbol))
                                  symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc
             (lambda (symbol)
               (setq symbol-names (cons symbol (delete symbol symbol-names))))
             matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

;; Push mark when using ido-imenu

(defvar push-mark-before-goto-char nil)

(defadvice goto-char (before push-mark-first activate)
  (when push-mark-before-goto-char
    (push-mark)))

(defun ido-imenu-push-mark ()
  (interactive)
  (let ((push-mark-before-goto-char t))
    (ido-imenu)))

;; Smex: Autocomplete meta-x command
(global-set-key [(meta x)]
                (lambda ()
                  (interactive)
                  (or (boundp 'smex-cache)
                      (smex-initialize))
                  (global-set-key [(meta x)] 'smex)
                  (smex)))

(global-set-key [(shift meta x)]
                (lambda ()
                  (interactive)
                  (or (boundp 'smex-cache)
                      (smex-initialize))
                  (global-set-key [(shift meta x)] 'smex-major-mode-commands)
                  (smex-major-mode-commands)))

(require 'multiple-cursors)
(global-set-key (kbd "C-c m") 'helm-mini)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(defun turn-off-whitespace-mode () (whitespace-mode -1))
(defun turn-on-whitespace-mode () (whitespace-mode 1))

(require 'key-chord)
(key-chord-mode 1)

(defun paren-sexp ()
  (interactive)
  (insert "(")
  (forward-sexp)
  (insert ")"))

(defun code-quote-sexp ()
  (interactive)
  (insert "=")
  (forward-sexp)
  (insert "="))

(key-chord-define-global "jk"     'ace-jump-char-mode)
(key-chord-define-global "jj"     'ace-jump-word-mode)
(key-chord-define-global "jl"     'ace-jump-line-mode)

(key-chord-define-global "hj"     'undo)

(key-chord-define-global "{}"     "{   }\C-b\C-b\C-b")
(key-chord-define-global "()"     'paren-sexp)
(key-chord-define-global "(_"     "()\C-b")
(key-chord-define-global "=="     'code-quote-sexp)

;; Exit auto-complete, keeping the current selection,
;; while avoiding possible side-effects of TAB or RETURN.
(key-chord-define-global "KK"      "\C-f\C-b")
;; Trick for triggering yasnippet when using in tandem with auto-complete:
;; Move forward once to get out of auto-complete, then backward once to
;; end of keyword, and enter tab to trigger yasnippet.
(key-chord-define-global "KL"      "\C-f\C-b\C-i")

;; Jump to any symbol in buffer using ido-imenu
(key-chord-define-global "KJ"      'ido-imenu)

;;(require 'hl-sexp)
(require 'highlight-sexps)
;; Include color customization for dark color theme here.
(custom-set-variables
 '(hl-sexp-background-colors (quote ("gray0"  "#0f003f"))))

;; (require 'dired+)

(define-key dired-mode-map (kbd "<SPC>")
  (lambda () (interactive)
    (let ((lawlist-filename (dired-get-file-for-visit)))
      (if (equal (file-name-extension lawlist-filename) "pdf")
          (start-process "default-pdf-app" nil "open" lawlist-filename)))))

(load "dired-x")

(eval-after-load "dired"
'(progn
   (define-key dired-mode-map "F" 'my-dired-find-file)
   (defun my-dired-find-file (&optional arg)
     "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
     (interactive "P")
     (let* ((fn-list (dired-get-marked-files nil arg)))
       (mapc 'find-file fn-list)))))

;;; Directory of SuperCollider support, for quarks, plugins, help etc.
(defvar sc_userAppSupportDir
  (expand-file-name "~/Library/Application Support/SuperCollider"))

;; Make path of sclang executable available to emacs shell load path
(add-to-list
 'exec-path
 "/Applications/SuperCollider/SuperCollider.app/Contents/Resources/")

;; Global keyboard shortcut for starting sclang
(global-set-key (kbd "C-c M-s") 'sclang-start)
;; overrides alt-meta switch command
(global-set-key (kbd "C-c W") 'sclang-switch-to-workspace)

;; Disable switching to default SuperCollider Workspace when recompiling SClang
(setq sclang-show-workspace-on-startup nil)

;; Save results of sc evaluation in elisp variable for access in emacs
(defvar sclang-return-string  nil
  "The string returned by sclang process after evaluating expressions.")

(defadvice sclang-process-filter (before provide-sclang-eval-results)
  "Pass sc eval return string to elisp by setting sclang-return-string variable."
  (setq sclang-return-string (ad-get-arg 1)))

(ad-activate 'sclang-process-filter)

(require 'sclang)

;; paredit mode breaks re-starting sclang! Therefore, do not use it.
;; Note: Paredit-style bracket movement commands d, u, f, b, n, p work
;; in sclang-mode without loading Paredit.
;; (add-hook 'sclang-mode-hook 'paredit-mode)
(add-hook 'sclang-mode-hook 'rainbow-delimiters-mode)
(add-hook 'sclang-mode-hook 'highlight-sexps-mode)
(add-hook 'sclang-mode-hook 'sclang-ac-mode)
;; Following possibly breaks auto-complete in my setup:  Disabled for now.
;; (add-hook 'sclang-mode-hook 'sclang-extensions-mode)

;; Global keyboard shortcut for starting sclang
(global-set-key (kbd "C-c M-s") 'sclang-start)
;; Show workspace
(global-set-key (kbd "C-c C-M-w") 'sclang-switch-to-workspace)

;; (add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-sexps-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-whitespace-mode)

(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'turn-off-whitespace-mode)

(setq org-startup-indented t) ;; auto-indent text in subtrees
(setq org-hide-leading-stars t) ;; hide leading stars in subtree headings
(setq org-src-fontify-natively t) ;; colorize source blocks natively

(defun log (topic)
  "Write countdown file for countdown geeklet.
  Ask user number of seconds to plan countdown in future."
  (interactive "MEnter task topic: ")
  (if (< (length topic) 1) (setq topic "Untitled task"))
  (let ((timer-string
         (concat
          (replace-regexp-in-string " " "_" topic)
          (format-time-string ": %D_%T" (current-time)))))
    (find-file
     "/Users/iani2/Dropbox/000WORKFILES/org/monitoring/stopwatch.txt")
    (beginning-of-buffer)
    (kill-line)
    (insert-string timer-string)
    (save-buffer)
    (message (concat "Now timing: " timer-string))
    (find-file
     "/Users/iani2/Dropbox/000WORKFILES/org/monitoring/log.org")
    (widen)
    (end-of-buffer)
    (insert-string "-")
    (org-insert-time-stamp (current-time) t)
    (beginning-of-line)
    (kill-line)
    (if (> (org-outline-level) 1) (outline-up-heading 100 t))
    (org-set-property
     "END_TIME"
     (replace-regexp-in-string
      ">" "]"
      (replace-regexp-in-string "<" "[" org-last-inserted-timestamp)))
    (org-set-property
     "TIMER_SPAN"
     (concat
      (replace-regexp-in-string
       ">" "]"
       (replace-regexp-in-string "<" "[" (org-entry-get (point) "START_TIME")))
      "--"
      (org-entry-get (point) "END_TIME")))
    (let* ((seconds
            (-
             (org-float-time
              (apply
               'encode-time
               (org-parse-time-string (org-entry-get (point) "END_TIME"))))
             (org-float-time
              (apply
               'encode-time
               (org-parse-time-string (org-entry-get (point) "START_TIME"))))
             ))
           (hours (floor (/ seconds 3600)))
           (seconds (- seconds (* 3600 hours)))
           (minutes (floor (/ seconds 60))))
      (org-set-property
       "DURATION"
       (replace-regexp-in-string " " "0" (format "%2d:%2d" hours minutes))))
    (end-of-buffer)
    (insert-string "\n* ")
    (insert-string (replace-regexp-in-string "_" " " timer-string))
    ;;      (insert-string "\n")
    (org-set-property "START_TIME" org-last-inserted-timestamp)
    (org-id-get-create)
    (org-set-tags-command)
;;    (if narrow-p
    (org-narrow-to-subtree)
    (goto-char (point-max))
    (org-show-subtree)
    (org-show-entry)
    (save-buffer)
;;    )
    ))

(global-set-key (kbd "C-M-l") 'log)
