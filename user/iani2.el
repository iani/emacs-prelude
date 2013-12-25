
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

(desktop-save-mode 1)

(require 'bookmark+)
(require 'ido)
(require 'imenu+)
(require 'auto-complete)
(ido-mode t)
(icicle-mode)
;; (yas-global-mode)

(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)

(require 'windmove)
(global-set-key (kbd "<C-s-up>") 'windmove-up)
(global-set-key (kbd "<C-s-down>") 'windmove-down)
(global-set-key (kbd "<C-s-right>") 'windmove-right)
(global-set-key (kbd "<C-s-left>") 'windmove-left)

(require 'buffer-move)
(global-set-key (kbd "<S-prior>") 'buf-move-up)
(global-set-key (kbd "<S-next>") 'buf-move-down)
(global-set-key (kbd "<S-end>") 'buf-move-right)
(global-set-key (kbd "<S-home>") 'buf-move-left)

(global-set-key (kbd "<s-home>") 'previous-buffer)
(global-set-key (kbd "<s-end>") 'next-buffer)

(require 'lacarte)
(global-set-key [?\e ?\M-x] 'lacarte-execute-command)

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

(require 'dired+)

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

(defun open-finder ()
  (interactive)
  ;; IZ Dec 25, 2013 (3:25 PM): Making this work in dired:
  (if (equal major-mode 'dired-mode)
      (open-finder-dired)
      (let ((path
             (if (equal major-mode 'dired-mode)
                 (file-truename (dired-file-name-at-point))
               (buffer-file-name)))
            dir file)
        (when path
          (setq dir (file-name-directory path))
          (setq file (file-name-nondirectory path))
          (open-finder-1 dir file)))))

(defun open-finder-1 (dir file)
  (message "open-finder-1 dir: %s\nfile: %s" dir file)
  (let ((script
         (if file
             (concat
              "tell application \"Finder\"\n"
              " set frontmost to true\n"
              " make new Finder window to (POSIX file \"" dir "\")\n"
              " select file \"" file "\"\n"
              "end tell\n")
           (concat
            "tell application \"Finder\"\n"
            " set frontmost to true\n"
            " make new Finder window to {path to desktop folder}\n"
            "end tell\n"))))
    (start-process "osascript-getinfo" nil "osascript" "-e" script)))

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
(add-hook 'sclang-mode-hook 'hl-sexp-mode)
(add-hook 'sclang-mode-hook 'sclang-ac-mode)
;; Following possibly breaks auto-complete in my setup:  Disabled for now.
;; (add-hook 'sclang-mode-hook 'sclang-extensions-mode)

;; Global keyboard shortcut for starting sclang
(global-set-key (kbd "C-c M-s") 'sclang-start)
;; Show workspace
(global-set-key (kbd "C-c C-M-w") 'sclang-switch-to-workspace)

;; (add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)
(add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-whitespace-mode)
(add-hook 'emacs-lisp-mode-hook 'auto-complete-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'turn-off-whitespace-mode)
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(setq org-startup-indented t) ;; auto-indent text in subtrees
(setq org-hide-leading-stars t) ;; hide leading stars in subtree headings
(setq org-src-fontify-natively t) ;; colorize source-code blocks natively

(require 'calfw)

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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (ruby . t)
   (python . t)
   (perl . t)
   ))

;;; Load latex package
(require 'ox-latex)

;;; Use xelatex instead of pdflatex, for support of multilingual fonts (Greek etc.)
(setq org-latex-pdf-process (list "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f"))

;;; Add beamer to available latex classes, for slide-presentaton format
(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass\[presentation\]\{beamer\}"
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

;;; Add memoir class (experimental)
(add-to-list 'org-latex-classes
             '("memoir"
               "\\documentclass[12pt,a4paper,article]{memoir}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)

(require 'ox-reveal)

(fset 'org-toggle-drawer
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([67108896 3 16 14 tab 24 24] 0 "%d")) arg)))

(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c M-d") 'org-toggle-drawer))

(defun org-cycle-current-entry ()
  "toggle visibility of current entry from within the etnry."
  (interactive)
  (save-excursion)
  (outline-back-to-heading)
  (org-cycle))

(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c C-/") 'org-cycle-current-entry))

(defun org-select-heading ()
  "Go to heading of current node, select heading."
  (interactive)
  (outline-previous-heading)
  (search-forward (plist-get (cadr (org-element-at-point)) :raw-value))
  (set-mark (point))
  (beginning-of-line)
  (search-forward " "))

(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c C-h") 'org-select-heading))

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)

(add-hook 'org-mode-hook
          (lambda () (imenu-add-to-menubar "Imenu")))
(setq org-imenu-depth 3)

(defun org-icicle-occur ()
  "In org-mode, show entire buffer contents before running icicle-occur.
Otherwise icicle-occur will not place cursor at found location,
if the location is hidden."
  (interactive)
  (show-all)
  (icicle-occur (point-min) (point-max)))

(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c C-'") 'org-icicle-occur))

(defun org-icicle-imenu ()
  "In org-mode, show entire buffer contents before running icicle-imenu.
Otherwise icicle-occur will not place cursor at found location,
if the location is hidden."
  (interactive)
  (show-all)
  (icicle-imenu (point-min) (point-max) t))

(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c C-=") 'org-icicle-imenu))

(setq org-outline-path-complete-in-steps nil)

(defun org-refile-icy (as-subtree)
  "Alternative to org-refile using icicles.
Icicles seems to break org-modes headline buffer display, so one
has to use icicles for all headline navigation if it is loaded.
If quit with C-g, this function will have removed the section that
is to be refiled.  To get it back, one has to undo, or paste."
  (interactive "P")
  (outline-back-to-heading)
  (org-cut-subtree)
  (show-all)
  (icicle-imenu (point-min) (point-max) t)
  (outline-next-heading)
  (unless (eq (current-column) 0) (insert "\n"))
  (org-paste-subtree)
  (if as-subtree (org-demote-subtree )))

;; Using key binding for org-reveal, since org-reveal
;; does nothing for me.
(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c C-R") 'org-refile-icy))

(setq magit-repo-dirs
      '(
        "~/Dropbox/000WORKFILES/org"
        "~/Documents/Dev"
        "~/.emacs.d/personal"
))
