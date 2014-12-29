
(require 'moe-theme)
(moe-dark)

(add-to-list 'default-frame-alist
             '(font . "Anonymous Pro-14"))

(defun larger-frame-font ()
  (interactive)
  (set-face-attribute
   'default nil
   :height
   (+ (face-attribute 'default :height) 10)) )

(defun smaller-frame-font ()
  (interactive)
  (set-face-attribute
   'default nil
   :height
   (- (face-attribute 'default :height) 10)) )

(global-set-key (kbd "C-c C--") 'smaller-frame-font)

(global-set-key (kbd "C-c C-+") 'larger-frame-font)

(guru-mode -1)
(guru-global-mode -1)
(setq prelude-guru nil)
(add-hook 'prelude-prog-mode-hook
          (lambda ()
            (guru-mode -1)) t)

(global-set-key (kbd "C-c p C-c") 'prelude-copy-file-name-to-clipboard)

(setq visible-bell t)

(blink-cursor-mode 1)

(set-fontset-font "fontset-default"
                  'japanese-jisx0208
                  '("Hiragino Mincho Pro" . "iso10646-1"))
(set-fontset-font "fontset-default"
                  'greek
 ;; Note: iso10646-1 = Universal Character set (UCS)
 ;; It is compatible to Unicode, in its basic range
                  '("Menlo" . "iso10646-1"))

;; (maximize-frame) ;; maximize frame on startup
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(tool-bar-mode -1)

(global-set-key (kbd "H-f") 'toggle-fullscreen)

(require 'maxframe) ;; (maximize-frame) command/function

(global-set-key (kbd "H-h v") 'visual-line-mode)

(global-set-key (kbd "M-B") 'backward-sentence)
(global-set-key (kbd "M-F") 'forward-sentence)
(global-set-key (kbd "M-[") 'backward-sentence)
(global-set-key (kbd "M-]") 'forward-sentence)

(defun insert-timestamp (&optional short-type)
  "Insert a timestamp."
  (interactive "P")
  (if short-type
      (insert
       (let ((date (calendar-current-date)))
         (format "%s. %s. %s"
                 (nth 1 date)
                 (nth 0 date)
                 (nth 2 date))))
   (insert (format-time-string "%a, %b %e %Y, %R %Z"))))

(global-set-key (kbd "C-c C-x t") 'insert-timestamp)

(require 'org-drill)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

(require 'dash)

(require 'breadcrumb)

;; (global-set-key [(shift space)]         'bc-set)              ;; Shift-SPACE for set bookmark
(global-set-key (kbd "S-SPC")            'bc-set) ;; Shift-SPACE for set bookmark
(global-set-key [(meta j)]              'bc-previous)       ;; M-j for jump to previous
(global-set-key [(shift meta j)]        'bc-next)           ;; Shift-M-j for jump to next
(global-set-key [(meta up)]             'bc-local-previous) ;; M-up-arrow for local previous
(global-set-key [(meta down)]           'bc-local-next)     ;; M-down-arrow for local next
(global-set-key [(control c)(j)]        'bc-goto-current)   ;; C-c j for jump to current bookmark
(global-set-key [(control x)(meta j)]   'bc-list)           ;; C-x M-j for the bookmark menu list

(require 'desktop)
(require 'bookmark+)

(setq bookmark-default-file
      "~/.emacs.d/personal/bookmarks/default-bookmarks.bmk")

(defun bookmark-save-named (&optional name)
  "mod of bookmark-save to save bookmark under name
in one default directory in users prelude folder."
  (interactive "Mbookmark filename: ~/.emacs.d/personal/bookmarks/: ")
  (let ((path
         (file-truename
          (concat
           "~/.emacs.d/personal/bookmarks/"
           (replace-regexp-in-string "/" "_" name)
           ".bmk"))))
    (setq bmkp-current-bookmark-file path)
    (bookmark-save)))

(global-set-key (kbd "C-x r C-s") 'bookmark-save-named)

(defun bmkp-desktop-save-named (&optional name)
  "mod of bmkp-desktop-save to save desktop bookmark under name
in under one default directory in users prelude folder."
  (interactive "MSave desktop ~/.emacs/personal/bookmarks/desktops/?: ")
  (let ((path
         (file-truename
          (concat
           "~/.emacs.d/personal/bookmarks/desktops/"
           (replace-regexp-in-string "/" "_" name)
           ".desktop"))))
    (bmkp-desktop-save path)
    (let ((bookmark-make-record-function
           (lexical-let ((df path))
             (lambda () (bmkp-make-desktop-record df))))
          (current-prefix-arg 99)) ; Use all bookmarks for completion, for `bookmark-set'.
      (bookmark-set name))))

(defun bmkp-load-auto-saved-desktop ()
  (interactive)
;;  (bookmark-bmenu-list) ;; needed to update list if never loaded
  (bmkp-desktop-jump "auto-save-desktop"))

(add-hook 'kill-emacs-hook
          (lambda () (bmkp-desktop-save-named "auto-save-desktop")))

(global-set-key (kbd "C-x r C-k") 'bmkp-desktop-save-named)
(global-set-key (kbd "C-x p r") 'bookmark-rename)
(define-key bookmark-bmenu-mode-map "r" 'bookmark-rename)
(global-set-key (kbd "C-x j M-k") 'bmkp-load-auto-saved-desktop)

(bookmark-bmenu-list) ;; make sure bookmark list is loaded

(require 'ido)
(require 'flx-ido)
(require 'imenu+)
(require 'auto-complete)
(ido-mode t)
(ido-vertical-mode t)
(icicle-mode) ;; breaks dired? Tue, Nov  4 2014, 19:17 EET
;; guide-key causes erratic delays when posting in ths SC post buffer
;; from sclang.  Therefore disabled.
;; (require 'guide-key)
;; (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "H-h" "H-m" "H-p" "H-d" "C-c"))
;;  (guide-key-mode 1)  ; Enable guide-key-mode
;; (yas-global-mode) ; interferes with auto-complete in elisp mode.

(require 'windmove)
(global-set-key (kbd "H-{") 'windmove-up)
(global-set-key (kbd "H-}") 'windmove-down)
(global-set-key (kbd "H-]") 'windmove-right)
(global-set-key (kbd "H-[") 'windmove-left)

(require 'buffer-move)
(global-set-key (kbd "<S-prior>") 'buf-move-up)
(global-set-key (kbd "<S-next>") 'buf-move-down)
(global-set-key (kbd "<S-end>") 'buf-move-right)
(global-set-key (kbd "<S-home>") 'buf-move-left)

(global-set-key (kbd "<s-home>") 'previous-buffer)
(global-set-key (kbd "<s-end>") 'next-buffer)

(setq projectile-completion-system 'grizzl)
(setq *grizzl-read-max-results* 40)
(defun projectile-dired-project-root ()
  "Dired root of current project.  Can be set as value of
projectile-switch-project-action to dired root of project when switching.
Note: projectile-find-dir (with grizzl) does not do this, but it
asks to select a *subdir* of selected project to dired."
  (interactive)
  (dired (projectile-project-root)))

(setq projectile-switch-project-action 'projectile-commander)

(defun projectile-post-project ()
  "Which project am I actually in?"
  (interactive)
  (message (projectile-project-root)))

(defun projectile-add-project ()
  "Add folder of current buffer's file to list of projectile projects"
  (interactive)
  (if (buffer-file-name (current-buffer))
      (projectile-add-known-project
       (file-name-directory (buffer-file-name (current-buffer))))))

(global-set-key (kbd "H-p c") 'projectile-commander)
(global-set-key (kbd "H-p h") 'helm-projectile)
(global-set-key (kbd "H-p s") 'projectile-switch-project)
(global-set-key (kbd "H-p d") 'projectile-find-dir)
(global-set-key (kbd "H-p f") 'projectile-find-file)
(global-set-key (kbd "H-p w") 'projectile-post-project)
(global-set-key (kbd "H-p D") 'projectile-dired-project-root)
(global-set-key (kbd "H-p +") 'projectile-add-project)
(global-set-key (kbd "H-p -") 'projectile-remove-known-project)
(global-set-key (kbd "H-p a") 'projectile-ack) ;; better search than grep

;; must call these to initialize  helm-source-find-files

    (require 'helm-files) ;; (not auto-loaded by system!)
;;    (require 'helm-projectile)
    (require 'helm-swoop) ;; must be put into packages
    ;; Don't bicker if not in a project:
    (setq projectile-require-project-root)

    ;; Added by IZ following this:
    ;; https://github.com/emacs-helm/helm/issues/604
    ;; :

    (add-hook 'helm-find-files-before-init-hook
              (lambda ()
                (progn
                  ;; List Hg files in project.
                  (helm-add-action-to-source-if
                   "Hg list files"
                   'helm-ff-hg-find-files
                   helm-source-find-files
                   'helm-hg-root-p)
                  ;; Byte compile files async
                  (helm-add-action-to-source-if
                   "Byte compile file(s) async"
                   'async-byte-compile-file
                   helm-source-find-files
                   'helm-ff-candidates-lisp-p)
                  ;; Add add-to-projectile action after helm-find-files.
                  (let ((find-files-action (assoc 'action helm-source-find-files)))
                    (setcdr find-files-action
                            (cons
                             (cadr find-files-action)
                             (cons '("Add to projectile" . helm-add-to-projectile)
                                   (cddr find-files-action))))))))

    ;; Use helm-find-files actions in helm-projectile
 ;;   (let ((projectile-files-action (assoc 'action helm-source-projectile-files-list)))
 ;;       (setcdr projectile-files-action (cdr (assoc 'action helm-source-find-files))))

    (defun helm-add-to-projectile (path)
      "Add directory of file to projectile projects.
    Used as helm action in helm-source-find-files"
      (projectile-add-known-project (file-name-directory path)))

    (global-set-key (kbd "H-h p") 'helm-projectile)
    (global-set-key (kbd "H-h g") 'helm-do-grep)
    (global-set-key (kbd "H-h f") 'helm-find-files)
    (global-set-key (kbd "H-h r") 'helm-resume)
    (global-set-key (kbd "H-h b") 'helm-bookmarks)
    (global-set-key (kbd "H-h l") 'helm-buffers-list)
    (global-set-key (kbd "H-M-h") 'helm-M-x)
    (global-set-key (kbd "H-h w") 'helm-world-time)
    (global-set-key (kbd "H-h s") 'helm-swoop)
    (global-set-key (kbd "C-c m") 'helm-mini)

    (setq display-time-world-list
          '(("America/Los_Angeles" "Santa Barbara")
            ("America/New_York" "New York")
            ("Europe/London" "London")
            ("Europe/Lisbon" "Lisboa")
            ("Europe/Madrid" "Barcelona")
            ("Europe/Paris" "Paris")
            ("Europe/Berlin" "Berlin")
            ("Europe/Rome" "Rome")
            ;; ("Europe/Albania" "Gjirokastra") ;; what city to name here?
            ("Europe/Athens" "Athens")
            ("Asia/Calcutta" "Kolkatta")
            ("Asia/Jakarta" "Jakarta")
            ("Asia/Shanghai" "Shanghai")
            ("Asia/Tokyo" "Tokyo")))

(require 'lacarte)
;; (global-set-key [?\e ?\M-x] 'lacarte-execute-command)

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
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M->") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; (global-set-key (kbd "C->") 'mc/mark-next-symbol-like-this)
;; (global-set-key (kbd "C->") 'mc/mark-next-word-like-this)

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
(key-chord-define-global "-="     'code-quote-sexp)
;; to add: quote, single quote around word/sexp
;; Exit auto-complete, keeping the current selection,
;; while avoiding possible side-effects of TAB or RETURN.
(key-chord-define-global "KK"      "\C-f\C-b")
;; Trick for triggering yasnippet when using in tandem with auto-complete:
;; Move forward once to get out of auto-complete, then backward once to
;; end of keyword, and enter tab to trigger yasnippet.
(key-chord-define-global "KL"      "\C-f\C-b\C-i")

;; Jump to any symbol in buffer using ido-imenu
(key-chord-define-global "KJ"      'ido-imenu)

(require 'hl-sexp)
;; (require 'highlight-sexps)
;; Include color customization for dark color theme here.
(custom-set-variables
 '(hl-sexp-background-colors (quote ("gray0"  "#0f003f"))))

;;  (require 'dired+)
  (require 'dirtree)
  (global-set-key (kbd "H-d d") 'dirtree-show)
  ;; sr-speedbar is broken in emacs 24.4.1
  ;; (require 'sr-speedbar)
  ;; (speedbar-add-supported-extension ".sc")
  ;; (speedbar-add-supported-extension ".scd")
  ;; (global-set-key (kbd "H-d H-s") 'sr-speedbar-toggle)

(define-key dired-mode-map (kbd "<SPC>")
  (lambda () (interactive)
    (let ((lawlist-filename (dired-get-file-for-visit)))
      (if (equal (file-name-extension lawlist-filename) "pdf")
          (start-process "default-pdf-app" nil "open" lawlist-filename)))))

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

;; own mod
(defun open-folder-in-finder (&optional dir)
  (interactive "DSelect folder:")
  (setq dir (expand-file-name dir))
  (let ((script
         (concat
          "tell application \"Finder\"\n"
          " set frontmost to true\n"
          " make new Finder window to (POSIX file \"" dir "\")\n"
          "end tell\n")))
    (start-process "osascript-getinfo" nil "osascript" "-e" script)))

(global-set-key (kbd "H-o") 'open-folder-in-finder)

(defvar scratchpad-main-directory "SCRIPTS")

(defvar scratchpad-languages
  '(("emacslisp" .
               (:extension "el" :template-func make-el-template))
    ("supercollider" .
                   (:extension "scd" :template-func make-sc-template))
    ("markdown" .
     (:extension "md" :template-func make-md-template))
    ("shell" .
     (:extension "sh" :template-func make-sh-template))
    ("git" .
     (:extension "sh" :template-func make-sh-template))
    ("org-mode" .
     (:extension "org" :template-func make-org-template))))

(defun scratchpad-menu (&optional folderp)
  (interactive "P")
  (let* ((menu (grizzl-make-index (mapcar 'car scratchpad-languages)))
         (language (grizzl-completing-read "Select language: " menu))
         (language-plist (cdr (assoc language scratchpad-languages))))
    (if folderp
        (dirtree (scratchpad-make-folder-name language) t)
      (apply
       (plist-get language-plist :template-func)
       (list
        language
        (read-no-blanks-input "Title? (only alpha-numeric, - and _ chars): " "")
        (plist-get language-plist :extension))))))

(file-name-sans-extension "/test/abcd.efgh")

(defun make-el-template (folder title extension)
  (let* (
         (full-path (scratchpad-make-full-path folder title extension))
         (file-name (file-name-nondirectory full-path))
         (package-name (file-name-sans-extension file-name)))
    (find-file full-path)
    (insert
     (concat
      ";;; package --- Summary\n\n"
      ";;; Commentary:\n\n"
      ";;; Code:\n\n()\n\n"
      ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
      "(provide '" package-name
      ")\n;;; " file-name " ends here"
      ))
    (goto-char 0)
    (search-forward "\(\)")
    (backward-char 1)))

(defun scratchpad-make-full-path (folder title extension)
  (concat (scratchpad-make-folder-name folder)
          (scratchpad-make-file-name title extension)))

(defun scratchpad-make-file-name (file-name extension)
  (concat title
          (format-time-string "_%y%m%d_%H-%M" (current-time))
          "."
          extension))

(defun scratchpad-find-file (folder file-name)
  (find-file (concat (scratchpad-make-folder-name folder) file-name)))

(defun scratchpad-make-folder-name (folder)
  (concat iz-log-dir scratchpad-main-directory "/" folder "-scratchpad/"))

(defun make-sc-template (folder title &optional extension)
  (unless extension (setq extension "scd"))
  (find-file
   (scratchpad-make-full-path folder title extension))
  (insert
   (concat "/* " (format-time-string "%c %Z") " */\n\n"
           "(\nServer.default.boot;\n)\n//:\n(\n"
           "~mySound = { | amp = 0.1 | WhiteNoise.ar(amp) }.play;\n)"
           ))
  (unless (sclang-get-process) (sclang-start)))

(defun make-md-template (folder title &optional extension)
  (unless extension (setq extension "md"))
  (find-file
   (scratchpad-make-full-path folder title extension))
  (insert
   (concat "# " title (format-time-string "\n(%c %Z)\n\n"))))

(defun make-sh-template (folder title &optional extension)
  (unless extension (setq extension "sh"))
  (find-file
   (scratchpad-make-full-path folder title extension))
  (insert
   (concat "#!/bin/sh\n# " title (format-time-string "(%c %Z)\n\n"))))

(defun make-org-template (folder title &optional extension)
  (unless extension (setq extension "org"))
  (find-file
   (scratchpad-make-full-path folder title extension))
  (insert
   (concat "#+TITLE: " title (format-time-string "\n#+DATE: %c %Z\n\n"))))

(global-set-key (kbd "H-h H-n") 'scratchpad-menu)

(add-hook 'after-save-hook
          #'(lambda ()
              (and (save-excursion
                     (save-restriction
                       (widen)
                       (goto-char (point-min))
                       (save-match-data
                         (looking-at "^#!"))))
                   (not (file-executable-p buffer-file-name))
                   (shell-command (concat "chmod u+x " buffer-file-name))
                   (message
                    (concat "Saved as script: " buffer-file-name)))))

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

(require 'sclang)

;; Note: Paredit-style bracket movement commands d, u, f, b, n, p work
;; in sclang-mode without loading Paredit.
;; (add-hook 'sclang-mode-hook 'paredit-mode)
(add-hook 'sclang-mode-hook 'rainbow-delimiters-mode)
(add-hook 'sclang-mode-hook 'hl-sexp-mode)
(add-hook 'sclang-mode-hook 'electric-pair-mode)
(add-hook 'sclang-mode-hook 'yas-minor-mode)
(add-hook 'sclang-mode-hook 'auto-complete-mode)
;; sclang-ac-mode is included in sclang-extensions-mode:
;; (add-hook 'sclang-mode-hook 'sclang-ac-mode)
;; sclang-ac mode constantly tries to run code.
;; that can lead to loops that hang, for example constantly creating a view.
;; (add-hook 'sclang-mode-hook 'sclang-extensions-mode)

;; Global keyboard shortcut for starting sclang
(global-set-key (kbd "C-c M-s") 'sclang-start)
;; Show workspace
(global-set-key (kbd "C-c C-M-w") 'sclang-switch-to-workspace)

(add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(global-set-key (kbd "H-l h") 'hs-hide-level)
(global-set-key (kbd "H-l s") 'hs-show-all)

(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(require 'paredit) ;; smart edit parentheses
(require 'cl)
(require 'litable) ;; show lisp eval results in the buffer, interactively
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-whitespace-mode)
(add-hook 'emacs-lisp-mode-hook 'auto-complete-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
;; H-C-i:
(define-key emacs-lisp-mode-map (kbd "H-TAB") 'icicle-imenu-command)

(eval-after-load 'org
    '(define-key org-mode-map (kbd "C-c C-x s") 'org-show-subtree))

(setq org-goto-interface 'outline-path-completion
      org-goto-max-level 10)

;; Previously bound only to org-mode map.
(global-set-key (kbd "H-TAB") 'icicle-imenu)
(global-set-key (kbd "H-C-l") 'lacarte-execute-menu-command)

(defun org-icicle-occur ()
  "In org-mode, show entire buffer contents before running icicle-occur.
 Otherwise icicle-occur will not place cursor at found location,
 if the location is hidden."
  (interactive)
  (show-all)
  (icicle-occur (point-min) (point-max))
  (recenter 3))

(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c '") 'org-edit-special))
(eval-after-load 'org
  '(define-key org-mode-map (kbd "H-i") 'org-icicle-occur))
(defun org-icicle-imenu (separate-buffer)
  "In org-mode, show entire buffer contents before running icicle-imenu.
Otherwise icicle-occur will not place cursor at found location,
if the location is hidden.
If called with prefix argument (C-u), then:
- open the found section in an indirect buffer.
- go back to the position where the point was before the command, in the
  original buffer."
  (interactive "P")
  (icicle-mode 1)
  (show-all)
  (let ((mark (point)))
    (icicle-imenu (point-min) (point-max) t)
    (cond (separate-buffer
           (org-tree-to-indirect-buffer)
           (goto-char mark))
          (t (recenter 4))))
  (icicle-mode -1))

(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c C-=") 'org-icicle-imenu))
(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c i m") 'org-icicle-imenu))

;; install alternative for org-mode C-c = org-table-eval-formula
;; which is stubbornly overwritten by icy-mode.
(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c C-x =") 'org-table-eval-formula))

;; Both eval-after-load and org-mode hook do not work for switching off
;; prelude mode, whitespace.  So using shortcuts as workaround:

(defun turn-off-whitespace-mode ()
  (interactive)
  (whitespace-mode -1))

(defun turn-off-icicle-mode ()
  (interactive)
  (icicle-mode -1))

(defun turn-off-prelude-mode ()
  (interactive)
  (prelude-mode -1))

(global-set-key (kbd "H-x w") 'turn-off-whitespace-mode)
(global-set-key (kbd "H-x p") 'turn-off-prelude-mode)
(global-set-key (kbd "H-x i") 'turn-off-icicle-mode)

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c M-=") 'org-table-eval-formula)
            (local-set-key (kbd "C-c '") 'org-edit-special)))

;;; ???? Adapt org-mode to icicle menus when refiling (C-c C-w)
;;; Still problems. Cannot use standard org refiling with icicles activated!
(setq org-outline-path-complete-in-steps nil)

(add-hook 'org-mode-hook
          (lambda ()
            (prelude-mode -1)))
(add-hook 'org-mode-hook 'prelude-off)

(defun org-refile-icy (as-subtree &optional do-copy-p)
  "Alternative to org-refile using icicles.
Refile or copy current section, to a location in the file selected with icicles.
Without prefix argument: Place the copied/cut section it *after* the selected section.
With prefix argument: Make the copied/cut section *a subtree* of the selected section.

Note 1: If quit with C-g, this function will have removed the section that
is to be refiled.  To get it back, one has to undo, or paste.

Note 2: Reason for this function is that icicles seems to break org-modes headline
buffer display, so onehas to use icicles for all headline navigation if it is loaded."
  (interactive "P")
  (outline-back-to-heading)
  (if do-copy-p (org-copy-subtree) (org-cut-subtree))
  (show-all)
  (icicle-imenu (point-min) (point-max) t)
  (outline-next-heading)
  (unless (eq (current-column) 0) (insert "\n"))
  (org-paste-subtree)
  (if as-subtree (org-demote-subtree)))

(defun org-copy-icy (as-subtree)
  "Copy section to another location in file, selecting the location with icicles.
See org-refile-icy."
  (interactive "P")
  (org-refile-icy as-subtree t))

(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c i r") 'org-refile-icy))
(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c i c") 'org-copy-icy))

(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'turn-off-whitespace-mode)
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(setq org-startup-indented t) ;; auto-indent text in subtrees
(setq org-hide-leading-stars t) ;; hide leading stars in subtree headings
(setq org-src-fontify-natively t) ;; colorize source-code blocks natively
(setq org-todo-keywords
      '((sequence
         "!!!(1)"  ; next action
         "!!(2)"  ; next action
         "!(3)"  ; next action
         "TODO(t)"  ; next action
         "STARTED(s)"
         "WAITING(w@/!)"
         "TOBLOG(b)"  ; next action
         "SOMEDAY(.)" "|"
         "DONE(x@/@)"
         "CANCELLED(c@)"
         "OBSOLETE(o@)")
        (sequence
         "TODELEGATE(-)"
         "DELEGATED(d)"
         "DELEGATE_DONE(l!)")))

(setq org-todo-keyword-faces
      '(("!!!" . (:foreground "red" :weight bold))
        ("!!" . (:foreground "tomato" :weight bold))
        ("!" . (:foreground "coral" :weight bold))
        ("TODO" . (:foreground "LightSalmon" :weight bold))
        ("TOBLOG" . (:foreground "MediumVioletRed" :weight bold))
        ("STARTED" . (:foreground "DeepPink" :weight bold))
        ("WAITING" . (:foreground "gold" :weight bold))
        ("DONE" . (:foreground "SeaGreen" :weight bold))
        ("CANCELLED" . (:foreground "wheat" :weight bold))
        ("OBSOLETE" . (:foreground "CadetBlue" :weight bold))
        ("TODELEGATE" . (:foreground "DeepSkyBlue" :weight bold))
        ("DELEGATED" . (:foreground "turquoise" :weight bold))
        ("DELEGATE_DONE" . (:foreground "LawnGreen" :weight bold))
        ("WAITING" . (:foreground "goldenrod" :weight bold))
        ("SOMEDAY" . (:foreground "gray" :weight bold))))

;; the rest of the setup was done by customizing the variables
;; org-mobile-directory and org-mobile-inbox-for-pull, and is in custom.el

(global-set-key (kbd "H-h m p") 'org-mobile-push)
(global-set-key (kbd "H-h m l") 'org-mobile-pull)

(defun org-headline-line ()
  "convert current line into headline at same level as above."
  (interactive)
  (beginning-of-line)
  (org-meta-return)
  (delete-char 1))

(eval-after-load 'org
  '(progn
     (define-key org-mode-map (kbd "C-M-<return>") 'org-headline-line)))

(global-set-key "\C-ca" 'org-agenda)

(defvar org-agenda-list-save-path
  "~/.emacs.d/savefile/org-agenda-list.el"
"Path to save the list of files belonging to the agenda.")

(defun org-agenda-save-file-list ()
  "Save list of desktops from file in org-agenda-list-save-path"
  (interactive)
  (save-excursion
    (let ((buf (find-file-noselect org-agenda-list-save-path)))
      (set-buffer buf)
      (erase-buffer)
      (print (list 'quote org-agenda-files) buf)
      (save-buffer)
      (kill-buffer)
      (message "org-agenda file list saved to: %s" org-agenda-list-save-path))))

(defun org-agenda-load-file-list ()
  "Load list of desktops from file in org-agenda-list-save-path"
  (interactive)
  (save-excursion
    (let ((buf (find-file-noselect org-agenda-list-save-path)))
      (set-buffer buf)
      (setq org-agenda-files (eval (read (buffer-string))))
      (kill-buffer)
      (message "org-agenda file list loaded from: %s" org-agenda-list-save-path))))

(defun org-agenda-add-this-file-to-agenda ()
  "Add the file from the current buffer to org-agenda-files list."
  (interactive)
  (let (path)
    ;; (org-agenda-file-to-front) ;; adds path relative to user home dir
    ;; (message "Added current buffer to agenda files.")
    (let ((path (buffer-file-name (current-buffer))))
      (cond (path
        (add-to-list 'org-agenda-files path)
        (org-agenda-save-file-list)
        (message "Added file '%s' to agenda file list"
                 (file-name-base path)))
            (t (message "Cannot add buffer to file list. Save buffer first."))))))

(defun org-agenda-remove-this-file-from-agenda (&optional select-from-list)
  "Remove a file from org-agenda-files list.
If called without prefix argument, remove the file of the current buffer.
If called with prefix argument, then select a file from org-agenda-files list."
  (interactive "P")
  (let (path)
   (if select-from-list
       (let  ((menu (grizzl-make-index org-agenda-files)))
         (setq path (grizzl-completing-read "Choose an agenda file: " menu)))
     (setq path (buffer-file-name (current-buffer))))
   (setq org-agenda-files
         (remove (buffer-file-name (current-buffer)) org-agenda-files)))
  (org-agenda-save-file-list)
  (message "Removed file '%s' from agenda file list"
           (file-name-base (buffer-file-name (current-buffer)))))

(defun org-agenda-open-file ()
  "Open a file from the current agenda file list."
  (interactive)
  (let* ((menu (grizzl-make-index org-agenda-files))
        (answer (grizzl-completing-read "Choose an agenda file: " menu)))
    (find-file answer)))

(defun org-agenda-list-files ()
  "List the paths that are currently in org-agenda-files"
  (interactive)
  (let  ((menu (grizzl-make-index org-agenda-files)))
    (grizzl-completing-read "These are currently the files in list org-agenda-files. " menu)))

(defun org-agenda-list-menu ()
 "Present menu with commands for loading, saving, adding and removing
files to org-agenda-files."
 (interactive)
 (let* ((menu (grizzl-make-index
               '("org-agenda-save-file-list"
                 "org-agenda-load-file-list"
                 "org-agenda-list-files"
                 "org-agenda-open-file"
                 "org-agenda-add-this-file-to-agenda"
                 "org-agenda-remove-this-file-from-agenda")))
        (command (grizzl-completing-read "Choose a command: " menu)))
   (call-interactively (intern command))))

(global-set-key (kbd "H-a H-a") 'org-agenda-list-menu)

(require 'calfw-org)

(global-set-key "\C-c\M-a" 'cfw:open-org-calendar)
(global-set-key "\C-c\C-xm" 'org-mark-ring-goto)

(defun org-set-date (&optional active property)
  "Set DATE property with current time.  Active timestamp."
  (interactive "P")
  (org-set-property
   (if property property "DATE")
   (cond ((equal active nil)
          (format-time-string (cdr org-time-stamp-formats) (current-time)))
         ((equal active '(4))
          (concat "["
                  (substring
                   (format-time-string (cdr org-time-stamp-formats) (current-time))
                   1 -1)
                  "]"))
         ((equal active '(16))
          (concat
           "["
           (substring
            (format-time-string (cdr org-time-stamp-formats) (org-read-date t t))
            1 -1)
           "]"))
         ((equal active '(64))
          (format-time-string (cdr org-time-stamp-formats) (org-read-date t t))))))

;; Note: This keybinding is in analogy to the standard keybinding:
;; C-c . -> org-time-stamp
(eval-after-load 'org
  '(progn
     (define-key org-mode-map (kbd "C-c C-.") 'org-set-date)
     ;; Prelude defines C-c d as duplicate line
     ;; But we disable prelude in org-mode because of other, more serious conflicts,
     ;; So we keep this alternative key binding:
     (define-key org-mode-map (kbd "C-c d") 'org-set-date)))

(defun org-set-due-property ()
  (interactive)
  (org-set-property
   "DUE"
   (format-time-string (cdr org-time-stamp-formats) (org-read-date t t))))

(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c M-.") 'org-set-due-property))

(setq org-tag-alist
      '(
        ("home" . ?h)
        ("finance" . ?f)
        ("eastn" . ?e)
        ("avarts" . ?a)
        ("erasmus" . ?E)
        ("researchfunding" . ?r)))

(defvar iz-log-dir
  (expand-file-name
   "~/Dropbox/000WORKFILES/")
  "This directory contains all notes on current projects and classes")

(setq diary-file (concat iz-log-dir "PRIVATE/diary"))

(defadvice org-agenda (before update-agenda-file-list ())
  "Re-createlist of agenda files from contents of relevant directories."
  (iz-update-agenda-file-list)
  (icicle-mode 1))

(defadvice org-agenda (after turn-icicles-off ())
  "Turn off icicle mode since it interferes with some other keyboard shortcuts."
  (icicle-mode -1))

(ad-activate 'org-agenda)

(defadvice org-refile (before turn-icicles-on-for-refile ())
  "Re-createlist of agenda files from contents of relevant directories."
  (icicle-mode 1))

(defadvice org-refile (after turn-icicles-off-for-refile ())
  "Turn off icicle mode since it interferes with some other keyboard shortcuts."
  (icicle-mode -1))

(ad-activate 'org-refile)

(defun iz-update-agenda-file-list ()
  "Set value of org-agenda-files from contents of relevant directories."
  (setq org-agenda-files
        (let ((folders (file-expand-wildcards (concat iz-log-dir "*")))
              (files (file-expand-wildcards (concat iz-log-dir "*.org"))))
          (dolist (folder folders)
            (setq files
                  (append
                   files ;; ignore files whose name starts with underscore (_)
                   (file-expand-wildcards (concat folder "/[!_]*.org")))))
          (-reject
           (lambda (f)
             (string-match-p "/\\." f))
           files)))
  (message "the value of org-agenda-files was updated"))

(defvar iz-last-selected-file
  nil
  "Path of file last selected with iz-org-file menu.
Used to refile to date-tree of last selected file.")

(defun iz-goto-last-selected-file ()
  (interactive)
  (if iz-last-selected-file
      (find-file iz-last-selected-file)
    (iz-find-file)))

(defun iz-refile-to-date-tree (&optional use-last-selected)
  "Refile to last selected file, using DATE timestamp
to move to file-datetree."
  (interactive "P")
  (let ((origin-buffer (current-buffer))
        (origin-filename (buffer-file-name (current-buffer)))
        (date (calendar-gregorian-from-absolute
               (org-time-string-to-absolute
                (or (org-entry-get (point) "CLOSED")
                 (org-entry-get (point) "DATE"))))))
    (org-cut-subtree)
    (if (and iz-last-selected-file use-last-selected)
        (find-file iz-last-selected-file)
      (iz-find-file))
    (org-datetree-find-date-create date)
    (move-end-of-line nil)
    (open-line 1)
    (next-line)
    (org-paste-subtree 4)
    (save-buffer)
    (find-file origin-filename)))

(defun org-process-entry-from-mobile-org ()
  (interactive)
  (org-back-to-heading 1)
  (next-line 1)
  (let ((time (cadr (org-element-timestamp-parser))))
    (org-entry-put nil "DATE" (plist-get time :raw-value)))
  (outline-next-heading))

(defun iz-get-and-refile-mobile-entries ()
  (interactive)
 (org-mobile-pull)
 (let* ((mobile-file (file-truename "~/org/from-mobile.org"))
        (mobile-buffer (find-file mobile-file))
        (log-buffer (find-file (concat iz-log-dir "PRIVATE/LOG.org"))))
   (with-current-buffer
       mobile-buffer
     (org-map-entries
      (lambda ()
        (let* ((timestamp
                (cdr (assoc "TIMESTAMP_IA" (org-entry-properties))))
               (date
               (calendar-gregorian-from-absolute
                (org-time-string-to-absolute timestamp))))
          (org-copy-subtree)
          (with-current-buffer
              log-buffer
            (org-datetree-find-date-create date)
            (move-end-of-line nil)
            (open-line 1)
            (next-line)
            (org-paste-subtree 4)
            (org-set-property "DATE" (concat "<" timestamp ">"))
            (org-set-tags-to ":mobileorg:"))))))
   (copy-file
    mobile-file
    (concat
     (file-name-sans-extension mobile-file)
     (format-time-string "%Y-%m-%d-%H-%M-%S")
     ".org"))
   (with-current-buffer
       mobile-buffer
     (erase-buffer)
     (save-buffer))))

(defun iz-refile-notes-to-log ()
  (interactive)
 (let* ((notes-file (concat iz-log-dir "NOTES/notes.org"))
        (notes-buffer (find-file notes-file))
        (log-buffer (find-file (concat iz-log-dir "PRIVATE/LOG.org"))))
   (with-current-buffer
       notes-buffer
     (org-map-entries
      (lambda ()
        (let* ((timestamp (org-entry-get (point) "DATE"))
               (date
               (calendar-gregorian-from-absolute
                (org-time-string-to-absolute timestamp))))
          (org-copy-subtree)
          (with-current-buffer
              log-buffer
            (org-datetree-find-date-create date)
            (move-end-of-line nil)
            (open-line 1)
            (next-line)
            (org-paste-subtree 4)
            (org-set-property "DATE" (concat "<" timestamp ">")))))))
   (copy-file
    notes-file
    (concat
     (file-name-sans-extension notes-file)
     (format-time-string "%Y-%m-%d-%H-%M-%S")
     ".org"))
   (with-current-buffer
       notes-buffer
     (erase-buffer)
     (save-buffer))))

(defun iz-insert-file-as-snippet ()
  (interactive)
  (insert-file-contents (iz-select-file-from-folders)))

(defun iz-select-file-from-folders ()
  (iz-org-file-menu (iz-select-folder)))

(defun iz-select-folder ()
  (let*
      ((folders (-select 'file-directory-p
                         (file-expand-wildcards
                          (concat iz-log-dir "*"))))
       (folder-menu (grizzl-make-index
                     (mapcar 'file-name-nondirectory folders)))
       (folder (grizzl-completing-read "Select folder:" folder-menu)))
    folder))

(defun iz-org-file-menu (subdir)
  (let*
      ((files
        (file-expand-wildcards (concat iz-log-dir subdir "/[a-zA-Z0-9]*.org")))
       (projects (mapcar 'file-name-sans-extension
                         (mapcar 'file-name-nondirectory files)))
       (dirs
        (mapcar (lambda (dir)
                  (cons (file-name-sans-extension
                                (file-name-nondirectory dir)) dir))
                files))
       (project-menu (grizzl-make-index projects))
       (selection (cdr (assoc (grizzl-completing-read "Select file: " project-menu)
                              dirs))))
    (setq iz-last-selected-file selection)
    selection))

(defun iz-get-refile-targets ()
  (interactive)
  (setq org-refile-targets '((iz-select-file-from-folders . (:maxlevel . 2)))))

(defun iz-find-file (&optional dired)
  "open a file by selecting from subfolders."
  (interactive "P")
  (cond ((equal dired '(4))
         (dired (concat iz-log-dir (iz-select-folder))))
        ((equal dired '(16)) (dired iz-log-dir))
        ((equal dired '(64))
         (dirtree (concat iz-log-dir (iz-select-folder)) nil))
        ((equal dired '(256))
         (dirtree iz-log-dir nil))
        (t
         (find-file (iz-select-file-from-folders))
         (goto-char 0)
         (if (search-forward "*# -*- mode:org" 100 t)
             (org-decrypt-entries)))))

;; Following needed to avoid error message ls does not use dired.
(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)

(defun iz-open-project-folder (&optional open-in-finder)
  "Open a folder associated with a project .org file.
Select the file using iz-select-file-from-folders, and then open folder instead.
If the folder does not exist, create it."
  (interactive "P")
  (let ((path (file-name-sans-extension (iz-select-file-from-folders))))
    (unless  (file-exists-p path) (make-directory path))
    (if open-in-finder (open-folder-in-finder path) (dired path))))

(defvar iz-capture-keycodes "abcdefghijklmnoprstuvwxyzABDEFGHIJKLMNOPQRSTUVWXYZ1234567890.,(){}!@#$%^&*-_=+")

(defun iz-log (&optional goto)
  "Capture log entry in date-tree of selected file."
  (interactive "P")
  (iz-make-log-capture-templates (iz-select-folder))
  (org-capture goto))

(defun iz-select-folder ()
  (let*
      ((folders (-select 'file-directory-p
                         (file-expand-wildcards
                          (concat iz-log-dir "*"))))
       (folder-menu (grizzl-make-index
                     (mapcar 'file-name-nondirectory folders)))
       (folder (grizzl-completing-read "Select folder:" folder-menu)))
    (file-name-nondirectory folder)))

(defun iz-make-log-capture-templates (subdir)
  "Make capture templates for selected subdirectory under datetree."
 (setq org-capture-templates
       (setq org-capture-templates
             (let* (
                    (files
                     (file-expand-wildcards
                      (concat iz-log-dir subdir "/[a-zA-Z0-9]*.org")))
                    (projects (mapcar 'file-name-nondirectory files))
                    (dirs
                     (mapcar (lambda (dir) (cons (file-name-sans-extension
                                                  (file-name-nondirectory dir))
                                                 dir))
                             files)))
               (-map-indexed (lambda (index item)
                               (list
                                (substring iz-capture-keycodes index (+ 1 index))
                                (car item)
                                'entry
                                (list 'file+datetree (cdr item))
                                "* %?\n :PROPERTIES:\n :DATE:\t%T\n :END:\n\n%i\n"))
                             dirs)))))

(defun iz-todo (&optional goto)
  "Capture TODO entry in date-tree of selected file."
  (interactive "P")
  (iz-make-todo-capture-templates (iz-select-folder))
  (org-capture goto))

(defun iz-make-todo-capture-templates (subdir)
  "Make capture templates for project files"
 (setq org-capture-templates
       (setq org-capture-templates
             (let* (
                    (files
                     (file-expand-wildcards
                      (concat iz-log-dir subdir "/[a-zA-Z0-9]*.org")))
                    (projects (mapcar 'file-name-nondirectory files))
                    (dirs
                     (mapcar (lambda (dir) (cons (file-name-sans-extension
                                                  (file-name-nondirectory dir))
                                                 dir))
                             files)))
               (-map-indexed
                (lambda (index item)
                  (list
                   (substring iz-capture-keycodes index (+ 1 index))
                   (car item)
                   'entry
                   (list 'file+headline (cdr item) "TODOs")
                   "* TODO %?\n :PROPERTIES:\n :DATE:\t%U\n :END:\n\n%i\n"))
                dirs)))))

(defun iz-goto (&optional level)
  (interactive "P")
  (if level
      (setq org-refile-targets (list (cons (iz-select-file-from-folders) (cons :level level))))
    (setq org-refile-targets (list (cons (iz-select-file-from-folders) '(:maxlevel . 3)))))
  (org-refile '(4)))

(defun iz-refile (&optional goto)
  "Refile to selected file."
  (interactive "P")
  (setq org-refile-targets (list (cons (iz-select-file-from-folders) '(:maxlevel . 3))))
  (org-refile goto))

(defun iz-org-file-command-menu ()
  "Menu of commands operating on iz org files."
(interactive)
  (let* ((menu (grizzl-make-index
                '(
                  "iz-log"
                  "iz-todo"
                  "iz-refile-to-date-tree"
                  "iz-refile"
                  "iz-open-project-folder"
                  "iz-find-file"
                  "iz-goto"
                  "iz-goto-last-selected-file"
                  "org-agenda"
                  "iz-get-and-refile-mobile-entries"
                  "iz-refile-notes-to-log"
                  "iz-insert-file-as-snippet"
                  )))
         (selection (grizzl-completing-read "Select command: " menu)))
    (eval (list (intern selection)))))

(global-set-key (kbd "H-h H-m") 'iz-org-file-command-menu)
(global-set-key (kbd "H-h H-h") 'iz-org-file-command-menu)
(global-set-key (kbd "H-h H-f") 'iz-find-file)
(global-set-key (kbd "H-h H-d") 'iz-open-project-folder)
(global-set-key (kbd "H-h H-l") 'iz-log)
(global-set-key (kbd "H-h L") 'iz-goto-last-selected-file)
(global-set-key (kbd "H-h H-i") 'iz-insert-file-as-snippet)
(global-set-key (kbd "H-h H-t") 'iz-todo)
(global-set-key (kbd "H-h H-r") 'iz-refile)
(global-set-key (kbd "H-h r") 'iz-refile-to-date-tree)
(global-set-key (kbd "H-h H-g") 'iz-goto)
(global-set-key (kbd "H-h H-c H-w") 'iz-refile)
(global-set-key (kbd "H-h H-c H-a") 'org-agenda)

;; Experimental:
(defun iz-make-finance-capture-template ()
  (setq org-capture-templates
        (list
         (list
          "f" "FINANCE"
          'entry
          (list 'file+datetree (concat iz-log-dir "projects/FINANCE.org"))
          "* %^{title}\n :PROPERTIES:\n :DATE:\t%T\n :END:\n%^{TransactionType}p%^{category}p%^{amount}p\n%?\n"
          ))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (ruby . t)
   (python . t)
   (perl . t)
   ))

(defun org-babel-load-current-file ()
  (interactive)
  (org-babel-load-file (buffer-file-name (current-buffer))))

;; Note: Overriding default key binding to provide consistent pattern:
;; C-c C-v f -> tangle, C-c C-v C-f -> load
(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c C-v C-f") 'org-babel-load-current-file))

;;; Load latex package
(require 'ox-latex)

;;; Use xelatex instead of pdflatex, for support of multilingual fonts (Greek etc.)
;; Note: Use package polyglossia to customize dates and other details.
(setq org-latex-pdf-process
      (list "xelatex -interaction nonstopmode -output-directory %o %f"
            "xelatex -interaction nonstopmode -output-directory %o %f"
            "xelatex -interaction nonstopmode -output-directory %o %f"))

;; This is kept as reference. XeLaTeX covers all european/greek/asian needs.
;; It is the original setting for working with pdflatex:
;; (setq org-latex-pdf-process
;;  ("pdflatex -interaction nonstopmode -output-directory %o %f"
;;   "pdflatex -interaction nonstopmode -output-directory %o %f"
;;   "pdflatex -interaction nonstopmode -output-directory %o %f"))

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

;; Reconfigure memoir to make a book (or report) from a org subtree
(add-to-list 'org-latex-classes
             '("section-to-book"
               "\\documentclass{memoir}"
               ("\\chapter{%s}" . "\\chapter*{%s}") ;; actually: BOOK TITLE!
               ("\\section{%s}" . "\\section*{%s}") ;; actually: Chapter!
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")))

;; Letter
(add-to-list 'org-latex-classes
             '("letter"
               "\\documentclass{letter}"
               ;; Should not use subsections at all!:
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")))

(add-to-list 'org-latex-classes
             '("newlfm-letter"
               "\\documentclass[11pt,letter,dateno,sigleft]{newlfm}"
               ;; Should not use subsections at all!:
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")))

(defvar latex-templates-path
  (file-truename "~/Dropbox/000WORKFILES/SNIPPETS_AND_TEMPLATES"))

(defvar latex-section-template
  '(("\\section\{%s\}" . "\\section*\{%s\}")
    ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
    ("\\subsubsect1on\{%s\}" . "\\subsubsection*\{%s\}")))

(defvar org-latex-last-chosen-file-name)

(defun org-export-subtree-as-latex-with-header-from-file (&optional use-previous-setting-p)
  (interactive "P")
  (org-latex-export-with-file-template t use-previous-setting-p t))

(defun org-export-subtree-as-pdf-with-header-from-file (&optional use-previous-setting-p)
  (interactive "P")
  (org-latex-export-with-file-template nil use-previous-setting-p t))

(defun org-export-buffer-as-latex-with-header-from-file (&optional use-previous-setting-p)
  (interactive "P")
  (org-latex-export-with-file-template t use-previous-setting-p nil))

(defun org-export-buffer-as-pdf-with-header-from-file (&optional use-previous-setting-p)
  (interactive "P")
  (org-latex-export-with-file-template nil use-previous-setting-p nil))


(defun org-latex-export-with-file-template (&optional as-latex-buffer-p use-previous-setting-p subtree-p)
  (let* (;; backup to restore original latex-classes after this operation:
         (org-latex-classes-backup org-latex-classes)
         (paths (file-expand-wildcards (concat latex-templates-path "/*.tex")))
         (names-and-paths
          (mapcar
           (lambda (x)
             (cons (file-name-sans-extension (file-name-nondirectory x)) x))
           paths))
         (menu (grizzl-make-index (mapcar 'car names-and-paths)))
         (chosen-filename
          (if (and use-previous-setting-p org-latex-last-chosen-file-name)
              org-latex-last-chosen-file-name
            (grizzl-completing-read "Choose latex template: " menu)))
         (chosen-template-path (cdr (assoc chosen-filename names-and-paths)))
         (this-buffers-latex-class
          (plist-get (org-export-get-environment 'latex t nil) :latex-class))
         latex-header
         (latex-sections
          (or (cddr (assoc this-buffers-latex-class org-latex-classes))
              latex-section-templates)))
    (when chosen-template-path
      (setq org-latex-last-chosen-file-name chosen-filename)
      (setq latex-header
            (with-temp-buffer
              (insert-file-contents chosen-template-path)
              (concat
               "[NO-DEFAULT-PACKAGES]\n"
               "[NO-EXTRA]\n"
               "\n"
               (buffer-string))))
      ;; Create custom org-latex-classes to use this template:
      (setq org-latex-classes
            (list
             (append
              (list this-buffers-latex-class latex-header)
              latex-sections)))
      (if as-latex-buffer-p
          (org-latex-export-as-latex nil subtree-p nil nil)
        (org-open-file (org-latex-export-to-pdf nil subtree-p nil nil)))
      ;; restore original latex classes:
      (setq org-latex-classes org-latex-classes-backup)
      ;; Open the chosen template for inspection and tweaking:
      (unless (get-buffer (file-name-nondirectory chosen-template-path))
        (split-window-vertically)
        (find-file chosen-template-path)))))

(global-set-key (kbd "H-h H-e") 'org-export-subtree-as-pdf-with-header-from-file)
(global-set-key (kbd "H-h H-E") 'org-export-subtree-as-latex-with-header-from-file)
(global-set-key (kbd "H-h H-C-e") 'org-export-buffer-as-pdf-with-header-from-file)
(global-set-key (kbd "H-h H-C-E") 'org-export-buffer-as-latex-with-header-from-file)

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)

(eval-after-load 'org
  '(define-key org-mode-map (kbd "H-W") 'widen))

(fset 'org-toggle-drawer
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([67108896 3 16 14 tab 24 24] 0 "%d")) arg)))

(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c M-d") 'org-toggle-drawer))

(defun org-cycle-current-entry ()
  "toggle visibility of current entry from within the entry."
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

(defun org-from ()
  "Set property 'FROM'."
  (interactive)
  (org-set-property "FROM" (ido-completing-read "From whom? " '("ab" "iz"))))

(defun org-to ()
  "Set property 'TO'."
  (interactive)
  (org-set-property "TO" (ido-completing-read "To whom? " '("ab" "iz"))))

(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c x f") 'org-from))
(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c x t") 'org-to))

(fset 'org-toggle-drawer
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([67108896 3 16 14 tab 24 24] 0 "%d")) arg)))

(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c M-d") 'org-toggle-drawer))
(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c C-'") 'org-edit-special))

(defun org-html-export-as-html-body-only ()
  "Export only the body. Useful for using the built-in exporter of Org mode
with the docpad website framework."
    (interactive)
    (let ((path
           (concat
            (file-name-sans-extension (buffer-file-name))
            ".html")))
      (message path)
      (org-html-export-as-html
          nil ;; async
          nil ;; subtreep
          nil ;; visible-only
          t   ;; body only
          ;; ext-plist (not given here)
          )
      (write-file path)
      (message (format "written to path: %s" path))))

(global-set-key (kbd "H-e H-b") 'org-html-export-as-html-body-only)

(setq magit-repo-dirs
      '(
        "~/Dropbox/000WORKFILES/org"
        "~/Documents/Dev"
        "~/.emacs.d/personal"
))

(bmkp-desktop-jump "startup")
