;; Simple Desktops
;; Author: Iannis Zannos
;;  Dec 26, 2013 (7:30 PM)
;; A very simple scheme for saving the paths of current buffers
;; and restoring them later.  Each set has a name.
;; sd/save-desktop: (C-c c-d s) save current set to selected or new name.
;; sd/load-desktop (C-c c-d l) :
;;    - Kill current buffers with files,
;;      !!! if called with C-u prefix: Do not kill current buffers
;;    - Select desktop
;;    - Load files of selected desktop
;; Note: Saving multiple desktops with bookmark+ is complex:
;; Difficult to figure out the options to overwriting or not overwiting current
;; desktop.  Results in unwanted overwriting of desktops.
;; Simple desktops does not interfere with the desktop package.

(defvar sd/desktops nil
  "Associative list of desktops by name.")

(defvar sd/desktop-save-path
  (file-truename "~/.emacs.d/personal/desktop/simple-desktop-list.el")
"Position where the desktops are saved.")

(defun assoc-replace (alist key newlist)
  "Remove all sublists of alist whose car is equal to key, and then
       add (cons key newlist) to alist."
  (setq alist (assoc-remove-key alist key))
  (setq alist (cons (cons key newlist) alist)))

(defun assoc-remove-key (alist key)
  "Remove all sublists of alist whose car is equal to key."
  (setq alist (remove* key alist :test 'equal :key 'car)))

(defun sd/load-desktop-list-from-file ()
  "Load list of desktops from file in sd/desktop-save-path."
  (interactive)
  (cond ((file-exists-p sd/desktop-save-path)
         (find-file sd/desktop-save-path)
         (setq sd/desktops (eval (read (buffer-string))))
         (kill-buffer (current-buffer)))
      (t (setq sd/desktops nil))))

(defun sd/save-desktop-list-to-file ()
  "Save list of desktops from file in sd/desktop-save-path."
  (interactive)
  (save-excursion
    (let ((buf (find-file-noselect sd/desktop-save-path)))
      (set-buffer buf)
      (erase-buffer)
      (print (list 'quote sd/desktops) buf)
      (save-buffer)
      (kill-buffer))))

(defun sd/save-desktop ()
  "Ask user to select or input a desktop name.
Add list of all paths of all open buffers that belong to files to
sd/desktops under that name. Save sd/desktops to disk."
  (interactive)
  ;; make sure you have the up-to-date list, without auto-loading at boot
  (sd/load-desktop-list-from-file)
  (let ((query-func
        (if (fboundp 'ido-completing-read) 'ido-completing-read 'completing-read))
       selection buffers)
   (dolist (b (buffer-list)) (if (buffer-file-name b)
                                 (add-to-list 'buffers (buffer-file-name b))))
   (setq selection
         (apply query-func
                (list "Select or enter a desktop name: "
                      (mapcar (lambda (d) (car d)) sd/desktops))))
   (setq sd/desktops (assoc-replace sd/desktops selection buffers))
   (sd/save-desktop-list-to-file)
   (message "Saved file list as: %s" selection)))

(defun sd/load-desktop (&optional preserve-current-buffers)
  "Ask user to select or input a desktop name.
Add list of all paths of all open buffers that belong to files to
sd/desktops under that name. Save sd/desktops to disk."
  (interactive "P")
  ;; make sure you have the up-to-date list, without auto-loading at boot
  (sd/load-desktop-list-from-file)
  (let ((query-func
         (if (fboundp 'ido-completing-read) 'ido-completing-read 'completing-read))
        selection)
    (setq selection
          (apply query-func
                 (list "Select a desktop to load: "
                       (mapcar (lambda (d) (car d)) sd/desktops) nil t)))
   (unless preserve-current-buffers
	(dolist (buffer (buffer-list))
          (if (or (buffer-file-name buffer)
                  (equal 'dired-mode (buffer-local-value 'major-mode buffer)))
              (kill-buffer buffer))))
    (dolist (path (cdr (assoc selection sd/desktops)))
      (if (file-exists-p path) (find-file path)))))

(defun sd/menu ()
  "Menu for simple desktop."
  (interactive)
  (let* ((commands
         '(sd/load-desktop sd/save-desktop dirtree-show sr-speedbar-toggle))
         (index (grizzl-make-index (-map 'symbol-name commands)))
         (selection (grizzl-completing-read "Select command: " index)))
    (apply (list (intern selection)))))

(global-set-key (kbd "H-d l") 'sd/load-desktop)
(global-set-key (kbd "H-d s") 'sd/save-desktop)
(global-set-key (kbd "H-d H-d") 'sd/menu)
(provide 'simple-desktops)
