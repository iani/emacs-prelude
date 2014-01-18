;;; IZ Dec 22, 2013 (8:11 PM)
;;; This is the top level file of my emacs customization.
;;; It makes packages available and loads all other files.

;;; Details: 
;;; 1. Add subdirectories of "packages" to load path, thereby making packages 
;;;    available.
;;; 2. Compile and load file "user/<username>.org 
;;;    where <username> is the logged-in unix user account name.
;;; 3. Load any emacs-lisp files in user/<username>/

(let* (
       (base (file-name-directory (or load-file-name (buffer-file-name))))
       (default-directory (concat base "packages"))
       ;; .org file must be in subdir to avoid conflict with auto-loaded .el
       ;; files by prelude.
       (user-custom-org-file (concat base "user/" (user-login-name) ".org")))
    (dolist (path (file-expand-wildcards (concat base "packages/*")))
      (message path)
      (add-to-list 'load-path path))
    (if (file-exists-p default-directory)  ;; the above is enough?
        (normal-top-level-add-subdirs-to-load-path))
;;; Load user-specific files, if present.
;;; 1: user/<username>.org
    (if (file-exists-p user-custom-org-file)
        (org-babel-load-file user-custom-org-file))
;;; 2: user/<username>/*.el
    (dolist (path (file-expand-wildcards 
                   (concat base "user/" (user-login-name) "/*.el")))
      (load-file path)))
