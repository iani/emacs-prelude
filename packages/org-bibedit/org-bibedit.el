;;; org-bibedit.el --- Edit a tree of pdf file entries contained in a folder.

;;; Commentary: 
;;; Developing a way to manage a bibliography of pdf files stored in 
;;; nested folders under a single folder.
;;; First, build an Org mode file reflecting the hierarchy of subfolders
;;; and listing all pdf files in them. 
;;; See org-bibedit.org file for description.

;;; IZ [2013-10-19 Sat]

;;; Code: 
(require 'find-lisp)
(require 'org)

(defvar org-bibedit-list-files-folders ())

(defun org-bibedit-list-files (root &optional switches) 
  "Insert folders/files contained in root path, as orgmode tree."
  (interactive (dired-read-dir-and-switches ""))
  (setq org-bibedit-list-files-folders ())
  (let* 
      ((filename-filter (cdr (assoc "FILENAME_FILTER" org-file-properties)))
       (filename-filter (if filename-filter filename-filter "\.pdf$"))
       (files 
        (mapcar (lambda (path)
                  (list (file-name-directory path)
                        (file-name-nondirectory path)
                        path))
                (sort
                 (mapcar (lambda (string) (replace-regexp-in-string root "/" string))
                         (find-lisp-find-files 
                          root filename-filter
                            ;;; "\.pdf$"
                          ))
                 'string<))))
    (dolist (file-entry (sort files (lambda (a b) (string< (car a) (car b)))))
      (apply 'org-bibedit-make-file-entry file-entry))))




(defun org-bibedit-make-file-entry (path filename fullpath)
  (let (
        (node-prefix "\n")
        (folder-check org-bibedit-list-files-folders)
        (folders (cdr (split-string path "/")))
        (folderpath "/")
        )
    (dolist (folder folders)
      (setq node-prefix (concat node-prefix "*"))
      (setq folderpath (concat folderpath folder "/"))
      (if (equal folder (car folder-check))
          (setq folder-check (cdr folder-check))
        (progn
          (setq folder-check ())
          (setq org-bibedit-list-files-folders folders)
          (unless (equal folder "")
            (org-bibedit-list-files-insert-folder-node node-prefix folder folderpath)))
        ))
    (org-bibedit-list-files-insert-file-node node-prefix path filename fullpath)))

(defun org-bibedit-list-files-insert-folder-node (prefix folder folderpath)
  (insert (concat prefix " " folder))
  (insert (format "\n\t:PROPERTIES:\n\t:PATH: %s\n\t:END:" folderpath)))

(defun org-bibedit-list-files-insert-file-node (prefix path filename fullpath)
  (insert (concat prefix " " filename))
  (insert (format "\n\t:PROPERTIES:\n\t:PATH: %s\n\t:TYPE: FILE\n\t" fullpath))
  (insert (format "\n\t:FILENAME: %s\n\t:END:" filename))
)

;; DRAFT
(defun org-bibedit-open-file ()
  "Open file of current node."
  (interactive)
  (org-open-file-with-system (org-entry-get (point) "PATH")))


(defun org-bibedit-make-files-table ()
  "Create table listing files collected in org buffer by org-bibedit-list-files."
  (interactive)
  (let ((entries ()))
    (org-map-entries 
     (lambda ()
       (if (equal "FILE" (org-entry-get (point) "TYPE"))
           (setq 
            entries 
            (cons 
              (list 
               (org-entry-get (point) "FILENAME")
               (org-entry-get (point) "PATH"))
              entries))
         ))
     )
    (insert "|-|-|-|-|-|\n")
    (insert "| ! | filename | link | shortname | status |\n")
    (insert "|-|-|-|-|-|\n")
    (dolist (entry entries)
       (insert (format "| | %s | [[%s][link]] | | |\n" (car entry) (cadr entry)))) 
    (insert "|-|-|-|-|-|")))

;;;;; Helper functions

;;; Stolen from o-blog.
;;; Cloned here to avoid depending on o-blog package.
(defun org-bibedit-get-header (header &optional all)
  "Get HEADER from blog buffer as defined in BLOG global context
variable.

Returns only fist match except if ALL is defined."
  (with-current-buffer
      (current-buffer)
;;; Normally 'BLOG is not bound unless under special config of o-blog.
;;;      (if (boundp 'BLOG)
;;;          (ob:blog-buffer BLOG)
;;;        (current-buffer))
    (save-excursion
      (save-restriction
        (save-match-data
          (widen)
          (goto-char (point-min))
          (let (values)
            (while (re-search-forward (format "^#\\+%s:?[ \t]*\\(.*\\)" header) nil t)
              (add-to-list 'values (substring-no-properties (match-string 1))))
            (if all
                values
              (car values))))))))

(provide 'org-bibedit)
;;; org-bibedit.el ends here

