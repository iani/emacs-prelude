(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(calendar-latitude 39.619)
 '(calendar-longitude 19.919)
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("a31c86c0a9ba5d06480b02bb912ae58753e09f13edeb07af8927d67c3bb94d68" "2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" "442c946bc5c40902e11b0a56bd12edc4d00d7e1c982233545979968e02deb2bc" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "11d069fbfb0510e2b32a5787e26b762898c7e480364cbc0779fe841662e4cf5d" default)))
 '(guru-global-mode nil)
 '(hl-sexp-background-colors (quote ("gray0" "#0f003f")))
 '(icicle-buffer-configs
   (quote
    (("icybufferconfig1" "Event*" "\\.ck" nil nil nil)
     ("All" nil nil nil nil icicle-case-string-less-p)
     ("Files" nil nil
      (lambda
        (bufname)
        (buffer-file-name
         (get-buffer bufname)))
      nil icicle-case-string-less-p)
     ("Files and Scratch" nil nil
      (lambda
        (bufname)
        (buffer-file-name
         (get-buffer bufname)))
      ("*scratch*")
      icicle-case-string-less-p)
     ("All, *...* Buffers Last" nil nil nil nil icicle-buffer-sort-*\.\.\.*-last))))
 '(org-agenda-files nil)
 '(org-attach-directory "./attachments")
 '(org-export-backends (quote (ascii html icalendar latex md)))
 '(org-export-with-sub-superscripts (quote {}))
 '(org-mobile-directory "~/Dropbox/Apps/MobileOrg")
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-crypt org-docview org-gnus org-id org-info org-jsinfo org-habit org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-mac-iCal)))
 '(org-tags-column -90)
 '(tab-width 4)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
