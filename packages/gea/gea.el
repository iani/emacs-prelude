;;; package --- Summary

;;; Commentary:

;;; From: https://github.com/igoumeninja/ofxLua

;;; Code:

;; Use emacs as interpreter
;;
;; Aris Bezas 8:27 AM Apr 18, 2013
;; ========================================
;; Iannis Zannos Fri, Mar  6 2015, 12:09 EET
;;==========================================

;; Send OSC messages with Emacs.
;; Include osc.el from http://delysid.org/emacs/osc.html at Emacs.app/Contents/

;; Create SuperColider and openFrameworks OSC clients

(require 'osc)

(defvar sc-client (osc-make-client
                   "127.0.0.1" 57120)
  "Client to local sclang port.")

(defvar of-client (osc-make-client "127.0.0.1" 12345)
  "Client to local openFrameworks port.")

(defvar gea-client (osc-make-client "127.0.0.1" 12345)
  "Client to local openFrameworks port.
Note: In the future, gea may listen to a different port than oF.")

;;===========================================
(defun gea-send-test-osc ()
  "Just a test func."
  (interactive)
  (osc-send-message sc-client "1,2,3 Test"))

(defun gea-add-script ()
  "Add current lua script to oF."
  (interactive)
  (save-buffer)
  (osc-send-message gea-client "addScript" (buffer-file-name)))

(defun gea-update-script ()
  "Resend edited lua script to oF."
  (interactive)
  (save-buffer)
  (osc-send-message gea-client "updateScript" (buffer-file-name)))

(defun gea-shader ()
  "Reload shaders in oF."
  (interactive)
  (save-buffer)
  (osc-send-message gea-client "updateShaders"))

(defun gea-doshader ()
  "Activate shaders.
I.e. Make shaders active in current graphics synthesis in oF."
  (interactive)
  (save-buffer)
  (osc-send-message gea-client "doShader"))

(global-set-key (kbd "s-<return>") 'gea-update-script)
(global-set-key (kbd "C-M-+") 'gea-add-script)

;; (menu-bar-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'gea)
;;; gea.el ends here
