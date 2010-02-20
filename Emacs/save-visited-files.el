;; Save-visited-files.el - save opened files across sessions

;; Copyright (C) 2009 Nathaniel Flath <nflath@gmail.com>
;; Version: 1.2
;;
;; Commentary
;; Save-visited-files is a lightweight version of Desktop.el that
;; only save the files you have open(currently).  This was created because I
;; couldn't ever get Desktop to work and wanted to persist open files across
;; sessions.  This file is the result.
;;
;; To install, put this file somewhere in your load-path and add the following
;; to your .emacs file:
;;
;; (require 'save-visited-files)
;; (turn-on-save-visited-files-mode)
;;
;; This will load the set of saved files on startup, as well as updating this
;; list whenever the auto-save-timer is run.  This does not wait to save on
;; closing emacs because I wanted it to be useful even if emacs crashed.  To
;; save the visited files at any time, you can call M-x save-visited-files-save.
;; M-x save-visited-files-restore will open all files saved this way.  To turn
;; off the saving of files, you need to run (turn-off-save-visited-files-mode)
;;
;;
;; Changelog:
;; 1.2
;;  * Changed default value of save-visited-files-location to ~/.emacs.d/emacs-visisted-files
;;  * Improvements/rewriting by Jonathan Kotta
;;  ** Checks save-visited-files-location is writable, and gives a message if not
;;  ** Changed to use define-minor-mode
;;  ** Moved (setq save-visited-files-already-restored t) to the end of
;;  ** save-visited-files-restore from save-visited-files-mode.
;;  ** Doesn't print a message in the echo area every time it saves the file list.
;; 1.1
;;  * Improvements/rewriting by Ryan Thomson
;;  ** Use auto-save-hook instead of a periodic timer
;;  ** More consistent naming conventions
;;  ** Customization ability via M-x customize-group save-visited-files
;;  ** Better handling of the temp buffer
;; 1.0
;;  * Initial Release
;;
;; Code:


(defcustom save-visited-files-location "~/.emacs.d/emacs-visited-files"
  "Location of the file that contains the list of previously visited files"
  :type 'file
  :group 'save-visited-files)

(defcustom save-visited-files-auto-restore nil
  "If t, restore visited files the first time save-visited-files-mode is activated"
  :type 'boolean
  :group 'save-visited-files)

(defvar save-visited-files-already-restored nil
  "If t, then files have already been restored")

(defun save-visited-files-save (&optional location)
  "Save the list of currently visited files"
  (interactive)
  (save-window-excursion
    (setq location (or location save-visited-files-location))

    (if (not (file-writable-p location))
        (message "Save Visited Files: cannot write to %s" location)
      (switch-to-buffer "*Save Visited*")
      (ignore-errors
        (erase-buffer)
        (mapcar '(lambda (x) (insert x "\n"))
                (remove-if '(lambda (x)
                              (or (string-equal location x)
                                  (eq nil x))) (mapcar 'buffer-file-name (buffer-list))))
        (with-temp-message ""
          (write-file location nil)))
      (kill-buffer (get-buffer "*Save Visited*")))))

(defun save-visited-files-restore (&optional location)
  "Restore all files that were saved by save-visited-files-save."
  (interactive)
  (save-window-excursion
    (setq location (or location save-visited-files-location))
    (find-file location)
    (ignore-errors
      (save-window-excursion
        (beginning-of-buffer)
        (while (not (eq (point) (point-max)))
          (let ((point (point)))
            (end-of-line)
            (ignore-errors
              (save-window-excursion
                (find-file (buffer-substring point (point)))))
            (next-line)
            (beginning-of-line)))))
    (kill-buffer (current-buffer))
    (setq save-visited-files-already-restored t)))

(define-minor-mode save-visited-files-mode
  "Minor mode to automatically save a list of all open files, and
optionally open all files from such a list at startup."
  :init-value nil
  :global t
  :group 'save-visited-files

  (if save-visited-files-mode
      ;; activate
      (progn
        (add-hook 'auto-save-hook 'save-visited-files-save)
        (add-hook 'kill-emacs-hook 'save-visited-files-save)
        (unless save-visited-files-already-restored
          (when save-visited-files-auto-restore
            (save-visited-files-restore)))
        (message "Save visited files mode enabled"))
    ;; deactivate
    (progn
      (remove-hook 'auto-save-hook 'save-visited-files-save)
      (remove-hook 'kill-emacs-hook 'save-visited-files-save)
      (message "Save visited files mode disabled"))))

(defun turn-on-save-visited-files-mode ()
  "Turns save-visited-files-mode on"
  (interactive)
  (save-visited-files-mode 1))

(defun turn-off-save-visited-files-mode ()
  "Turns save-visited-files-mode off"
  (interactive)
  (setq save-visited-files-mode nil))

(provide 'save-visited-files)
