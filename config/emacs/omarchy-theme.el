;;; omarchy-theme.el --- Sync Emacs with Omarchy themes -*- lexical-binding: t; -*-

;;; Commentary:

;; Drop this file somewhere on your `load-path` (it lives at
;; ~/.config/emacs/omarchy-theme.el after a normal Omarchy install) and add:
;;   (require 'omarchy-theme)
;;   (omarchy-theme-follow)  ; optional watcher
;;   (omarchy-theme-apply)   ; apply current theme on startup

;;; Code:

(require 'json)

(defgroup omarchy-theme nil
  "Follow the active Omarchy desktop theme inside Emacs."
  :group 'faces)

(defcustom omarchy-theme-metadata-file
  (expand-file-name "~/.config/omarchy/current/theme/emacs.json")
  "Path to the JSON metadata describing the current Omarchy theme for Emacs."
  :type 'file
  :group 'omarchy-theme)

(defcustom omarchy-theme-watch-delay 0.25
  "Seconds to debounce consecutive theme change events."
  :type 'number
  :group 'omarchy-theme)

(defvar omarchy-theme--watch-handle nil)
(defvar omarchy-theme--watch-timer nil)
(defvar omarchy-theme--last-applied nil
  "Metadata of the last applied theme, used to prevent redundant reloads.")

(defun omarchy-theme--read-metadata ()
  "Return the current theme metadata alist, or nil if missing."
  (when (file-exists-p omarchy-theme-metadata-file)
    (with-temp-buffer
      (insert-file-contents omarchy-theme-metadata-file)
      (json-parse-buffer :object-type 'alist :null-object nil :false-object nil))))

(defun omarchy-theme--normalize-symbol (value)
  "Convert VALUE to a symbol when VALUE is a string; otherwise return VALUE."
  (cond
   ((symbolp value) value)
   ((stringp value) (intern value))
   (t value)))

(defun omarchy-theme-apply (&optional metadata)
  "Apply the Omarchy theme described by METADATA or the metadata file.

METADATA should be an alist with keys: theme, package, variant, variantVariable,
and loadPath. Missing keys are ignored gracefully."
  (interactive)
  (let* ((data (or metadata (omarchy-theme--read-metadata)))
         (theme (omarchy-theme--normalize-symbol (alist-get 'theme data)))
         (package (omarchy-theme--normalize-symbol (alist-get 'package data)))
         (variant (omarchy-theme--normalize-symbol (alist-get 'variant data)))
         (variant-var (omarchy-theme--normalize-symbol (alist-get 'variantVariable data)))
         (load-path-entry (alist-get 'loadPath data)))
    (if (null data)
        (message "[Omarchy] No Emacs theme metadata found at %s" omarchy-theme-metadata-file)
      (when load-path-entry
        (add-to-list 'custom-theme-load-path (expand-file-name load-path-entry)))
      (when package
        (require package nil 'noerror))
      (when (and variant-var variant)
        (set variant-var variant))
      (mapc #'disable-theme custom-enabled-themes)
      (condition-case err
          (progn
            (load-theme theme t)
            (message "[Omarchy] Applied Emacs theme %s" theme)
            t)
        (error
         (message "[Omarchy] Failed to load theme %s (%s)" theme err)
         nil)))))

(defun omarchy-theme--schedule-apply (_event)
  "Apply theme only if metadata changed, triggered by file-notify EVENT."
  (when omarchy-theme--watch-timer
    (cancel-timer omarchy-theme--watch-timer))
  (setq omarchy-theme--watch-timer
        (run-with-timer omarchy-theme-watch-delay nil
                        (lambda ()
                          (let ((new-metadata (omarchy-theme--read-metadata)))
                            (unless (equal new-metadata omarchy-theme--last-applied)
                              (setq omarchy-theme--last-applied new-metadata)
                              (omarchy-theme-apply new-metadata)))))))

(defun omarchy-theme-follow ()
  "Start watching the Omarchy theme metadata and re-apply on change."
  (interactive)
  (unless (featurep 'filenotify)
    (user-error "File notification is not available in this Emacs build"))
  (let* ((theme-dir (file-name-directory omarchy-theme-metadata-file))
         (watch-dir (file-name-directory (directory-file-name theme-dir))))
    (unless (file-exists-p theme-dir)
      (make-directory theme-dir t))
    ;; Watch the parent directory (current/) to catch symlink changes
    ;; when Omarchy switches themes by atomically replacing the theme/ symlink
    (unless omarchy-theme--watch-handle
      (setq omarchy-theme--watch-handle
            (file-notify-add-watch
             watch-dir
             '(change attribute-change)
             #'omarchy-theme--schedule-apply)))
    (message "[Omarchy] Watching %s for theme changes" watch-dir)))

(provide 'omarchy-theme)

;;; omarchy-theme.el ends here
