;;; omarchy-theme.el --- Sync Emacs with Omarchy themes -*- lexical-binding: t; -*-

;; Drop this file somewhere on your `load-path` (it lives at
;; ~/.config/emacs/omarchy-theme.el after a normal Omarchy install) and add:
;;   (require 'omarchy-theme)
;;   (omarchy-theme-follow)  ; optional watcher
;;   (omarchy-theme-apply)   ; apply current theme on startup

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
        (message "[omarchy] No Emacs theme metadata found at %s" omarchy-theme-metadata-file)
      (when load-path-entry
        (add-to-list 'custom-theme-load-path (expand-file-name load-path-entry)))
      (when package
        (when (fboundp 'package-initialize)
          (condition-case nil
              (package-initialize)
            (error nil)))
        (require package nil 'noerror))
      (when (and variant-var variant)
        (set variant-var variant))
      (mapc #'disable-theme custom-enabled-themes)
      (condition-case err
          (progn
            (load-theme theme t)
            (message "[omarchy] Applied Emacs theme %s" theme)
            t)
        (error
         (message "[omarchy] Failed to load theme %s (%s)" theme err)
         nil)))))

(defun omarchy-theme--schedule-apply (_event)
  "Debounce theme updates triggered by file-notify EVENT."
  (when omarchy-theme--watch-timer
    (cancel-timer omarchy-theme--watch-timer))
  (setq omarchy-theme--watch-timer
        (run-with-timer omarchy-theme-watch-delay nil #'omarchy-theme-apply)))

(defun omarchy-theme-follow ()
  "Start watching the Omarchy theme metadata and re-apply on changes."
  (interactive)
  (unless (featurep 'file-notify)
    (user-error "file-notify is not available in this Emacs build"))
  (unless (file-exists-p (file-name-directory omarchy-theme-metadata-file))
    (make-directory (file-name-directory omarchy-theme-metadata-file) t))
  (unless omarchy-theme--watch-handle
    (setq omarchy-theme--watch-handle
          (file-notify-add-watch
           (file-name-directory omarchy-theme-metadata-file)
           '(change attribute-change)
           #'omarchy-theme--schedule-apply)))
  (message "[omarchy] Watching %s for theme changes" omarchy-theme-metadata-file))

(provide 'omarchy-theme)

;;; omarchy-theme.el ends here
