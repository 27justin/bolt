;;; bolt-insert.el --- bolt insert mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Andreas Lacoste

;; Author: Justin Andreas Lacoste <me@justin.cx>
;; URL: https://github.com/27justin/bolt
;; Version: 0.1

(defun bolt-edit-line-at-beginning ()
  "Edit the current line at it's indentation (or beginning)"
  (interactive)
  (back-to-indentation)
  (bolt-insert-mode 1))

(defun bolt-delete ()
  (interactive)
  (if (region-active-p)
      (progn (delete-region (region-beginning) (region-end))
             (setq deactivate-mark nil))
    (delete-char 1)))

(defun bolt-delete-backward ()
  (interactive)
  (if (region-active-p)
      (progn (delete-region (region-beginning) (region-end))
             (setq deactivate-mark nil))
    (delete-char -1)))

(defun bolt-change ()
  (interactive)
  (if (region-active-p)
      (progn (delete-region (region-beginning) (region-end))
             (setq deactivate-mark nil))
    (delete-char 1)
    (bolt-switch-mode 'bolt-insert-mode)))

(defun bolt-open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command)
  (bolt-switch-mode 'bolt-insert-mode))

(defun bolt-open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command)
  (bolt-switch-mode 'bolt-insert-mode))

(defvar bolt-insert-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") #'bolt-switch-to-normal-mode)
    (define-key map (kbd "<esc>") #'bolt-switch-to-normal-mode)
    map)
  "Keymap for bolt insert mode.")

(defun bolt-insert-define-key (&rest bindings)
  (dolist (binding bindings)
    (define-key bolt-insert-mode-map (kbd (car binding)) (cdr binding))))

(define-minor-mode bolt-insert-mode
  "Bolt insert mode."
  :init-value nil
  :lighter " Bolt Insert"
  :keymap bolt-insert-mode-map
  (when (bound-and-true-p bolt-insert-mode)
    (bolt-ensure-mode 'bolt-insert-mode)
    (setq cursor-type 'bar)))

(add-to-list 'bolt-modes 'bolt-insert-mode)

(provide 'bolt-insert)
