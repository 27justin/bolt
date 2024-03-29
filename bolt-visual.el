;;; bolt-visual.el --- bolt visual mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Andreas Lacoste

;; Author: Justin Andreas Lacoste <me@justin.cx>
;; URL: https://github.com/27justin/bolt
;; Version: 0.1

(defun bolt-exit-visual-mode ()
       (interactive)
       (deactivate-mark)
       (remove-hook 'deactivate-mark-hook 'bolt-exit-visual-mode t)
       (bolt-switch-mode 'bolt-normal-mode))

(defun bolt-select-line ()
       (interactive)
       (bolt-visual-mode 1)
       (beginning-of-line)
       (exchange-point-and-mark)
       (end-of-line))

(defun bolt-select-word ()
       (interactive)
       (bolt-visual-mode 1)
       (let ((bounds (bounds-of-thing-at-point 'word)))
         (goto-char (car bounds))
         (exchange-point-and-mark)
         (goto-char (cdr bounds))))

(defun bolt-insert-at-lower ()
       (interactive)
       (bolt-visual-mode -1)
       (bolt-insert-mode 1)
       ;; If the mark is active, and mark is before point, go to mark
         (when (and (bound-and-true-p transient-mark-mode) mark-active (< (mark) (point)))
            (goto-char (mark))))

(defun bolt-insert-at-upper ()
       (interactive)
       (bolt-visual-mode -1)
       (bolt-insert-mode 1)
         (when (and (bound-and-true-p transient-mark-mode) mark-active (> (mark) (point)))
            (goto-char (mark))))

(defun bolt-expand-line ()
       "Expands the visual selection to the next line.
       If the point is at the end of the mark, it will expand to the next line.
       If the point is at the beginning of the mark, it will expand to the previous line."
       (interactive)
       (if (> (point) (mark))
           (forward-line)
           (forward-line -1)))

(defvar bolt-visual-mode-hook nil)
(defvar bolt-visual-mode-map
  (let ((map (make-keymap)))
       (suppress-keymap map)
       (define-key map (kbd "x") #'exchange-point-and-mark)
       (define-key map (kbd "y") #'kill-ring-save)
       (define-key map (kbd "h") #'bolt-backward-char)
       (define-key map (kbd "j") #'bolt-forward-line)
       (define-key map (kbd "k") #'bolt-backward-line)
       (define-key map (kbd "l") #'bolt-forward-char)
       (define-key map (kbd "w") #'bolt-select-word)
       (define-key map (kbd "b") #'bolt-backward-word)
       (define-key map (kbd "e") #'bolt-forward-word)
       (define-key map (kbd "g") #'bolt-exit-visual-mode)
       (define-key map (kbd "i") #'bolt-insert-at-lower)
       (define-key map (kbd "a") #'bolt-insert-at-upper)
       (define-key map (kbd "c") #'bolt-change)
       (define-key map (kbd "d") #'bolt-delete)

       (define-key map (kbd "s") #'bolt-expand-line)
    map))

(define-minor-mode bolt-visual-mode
  "Visual mode for bolt"
  :init-value nil
  :lighter " Visual"
  :keymap bolt-visual-mode-map
  (bolt-ensure-mode 'bolt-visual-mode)
  (add-hook 'deactivate-mark-hook 'bolt-exit-visual-mode nil t)
  (if bolt-visual-mode
      (progn
        (set-mark-command nil)
        (setq cursor-type 'box))))

(add-to-list 'bolt-modes 'bolt-visual-mode)

(provide 'bolt-visual)
