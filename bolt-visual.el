;;; bolt-visual.el --- bolt visual mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Andreas Lacoste

;; Author: Justin Andreas Lacoste <me@justin.cx>
;; URL: https://github.com/27justin/bolt
;; Version: 0.1

(defun bolt-exit-visual-mode ()
       (interactive)
       (deactivate-mark)
       (remove-hook 'deactivate-mark-hook 'bolt-exit-visual-mode t)
       (when bolt-visual-mode
             (bolt-switch-mode 'bolt-normal-mode)))

(defun bolt-select-line ()
       (interactive)
       (bolt-visual-mode 1)
       (beginning-of-line)
       (exchange-point-and-mark)
       (end-of-line)
       (setq bolt-repeatable-command `(bolt-expand-line)))

(defun bolt-select-word ()
       (interactive)
       (bolt-visual-mode 1)
       (let ((bounds (bounds-of-thing-at-point 'word)))
         (goto-char (car bounds))
         (exchange-point-and-mark)
         (goto-char (cdr bounds))))

(defun bolt-select-symbol ()
  "Select the symbol (word-like entity) at point, ignoring punctuation and including underscores."
  (interactive)
  (bolt-visual-mode 1)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (goto-char (car bounds))
      (set-mark (cdr bounds)))))

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

(defun bolt-visual-delete ()
       (interactive)
       (bolt-delete)
       (bolt-exit-visual-mode))

(defvar bolt-visual-mode-hook nil)
(defvar bolt-visual-mode-map
  (let ((map (make-keymap)))
       (suppress-keymap map)
       (define-key map (kbd "x") #'exchange-point-and-mark)
       (define-key map (kbd "y") #'kill-ring-save)
       (define-key map (kbd "h") #'bolt-backward-char)
       (define-key map (kbd "j") #'bolt-forward-line)
       (define-key map (kbd "k") #'bolt-backward-line)
       (define-key map (kbd "J") #'forward-paragraph)
       (define-key map (kbd "K") #'backward-paragraph)
       (define-key map (kbd "l") #'bolt-forward-char)
       (define-key map (kbd "w") #'bolt-select-word)
       (define-key map (kbd "W") #'bolt-select-symbol)
       (define-key map (kbd "b") #'bolt-backward-word)
       (define-key map (kbd "e") #'bolt-forward-word)
       (define-key map (kbd "g") #'bolt-exit-visual-mode)
       (define-key map (kbd "i") #'bolt-insert-at-lower)

       ;; Commented out, I don't really use "a" anywhere, replaced with action transient
       ;; (define-key map (kbd "a") #'bolt-insert-at-upper)

       (define-key map (kbd "c") #'bolt-change)
       (define-key map (kbd "d") #'bolt-visual-delete)

       (define-key map (kbd "s") #'bolt-expand-line)

       (define-key map (kbd "u") #'undo)

       (define-key map (kbd "f") #'bolt-find)
       (define-key map (kbd "F") #'bolt-find-backward)

       (define-key map (kbd "m") #'back-to-indentation)

       (define-key map (kbd "1") #'bolt-numeric-argument-1)
       (define-key map (kbd "2") #'bolt-numeric-argument-2)
       (define-key map (kbd "3") #'bolt-numeric-argument-3)
       (define-key map (kbd "4") #'bolt-numeric-argument-4)
       (define-key map (kbd "5") #'bolt-numeric-argument-5)
       (define-key map (kbd "6") #'bolt-numeric-argument-6)
       (define-key map (kbd "7") #'bolt-numeric-argument-7)
       (define-key map (kbd "8") #'bolt-numeric-argument-8)
       (define-key map (kbd "9") #'bolt-numeric-argument-9)
       (define-key map (kbd "0") #'bolt-numeric-argument-0)

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
