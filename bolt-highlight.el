;;; bolt-visual.el --- bolt visual mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Andreas Lacoste

;; Author: Justin Andreas Lacoste <me@justin.cx>
;; URL: https://github.com/27justin/bolt
;; Version: 0.1


;;; Commentary:

;; This file provides a repeatable overlay for visualizing the current
;; match, or similar, in a buffer.
;; By providing a numbered overlay, the user can easily determine
;; how many jumps he needs to where he wants to go.
;; Take for example the =bolt-find= =f= command. It will jump to the current match
;; and highlight any subsequent matches, the user can then just hit the number
;; to repeat the jump.


;;; Code:



(defvar bolt-highlight-overlays '()
  "The current visual overlays.")

(defface bolt-highlight-face
  '((t :background "gray" :foreground "black"))
  "Face for highlighting the current match."
  :group 'bolt)

(defun bolt-highlight-overlay (positions)
  "Create a visual overlay from START to END."
  (when (length> bolt-highlight-overlay 0)
        (mapc 'delete-overlay bolt-highlight-overlay))

        (dotimes (i (length positions))
            (let* ((start (nth i positions))
                   (end (+ start 1))
                   (overlay (make-overlay start end)))
                (overlay-put overlay 'before-string (propertize (format " %d " (+ i 1)) 'face 'bolt-highlight-face))
                (push overlay bolt-highlight-overlays))))

(defun bolt-highlight-overlay-delete ()
    "Delete the visual overlays."
    (interactive)
    (if bolt-highlight-overlays
        (mapc #'delete-overlay bolt-highlight-overlays)))

(provide 'bolt-highlight)
