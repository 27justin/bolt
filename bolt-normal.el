;;; bolt-normal.el --- bolt normal mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Andreas Lacoste

;; Author: Justin Andreas Lacoste <me@justin.cx>
;; URL: https://github.com/27justin/bolt
;; Version: 0.1

(defvar bolt-normal-mode-map
        (let ((map (make-keymap)))
        (suppress-keymap map t)
       map)
  "Keymap for bolt normal mode.")


(defun bolt-select-inside (&optional char)
  "Select the contents inside a given pair of delimiters based on the current nesting level.
CHAR is the delimiter character input by the user."
  (interactive "cSelect inside delimiter: ")
  (let* ((delimiter-pairs '((?\( . ?\)) (?\{ . ?\}) (?\[ . ?\]) (?\" . ?\") (?\' . ?\') (?\< . ?\>)))
         (reverse-pairs '((?\) . ?\() (?\} . ?\{) (?\] . ?\[) (?\> . ?\<)))
         (delimiter (or (cdr (assoc char reverse-pairs)) char))  ;; Normalize to opening delimiter
         (matching-delimiter (cdr (assoc delimiter delimiter-pairs)))
         (nesting-level 0)
         (start (point))
         (end (point)))

    ;; Find the opening delimiter if necessary
    (unless (looking-at (regexp-quote (char-to-string delimiter)))
      (setq nesting-level 1)
      (while (and (not (bobp)) (> nesting-level 0))
        (backward-char)
        (cond
         ((eq (char-after) delimiter) (setq nesting-level (1- nesting-level)))
         ((eq (char-after) matching-delimiter) (setq nesting-level (1+ nesting-level))))))

    ;; Set the region at the point of the opening delimiter
    (setq start (point))
    ;; Move forward to the matching closing delimiter
    (setq nesting-level 1)
    (while (and (not (eobp)) (> nesting-level 0))
      (forward-char)
      (cond
       ((eq (char-after) matching-delimiter) (setq nesting-level (1- nesting-level)))
       ((eq (char-after) delimiter) (setq nesting-level (1+ nesting-level)))))

    ;; Set the region after the closing delimiter
    (setq end (point))

    ;; Select the text inside the delimiters
    (bolt-visual-mode)
    (goto-char (1+ start))
    (exchange-point-and-mark)
    ;;(goto-mark end)
    ))


;; -------------

(defun bolt-normal-define-key (&rest bindings)
  (dolist (binding bindings)
    (define-key bolt-normal-mode-map (kbd (car binding)) (cdr binding))))

(define-minor-mode bolt-normal-mode
  "Bolt normal mode."
  :init-value nil
  :lighter " Bolt Normal"
  :keymap bolt-normal-mode-map
  (when (bound-and-true-p bolt-normal-mode)
;;    (bolt-ensure-mode 'bolt-normal-mode)
    (setq cursor-type 'box)))

(define-global-minor-mode bolt-global-mode
  bolt-normal-mode
  (lambda ()
    (unless (minibufferp)
      (bolt-normal-mode))))

(add-to-list 'bolt-modes 'bolt-normal-mode)

(provide 'bolt-normal)
