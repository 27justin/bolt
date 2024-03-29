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
