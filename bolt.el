;;; Bolt --- Instinctive Modal Editing -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Andreas Lacoste

;; Author: Justin Andreas Lacoste <me@justin.cx>
;; URL: https://github.com/27justin/bolt
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: modal, editing, modal

;;; Commentary:

;; This package implements an unobtrusive, instinctive, and powerful
;; modal editing system for Emacs.

;;; Global Variable:
(defvar bolt-modes '())

(defvar bolt-numeric-arg-stack nil)

(defvar bolt-try-expand '()
"List of hooks to run when trying to expand a numeric argument.
Upon successful expansion, the hook shall set the variable `bolt-numeric-arg-stack' to 0.")

(defvar bolt-repeatable-command nil
"Repeatable command to be executed after a numeric argument is expanded.
This variable has to contain Elisp expression that can be evaluated.")

(defvar bolt-disabled-modes '(
        dired-mode
        magit-mode
        eshell-mode
        shell-mode
        term-mode
        vterm-mode
        comint-mode
        magit-diff-mode
        magit-status-mode
        magit-log-mode
        org-agenda-mode
        minibuffer-mode
        mu4e-main-mode
        calendar-mode
        grep-mode
        occur-mode
        compilation-mode
        vundo-mode
        calc-mode
        mu4e-main-mode
        mu4e-headers-mode
        mu4e-view-mode
        xref--xref-buffer-mode
        git-rebase-mode
        Buffer-menu-mode))



;;; Requirements
(require 'bolt-visual)
(require 'bolt-normal)
(require 'bolt-insert)

(require 'bolt-highlight)

;;; Code:

(defun bolt-forward-char ()
  "Move point forward one character."
  (interactive)
  (forward-char))

(defun bolt-backward-char ()
  "Move point backward one character."
  (interactive)
  (backward-char))

(defun bolt-forward-line ()
  "Move point forward one line."
  (interactive)
  (next-line))

(defun bolt-backward-line ()
  "Move point backward one line."
  (interactive)
  (previous-line))

(defun bolt-switch-mode (mode)
  "Switch to MODE, where MODE is a symbol that represents a bolt mode.
  As such, this function will disable all other modes and then enable `<mode>'"
   (interactive)
   (dolist (m bolt-modes)
     (when (and (symbol-value m) (not (eq m mode)))
       (funcall m -1)))
    (funcall mode 1)
    (bolt-highlight-overlay-delete))

(defun bolt-ensure-mode (mode)
   "Ensures that only MODE is enabled.  This function will disable all other
   modes without enabling MODE itself, this is intended to be used in
   minor mode bodies."
    (dolist (m bolt-modes)
        (when (and (symbol-value m) (not (eq m mode)))
            (funcall m -1)))
     (bolt-highlight-overlay-delete))

(defun bolt-forward-word ()
  "Move point forward one word."
  (interactive)
  (forward-word))

(defun bolt-backward-word ()
  "Move point backward one word."
  (interactive)
  (backward-word))

(defun bolt-paste ()
  "Paste the contents of the kill ring."
  (interactive)
  (yank))

(defun bolt--find (char times)
       "This function searches for a character CHAR TIMES times.
       Returning all the positions in a list.
       TIMES may be negative to search backwards."
       (save-excursion
         (let ((positions '())
               (direction (if (< times 0) -1 1)))
            (dotimes (i (abs times))
              (let ((pos (search-forward (char-to-string char) nil t direction)))
                 (when pos
                   (push pos positions))))
              (reverse positions))))

(defun bolt-find (&optional char backwards)
       "Read a character from the user and move point to the next."
       (interactive)
       (let* ((character (if char char (read-char)))
              (positions (bolt--find character (if backwards -10 10))))
              (when positions
                  (goto-char (car positions))
                         (bolt-highlight-overlay (cdr positions)))
                (setq bolt-repeatable-command `(progn (bolt-highlight-overlay-delete) (bolt-find ,character ,backwards)))))

(defun bolt-find-backward (&optional char)
       (interactive)
       (bolt-find char t))

(defmacro bolt-expand-numeric-argument (number)
          `(defun ,(intern (format "bolt-numeric-argument-%s" number)) ()
                 (interactive)
                 (setq bolt-numeric-arg-stack (+ ,number))
                 (run-hooks 'bolt-try-expand)))

(bolt-expand-numeric-argument 0)
(bolt-expand-numeric-argument 1)
(bolt-expand-numeric-argument 2)
(bolt-expand-numeric-argument 3)
(bolt-expand-numeric-argument 4)
(bolt-expand-numeric-argument 5)
(bolt-expand-numeric-argument 6)
(bolt-expand-numeric-argument 7)
(bolt-expand-numeric-argument 8)
(bolt-expand-numeric-argument 9)

(defun bolt-repeat-command ()
       "Used for numeric arguments, will be run on bolt-try-expand and, if applicable, will run
       `bolt-repeatable-command' `bolt-numeric-arg-stack' times."
       (message "Repeatable Command: %s" bolt-repeatable-command)
       (dotimes (i bolt-numeric-arg-stack)
              (eval bolt-repeatable-command)))

(add-hook 'bolt-try-expand #'bolt-repeat-command)

(defun bolt-zap (character)
  "Zap to the nearest CHARACTER. Indiscriminate of direction."
  (interactive "c")
  (let ((cur (point))
        (left nil)
        (right nil))
    (save-excursion
      (setq left (search-backward (char-to-string character) nil t)))
    (save-excursion
      (setq right (search-forward (char-to-string character) nil t)))
    (if (and left right)
        (progn
          (if (< (abs (- cur left)) (abs (- cur right)))
              (goto-char left)
            (goto-char (- right 1))))
      (if left (goto-char left)
        (if right (goto-char right))))))

(defun bolt-switch-to-normal-mode ()
  "Switch to normal mode."
  (interactive)
  ;; Disables any other bolt mode
  (when (bound-and-true-p bolt-visual-mode)
    (bolt-visual-mode -1))
  (when (bound-and-true-p bolt-insert-mode)
    (bolt-insert-mode -1))
  (bolt-normal-mode 1))

(define-globalized-minor-mode bolt-global-mode bolt-normal-mode
(lambda()
    (when (not (seq-some (lambda(mode) (derived-mode-p mode)) bolt-disabled-modes))
    (bolt-normal-mode 1))))


(provide 'bolt)
