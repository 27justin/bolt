;;; Bolt --- Instinctive Modal Editing -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Andreas Lacoste

;; Author: Justin Andreas Lacoste <me@justin.cx>
;; URL: https://github.com/27justin/bolt
;; Version: 0.1
;; Package-Requires: ((emacs "?"))
;; Keywords: modal, editing, modal

;;; Commentary:

;; This package implements an unobtrusive, instinctive, and powerful
;; modal editing system for Emacs.

;;; Global Variable:
(defvar bolt-modes '())

;;; Requirements
(require 'bolt-visual)
(require 'bolt-normal)
(require 'bolt-insert)

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
  This may look something like `insert` or `visual`.  As such, this
  function will disable all other modes and then enable `<mode>'"
   (interactive)
   (dolist (m bolt-modes)
     (when (and (symbol-value m) (not (eq m mode)))
       (funcall m -1)))
    (funcall mode 1))

(defun bolt-ensure-mode (mode)
   "Ensures that only MODE is enabled.  This function will disable all other
   modes without enabling MODE itself, this is intended to be used in
   minor mode bodies."
    (dolist (m bolt-modes)
        (when (and (symbol-value m) (not (eq m mode)))
            (funcall m -1))))

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

(defun bolt-switch-to-normal-mode ()
  "Switch to normal mode."
  (interactive)
  ;; Disables any other bolt mode
  (when (bound-and-true-p bolt-visual-mode)
    (bolt-visual-mode -1))
  (when (bound-and-true-p bolt-insert-mode)
    (bolt-insert-mode -1))
  (bolt-normal-mode 1))

(define-globalized-minor-mode bolt-mode bolt-normal-mode
  (not (minibufferp)))

(provide 'bolt)
