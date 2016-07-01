;;; zoutline.el --- Simple outline interface. -*- lexical-binding: t -*-

;; Copyright (C) 2016 Oleh Krehel

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
(require 'outline)

(defun zo-up (arg)
  "Move ARG times up by outline."
  (interactive "p")
  (let ((i 0)
        out)
    (ignore-errors
      (while (<= (cl-incf i) arg)
        (outline-backward-same-level 1)
        t
        (setq out t)))
    out))

(defun zo-down (arg)
  "Move ARG times down by outline."
  (interactive "p")
  (let ((pt 0)
        (i 0)
        (outline-ok t))
    (while (and (<= (cl-incf i) arg)
                (> (point) pt)
                outline-ok)
      (setq pt (point))
      (condition-case nil
          (outline-forward-same-level 1)
        (error (setq outline-ok nil))))
    (unless (= i (1+ arg))
      (message "End reached after %s headings" i))))

(defvar zo-lvl-re [nil
                   "\n\\* "
                   "\n\\*\\{2\\} "
                   "\n\\*\\{3\\} "
                   "\n\\*\\{4\\} "
                   "\n\\*\\{5\\} "
                   "\n\\*\\{6\\} "
                   "\n\\*\\{7\\} "])

(defun zo-down-visible (&optional arg)
  "Move ARG times down by outline."
  (interactive "p")
  (setq arg (or arg 1))
  (let ((lvl (org-outline-level))
        res)
    (if (= lvl 1)
        (re-search-forward (aref zo-lvl-re lvl) nil t arg)
      (let ((end (save-excursion
                   (or (re-search-forward (aref zo-lvl-re (1- lvl)) nil t)
                       (point-max)))))
        (when (setq res (re-search-forward (aref zo-lvl-re lvl) end t arg))
          (reveal-post-command))))
    (when res
      (beginning-of-line)
      (point))))

(defun zo-left (arg)
  (outline-up-heading arg))

(provide 'zoutline)
