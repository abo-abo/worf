;;; worf.el --- A warrior does not press so many keys! (in org-mode)

;; Copyright (C) 2014 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/worf
;; Version: 0.1
;; Package-Requires: ((helm "1.5.3") (ace-jump-mode "2.0"))
;; Keywords: lisp

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

;;; Commentary:
;;
;; This extension allows to call commands with unprefixed
;; alpha-numeric keys when point is positioned at start of heading.
;;
;; It takes the idea of https://github.com/abo-abo/lispy for Lisp
;; and adapts it to org-mode.
;;
;; It's similar to what `org-use-speed-commands' provides, except
;; it's a minor mode.

;;; Code:

(require 'ace-jump-mode)
(require 'org)
(require 'org-id)

(defvar worf-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode worf-mode
    "Minor mode for navigating and editing `org-mode' files.

When `worf-mode' is on, various unprefixed keys call commands
if the (looking-back \"^*+\") is true.

\\{worf-mode-map}"
  :keymap worf-mode-map
  :group 'worf
  :lighter " ✇")

(defun worf-copy-heading-id (arg)
  "Copy the id link of current heading to kill ring."
  (interactive "P")
  (let ((heading (substring-no-properties
                  (org-get-heading)))
        id)
    (when arg
      (org-entry-put nil "CUSTOM_ID" heading))
    (if (setq id (org-entry-get nil "CUSTOM_ID"))
        (kill-new (format "[[#%s][%s]]" id heading))
      (setq id (org-id-get nil 'create))
      (kill-new (format "[[id:%s][%s]]" id heading)))))

(defun worf-follow ()
  "Follow the link at point."
  (interactive)
  (let ((org-file-apps '(("\\.pdf\\'" . "xournal %s"))))
    (if (org-babel-in-example-or-verbatim)
        (newline-and-indent)
      (condition-case nil
          (progn
            (push-mark)
            (org-open-at-point)
            (worf-more))
        (error (newline-and-indent))))))

(defun worf-different ()
  "Move point to different side of #+begin/#+end form."
  (interactive)
  (cond ((looking-back "#\\+\\(?:end\\|END\\)_\\([^\n]*\\)")
         (re-search-backward "#\\+begin_")
         (beginning-of-line))
        ((looking-at "#\\+\\(?:begin\\|BEGIN\\)_\\([^\n]*\\)")
         (re-search-forward "#\\+end_")
         (end-of-line))
        (t (org-self-insert-command 1))))

(defun worf-down ()
  "Move one heading down."
  (interactive)
  (ignore-errors
    (org-speed-move-safe 'outline-next-visible-heading)))

(defun worf-up ()
  "Move one heading up."
  (interactive)
  (ignore-errors
    (org-speed-move-safe 'outline-previous-visible-heading)))

(defun worf-out-backward ()
  "Unhide current heading."
  (interactive)
  (ignore-errors
    (org-up-heading-safe)))

(defun worf-tab ()
  "Hide/show heading."
  (interactive)
  (org-cycle))

(defun worf--pretty-heading (str lvl)
  "Prettify heading STR or level LVL."
  (setq str (propertize str 'face (nth (1- lvl) org-level-faces)))
  (while (string-match org-bracket-link-regexp str)
    (setq str (replace-match
               (propertize (match-string 3 str) 'face 'org-link)
               nil nil str)))
  str)

(defun worf-goto ()
  "Jump to a heading with `helm'."
  (interactive)
  (require 'helm-match-plugin)
  (let ((candidates
         (org-map-entries
          (lambda ()
            (let ((comp (org-heading-components)))
              (cons (format "%d%s%s" (car comp)
                            (make-string (1+ (* 2 (1- (car comp)))) ?\ )
                            ;; (worf--pretty-heading (nth 4 comp) (car comp))
                            (org-get-heading))
                    (point))))))
        helm-update-blacklist-regexps
        helm-candidate-number-limit)
    (helm :sources
          `((name . "Headings")
            (candidates . ,candidates)
            (action . (lambda (x) (goto-char x)
                         (call-interactively 'show-branches)
                         (worf-more)))
            (pattern-transformer . regexp-quote)))))

(defun worf-ace-link ()
  "Visit a link within current heading by ace jumping."
  (interactive)
  (org-narrow-to-subtree)
  (setq ace-jump-mode-end-hook
        (list `(lambda ()
                 (setq ace-jump-mode-end-hook)
                 (widen)
                 (worf-follow))))
  (let ((ace-jump-mode-scope 'window)
        (ace-jump-allow-invisible t))
    (unwind-protect
         (condition-case e
             (ace-jump-do "\\[\\[")
           (error
            (if (string= (error-message-string e) "[AceJump] No one found")
                (error "0 links visible in current subtree")
              (signal (car e) (cdr e)))))
      (widen))))

(defun worf-more ()
  "Unhide current heading."
  (interactive)
  (org-show-subtree)
  (org-cycle-hide-drawers 'all)
  (recenter))

(defun worf-view ()
  "Recenter current heading to the first screen line.
If already there, return it to previous position."
  (interactive)
  (defvar worf-view-line 0)
  (let ((window-line (count-lines (window-start) (point))))
    (if (or (= window-line 0)
            (and (not (bolp)) (= window-line 1)))
        (recenter worf-view-line)
      (setq worf-view-line window-line)
      (recenter 0))))

(defun worf-attachment ()
  "Interface to attachments."
  (interactive)
  (call-interactively 'org-attach-open))

(defun worf-refile (arg)
  "Interface to refile."
  (interactive "P")
  (if arg
      (let ((org-refile-targets
             (cl-remove-if
              (lambda (x) (null (car x)))
              org-refile-targets)))
        (call-interactively 'org-refile)))
  (call-interactively 'org-refile))
(defun worf-delete (arg)
  "Delete subtree."
  (interactive "p")
  (if (and (looking-at "\\*") (looking-back "^\\**"))
      (org-cut-subtree)
    (delete-char arg)))

(defun worf-reserved ()
  "Do some cybersquatting."
  (interactive)
  (message "Nothing here, move along."))

;; ——— keys setup ——————————————————————————————————————————————————————————————
(defun worf--insert-or-call (def)
  "Return a lambda to call DEF if position is special.
Otherwise call `self-insert-command'."
  `(lambda ,(help-function-arglist def)
     ,(format "Call `%s' when special, self-insert otherwise.\n\n%s"
              (symbol-name def) (documentation def))
     ,(interactive-form def)
     (let (cmd)
       (cond ((or (and (looking-at "\\*") (looking-back "^\\**"))
                  (looking-at "^#\\+"))
              (call-interactively ',def))

             (t
              (org-self-insert-command 1))))))

(defvar ac-trigger-commands '(self-insert-command))
(defvar company-begin-commands '(self-insert-command))

(defun worf-define-key (keymap key def)
  "Forward to (`define-key' KEYMAP KEY DEF)
DEF is modified by `worf--insert-or-call'."
  (let ((func (defalias (intern (concat "wspecial-" (symbol-name def)))
                  (worf--insert-or-call def))))
    (unless (member func ac-trigger-commands)
      (push func ac-trigger-commands))
    (unless (member func company-begin-commands)
      (push func company-begin-commands))
    (define-key keymap (kbd key) func)))

(let ((map worf-mode-map))
  ;; ——— Global ———————————————————————————————
  (define-key map "\C-j" 'worf-follow)
  (define-key map (kbd "M-j") 'worf-ace-link)
  (define-key map (kbd "C-M-g") 'worf-goto)
  (define-key map (kbd "C-d") 'worf-delete)
  ;; ——— Local ————————————————————————————————
  (mapc (lambda (k) (worf-define-key map k 'worf-reserved))
        '("A" "b" "B" "c" "C" "D" "e" "E" "f" "G" "H" "I" "J" "K" "l"
          "M" "n" "N" "o" "O" "p" "P" "q" "Q" "R" "s" "S" "t" "T" "u"
          "U" "w" "W" "x" "X" "y" "Y" "z" "Z"))
  (worf-define-key map "L" 'worf-copy-heading-id)
  (worf-define-key map "d" 'worf-different)
  (worf-define-key map "j" 'worf-down)
  (worf-define-key map "k" 'worf-up)
  (worf-define-key map "i" 'worf-tab)
  (worf-define-key map "g" 'worf-goto)
  (worf-define-key map "m" 'worf-more)
  (worf-define-key map "h" 'worf-ace-link)
  (worf-define-key map "a" 'worf-out-backward)
  (worf-define-key map "v" 'worf-view)
  (worf-define-key map "8" 'org-insert-heading-respect-content)
  (worf-define-key map "F" 'worf-attachment)
  (worf-define-key map "V" 'projectile-find-file)
  (worf-define-key map "r" 'worf-refile))

(provide 'worf)

;;; worf.el ends here
