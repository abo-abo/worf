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

(defvar worf-sharp "^#\\+"
  "Shortcut for the org's #+ regex.")

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

;; ——— Interactive —————————————————————————————————————————————————————————————
(defun worf-copy-heading-id (arg)
  "Copy the id link of current heading to kill ring.
When ARG is true, add a CUSTOM_ID first."
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
  (if (looking-at worf-sharp)
      (worf--sharp-down)
    (ignore-errors
      (org-speed-move-safe 'outline-next-visible-heading))))

(defun worf-up ()
  "Move one heading up."
  (interactive)
  (if (looking-at worf-sharp)
      (worf--sharp-up)
    (unless (ignore-errors
              (org-speed-move-safe 'outline-previous-visible-heading) t)
      (backward-char)
      (worf--sharp-up))))

(defun worf-flow ()
  "Move point current heading's first #+."
  (interactive)
  (org-narrow-to-subtree)
  (when (re-search-forward worf-sharp (cdr (worf--bounds-subtree)) t)
    (goto-char (match-beginning 0)))
  (widen))

(defun worf-out-backward ()
  "Move one level up backwards."
  (interactive)
  (if (looking-at worf-sharp)
      (goto-char (car (worf--bounds-subtree)))
    (ignore-errors
      (org-up-heading-safe))))

(defun worf-out-forward ()
  "Move one level up forwards."
  (interactive)
  (worf-out-backward)
  (worf-down))

(defun worf-tab ()
  "Hide/show heading."
  (interactive)
  (let ((case-fold-search t))
    (when (looking-at "#\\+end")
      (worf--sharp-up)))
  (org-cycle))

(defun worf-shifttab (arg)
  "Hide/show everything.
Forward to `org-shifttab' with ARG."
  (interactive "P")
  (org-shifttab arg))

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

(defun worf-attach ()
  "Interface to attachments."
  (interactive)
  (call-interactively 'org-attach))

(defun worf-attach-visit ()
  "Interface to attachments."
  (interactive)
  (call-interactively 'org-attach-open))

(defun worf-refile-other (arg)
  "Refile to other file.
ARG is unused currently."
  (interactive "p")
  (let ((org-refile-targets
             (cl-remove-if
              (lambda (x) (null (car x)))
              org-refile-targets)))
    (call-interactively 'org-refile)))

(defun worf-refile-this (arg)
  "Interface to refile with :maxlevel set to ARG."
  (interactive "p")
  (when (= arg 1)
    (setq arg 5))
  (let ((org-refile-targets `((nil :maxlevel . ,arg))))
    (call-interactively 'org-refile)))

(defun worf-delete (arg)
  "Delete subtree or ARG chars."
  (interactive "p")
  (if (and (looking-at "\\*") (looking-back "^\\**"))
      (org-cut-subtree)
    (delete-char arg)))

(defun worf-visit (arg)
  "Forward to find file in project with ARG."
  (interactive "P")
  (projectile-find-file arg))

(defun worf-todo (arg)
  "Forward to `org-todo' with ARG."
  (interactive "P")
  (org-todo arg))

(defun worf-save ()
  "Save buffer."
  (interactive)
  (save-buffer))

(defun worf-reserved ()
  "Do some cybersquatting."
  (interactive)
  (message "Nothing here, move along."))

;; ——— Predicates ——————————————————————————————————————————————————————————————
(defun worf--invisible-p ()
  "Test if point is hidden by an `org-block' overlay."
  (cl-some (lambda (ov) (eq (overlay-get ov 'invisible)
                       'org-hide-block))
           (overlays-at (point))))

;; ——— Pure ————————————————————————————————————————————————————————————————————
(defun worf--bounds-subtree ()
  "Return bounds of the current subtree as a cons."
  (save-excursion
    (save-match-data
      (condition-case e
          (cons
           (progn
             (org-back-to-heading t)
             (point))
           (progn
             (org-end-of-subtree t t)
             (when (and (org-at-heading-p)
                        (not (eobp)))
               (backward-char 1))
             (point)))
        (error
         (if (string-match
              "^Before first headline"
              (error-message-string e))
             (cons (point-min)
                   (progn
                     (org-speed-move-safe 'outline-next-visible-heading)
                     (point)))
           (signal (car e) (cdr e))))))))

;; ——— Utilities ———————————————————————————————————————————————————————————————
(defun worf--sharp-down ()
  "Move down to the next #+."
  (let ((pt (point))
        (bnd (worf--bounds-subtree)))
    (forward-char)
    (while (and (re-search-forward worf-sharp (cdr bnd) t)
                (worf--invisible-p)))
    (cond ((worf--invisible-p)
           (goto-char pt))
          ((looking-back worf-sharp)
           (goto-char (match-beginning 0)))
          (t
           (if (= (car bnd) (point-min))
               (ignore-errors
                 (org-speed-move-safe 'outline-next-visible-heading))
             (goto-char pt))))))

(defun worf--sharp-up ()
  "Move up to the next #+."
  (let ((pt (point)))
    (while (and (re-search-backward worf-sharp (car (worf--bounds-subtree)) t)
                (worf--invisible-p)))
    (cond ((worf--invisible-p)
           (prog1 nil
             (goto-char pt)))
          ((= pt (point))
           nil)
          (t
           (goto-char
            (match-beginning 0))))))

(defun worf--pretty-heading (str lvl)
  "Prettify heading STR or level LVL."
  (setq str (propertize str 'face (nth (1- lvl) org-level-faces)))
  (while (string-match org-bracket-link-regexp str)
    (setq str (replace-match
               (propertize (match-string 3 str) 'face 'org-link)
               nil nil str)))
  str)

;; ——— Key bindings ————————————————————————————————————————————————————————————
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
        '("b" "B" "c" "C" "D" "e" "E" "G" "H" "J" "K" "M" "n" "o" "O"
          "p" "P" "q" "Q" "S" "T" "u" "U" "w" "x" "X" "y" "Y" "z" "Z"))
  ;; ——— navigation/structured ————————————————
  (worf-define-key map "j" 'worf-down)
  (worf-define-key map "k" 'worf-up)
  (worf-define-key map "f" 'worf-flow)
  (worf-define-key map "d" 'worf-different)
  (worf-define-key map "a" 'worf-out-backward)
  (worf-define-key map "l" 'worf-out-forward)
  ;; ——— navigation/unstructured ——————————————
  (worf-define-key map "g" 'worf-goto)
  (worf-define-key map "h" 'worf-ace-link)
  ;; ——— navigation/misc ——————————————————————
  (worf-define-key map "v" 'worf-view)
  (worf-define-key map "V" 'worf-visit)
  ;; ——— hide/show ————————————————————————————
  (worf-define-key map "i" 'worf-tab)
  (worf-define-key map "I" 'worf-shifttab)
  (worf-define-key map "m" 'worf-more)
  ;; ——— attachments ——————————————————————————
  (worf-define-key map "F" 'worf-attach-visit)
  (worf-define-key map "A" 'worf-attach)
  ;; ——— refile ———————————————————————————————
  (worf-define-key map "r" 'worf-refile-other)
  (worf-define-key map "R" 'worf-refile-this)
  ;; ——— misc —————————————————————————————————
  (worf-define-key map "L" 'worf-copy-heading-id)
  (worf-define-key map "8" 'org-insert-heading-respect-content)
  (worf-define-key map "t" 'worf-todo)
  (worf-define-key map "s" 'worf-save)
  ;; ——— narrow/widen —————————————————————————
  (worf-define-key map "N" 'org-narrow-to-subtree)
  (worf-define-key map "W" 'widen))

(provide 'worf)

;;; Local Variables:
;;; outline-regexp: ";; ———"
;;; End:

;;; worf.el ends here
