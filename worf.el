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

(defvar worf-regex "^\\(?:\\*\\|#\\+\\)"
  "Shortcut for worf's special regex.")

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

;; ——— Macros ——————————————————————————————————————————————————————————————————
(defmacro dotimes-protect (n &rest bodyform)
  "Execute N times the BODYFORM unless an error is signaled.
Return nil couldn't execute BODYFORM at least once.
Otherwise return t."
  (declare (indent 1))
  `(let ((i 0)
         out)
     (ignore-errors
       (while (<= (incf i) ,n)
         ,@bodyform
         (setq out t)))
     out))

;; ——— Arrows ——————————————————————————————————————————————————————————————————
(defun worf-up (arg)
  "Move ARG headings up."
  (interactive "p")
  (cond ((worf-mod-keyword)
         (dotimes-protect arg
           (worf--prev-keyword (worf-mod-keyword))))
        ((eq (worf-mod-change) 'heading)
         (org-metaup))
        ((eq (worf-mod-change) 'tree)
         (org-shiftmetaup))
        ((eq (worf-mod-change) 'priority)
         (org-shiftup))
        ((looking-at worf-sharp)
         (worf--sharp-up))
        (t
         (unless (ignore-errors
                   (org-speed-move-safe 'outline-previous-visible-heading) t)
           (backward-char)
           (worf--sharp-up)))))

(defun worf-down (arg)
  "Move ARG headings down."
  (interactive "p")
  (cond ((worf-mod-keyword)
         (dotimes-protect arg
           (worf--next-keyword (worf-mod-keyword))))
        ((eq (worf-mod-change) 'heading)
         (org-metadown))
        ((eq (worf-mod-change) 'tree)
         (org-shiftmetadown))
        ((eq (worf-mod-change) 'priority)
         (org-shiftdown))
        ((worf-mod-delete)
         (org-cut-subtree arg)
         (setq worf--delete nil))
        ((looking-at worf-sharp)
         (worf--sharp-down))
        (t
         (ignore-errors
           (org-speed-move-safe 'outline-next-visible-heading)))))

(defun worf-right ()
  "Move right."
  (interactive)
  (cond ((eq (worf-mod-change) 'heading)
         (org-metaright))

        ((eq (worf-mod-change) 'tree)
         (org-shiftmetaright))

        (t
         (let ((pt (point)))
           (org-narrow-to-subtree)
           (forward-char)
           (if (re-search-forward worf-regex nil t)
               (goto-char (match-beginning 0))
             (goto-char pt))
           (widen)))))

(defun worf-left ()
  "Move one level up backwards."
  (interactive)
  (cond ((eq (worf-mod-change) 'heading)
         (org-metaleft))

        ((eq (worf-mod-change) 'tree)
         (org-shiftmetaleft))

        (t
         (if (looking-at worf-sharp)
             (goto-char (car (worf--bounds-subtree)))
           (ignore-errors
             (org-up-heading-safe))))))

;; ——— Other movement ——————————————————————————————————————————————————————————
(defun worf-beginning-of-line ()
  "Replaces `beginning-of-line'.
When already at beginning of line, move back to heading."
  (interactive)
  (if (looking-at "^[^*]")
      (progn
        (push-mark)
        (re-search-backward "^*"))
    (org-beginning-of-line)))

(defun worf-goto ()
  "Jump to a heading with `helm'."
  (interactive)
  (require 'helm-match-plugin)
  (let ((candidates
         (org-map-entries
          (lambda ()
            (let ((comp (org-heading-components))
                  (h (org-get-heading)))
              (cons (format "%d%s%s" (car comp)
                            (make-string (1+ (* 2 (1- (car comp)))) ?\ )
                            (if (get-text-property 0 'fontified h)
                                h
                              (worf--pretty-heading (nth 4 comp) (car comp)))
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
            (pattern-transformer . worf--pattern-transformer)))))

;; ——— View ————————————————————————————————————————————————————————————————————
(defun worf-tab (arg)
  "Hide/show heading.
When ARG isn't 1, call (`org-shifttab' ARG)."
  (interactive "p")
  (let ((v (this-command-keys-vector)))
    (if (and (= 2 (length v))
             (string-match "[0-9]" (concat v)))
        (org-shifttab arg)
      (let ((case-fold-search t))
        (when (looking-at "#\\+end")
          (worf--sharp-up))
        (org-cycle)))))

(defun worf-shifttab (arg)
  "Hide/show everything.
Forward to `org-shifttab' with ARG."
  (interactive "P")
  (org-shifttab arg))

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

;; ——— Links ———————————————————————————————————————————————————————————————————
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

;; ——— Files ———————————————————————————————————————————————————————————————————
(defun worf-attach ()
  "Interface to attachments."
  (interactive)
  (call-interactively 'org-attach))

(defun worf-attach-visit ()
  "Interface to attachments."
  (interactive)
  (call-interactively 'org-attach-open))

(defun worf-visit (arg)
  "Forward to find file in project with ARG."
  (interactive "p")
  (cond ((= arg 1)
         (projectile-find-file nil))
        ((= arg 2)
         (projectile-find-file-other-window))
        (t
         (projectile-find-file arg))))

(defun worf-save ()
  "Save buffer."
  (interactive)
  (save-buffer))

;; ——— Refile ——————————————————————————————————————————————————————————————————
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

;; ——— Keyword adverb/noun —————————————————————————————————————————————————————
(defvar worf--keyword nil
  "Current `org-mode' keyword, i.e. one of \"TODO\", \"DONE\" etc.")

(defsubst worf-mod-keyword ()
  "Return current keyword."
  worf--keyword)

(defun worf-keyword (keyword)
  "Set the current keyword.
All next `worf-down' and `worf-up' will move by this keyword.
When the chain is broken, the keyword is unset."
  (interactive
   (let ((c (read-char "[t]odo, [d]one, [n]ext, [c]ancelled")))
     (list
      (message
       (cl-case c
         (?t "TODO")
         (?d "DONE")
         (?n "NEXT")
         (?c "CANCELLED"))))))
  (unless (memq this-command worf--keyword-no-invalidate-list)
    (push this-command worf--keyword-no-invalidate-list))
  (if (worf-mod-change)
      (org-todo keyword)
    (setq worf--keyword keyword))
  (add-hook 'post-command-hook 'worf--invalidate-keyword))

(defvar worf--keyword-no-invalidate-list
  '(special-worf-keyword
    worf-keyword
    special-worf-up
    special-worf-down
    special-digit-argument))

(defun worf--invalidate-keyword ()
  (unless (memq this-command worf--keyword-no-invalidate-list)
    (worf--mode-keyword-off)
    (remove-hook 'post-command-hook 'worf--invalidate-keyword)))

(defun worf--mode-keyword-off ()
  "Turn off `worf--keyword' modifier."
  (when (worf-mod-keyword)
    (setq worf--keyword nil)
    ;; (message "keyword mode off")
    ))

;; ——— Change verb —————————————————————————————————————————————————————————————
(defvar worf--change nil
  "Current change mode. Can be 'tree, 'heading or nil.")

(defsubst worf-mod-change ()
  "Return current change mode."
  worf--change)

(defun worf-change-heading ()
  "Start changing the heading."
  (interactive)
  (if (looking-at worf-sharp)
      (org-edit-special)
    (setq worf--change 'heading)
    ;; (message "change heading on")
    (add-hook 'post-command-hook 'worf--invalidate-change)))

(defun worf-change-tree ()
  "Operate on tree."
  (interactive)
  (setq worf--change 'tree)
  ;; (message "change tree on")
  (add-hook 'post-command-hook 'worf--invalidate-change))

(defun worf-change-priority ()
  "Change priority."
  (interactive)
  (setq worf--change 'priority)
  ;; (message "change priority on")
  (add-hook 'post-command-hook 'worf--invalidate-change))

(defun worf--invalidate-change ()
  (unless (memq this-command
                '(special-worf-change-heading
                  special-worf-change-tree-or-todo
                  special-worf-change-priority
                  special-worf-up
                  special-worf-down
                  special-worf-right
                  special-worf-left
                  special-digit-argument))
    (worf--change-off)
    (remove-hook 'post-command-hook 'worf--invalidate-change)))

(defun worf--change-off ()
  "Turn off `worf--change' modifier."
  (when (worf-mod-change)
    ;; (message "change %s off" (worf-mod-change))
    (setq worf--change nil)))

;; ——— Delete verb —————————————————————————————————————————————————————————————
(defvar worf--delete nil
  "Current delete mode. t or nil.")

(defsubst worf-mod-delete ()
  "Return current delete mode."
  worf--delete)

(defun worf-delete ()
  "Delete verb."
  (interactive)
  (worf-quit)
  (setq worf--delete t))

;; ——— Nouns ———————————————————————————————————————————————————————————————————
(defun worf-property ()
  "Operate on property."
  (interactive)
  (cond ((worf-mod-change)
         (call-interactively 'org-set-property))

        ((worf-mod-delete)
         (call-interactively 'org-delete-property)
         (setq worf--delete nil))

        (t
         (error "Not in change or delete mode"))))

;; ——— Misc ————————————————————————————————————————————————————————————————————
(defun worf-add ()
  "Add a new heading below."
  (interactive)
  (org-insert-heading-respect-content)
  (when (worf-mod-keyword)
    (insert (worf-mod-keyword) " ")
    (worf--keyword-off)))

(defun worf-delete-subtree (arg)
  "Delete subtree or ARG chars."
  (interactive "p")
  (if (and (looking-at "\\*") (looking-back "^\\**"))
      (org-cut-subtree)
    (delete-char arg)))

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

(defun worf-quit ()
  "Remove modifiers."
  (interactive)
  (worf--change-off)
  (worf--mode-keyword-off))

(defun worf-change-tree-or-todo (arg)
  "Forward to `org-todo' with ARG."
  (interactive "P")
  (if (eq (worf-mod-change) 'heading)
      (worf-change-tree)
    (org-todo arg)))

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
(defun worf--pattern-transformer (x)
  "Transform X to make 1-9 select the heading level in `worf-goto'."
  (if (string-match "[1-9]" x)
      (setq x (format "^%s" x))
    x))

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

(defun worf--prev-keyword (str)
  "Move to the prev keyword STR within current file."
  (reveal-mode 1)
  (let ((pt (point)))
    (unless (catch 'break
              (while t
                (outline-previous-heading)
                (when (= (point) (point-min))
                  (throw 'break nil))
                (when (string= str (nth 2 (org-heading-components)))
                  (throw 'break t))))
      (goto-char pt))))

(defun worf--next-keyword (str)
  "Move to the next keyword STR within current file."
  (reveal-mode 1)
  (let ((pt (point)))
    (unless (catch 'break
              (while t
                (outline-next-heading)
                (when (= (point) (point-max))
                  (throw 'break nil))
                (when (string= str (nth 2 (org-heading-components)))
                  (throw 'break t))))
      (goto-char pt))))

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
              (,def ,@(delq '&rest (delq '&optional (help-function-arglist def)))))

             (t
              (org-self-insert-command 1))))))

(defvar ac-trigger-commands '(self-insert-command))
(defvar company-begin-commands '(self-insert-command))

(defun worf-define-key (keymap key def)
  "Forward to (`define-key' KEYMAP KEY DEF)
DEF is modified by `worf--insert-or-call'."
  (let ((func (defalias (intern (concat "special-" (symbol-name def)))
                  (worf--insert-or-call def))))
    (unless (member func ac-trigger-commands)
      (push func ac-trigger-commands))
    (unless (member func company-begin-commands)
      (push func company-begin-commands))
    (define-key keymap (kbd key) func)))

(let ((map worf-mode-map))
  ;; ——— Global ———————————————————————————————
  (define-key map "\C-a" 'worf-beginning-of-line)
  (define-key map "\C-j" 'worf-follow)
  (define-key map (kbd "M-j") 'worf-ace-link)
  (define-key map (kbd "C-M-g") 'worf-goto)
  (define-key map (kbd "C-d") 'worf-delete-subtree)
  ;; ——— Local ————————————————————————————————
  (mapc (lambda (k) (worf-define-key map k 'worf-reserved))
        '("b" "B" "C" "d" "D" "e" "E" "f" "G" "H" "J" "M" "n" "O" "p"
          "P" "Q" "S" "T" "U" "w" "x" "X" "y" "Y" "z" "Z"))
  ;; ——— navigation/arrows ————————————————————
  (worf-define-key map "j" 'worf-down)
  (worf-define-key map "k" 'worf-up)
  (worf-define-key map "h" 'worf-left)
  (worf-define-key map "l" 'worf-right)
  ;; ——— navigation/unstructured ——————————————
  (worf-define-key map "g" 'worf-goto)
  (worf-define-key map "o" 'worf-ace-link)
  ;; ——— hide/show ————————————————————————————
  (worf-define-key map "i" 'worf-tab)
  (worf-define-key map "I" 'worf-shifttab)
  (worf-define-key map "m" 'worf-more)
  (worf-define-key map "v" 'worf-view)
  ;; ——— files ————————————————————————————————
  (worf-define-key map "F" 'worf-attach-visit)
  (worf-define-key map "A" 'worf-attach)
  (worf-define-key map "V" 'worf-visit)
  ;; ——— refile ———————————————————————————————
  (worf-define-key map "r" 'worf-refile-other)
  (worf-define-key map "R" 'worf-refile-this)
  ;; ——— misc —————————————————————————————————
  (worf-define-key map "L" 'worf-copy-heading-id)
  (worf-define-key map "a" 'worf-add)
  (worf-define-key map "s" 'worf-save)
  ;; ——— narrow/widen —————————————————————————
  (worf-define-key map "N" 'org-narrow-to-subtree)
  (worf-define-key map "W" 'widen)
  ;; ——— modifiers ————————————————————————————
  (worf-define-key map "t" 'worf-change-tree-or-todo)
  (worf-define-key map "c" 'worf-change-heading)
  (worf-define-key map "d" 'worf-delete)
  (worf-define-key map "P" 'worf-change-priority)
  (worf-define-key map "w" 'worf-keyword)
  (worf-define-key map "q" 'worf-quit)
  ;; ——— nouns ————————————————————————————————
  (worf-define-key map "p" 'worf-property)
  ;; ——— misc —————————————————————————————————
  (worf-define-key map "u" 'undo)
  ;; ——— digit argument ———————————————————————
  (mapc (lambda (x) (worf-define-key map (format "%d" x) 'digit-argument))
        (number-sequence 0 9)))

(provide 'worf)

;;; Local Variables:
;;; outline-regexp: ";; ———"
;;; End:

;;; worf.el ends here
