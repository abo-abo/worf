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

;; ——— Requires ————————————————————————————————————————————————————————————————
(require 'ace-jump-mode)
(require 'org)
(require 'org-id)
(require 'org-clock)

;; ——— Minor mode ——————————————————————————————————————————————————————————————
(defvar worf-sharp "^#\\+"
  "Shortcut for the org's #+ regex.")

(defvar worf-regex "^\\(?:\\*\\|#\\+\\)"
  "Shortcut for worf's special regex.")

(defvar worf-regex-full "^\\(?:\\*\\|#\\+\\|:\\)"
  "Shortcut for worf's special regex.")

(defvar worf-mode-map
  (make-sparse-keymap))

;;;###autoload
(define-minor-mode worf-mode
    "Minor mode for navigating and editing `org-mode' files.

When `worf-mode' is on, various unprefixed keys call commands
if the (looking-back \"^*+\") is true.

\\{worf-mode-map}"
  :group 'worf
  :lighter " ✇")

;; ——— Macros ——————————————————————————————————————————————————————————————————
(defmacro worf-dotimes-protect (n &rest bodyform)
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

;; ——— Key binding machinery ———————————————————————————————————————————————————
(defun worf--insert-or-call (def alist)
  "Return a lambda to call DEF if position is special.
Otherwise call `self-insert-command'."
  (let ((disable (cdr (assoc :disable alist)))
        (break (cdr (assoc :break alist))))
    `(lambda ,(help-function-arglist def)
       ,(format "Call `%s' when special, self-insert otherwise.\n\n%s"
                (symbol-name def) (documentation def))
       ,(interactive-form def)
       (let (cmd)
         (cond ((worf--special-p)
                ,(when disable `(,disable -1))
                (,def ,@(delq '&rest (delq '&optional (help-function-arglist def))))
                (unless (or ,break (worf--special-p))
                  (worf-up 1)))

               (t
                (org-self-insert-command 1)))))))

(defun worf--flag-to-alist (lst flag)
  "If FLAG is on LST, change it to (FLAG . t)."
  (let ((x (memq flag lst)))
    (when x
      (setcar x (cons flag t)))
    lst))

(defun worf-define-key (keymap key def &rest plist)
  "Forward to (`define-key' KEYMAP KEY DEF)
DEF is modified by `worf--insert-or-call'."
  (let ((func (defalias (intern (concat "wspecial-" (symbol-name def)))
                  (worf--insert-or-call def (worf--flag-to-alist
                                             plist :break)))))
    (when (boundp 'ac-trigger-commands)
      (add-to-list 'ac-trigger-commands func))
    (when (boundp 'company-begin-commands)
      (add-to-list 'company-begin-commands func))
    (define-key keymap (kbd key) func)))

;; ——— Verb machinery ——————————————————————————————————————————————————————————
(defvar worf-known-verbs '(worf-keyword-mode)
  "List of registered verbs.")

(defun worf-disable-verbs-except (verb)
  "Disable all verbs except VERB."
  (mapc
   (lambda (v) (funcall v -1))
   (remq verb worf-known-verbs)))

(defmacro worf-defverb (name grammar)
  (let ((sym (intern (format "worf-%s-mode" name)))
        (keymap (intern (format "worf-%s-mode-map" name)))
        (doc (format "%s verb." (capitalize name)))
        (lighter (format " [%s]" name)))
    `(progn
       (defvar ,keymap (make-sparse-keymap))
       (define-minor-mode ,sym
           ,doc
         :keymap ,keymap
         :group 'worf
         :lighter ,lighter
         (cond (,sym
                (worf-disable-verbs-except ',sym))
               (t nil)))
       (mapc (lambda (x)
               (let ((y (memq :disable x)))
                 (when y
                   (setcar y (cons :disable ',sym))))
               (apply 'worf-define-key (cons ,keymap x)))
             ,grammar)
       (unless (memq ',sym worf-known-verbs)
         (push ',sym worf-known-verbs))
       worf-known-verbs)))

;; ——— Verbs: change ———————————————————————————————————————————————————————————
(worf-defverb
 "change"
 '(("j" org-metadown)
   ("k" org-metaup)
   ("h" org-metaleft)
   ("l" org-metaright)
   ("t" org-set-tags)))

;; ——— Verbs: change tree ——————————————————————————————————————————————————————
(worf-defverb
 "change-tree"
 '(("j" org-shiftmetadown)
   ("k" org-shiftmetaup)
   ("h" org-shiftmetaleft)
   ("l" org-shiftmetaright)))

;; ——— Verbs: change shift —————————————————————————————————————————————————————
(worf-defverb
 "change-shift"
 '(("j" org-shiftdown)
   ("k" org-shiftup)
   ("h" org-shiftleft)
   ("l" org-shiftright)))

;; ——— Verbs: change shift control —————————————————————————————————————————————
(worf-defverb
 "change-shiftcontrol"
 '(("j" org-shiftcontroldown)
   ("k" org-shiftcontrolup)
   ("h" org-shiftcontrolleft)
   ("l" org-shiftcontrolright)))

;; ——— Verbs: clock ————————————————————————————————————————————————————————————
(worf-defverb
 "clock"
 '(("i" org-clock-in :disable)
   ("o" org-clock-out :disable)))

;; ——— Verbs: delete ———————————————————————————————————————————————————————————
(defun worf-delete-k (arg)
  (interactive "p")
  (let ((pt (point)))
    (when (ignore-errors
            (org-speed-move-safe
             'outline-previous-visible-heading) t)
      (kill-region pt (point)))))

(worf-defverb
 "delete"
 '(("p" org-delete-property :disable)
   ("k" worf-delete-k :disable)
   ("j" org-cut-subtree :disable)))

;; ——— Verbs: yank —————————————————————————————————————————————————————————————
(worf-defverb
 "yank"
 '(("j" org-copy-subtree :disable)))

;; ——— Verbs: mark —————————————————————————————————————————————————————————————
(defun worf-mark-down (arg)
  (interactive "p")
  (let ((bnd (worf--bounds-subtree)))
    (worf-down (- arg 1))
    (set-mark (cdr (worf--bounds-subtree)))
    (goto-char (car bnd))))

(defun worf-mark-left ()
  (interactive)
  (worf-left)
  (let ((bnd (worf--bounds-subtree)))
    (goto-char (car bnd))
    (set-mark (cdr bnd))))

(worf-defverb
 "mark"
 '(("j" worf-mark-down :disable)
   ("h" worf-mark-left :disable)))

;; ——— Verbs: keyword ——————————————————————————————————————————————————————————
(defvar worf-keyword-mode-map
  (make-sparse-keymap))

(defvar worf-keyword-mode-lighter "")

(defvar worf--keyword nil
  "Current `org-mode' keyword, i.e. one of \"TODO\", \"DONE\" etc.")

(defsubst worf-mod-keyword ()
  "Return current keyword."
  worf--keyword)

(define-minor-mode worf-keyword-mode
    "Minor mode that makes j/k to move by keywords."
  :keymap worf-keyword-mode-map
  :group 'worf
  :lighter worf-keyword-mode-lighter
  (cond (worf-keyword-mode
         (call-interactively 'worf-keyword))
        (t
         (setq worf--keyword nil))))

(defun worf-keyword (keyword)
  "Set the current keyword.
All next `worf-down' and `worf-up' will move by this keyword.
When the chain is broken, the keyword is unset."
  (interactive
   (progn
     (setq worf-keyword-mode-lighter " [keyword ?]")
     (let ((c (read-char "[t]odo, [d]one, [n]ext, [c]ancelled")))
       (list
        (message
         (cl-case c
           (?t "TODO")
           (?d "DONE")
           (?n "NEXT")
           (?c "CANCELLED")))))))
  (add-to-list 'worf--keyword-no-invalidate-list this-command)
  (if worf-change-mode
      (progn
        (org-todo keyword)
        (worf-change-mode -1)
        (worf-keyword-mode -1))
    (setq worf--keyword keyword)
    (setq worf-keyword-mode-lighter
          (format " [keyword %s]" keyword))
    (add-hook 'post-command-hook 'worf--invalidate-keyword)))

(defvar worf--keyword-no-invalidate-list
  '(wspecial-worf-keyword-mode
    worf-keyword
    wspecial-worf-up
    wspecial-worf-down
    wspecial-digit-argument))

(defun worf--invalidate-keyword ()
  (message "%s" this-command)
  (unless (memq this-command worf--keyword-no-invalidate-list)
    (worf-keyword-mode -1)
    (remove-hook 'post-command-hook 'worf--invalidate-keyword)))

(let ((map worf-keyword-mode-map))
  (worf-define-key map "w" 'worf-keyword))

(defun worf--set-change-switches (key mode)
  "Bind MODE to KEY for change modes."
  (mapc (lambda (map) (worf-define-key map key mode))
        (list worf-change-mode-map
              worf-change-tree-mode-map
              worf-change-shift-mode-map
              worf-change-shiftcontrol-mode-map)))

(worf--set-change-switches "c" 'worf-change-mode)
(worf--set-change-switches "f" 'worf-change-tree-mode)
(worf--set-change-switches "s" 'worf-change-shift-mode)
(worf--set-change-switches "r" 'worf-change-shiftcontrol-mode)

;; ——— Nouns: arrows ———————————————————————————————————————————————————————————
(defun worf-up (arg)
  "Move ARG headings up."
  (interactive "p")
  (cond ((worf-mod-keyword)
         (worf-dotimes-protect arg
           (worf--prev-keyword (worf-mod-keyword))))
        ((worf--at-property-p)
         (worf--prev-property arg))
        ((looking-at worf-sharp)
         (worf--sharp-up))
        (t
         (unless (worf-dotimes-protect arg
                   (outline-previous-visible-heading 1)
                   t)
           (backward-char)
           (worf--sharp-up)))))

(defun worf-down (arg)
  "Move ARG headings down."
  (interactive "p")
  (cond ((worf-mod-keyword)
         (worf-dotimes-protect arg
           (worf--next-keyword (worf-mod-keyword))))
        ((worf--at-property-p)
         (worf--next-property arg))
        ((looking-at worf-sharp)
         (worf--sharp-down))
        (t
         (worf-dotimes-protect arg
           (outline-next-visible-heading 1)))))

(defun worf-right ()
  "Move right."
  (interactive)
  (let ((pt (point))
        result)
    (org-narrow-to-subtree)
    (forward-char)
    (if (re-search-forward worf-regex nil t)
        (progn
          (goto-char (match-beginning 0))
          (setq result t))
      (goto-char pt))
    (widen)
    result))

(defun worf-left ()
  "Move one level up backwards."
  (interactive)
  (if (looking-at worf-sharp)
      (goto-char (car (worf--bounds-subtree)))
    (ignore-errors
      (org-up-heading-safe))))

;; ——— Nouns: property —————————————————————————————————————————————————————————
(defun worf-property ()
  "Operate on property."
  (interactive)
  (cond (worf-change-mode
         (call-interactively 'org-set-property)
         (worf-quit))

        (t
         (cl-destructuring-bind (beg . end)
             (worf--bounds-subtree)
           (let ((pt (car (org-get-property-block beg end nil))))
             (if pt
                 (progn
                   (unless (bound-and-true-p reveal-mode)
                     (goto-char beg)
                     (org-show-subtree))
                   (goto-char pt))
               (error "No properties. Use \"c p\" to add properties")))))))

;; ——— Nouns: new heading ——————————————————————————————————————————————————————
(defun worf-add ()
  "Add a new heading below."
  (interactive)
  (org-insert-heading-respect-content)
  (when worf-keyword-mode
    (insert (worf-mod-keyword) " ")
    (worf-keyword-mode -1)))

;; ——— Other movement ——————————————————————————————————————————————————————————
(defun worf-backward ()
  "Go backwards to closest special position."
  (interactive)
  (re-search-backward worf-regex-full nil t))

(defun worf-forward ()
  "Go forwards to closest special position."
  (interactive)
  (forward-char 1)
  (re-search-forward worf-regex-full nil t)
  (beginning-of-line))

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
                              (worf--pretty-heading (nth 4 comp) (car comp))))
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
        (progn
          (org-shifttab arg)
          (worf-dotimes-protect (- (car (org-heading-components)) arg)
            (worf-left)))
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

;; ——— Misc ————————————————————————————————————————————————————————————————————

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
  (mapc (lambda (x) (funcall x -1)) worf-known-verbs))

(defun worf-todo (arg)
  "Forward to `org-todo' with ARG."
  (interactive "P")
  (org-todo arg))

(defun worf-reserved ()
  "Do some cybersquatting."
  (interactive)
  (message "Nothing here, move along."))

;; ——— Predicates ——————————————————————————————————————————————————————————————
(defun worf--at-property-p ()
  "Return t if point is at property."
  (looking-at "^:"))

(defun worf--special-p ()
  "Return t if point is special.
When point is special, alphanumeric keys call commands instead of
calling `self-insert-command'."
  (or (bobp)
      (looking-at worf-regex)
      (worf--at-property-p)
      (looking-back "^\\*+")))

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
(defun worf--next-property (arg)
  "Move to the next property line."
  (interactive "p")
  (let ((bnd (worf--bounds-subtree)))
    (forward-char 1)
    (if (re-search-forward "^:" (cdr bnd) t arg)
        (backward-char 1)
      (or (worf-right)
          (worf-down arg)))))

(defun worf--prev-property (arg)
  "Move to the previous property line."
  (interactive "p")
  (let ((bnd (worf--bounds-subtree)))
    (unless (re-search-backward "^:" (car bnd) t arg)
      (org-speed-move-safe 'outline-previous-visible-heading))))

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
  (setq str (or str ""))
  (setq str (propertize str 'face (nth (1- lvl) org-level-faces)))
  (let (desc)
    (while (and (string-match org-bracket-link-regexp str)
                (stringp (setq desc (match-string 3 str))))
      (setq str (replace-match
                 (propertize desc 'face 'org-link)
                 nil nil str)))
    str))

(defun worf--prev-keyword (str)
  "Move to the prev keyword STR within current file."
  (reveal-mode 1)
  (let ((pt (point)))
    (unless (catch 'break
              (while t
                (outline-previous-heading)
                (when (and (string= str (nth 2 (org-heading-components)))
                           (looking-at "^\\*"))
                  (throw 'break t))
                (when (= (point) (point-min))
                  (throw 'break nil))))
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

(let ((map worf-mode-map))
  ;; ——— Global ———————————————————————————————
  (define-key map "[" 'worf-backward)
  (define-key map "]" 'worf-forward)
  (define-key map "\C-a" 'worf-beginning-of-line)
  (define-key map "\C-j" 'worf-follow)
  (define-key map (kbd "M-j") 'worf-ace-link)
  (define-key map (kbd "C-M-g") 'worf-goto)
  (define-key map (kbd "C-d") 'worf-delete-subtree)
  ;; ——— Local ————————————————————————————————
  (mapc (lambda (k) (worf-define-key map k 'worf-reserved))
        '("b" "B" "C" "D" "e" "E" "f" "G" "H" "J" "M" "n" "O" "P" "Q"
          "S" "T" "U" "w" "x" "X" "Y" "z" "Z"))
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
  ;; (worf-define-key map "m" 'worf-more)
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
  (worf-define-key map "a" 'worf-add :break t)
  (worf-define-key map "s" 'worf-save)
  ;; ——— narrow/widen —————————————————————————
  (worf-define-key map "N" 'org-narrow-to-subtree)
  (worf-define-key map "W" 'widen)
  ;; ——— verbs ————————————————————————————————
  (worf-define-key map "c" 'worf-change-mode)
  (worf-define-key map "d" 'worf-delete-mode)
  (worf-define-key map "y" 'worf-yank-mode)
  (worf-define-key map "C" 'worf-clock-mode)
  (worf-define-key map "w" 'worf-keyword-mode)
  (worf-define-key map "m" 'worf-mark-mode)
  (worf-define-key map "q" 'worf-quit)
  ;; ——— nouns ————————————————————————————————
  (worf-define-key map "p" 'worf-property)
  ;; ——— misc —————————————————————————————————
  (worf-define-key map "t" 'worf-todo)
  (worf-define-key map "u" 'undo)
  ;; ——— digit argument ———————————————————————
  (mapc (lambda (x) (worf-define-key map (format "%d" x) 'digit-argument))
        (number-sequence 0 9)))

(provide 'worf)

;;; Local Variables:
;;; outline-regexp: ";; ———"
;;; End:

;;; worf.el ends here
