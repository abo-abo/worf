(defun make-html-cursor--replace (x)
  (cond ((string= "||\n" x)
         "<cursor> </cursor>\n")
        ((string= "||[" x)
         "<cursor>[</cursor>")
        ((string-match "||\\*" x)
         (format "<cursor>%s</cursor>"
                 (replace-regexp-in-string "||\\*" "*" x)))

        (t
         (format "<cursor>%s</cursor>"
                 (regexp-quote
                  (substring x 2))))))

(defun make-html-cursor (str x y)
  (replace-regexp-in-string
   "||\\(.\\|\n\\)"
   #'make-html-cursor--replace
   str))

(setq org-export-filter-src-block-functions '(make-html-cursor))
(setq org-html-validation-link nil)
(setq org-html-postamble nil)
(setq org-html-preamble "<link rel=\"icon\" type=\"image/x-icon\" href=\"https://github.com/favicon.ico\"/>")
(setq org-html-text-markup-alist
  '((bold . "<b>%s</b>")
    (code . "<kbd>%s</kbd>")
    (italic . "<i>%s</i>")
    (strike-through . "<del>%s</del>")
    (underline . "<span class=\"underline\">%s</span>")
    (verbatim . "<code>%s</code>")))
(setq org-html-style-default nil)
(setq org-html-head-include-scripts nil)

(setq org-export-filter-parse-tree-functions '(make-el-links-symbols))

(defun make-el-links-symbols (tree back-end channel)
  (el-filter-tree tree))

(defun el-filter-tree (tree)
  (cond ((not (consp tree))
         tree)
        ((eq (car tree) 'link)
         (let ((plist (nth 1 tree)))
           (if (equal (plist-get plist :type) "el")
               `(verbatim
                 (
                  :value ,(plist-get plist :path)
                  :begin ,(plist-get plist :begin)
                  :end ,(plist-get plist :end)
                  :post-blank ,(plist-get plist :post-blank)
                  :parent ,(plist-get plist :parent)))
             tree)))
        ((eq (car tree) 'headline)
         (let* ((plist (nth 1 tree))
                (id (plist-get plist :CUSTOM_ID)))
           (when id
             (let ((p (cl-position :title plist)))
               (when p
                 (setf (nth (1+ p) plist)
                       (cons `(link
                               (
                                :type "custom-id"
                                :attr_html (":class anchor")
                                :path ,id
                                :post-blank 1)
                               "*")
                             (plist-get plist :title))))))
           tree)
         (cons 'headline (el-filter-tree (cdr tree))))
        ((eq (car tree) :parent)
         tree)
        (t
         (setcar tree (el-filter-tree (car tree)))
         (setcdr tree (el-filter-tree (cdr tree)))
         tree)))

(defun strip-org-object (x)
  (if (and (list x)
           (> (length x) 1)
           (listp (nth 1 x)))
      (let* ((plist (nth 1 x))
             (p (cl-position :parent plist)))
        (setf (nth (1+ p) plist) nil)
        x)
    x))

(provide 'worf-ox)
