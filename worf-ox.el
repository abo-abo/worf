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

(provide 'worf-ox)
