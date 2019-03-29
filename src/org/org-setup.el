;;
;; Run `org-publish-current-file' to write the .md file.
;;

;; Add a format for "misc" reference types.
(setq org-ref-bibliography-entry-format
          (add-to-list 'org-ref-bibliography-entry-format
                       '("misc" . "%a, %t, <i>%j</i>, %p (%y). <a href=\"%U\">link</a>.")))

(defmacro create-org-ref-generic-format-function (link-type)
  "A macro that turns `org-ref-format-*' functions into a generic methods, so
 that they're easier to extend."
  (pcase (macroexpand-1 `(org-ref-make-format-function ,link-type))
    (`(defun ,name ,args ,doc . ,body)
     `(cl-defgeneric ,name ,args ,@body))))

;; (fmakunbound #'org-ref-format-citet)
;; (fmakunbound #'org-ref-format-citep)
;; (fmakunbound #'btw--create-md-link)

(create-org-ref-generic-format-function "citet")
(create-org-ref-generic-format-function "citep")

(defun* btw--create-md-link (key &optional parens)
  "A custom Markdown formatting for references."
     (format "<a href=\"#%s\">%s</a>"
             (md5 key)
             (let ((org-ref-bibliography-files (org-ref-find-bibliography))
                   (bib-file)
                   (author)
                   (year)
                   (entry)
                   (bibtex-entry))
               (setq bib-file (catch 'result
                            (cl-loop for file in org-ref-bibliography-files do
                                     (if (org-ref-key-in-file-p key
                                                                (file-truename file))
                                         (throw 'result file)
                                       (message "%s not found in %s"
                                                key
                                                (file-truename file))))))
               (with-temp-buffer
                 (insert-file-contents bib-file)
                 (bibtex-set-dialect (parsebib-find-bibtex-dialect)
                                     t)
                 (bibtex-search-entry key nil 0)
                 (setq bibtex-entry (bibtex-parse-entry))
                 (dolist (cons-cell bibtex-entry)
                   (setf (car cons-cell) (downcase (car cons-cell))))
                 (setq author (cdr (assoc "author" bibtex-entry)))
                 (setq year (cdr (assoc "year" bibtex-entry)))
                 (setq entry
                       (if parens
                           (format "(%s %s)" author year)
                         (format "%s (%s)" author year))))
               (replace-regexp-in-string "[\"\{\}]"
                                         ""
                                         (htmlize-escape-or-link entry)))))


;; These versions use `org-ref' for formatting.
(cl-defmethod org-ref-format-citet (keyword desc (format (eql md)))
  "A specialized method for citet (textual) links/references with Markdown
 formatting."
  (mapconcat #'btw--create-md-link
             (s-split "," keyword)
             ", "))

(cl-defmethod org-ref-format-citep (keyword desc (format (eql md)))
  "A specialized method for citep (parenthetical) links/references with Markdown
 formatting."
  (mapconcat (lambda (x) (btw--create-md-link x t))
             (s-split "," keyword)
             ", "))

;; (cl-defmethod org-ref-format-citet (keyword desc (format (eql md)))
;;   "Use Pandoc's citation formatting instead of `org-ref''s."
;;   (mapconcat (lambda (key) (format "@%s" key))
;;              (s-split "," keyword)
;;              ", "))

;; (cl-defmethod org-ref-format-citep (keyword desc (format (eql md)))
;;   "Use Pandoc's citation formatting instead of `org-ref''s."
;;   (mapconcat (lambda (key) (format "[@%s]" key))
;;              (s-split "," keyword)
;;              ", "))

;; Test 'em out.
;; (org-ref-format-citet "WillardProgrammingIntelligentCity2018a" nil 'html)
;; (org-ref-format-citet "WillardSymbolicMathPyMC32018" nil 'md)

;; TODO: Make `[[eqref:...]]` output `\(\eqref{...}\)` in Markdown output.
;; Might be related to `org-ref-ref-html', but definetly involves `org-ref-eqref-export'.
(defun btw--org-ref-eqref-export (old-fun keyword desc format)
  (cond
   ((or (eq format 'html) (eq format 'md))
    (format "\\(\\eqref{%s}\\)" keyword))
   (t (funcall old-fun keyword desc format))))

(advice-add #'org-ref-eqref-export :around #'btw--org-ref-eqref-export)

;; Don't attempt to convert the following drawers:
(setq org-latex-format-non-env-drawers '("results"))

(defun btw--org-latex-format-drawer-function (name contents)
  "Turn drawers into custom LaTeX blocks."
  (let ((name (downcase name)))
    (unless (member name org-latex-format-non-env-drawers)
      (format "\\begin{%s}\n%s\n\\end{%s}" name contents name))))

(setq org-latex-format-drawer-function #'btw--org-latex-format-drawer-function)

(defun btw--org-publish-property (prop)
  "Get the publish property PROP (a tag/keyword like `:base-directory') for
the current file's project."
    (org-publish-property prop
                          (org-publish-get-project-from-filename
                           (buffer-file-name (buffer-base-buffer)))))
