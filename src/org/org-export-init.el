(add-to-list 'load-path (or (getenv "CASK_DIR") "~/.cask"))
(require 'cask)
(cask-initialize "./")

(setq working-dir (f-dirname (f-this-file)))
(add-to-list 'load-path working-dir)

(setq make-backup-files nil)

(require 'org)
(require 'ox)
(require 'ob-ipython)
(require 'projectile)

(projectile-mode)

(setq org-export-allow-bind-keywords t)
(setq org-export-backends '(latex html))

(add-to-list 'org-babel-load-languages '(emacs-lisp . t))
(add-to-list 'org-babel-load-languages '(org . t))
(add-to-list 'org-babel-load-languages '(latex . t))
(add-to-list 'org-babel-load-languages '(python . t))
(add-to-list 'org-babel-load-languages '(ipython . t))

(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

(setq org-latex-listings 'minted
  org-latex-prefer-user-labels t
  org-confirm-babel-evaluate nil
  org-latex-packages-alist '(("" "minted"))
  org-babel-default-inline-header-args '((:exports . "code") (:eval . "never") (:results . "none")))

;;; Setup `ob-ipython'
(setq ob-ipython-resources-dir (or (getenv "FIGURES_DIR") "../../figures"))

(defun ob-ipython--process-response (ret file result-type)
  "Don't append 'Out[...]:\n' junk to value-type output!"
  (let ((result (cdr (assoc :result ret)))
         (output (cdr (assoc :output ret))))
    (if (eq result-type 'output)
      output
      (car (->> (-map (-partial 'ob-ipython--render file)
                  (list (cdr (assoc :value result))
                    (cdr (assoc :display result))))
             (remove-if-not nil))))))

(defun ob-ipython--render (file-or-nil values)
  "Display `value' output without prepended prompt."
  (let ((org (lambda (value)
               value))
        (png (lambda (value)
               (let ((file (or file-or-nil
                               (ob-ipython--generate-file-name ".png"))))
                 (ob-ipython--write-base64-string file value)
                 (format "[[file:%s]]" file))))
        (svg (lambda (value)
               (let ((file (or file-or-nil
                               (ob-ipython--generate-file-name ".svg"))))
                 (ob-ipython--write-string-to-file file value)
                 (format "[[file:%s]]" file))))
        (html (lambda (value)))
        (txt (lambda (value)
               (when (s-present? value)
                 (s-replace "'" "" value)))))
    (or (-when-let (val (cdr (assoc 'text/org values)))
          (funcall org val))
        (-when-let (val (cdr (assoc 'image/png values)))
          (funcall png val))
        (-when-let (val (cdr (assoc 'image/svg+xml values)))
          (funcall svg val))
        (-when-let (val (cdr (assoc 'text/plain values)))
          (funcall txt val)))))
