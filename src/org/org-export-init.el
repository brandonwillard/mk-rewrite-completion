(add-to-list 'load-path (or (getenv "CASK_DIR") "~/.cask"))
(require 'cask)
(cask-initialize "./")

(setq working-dir (f-dirname (f-this-file)))
(add-to-list 'load-path working-dir)

(setq make-backup-files nil)

(require 'org)
(require 'ox)
(require 'projectile)
(require 'geiser)
(require 'org-ref)
(require 'org-ref+)
(require 'org-ref+)
(require 'ox-latex+)

(org-ref+-mode)
(projectile-mode)

(setq org-export-allow-bind-keywords t)
(setq org-export-backends '(latex html))

(add-to-list 'org-babel-load-languages '(emacs-lisp . t))
(add-to-list 'org-babel-load-languages '(org . t))
(add-to-list 'org-babel-load-languages '(latex . t))
(add-to-list 'org-babel-load-languages '(scheme . t))

(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

(setq org-latex-listings 'minted
  org-latex-prefer-user-labels t
  org-confirm-babel-evaluate nil
  org-latex-packages-alist '(("" "minted"))
  org-babel-default-inline-header-args '((:exports . "code") (:eval . "never") (:results . "none")))

(setq geiser-default-implementation 'racket
      geiser-scheme-implementation 'racket)
