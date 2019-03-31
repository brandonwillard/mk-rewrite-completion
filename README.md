# Introduction

This project contains source for reports and code researching the use of term rewriting algorithms and considerations (e.g. Knuth-Bendix) in [miniKanren](http://minikanren.org/).

# Development

The reports in this project are [literate programming documents](https://www.jstatsoft.org/article/view/v046i03) in [`org-mode`](https://orgmode.org/) format (i.e. `.org` files).  The `org-mode` files are exported and/or tangled to LaTeX, Markdown, source language files (e.g. Scheme, Python, etc.) by the =Makefile= in `src/org`.  Likewise, the relevant Emacs dependencies can be installed by the `Makefile` (or `Cask` directly).

Code embedded in the `org-mode` files can also be executed or run interactively within `org-mode` by way of [`org-babel`](https://orgmode.org/worg/org-contrib/babel/).  (Some setup may be required.)

Exported results are created in their respective project directories (e.g. LaTeX in `src/tex`, Scheme code in `src/scm`, etc.)  The [GitHub releases page](https://github.com/brandonwillard/mk-rewrite-completion/releases) for this project hosts fully compiled results (e.g. generated figures, datasets, PDFs, and binaries).

## Racket Dependencies

This project depends on `miniKanren`, which can be installed for [Racket](https://www.racket-lang.org/) with the following:
```
$ raco pkg install minikanren
```
