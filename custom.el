(use-package org-mac-link)

(use-package ivy-bibtex
  :init
  (setq bibtex-completion-bibliography '("~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/master.bib"
					 "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/pain.bib"
					 "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/capsaicin.bib"
					 "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/placebo-anxiety.bib"
					 "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/thesis.bib")
	bibtex-completion-library-path '("~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-pdfs/")
	bibtex-completion-notes-path "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/notes/"
	bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"

	bibtex-completion-additional-search-fields '(keywords)
	bibtex-completion-display-formats
	'((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	  (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	  (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
	bibtex-completion-pdf-open-function
	(lambda (fpath)
	  (call-process "open" nil 0 nil fpath))))

(use-package org-ref
  :init
  (require 'bibtex)
  (setq bibtex-autokey-year-length 4
	bibtex-autokey-name-year-separator "-"
	bibtex-autokey-year-title-separator "-"
	bibtex-autokey-titleword-separator "-"
	bibtex-autokey-titlewords 2
	bibtex-autokey-titlewords-stretch 1
	bibtex-autokey-titleword-length 5)
  (require 'org-ref-ivy)
  (require 'org-ref-arxiv)
  (require 'org-ref-scopus)
  (require 'org-ref-wos))

;; Fixes for Latex beamer
(require 'ox-latex)
(setq org-latex-title-command "\\maketitle")
;; (setq org-latex-hyperref-template "\\hypersetup{\n
;; pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},\n pdfsubject={%d},\n
;; pdfcreator={%c}, \n pdflang={%L}}\n")

(setq scimax-autoformat-sentence-capitalization nil)
