;;; vm-biblio.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Vincent Montero
;;
;; Author: Vincent Montero <vincent_montero@icloud.com>
;; Maintainer: Vincent Montero <vincent_montero@icloud.com>
;; Created: September 01, 2022
;; Modified: September 01, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/vincentmontero/vm-biblio
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
;; (defconst biblio-dir (file-name-directory (or load-file-name (buffer-file-name)))
;;   "Directory where the bibliography files are stored.")

(defun load-biblio (directory)
  "Load recursively all `.bib' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".bib"))
        (load (file-name-sans-extension fullpath)))))))


(setq! biblio-dir "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries")

(after! citar
  (setq! citar-bibliography (load-biblio "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries"))
  (setq! citar-bibliography (load-biblio biblio-dir))
  (setq! citar-bibliograhy (list biblio-dir))
  )

(setq! bibtex-completion-bibliography (load-biblio "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries"))
(setq! bibtex-completion-bibliography (load-biblio biblio-dir))
(setq! bibtex-completion-bibliography (list biblio-dir))

;; (after! citar
;;   (setq! citar-bibliography '("~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/master.bib"
;;                               "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/pain.bib"
;;                               "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/capsaicin.bib"
;;                               "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/placebo-anxiety.bib"
;;                               "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/thesis.bib"))
;;   (setq! citar-library-paths '("~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-pdfs"))
;;   (setq! citar-notes-paths '("~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/notes/")))

;; (setq! bibtex-completion-bibliography '("~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/master.bib"
;;                                         "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/pain.bib"
;;                                         "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/capsaicin.bib"
;;                                         "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/placebo-anxiety.bib"
;;                                         "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/thesis.bib")
;;        bibtex-completion-library-path '("~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-pdfs")
;;        bibtex-completion-notes-path '("~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/notes/"))


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

;; (setq! ido-mode nil)
;; (setq! ido-everywhere nil)


(provide 'vm-biblio)
;;; vm-biblio.el ends here
