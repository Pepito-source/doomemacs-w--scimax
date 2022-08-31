;;    WRITING SCIENCE WITH LATEX IN ORGMODE
;; (setq org-latex-packages-alist
;;         (quote (("" "color" t) ("" "minted" t) ("" "parskip" t)))
;;         org-latex-pdf-process
;;         (quote (
;; "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
;; "bibtex $(basename %b)"
;; "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
;; "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f")))


(with-eval-after-load 'ox-latex

  (add-to-list 'org-latex-classes
               '("copernicus_discussions"
               	 "\\documentclass{copernicus_discussions}
               	  [NO-DEFAULT-PACKAGES]
               	  [PACKAGES]
               	  [EXTRA]"
               	 ("\\section{%s}" . "\\section*{%s}")
               	 ("\\subsection{%s}" "\\newpage" "\\subsection*{%s}" "\\newpage")
               	 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               	 ("\\paragraph{%s}" . "\\paragraph*{%s}")
               	 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("mdpi"
               	 "\\documentclass{Definitions/mdpi} 
               	  [NO-DEFAULT-PACKAGES]
               	  [PACKAGES]
               	  [EXTRA]"
               	 ("\\section{%s}" . "\\section*{%s}")
               	 ("\\subsection{%s}" "\\newpage" "\\subsection*{%s}" "\\newpage")
               	 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               	 ("\\paragraph{%s}" . "\\paragraph*{%s}")
               	 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("book"
               	 "\\documentclass{book}"
               	 ("\\part{%s}" . "\\part*{%s}")
              	 ("\\chapter{%s}" . "\\chapter*{%s}")
               	 ("\\section{%s}" . "\\section*{%s}")
               	 ("\\subsection{%s}" . "\\subsection*{%s}")
               	 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  )

;; Nice SourceCode Highlighting at export

(add-to-list 'org-latex-packages-alist '("" "minted"))
(add-to-list 'org-latex-packages-alist '("" "color"))

(setq org-latex-listings 'minted)

(setq org-latex-custom-lang-environments
      '((emacs-lisp "common-lispcode")))
