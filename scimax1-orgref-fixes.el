(use-package parsebib)
(use-package helm)
(use-package helm-bibtex)
(use-package ivy-bibtex)
(use-package citeproc)

;; this is in a git submodule
(use-package org-ref
  :init
  (require 'bibtex)
  ;; (setq bibtex-autokey-year-length 4
  ;; 	bibtex-autokey-name-year-separator "-"
  ;; 	bibtex-autokey-year-title-separator "-"
  ;; 	bibtex-autokey-titleword-separator "-"
  ;; 	bibtex-autokey-titlewords 2
  ;; 	bibtex-autokey-titlewords-stretch 1
  ;; 	bibtex-autokey-titleword-length 5)
  (define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)
  (define-key org-mode-map (kbd "C-c )") 'org-ref-insert-link)
  (define-key org-mode-map (kbd "s-(") 'org-ref-insert-link-hydra/body)
  (require 'org-ref-ivy)
  (require 'org-ref-arxiv)
  (require 'org-ref-scopus)
  (require 'org-ref-wos))


(use-package org-ref-ivy
  :init (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
	      org-ref-insert-cite-function 'org-ref-cite-insert-ivy
	      org-ref-insert-label-function 'org-ref-insert-label-link
	      org-ref-insert-ref-function 'org-ref-insert-ref-link
	      org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))))
