					;Programming

(require 'org)

;;Ditaa and plantuml
(setq! org-ditaa-jar-path "/Applications/ditaa0_9.jar")
(setq! org-plantuml-jar-path "/Applications/plantuml.jar")

;; Python
(setq! org-babel-python-command "/opt/anaconda3/bin/python3")
(setq! python-shell-interpreter "/opt/anaconda3/bin/python3")
(setq! python-shell-completion-native-enable nil)
(setq! python-indent-guess-indent-offset-verbose nil)

;; R
(setq! ess-eval-visibly-p nil)
(setq! ess-ask-for-ess-directory nil)
;;(setq! org-babel-R-command "/usr/local/bin/R --slave --no-save")
;;(setq! inferior-R-program-name "/usr/local/bin/R")


					;Org-mode + Babel

(setq! org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
;;   (emacs-lisp . t)
;;   (shell . t)
;;   (python . t)
;;   (R . t)
;;   (ruby . t)
;;   (ocaml . t)
   (ditaa . t)
;;   (dot . t)
;;   (latex . t)
;;   (octave . t)
   (sqlite . t)
   (perl . t)
   (screen . t)
;;   (plantuml . t)
   (lilypond . t)
;;   (org . t)
   (makefile . t)
   (gnuplot . t)
   (clojure . t)
   ))
(setq! org-src-preserve-indentation nil)

(setq! org-babel-results-keyword "results")

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
