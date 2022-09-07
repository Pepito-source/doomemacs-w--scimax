					;Programming

(require 'org)

;;Ditaa and plantuml
(setq! org-ditaa-jar-path "/Applications/ditaa0_9.jar")
(setq! org-plantuml-jar-path "/Applications/plantuml.jar")

;; Python
;; Python paths
;; /opt/homebrew/bin/python3
;; /usr/local/bin/python3
;; /usr/bin/python3
;; /opt/anaconda3/bin/python3

;;(setq! org-babel-python-command "/opt/homebrew/bin/python3")
;;(setq! python-shell-interpreter "/opt/homebrew/bin/python3")
;;(setq! python-indent-guess-indent-offset-verbose nil)
;;(setq! python-shell-completion-native-enable t)
;;  (setq! python-shell-completion-native-turn-on t)
;;(add-to-list 'python-shell-completion-native-disabled-interpreters "/opt/homebrew/bin/python3")

;;(with-eval-after-load 'python
;;  (defun python-shell-completion-native-try ()
;;    "Return non-nil if can trigger native completion."
;;    (let ((python-shell-completion-native-enable t)
;;          (python-shell-completion-native-output-timeout
;;           python-shell-completion-native-try-output-timeout
;;           python-shell-completion-native-disabled-interpreters nil))
;;      (python-shell-completion-native-get-completions
;;       (get-buffer-process (current-buffer))
;;       nil "_"))))

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
