					;Shortcuts

;;					Meta key on apple keyboard
(setq ns-alternate-modifier 'meta)
(setq ns-right-alternate-modifier 'none)

;; set keys for Apple keyboard, for emacs in OS X
(setq mac-control-modifier 'control) ; make Control key do Control
(setq mac-option-modifier 'meta) ; make cmd key do Meta
(setq mac-left-command-modifier 'super) ; make opt key do Super
(setq mac-right-command-modifier 'hyper)  ; make cmd right key do Hyper

(add-hook 'org-mode-hook 'scimax-autoformat-mode)

;; moving between windows with Shift + arrows
;; (windmove-default-keybindings)
(global-set-key (kbd "H-<up>") 'windmove-up)
(global-set-key (kbd "H-<down>") 'windmove-down)
(global-set-key (kbd "H-<right>") 'windmove-right)
(global-set-key (kbd "H-<left>") 'windmove-left)

(global-set-key (kbd "M-q") 'toggle-truncate-lines)

(global-set-key (kbd "H-l") 'ns-copy-including-secondary)

;; (global-set-key "\M-g" 'goto-line)

;; (global-set-key (kbd "C-+") 'text-scale-increase)
;; (global-set-key (kbd "C--") 'text-scale-decrease)

;; Magit
;; (global-set-key (kbd "C-x g") 'magit-status)
;; (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)


;; Shortcuts and agenda
;; (global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-cc" 'org-capture)
;; (global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key "\C-cb" 'org-iswitchb)
;; (global-set-key (kbd "C-c S-t") 'org-babel-execute-subtree)

;; Navigating through Org-mode
;; (global-set-key (kbd "C-c <up>") 'outline-up-heading)
;; (global-set-key (kbd "C-c <left>") 'outline-previous-visible-heading)
;; (global-set-key (kbd "C-c <right>") 'outline-next-visible-heading)

;; (global-set-key (kbd "<f12>") 'org-agenda)
;; (global-set-key (kbd "<f5>") 'bh/org-todo)
;; (global-set-key (kbd "<S-f5>") 'bh/widen)
;; (global-set-key (kbd "<f9> c") 'calendar)

;; (global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
;; (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
;; (global-set-key (kbd "S-C-<down>") 'shrink-window)
;; (global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; (global-set-key (kbd "H-m") 'ns-copy-including-secondary)

;; (require 'org-tempo)
;; (setq org-structure-template-alist
;;       '(("a" . "export ascii")
;; 	;; ("b" . "src shell :session session :results output :exports both")
;; 	;; ("B" . "src shell :session *shell* :results output :exports both")
;; 	;; ("c" . "center")
;; 	;; ("C" . "comment")
;; 	;; ("dit" . "src ditaa :file NAME_FILE.png")
;; 	;; ("e" . "example")
;; 	;; ("E" . "export")
;; 	;; ("h" . "export html")
;; 	;; ("l" . "export latex")
;; 	;; ("m" . "src emacs-lisp")
;; 	;; ("plant" . "src plantuml :file NAME_FILE.png")
;; 	;; ("q" . "quote")
;; 	;; ("r" . "src R :results output :session *R* :exports results")
;; 	;; ("R" . "src R :results file graphics :file (org-babel-temp-file \"figure\" \".png\") :exports results :width 600 :height 400 :session *R*")
;; 	;; ("RR" . "src R :results file graphics :file (org-babel-temp-file (concat (file-name-directory (or load-file-name buffer-file-name)) \"figure-\") \".png\") :exports results :width 600 :height 400 :session *R*")
;; 	;; ("p" . "src python :results output :exports results")
;; 	;; ("P" . "src python :results output :session :exports results")
;; 	;; ("PP" . "src python :results file :session :var matplot_lib_filename=(org-babel-temp-file \"figure\" \".png\") :exports results\nimport matplotlib.pyplot as plt\n\nimport  numpy\nx=numpy.linspace(-15,15)\nplt.figure(figsize=(10,5))\nplt.plot(x,numpy.cos(x)/x)\nplt.tight_layout()\n\nplt.savefig(matplot_lib_filename)\nmatplot_lib_filename")
;; 	;; ("s" . "src")
;; 	;; ("v" . "verse")
;; 	))


