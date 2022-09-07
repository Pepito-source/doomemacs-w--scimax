					;Shortcuts

;;					Meta key on apple keyboard
(setq ns-alternate-modifier 'meta)
(setq ns-right-alternate-modifier 'none)

;; set keys for Apple keyboard, for emacs in OS X
(setq! mac-control-modifier 'control) ; make Control key do Control
(setq! mac-option-modifier 'meta) ; make cmd left key do Meta
(setq! mac-left-command-modifier 'super) ; make left opt key do Super
(setq! mac-right-command-modifier 'hyper)  ; make cmd right key do Hyper

(add-hook 'org-mode-hook 'scimax-autoformat-mode)

(delete-selection-mode t)

(with-eval-after-load 'evil-maps
    (define-key evil-insert-state-map (kbd "s-i") 'evil-normal-state))

(add-hook 'evil-insert-state-exit-hook
          (lambda ()
            (call-interactively #'evil-write)))

;; moving between windows with Shift + arrows
;; (windmove-default-keybindings)
(global-set-key (kbd "H-<up>") 'windmove-up)
(global-set-key (kbd "H-<down>") 'windmove-down)
(global-set-key (kbd "H-<right>") 'windmove-right)
(global-set-key (kbd "H-<left>") 'windmove-left)

(global-set-key (kbd "M-q") 'toggle-truncate-lines)

(global-set-key (kbd "H-l") 'ns-copy-including-secondary)


(global-set-key (kbd "H-.") 'org-time-stamp-inactive)
(global-set-key (kbd "H--") 'org-subscript-region-or-point)
(global-set-key (kbd "H-=") 'org-superscript-region-or-point)
(global-set-key (kbd "H-i") 'org-italics-region-or-point)
(global-set-key (kbd "H-b") 'org-bold-region-or-point)
(global-set-key (kbd "H-c") 'org-code-region-or-point)
(global-set-key (kbd "H-u") 'org-underline-region-or-point)
(global-set-key (kbd "H-v") 'org-verbatim-region-or-point)
(global-set-key (kbd "H-+") 'org-strikethrough-region-or-point)
(global-set-key (kbd "H-4") 'org-latex-math-region-or-point)
(global-set-key (kbd "H-e") 'ivy-insert-org-entity)
(global-set-key (kbd "H-\"") 'org-double-quote-region-or-point)
(global-set-key (kbd "H-'") 'org-single-quote-region-or-point)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c L") 'org-insert-link-global)
