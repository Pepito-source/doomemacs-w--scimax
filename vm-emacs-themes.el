					;EMACS APPEARANCE

;; Emacs themes

;; (setq doom-theme 'doom-acario-dark)


;;(setq-default dotspacemacs-themes '(list-themes-here))

;;(use-package spacemacs-theme
;;  :defer t
;;  :init
;;  (setq spacemacs-theme-org-bold t
;;        spacemacs-theme-org-height t)
;;  :config
;;  (load-theme 'spacemacs-dark t)
;;  )

;;(load-theme 'spacemacs-dark t)
;;(load-theme 'immaterial-dark t)
;;(load-theme 'afternoon t)
;;(load-theme 'kooten t)
;;(load-theme 'material t)
;;(load-theme 'naga-theme t)
;;(load-theme 'nyx-theme t)
;;(load-theme 'termbright-theme t)
;;(load-theme 'timu-rouge t)

;;(require 'doom-themes)

;; Global settings (defaults)
;;  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;        doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each
;; theme may have their own settings.
;;  (load-theme 'doom-one t)

;; Enable flashing mode-line on errors
;;  (doom-themes-visual-bell-config)

;; Enable custom neotree theme
;;  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
;;Themes in this pack:
;; - doom-1337 -- ported from VSCode's 1337 theme (ported by @ccmywish)
;; - doom-acario-dark -- an original dark theme (ported by @gagbo)
;; - doom-acario-light -- an original light theme (ported by @gagbo)
;; - doom-ayu-dark -- inspired by Ayu Dark (ported by @ashton)
;; - doom-ayu-light -- inspirted by Ayu Light (ported by @LoveSponge)
;; - doom-ayu-mirage -- inspired by Ayu Mirage (ported by @LoveSponge)
;; - doom-badger -- inspired by cann's Badger colorscheme (ported by @jsoa)
;; - doom-challenger-deep -- inspired by Vim's Challenger Deep theme (ported by @fuxialexander)
;; - doom-city-lights -- inspired by Atom's City Lights theme (ported by @fuxialexander)
;; - doom-dark+ -- ported from equinusocio's VSCode Theme, dark+ (ported by @ema2159)
;; - doom-dracula -- inspired by the popular Dracula theme (ported by @fuxialexander)
;; - doom-earl-grey -- a gentle color scheme, for code (ported by @JuneKelly)
;; - doom-ephemeral -- inspired by the Ephemeral Theme from elenapan's dotfiles (ported by @karetsu)
;; - doom-fairy-floss -- a candy colored theme by sailorhg (ported by @ema2159)
;; - doom-flatwhite -- inspired by Atom's Flatwhite Syntax theme (ported by @JuneKelly)
;; - doom-gruvbox -- inspired by morhetz's Gruvbox (ported by @JongW)
;; - doom-gruvbox-light -- inspired by morhetz's Gruvbox (light) (ported by @jsoa)
;; - doom-henna -- based on VSCode's Henna theme (ported by @jsoa)
;; - doom-homage-black -- a minimalistic, colorless theme inspired by eziam, tao, and jbeans (ported by @mskorzhinskiy)
;; - doom-homage-white -- minimal white theme inspired by editors from 2000s (ported by @mskorzhinskiy)
;; - doom-horizon -- ported from VSCode Horizon (ported by @karetsu)
;; - doom-Iosvkem -- ported from the default dark theme for Adobe Brackets (ported by @neutaaaaan)
;; - doom-ir-black -- ported from Vim's ir_black colorscheme (ported by @legendre6891)
;; - doom-lantern -- based on Gitleptune's Lantern theme (ported by @paladhammika)
;; - doom-laserwave -- a clean synthwave/outrun theme inspired by VSCode's Laserwave (ported by @hyakt)
;; - doom-manegarm -- an original autumn-inspired dark theme (ported by @kenranunderscore)
;; - doom-material -- adapted from equinusocio's Material themes (ported by @tam5)
;; - doom-material-dark -- inspired by Material Theme by xrei (ported by @trev-dev)
;; - doom-meltbus -- a dark (mostly) monochromatic theme (ported by @spacefrogg)
;; - doom-miramare -- a port of Franbach's Miramare theme; a variant of Grubox (ported by @sagittaros)
;; - doom-molokai -- inspired by Tomas Restrepo's Molokai (ported by @hlissner)
;; - doom-monokai-classic -- port of Monokai Classic (ported by @ema2159)
;; - doom-monokai-machine -- port of Monokai Classic (ported by @minikN)
;; - doom-monokai-octagon -- port of Monokai Octagon (ported by @minikN)
;; - doom-monokai-pro -- Port of Monokai Pro (ported by @minikN)
;; - doom-monokai-ristretto -- Port of Monokai Ristretto (ported by @minikN)
;; - doom-monokai-spectrum -- port of Monokai Spectrum (ported by @minikN)
;; - doom-moonlight -- inspired by VS code's Moonlight (ported by @Brettm12345)
;; - doom-nord -- dark variant of Nord (ported by @fuxialexander)
;; - doom-nord-aurora -- a light variant of Nord (ported by @MoskitoHero)
;; - doom-nord-light -- light variant of Nord (ported by @fuxialexander)
;; - doom-nova -- inspired by Trevord Miller's Nova (ported by @bigardone)
;; - doom-oceanic-next -- inspired by Oceanic Next (ported by @juanwolf)
;; - doom-old-hope -- inspired by An Old Hope, in a galaxy far far away (ported by @teesloane)
;; - doom-one -- inspired by Atom One Dark (ported by @hlissner)
;; - doom-one-light -- inspired by Atom One Light (ported by @ztlevi)
;; - doom-opera -- an original light theme (ported by @jwintz)
;; - doom-opera-light -- an original light theme (ported by @jwintz)
;; - doom-outrun-electric -- a high contrast, neon theme inspired by Outrun Electric on VSCode (ported by @ema2159)
;; - doom-palenight -- adapted from equinusocio's Material themes (ported by @Brettm12345)
;; - doom-peacock -- inspired by daylerees' Peacock (ported by @teesloane)
;; - doom-plain -- inspired by gko's plain theme for VSCode (ported by @das-s)
;; - doom-plain-dark -- inspired by gko's plain theme for VSCode (ported by @das-s)
;; - doom-rouge -- ported from VSCode's Rouge Theme (ported by @das-s)
;; - doom-shades-of-purple -- a port of VSCode's Shades of Purple (ported by @jwbaldwin)
;; - doom-snazzy -- inspired by Hyper Snazzy (ported by @ar1a)
;; - doom-solarized-dark -- a dark variant of Solarized (ported by @ema2159)
;; - doom-solarized-dark-high-contrast -- a high-contrast variant of Solarized Dark (ported by @jmorag)
;; - doom-solarized-light -- a light variant of Solarized (ported by @fuxialexander)
;; - doom-sourcerer -- a port of xero's Sourcerer (ported by @fm0xb)
;; - doom-spacegrey -- I'm sure you've heard of it (ported by @teesloane)
;; - doom-tokyo-night -- inspired by VSCode's Tokyo Night theme (ported by @FosterHangdaan)
;; - doom-tomorrow-day -- a light variant of Tomorrow (ported by @emacswatcher)
;; - doom-tomorrow-night -- One of the dark variants of Tomorrow (ported by @hlissner)
;; - doom-vibrant -- a more vibrant variant of doom-one (ported by @hlissner)
;; - doom-wilmersdorf -- port of Ian Pan's Wilmersdorf (ported by @ema2159)
;; - doom-xcode -- based off of Apple's Xcode Dark Theme (ported by @kadenbarlow)
;; - doom-zenburn -- port of the popular Zenburn theme (ported by @jsoa)

;; Pretty Appearance
(use-package org-superstar
  :config
  (setq org-superstar-special-todo-items t)
  (setq org-hide-leading-stars t)
  (add-hook 'org-mode-hook (lambda ()
                             (org-superstar-mode 1))))

(setq org-pretty-entities t)

(setq org-hide-emphasis-markers t)


(use-package org-appear
  :hook (org-mode . org-appear-mode))

(with-eval-after-load 'org-superstar
  (setq org-superstar-item-bullet-alist
        '((?* . ?•)
          (?+ . ?➤)
          (?- . ?-)))
  (setq org-superstar-special-todo-items t))

  ;; Enable custom bullets for TODO items
;;  (setq org-superstar-todo-bullet-alist
;;        '(("TODO" . ?☐)
;;          ("NEXT" . ?✒)
;;          ("HOLD" . ?✰)
;;          ("WAITING" . ?☕)
;;          ("CANCELLED" . ?✘)
;;          ("DONE" . ?✔)))
;;  (org-superstar-restart))
(setq org-ellipsis " ▼ ")

(global-display-line-numbers-mode)
;;(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;Mode line
;;(use-package moody
;;  :config
;;  (setq x-underline-at-descent-line t)
;;  (moody-replace-mode-line-buffer-identification)
;;  (moody-replace-vc-mode))


;;Scroll conservatively
(setq scroll-conservatively 100)

;;Highlight current line
(global-hl-line-mode)

;;					Scroll and Tool bar modes
(recentf-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
