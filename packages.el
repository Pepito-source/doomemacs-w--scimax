;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
;;(package! flycheck-aspell)

(package! async)

(package! elfeed-goodies)

(package! resize-window)

(package! evil-tutor)

(package! org-appear)

(package! moody)

(package! org-superstar)

(package! aggressive-indent)
(package! auto-complete)
(package! button-lock)
(package! multiple-cursors)
(package! dashboard)
(package! google-this)
(package! esup)
(package! flx)
(package! hy-mode)
(package! jedi)
(package! jedi-direx)
(package! lispy)
(package! magithub)
(package! mustache)
(package! jupyter)
(package! ov)
(package! pdf-tools)
(package! org-mime)
(package! pydoc)
(package! smart-mode-line)
(package! undo-tree)
(package! pretty-hydra)
(package! ob-ipython)
(package! smex)
(package! org-mac-link)
(package! ggtags)
(package! ibuffer-projectile)
(package! org-ql)
(package! gitter)
;;(package! ess-site)

;;(package! scimax
;;  :recipe (:host github :repo "jkitchin/scimax"))


(package! org-ref)
(package! org-ref-prettify)
(package! emacsql-sqlite)
(package! ess-smart-equals)
(package! ess-smart-underscore)
(package! ivy-xref)
