;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Vincent Montero"
      user-mail-address "vincent_montero@icloud.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

(setq org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/02_work")

;;(load-library "find-lisp")
;;(add-hook 'org-agenda-mode-hook (lambda ()
;;                                  (setq org-agenda-files
;;                                        (find-lisp-find-files org-directory "\.org$"))
;;                                  ))

;;(setq org-agenda-files (quote ("~/Library/Mobile Documents/com~apple~CloudDocs/02_work")))
;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq current-language-environment "UTF-8")

(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))



;;(setq scimax-dir "~/.emacs.d/.local/straight/repos/scimax")

;;(load-file "~/.emacs.d/.local/straight/repos/scimax/init.el")

;;(add-to-list 'load-path "~/.emacs.d/.local/straight/repos/scimax")

(load-directory "~/.config")


;;(require 'package)
;;(add-to-list 'package-archives
;;             '("melpa" . "https://melpa.org/packages/"))
;;(package-refresh-contents)
;;(package-initialize)

;;(unless (package-installed-p 'use-package)
;;  (package-install 'use-package))
;;(setq use-package-always-ensure t)

(after! citar
  (setq! citar-bibliography '("~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/master.bib"
                              "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/pain.bib"
                              "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/capsaicin.bib"
                              "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/placebo-anxiety.bib"
                              "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/thesis.bib"))
  (setq! citar-library-paths '("~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-pdfs"))
  (setq! citar-notes-paths '("~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/notes/")))

(setq! bibtex-completion-bibliography '("~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/master.bib"
                                        "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/pain.bib"
                                        "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/capsaicin.bib"
                                        "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/placebo-anxiety.bib"
                                        "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/thesis.bib")
       bibtex-completion-library-path '("~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-pdfs")
       bibtex-completion-notes-path '("~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/notes/"))

(setq! ido-mode nil)
