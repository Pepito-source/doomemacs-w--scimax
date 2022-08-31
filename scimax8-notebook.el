;;; scimax-notebook.el ---    -*- lexical-binding: t -*-

;;; Commentary:
;; This is an experiment in using scimax and org-mode for scientific notebook
;; purposes. The idea is you have a "project" that is a set of org and other
;; files under version control (git). There is a "master" file that is the
;; starting point, e.g. the README.org file. You can use `projectile' to switch
;; between projects easily, or search/find files within a project.
;;
;; `nb-new' is command to create a new project, it is just a thin wrapper that
;; creates the directories, registers them with projectile, and opens the master
;; file.
;;
;; `nb-open' is a command to open an existing project. It is a thin wrapper
;; around the projectile-switch-project command that opens the master file.
;;
;; `nb-agenda' to see the TODO items within a project, or do other org-agenda
;; things within the scope of the project, e.g. search by tag/property.
;;
;; `nb-archive' creates a zip-archive of the project.
;;
;; Note there is a projectile hydra defined: `hydra-projectile/body' that may be
;; useful for scimax-notebooks.

;;; Code:

;; * Setup
(projectile-mode +1)

(use-package ggtags)
(use-package ibuffer-projectile)
(when (executable-find "ag")
  (use-package ag))

(use-package org-ql)

(require 'scimax-apps)

(defcustom nb-notebook-directory
  "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/"
  "Directory where projects are stored."
  :group 'scimax-notebook
  :type 'directory)


(unless (file-directory-p nb-notebook-directory)
  (make-directory nb-notebook-directory t))


(defcustom nb-main-file (lambda (&optional name)
			    "Return the main file name for the project."
			    "README.org")
  "A function that returns the main file in each project.
The function must take one optional argument that is a project
name. This function will be run in the root directory of the
project. The function should return a string of the main file
name. See `nb-example-main' for an example of a computed main
file."
  :group 'scimax-notebook
  :type 'function)

(defcustom nb-project-type 'git
  "Symbol for what type of project to make.
projectile will just put a .projectile file in the directory
git will initialize the directory as a git repo."
  :group 'scimax-notebook
  :type 'symbol)


(defun nb-example-main (&optional name)
  "Return the main filename for the project of NAME.
NAME is optional, and if it is nil, compute the filename from the
current directory. In this example the main file is an org-file
with the name of the root directory, with a @ prefix so it sorts
to the top of the directory with ls."
  (concat "@"
	  (file-name-base (directory-file-name default-directory))
	  ".org"))

(defcustom nb-switch-project-action
  (lambda ()
    (find-file (read-file-name "File: " "." (funcall nb-main-file))))
  "Function to run after switching projects with `nb-open'."
  :group 'scimax-notebook
  :type 'function)

;;;###autoload
(defun nb-new (name)
  "Create a new project of NAME in `nb-notebook-directory'."
  (interactive (list (read-directory-name "New project name: " nb-notebook-directory)))
  (when (file-directory-p name)
    (user-error "%s already exists." name))
  (let ((dir (file-name-as-directory (expand-file-name name nb-notebook-directory)))
	(nb-main-file-name (funcall nb-main-file name)))
    (unless (file-directory-p dir)
      (make-directory dir t)
      (cond ((eq 'git nb-project-type)
	     (let ((default-directory dir))
	       (shell-command "git init")
	       ;; We also should have a .projectile. I think this will make some
	       ;; searches work on files not committed to the git repo. In any
	       ;; case, it should not hurt anythin.
	       (shell-command "touch .projectile")))
	    ((eq 'projectile nb-project-type)
	     (let ((default-directory dir))
	       (shell-command "touch .projectile")))
	    (t
	     (error "Unknown kind of project: %s" nb-project-type))))
    (projectile-add-known-project dir)
    (projectile-save-known-projects)
    (find-file (expand-file-name nb-main-file-name dir))))

;;;###autoload
(defun nb-open ()
  "Switch to a project and open the main file.
This is a thin wrapper on `projectile-switch-project' that opens the main file."
  (interactive)
  (let ((projectile-switch-project-action nb-switch-project-action))
    (projectile-switch-project)))

;;;###autoload
(defun nb-git-clone (url path)
  "Clone a git repo at URL as a project at PATH in `nb-notebook-directory'.
The URL and PATH should work in a command like: git clone URL
PATH. You need to specify the path you want the file to be in. A
default name based on the url is suggested."
  (interactive (list (read-string "git url: ") nil))
  (setq path (read-directory-name "Path: " nb-notebook-directory
				  nil nil
				  (replace-regexp-in-string
				   "\\.git\\'" ""
				   (car (last (f-split url))))))
  (let ((default-directory nb-notebook-directory))
    (when (file-exists-p path)
      (error "%S already exists" path))
    (make-directory path t)
    (shell-command-to-string (format "git clone %s \"%s\"" url path))
    (dired path)))

;;;###autoload
(defun nb-clone ()
  "Create a clone (by a recursive copy) of the current notebook."
  (interactive)
  (let* ((project-root (projectile-project-root))
	 (dir-one-up (file-name-directory (directory-file-name project-root)))
	 (name (file-name-base (directory-file-name project-root)))
	 (clone-base-name (read-directory-name
			   "Clone name: "
			   dir-one-up  nil nil
			   (concat name "-clone"))))
    (let ((default-directory dir-one-up))
      (shell-command (format "cp -R %s %s" name clone-base-name))
      (projectile-add-known-project clone-base-name)
      (projectile-save-known-projects)
      (projectile-switch-project-by-name clone-base-name))))

(defcustom nb-agenda-files nil
  "A file, a list of files or function to generate a list of org-files to make an agenda from.
The function should return a string filepath or list of absolute
file paths. The function will be run in the root project
directory. You may want to make this a directory local variable."

  :group 'scimax-notebook
  :type '(string list function))


(defun nb-org-files ()
  "Return a list of org files in the current notebook.
You should manually invalidate the cache with
`projectile-invalidate-cache' if it causes a problem.
For example, if you delete or rename an org-file you will get an error that it cannot be opened."
  (mapcar
   (lambda (f)
     (f-join (projectile-project-root) f))
   (-filter (lambda (f)
	      (and (f-ext? f "org")
		   (not (s-contains? "#" f))))
	    (projectile-project-files
	     (projectile-ensure-project
	      (projectile-project-root))))))


;;;###autoload
(defun nb-agenda (&optional all-org-files)
  "Show org-agenda for org-files in the current notebook."
  (interactive "P")
  (let ((org-agenda-files (if (or all-org-files
				  (null nb-agenda-files))
			      (nb-org-files)
			    (cond
			     ((listp nb-agenda-files)
			      nb-agenda-files)
			     ((functionp nb-agenda-files)
			      (let ((default-directory (projectile-project-root
							(projectile-project-name))))
				(funcall nb-agenda-files)))
			     (t
			      nb-agenda-files)))))
    (org-agenda)))

(defun nb-search-agenda (&optional todo-only match)
  "Search notebook agenda similar to `org-tags-vew'."
  (interactive "P")
  (let ((org-agenda-files (nb-org-files)))
    (org-tags-view todo-only match)))

;;;###autoload
(defun nb-git-archive ()
  "Create an archive of the current notebook.
This uses git archive to create an archive of the current state
of the notebook. The zip file will be timestamped in the root
project directory. If your repo contains untracked files or
uncommitted changes, you will be prompted to continue."
  (let ((output (shell-command-to-string "git status --porcelain")))
    (unless (string= "" output)
      (when
	  (y-or-n-p
	   (format
	    "Your notebook contains uncommitted changes or files:\n%s\n Continue? " output))
	(shell-command
	 (format
	  "git archive --format zip HEAD -o \"%s-%s.zip\""
	  (f-join (projectile-project-root)
		  (car (last (f-split (projectile-project-root)))))
	  (format-time-string "%Y-%m-%d-%H:%M%p")))))))

(defcustom nb-archive-command "zip"
  "Command to make archives.
An alternative is tar."
  :group 'scimax-notebook
  :type 'string)


(defcustom nb-archive-command-options "-r"
  "Command options to make archives.
For tar you want -czf for a tar.gz
or tar -cjf for a bzipped file"
  :group 'scimax-notebook
  :type 'string)


(defcustom nb-archive-extension ".zip"
  "Default extension for the archive.
For tar with -czf I recommend .tar.gz
For tar with -cjf I recommend .tbz2"
  :group 'scimax-notebook
  :type 'string)


(defun nb-archive (zip-file)
  "Create an archive file of the project.
The type of archive is determined by `nb-archive-command'."
  (interactive (list (read-string
		      "Archive name: "
		      (concat (projectile-project-name) nb-archive-extension))))
  (let ((default-directory (projectile-project-root)))
    (message "%s"
	     (shell-command-to-string
	      (format-spec "%c %o %z ."
			   `((?c . ,nb-archive-command)
			     (?o . ,nb-archive-command-options)
			     (?z . ,zip-file)))))))

(defun nb-list-tags ()
  "Get a list of tags in the notebook."
  (interactive)
  (-uniq
   (-flatten
    (org-ql-query
      :select #'org-get-tags
      :from (nb-org-files)))))

(defun nb-set-tags ()
  "Set tags in current heading with tags from this project."
  (interactive)
  (save-excursion
    (unless (org-at-heading-p)
      (org-previous-visible-heading 1))
    (org-set-tags (completing-read
		   "Tag: "
		   ;; nb-list-tags goes through files, and may change the active
		   ;; buffer.
		   (save-window-excursion (nb-list-tags))))))

;;(progn
;;  (require 'helm-easymenu)
;;  (easy-menu-add-item
;;   '("Scimax") "notebook"
;;   '(["New notebook" nb-new t]
;;     ["Open notebook" nb-open t]
;;     ["Insert a notebook link" nb-insert-link t]
;;     ["Update project list" nb-update-scimax-projects-menu t]
;;     ("Projects"))
;;   "words"))


;;(defun nb-update-scimax-projects-menu ()
;;  "Update the projects menu."
;;  (interactive)
;;  (easy-menu-change
;;   '("Scimax" "notebook") "Projects"
;;   (mapcar (lambda (x)
;;	     (vector
;;	      ;; entry
;;	      (file-name-nondirectory (substring x 0 -1))
;;	      ;; action
;;	      `(lambda ()
;;		 (interactive)
;;		 (projectile-switch-project-by-name
;;		  ,x))
;;	      ;; visibility
;;	      t))
;;	   (projectile-relevant-known-projects))
;;   "words"))


;; update the project list once on loading.
;;(nb-update-scimax-projects-menu)

(use-package ivy-xref
;;  :ensure t
  :init (if (< emacs-major-version 27)
            (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
          (setq xref-show-definitions-function #'ivy-xref-show-defs)))


(defun nb-search (file-pattern regexp)
  "Search files matching FILE-PATTERN for REGEXP and show matches."
  (interactive "sfile pattern: \nsSearch for: ")
  (ivy-xref-show-xrefs
   (lambda ()
     (xref-matches-in-directory regexp file-pattern (projectile-project-root)
				nil))
   nil))


(defun nb-search-all (regexp)
  "Search files for REGEXP and show matches."
  (interactive "sSearch for: ")
  (let* ((files (projectile-project-files (projectile-project-root)))
	 (ignores (nconc (mapcar
                          (lambda (s) (concat s "/"))
                          grep-find-ignored-directories)
                         (append '("*.pdf") grep-find-ignored-files)))
	 ;; This was a little surprising, I had to add -a in a few places to treat binary files like text
	 (grep-host-defaults-alist '((localhost
				      (grep-command "grep  -nH --null -e -a ")
				      (grep-template "grep <X> <C> -nH --null -e <R> <F>")
				      (grep-use-null-device nil)
				      (grep-find-command ("find . -type f -exec grep  -nH --null -e -a \\{\\} +" . 42))
				      (grep-find-template "find <D> <X> -type f <F> -exec grep <C> -nH --null -e <R> -a \\{\\} +")
				      (grep-use-null-filename-separator t) (grep-find-use-xargs exec-plus)
				      (grep-highlight-matches nil))))

	 (xrefs (cl-mapcan
                 (lambda (file)
		   (when (file-exists-p file)
                     (xref-matches-in-directory regexp "*" file
                                           (and (file-directory-p file)
						ignores))))
                 files)))
    (unless xrefs
      (user-error "No matches for: %s" regexp))
    (ivy-xref-show-xrefs
     (lambda ()
       xrefs)
     nil)))

(defun nb-search-title ()
  "Select a notebook file by title, author, date or filename."
  (interactive)
  (let* ((project-root (projectile-project-root))
	 (org-files (mapcar (lambda (f)
			      (expand-file-name f project-root))
			    (-filter (lambda (f) (f-ext? f "org"))
				     (projectile-project-files project-root))))
	 data
	 candidates
	 format-string
	 title
	 author
	 date
	 ntitle
	 nauthor
	 ndate)

    ;; Get title, author date and filename for each org-file. If there is no date, we
    ;; use last modified time. It doesn't seem possible to get the file creation
    ;; time. I am not sure this is robust for all things you might put into the
    ;; DATE field. I assume it is something org can read and convert to a time.
    (setq data (mapcar
		(lambda (f)
		  (when (file-exists-p f)
		    (with-temp-buffer
		      (insert-file-contents f)
		      (setq title (if (re-search-forward "#\\+TITLE:\\(.*\\)" nil t)
				      (match-string 1)
				    "No title"))
		      (goto-char (point-min))
		      (setq date (if (re-search-forward "#\\+DATE:\\(.*\\)" nil t)
				     (format-time-string "%Y-%m-%d"
							 (org-read-date nil t (match-string 1)))
				   (format-time-string
				    "(last modified) %Y-%m-%d"
				    (file-attribute-modification-time
				     (file-attributes f)))))
		      (goto-char (point-min))
		      (setq author (if (re-search-forward "#\\+AUTHOR:\\(.*\\)" nil t)
				   (match-string 1)
				 ""))
		      (list title author date f))))
		org-files))

    ;; Sort by date, more recent things will be first
    (setq data (cl-sort (copy-sequence data)
			(lambda (a b) (org-time> (nth 2 a) (nth 2 b)))))

    (setq ntitle (apply 'max (mapcar (lambda (e) (length (nth 0 e))) data))
	  nauthor (apply 'max (mapcar (lambda (e) (length (nth 1 e))) data))
	  ndate (apply 'max (mapcar (lambda (e) (length (nth 2 e))) data)))

    ;; Now create a format string so the longest title fits and is aligned.
    (setq format-string (format "%%%ss | %%%ss | %%%ss | %%s"
				ntitle nauthor ndate))
    ;; These are the candidates we will choose from.
    (setq candidates (cl-loop for (title author date f) in data
			      collect
			      (list (format format-string title author date f) f)))

    ;; I use completing-read here so you can use your own backend. The only
    ;; downside is I can't put many actions like in a dedicated ivy command.
    (find-file (cadr (assoc
		      (completing-read "Open: " candidates)
		      candidates)))))

(defun nb-help ()
  "Open the org-file describing how to use scimax-notebook."
  (interactive)
  (find-file (expand-file-name "scimax-notebook.org" scimax-dir)))

(defun nb-assign-task ()
  "Mark current headline with TODO, set a deadline and add set ASSIGNEDTO property with email.
You will be prompted for contacts in the project.

TODO: add a time stamp or log entry or date assigned property?"
  (interactive)
  (save-excursion
    (unless (org-at-heading-p)
      (org-previous-visible-heading 1))
    (org-todo "TODO")
    (let* ((participants (org-ql-query
			   :select '(cons (fifth (org-heading-components))
					  (org-entry-get (point) "EMAIL"))
			   :from (nb-org-files)
			   :where '(and (property "EMAIL"))))
	   (emails (org-entry-get-multivalued-property (point) "ASSIGNEDTO")))
      (ivy-read "Assign to: " participants
		:action '(1
			  ("a"
			   (lambda (participant)
			     (setq emails (append
					   (org-entry-get-multivalued-property (point) "ASSIGNEDTO")
					   (list (cdr participant))))
			     (with-ivy-window
			       (apply 'org-entry-put-multivalued-property (point)
				      "ASSIGNEDTO" emails)
			       (unless (org-get-deadline-time nil)
				 (org-deadline nil))
			       ;; Maybe this should not be hard-coded, in case
			       ;; you want it to be something else, like a TASK
			       (unless (string= "TODO" (org-get-todo-state))
				 (org-todo "TODO"))))
			   "Assign"))))))


(defun nb-contacts ()
  "Jump to a contact in the notebook.
Maybe I should add more actions."
  (interactive)
  (let* ((participants (org-ql-query
			 :select '(list
				   ;; contact heading
				   (fifth (org-heading-components))
				   ;; buffer the contact is in
				   (buffer-name)
				   (point))
			 :from (nb-org-files)
			 :where '(and (property "EMAIL")))))
    (ivy-read "Jump to: " participants
	      :action '(1
			("o" (lambda (choice)
			       (pop-to-buffer (second choice))
			       (goto-char (third choice))
			       (outline-show-entry)))))))

(defun nb-parse-path (path)
  "Parse PATH into parts.
PATH is a :: separated string with up to 3 parts.
Returns a list of (project fpath link-target).
The link target is optional, and defaults to line 1."
  ;; Somehow split-string must change match-data. This messes up
  ;; fontification...
  (save-match-data
    (let* ((parts (split-string path "::")))
      (when (> (length parts) 3)
	(error "There should only be 3 parts separated by ::"))
      (list
       (nth 0 parts)
       (nth 1 parts)
       (or (nth 2 parts) "1")))))

(defun nb-follow (path)
  "Open the project at PATH."
  (interactive (list (org-element-property :path (org-element-context))))
  (cl-destructuring-bind (project fpath link-target) (nb-parse-path path)
    (let* ((projects (remove nil (append (projectile-relevant-known-projects)
					 (list
					  (when (projectile-project-p)
					    (projectile-project-root))))))
	   ;; These are projects that match the project spec
	   (project-candidates (-filter (lambda (p)
					  (string-match (concat project "/\\'") p))
					projects))
	   ;; These are projects that match the spec, and that have the file we want.
	   (candidates (-filter (lambda (p)
				  (file-exists-p (expand-file-name fpath p)))
				project-candidates)))
      (cond
       ((null project-candidates)
	(error "%s is not a known project" project))
       ((null candidates)
	(error "%s was not found in %s\nproject-candidates: %S\ncandidates: %s" fpath project project-candidates candidates))
       ;; one project, and the file exists
       ((and (= 1 (length candidates))
	     (file-exists-p (expand-file-name fpath (car candidates))))
	(org-mark-ring-push)
	(find-file (expand-file-name fpath (car candidates))))
       ;; multiple matches, select project interactively
       (t
	(org-mark-ring-push)
	(find-file (expand-file-name fpath (completing-read "Project: " candidates)))))
      ;; If we get here, we have not errored and should have opened a file. Now,
      ;; link-target the end link.
      (cond
       ((eq major-mode 'org-mode)
	(when (not (or (null link-target) (string= "" link-target)))
	  (cond
	   ((string-match "\\<[0-9]+\\>" link-target)
	    (forward-line (- (string-to-number link-target) 1)))
	   ((string-match "\\<c\\([0-9]+\\)\\>" link-target)
	    (goto-char (string-to-number (match-string 1 link-target))))
	   (t
	    (org-link-open-from-string (format "[[%s]]" link-target)))))
	(org-show-entry))
       ;; everything else
       (t
	(cond
	 ;; if it is just a number it is a line number
	 ((string-match "\\<[0-9]*\\>" link-target)
	  (forward-line (- (string-to-number link-target) 1)))
	 ;; a pattern like c23 means go to char 23
	 ((string-match "^c\\([0-9]*\\)" link-target)
	  (goto-char (string-to-number (match-string 1 link-target))))
	 (t
	  (goto-char (point-min))
	  (goto-char (re-search-forward (regexp-quote link-target) nil t)))))))))

(defun nb-follow-other (path &optional new-frame)
  "Open the project at PATH in other window."
  (interactive (list (org-element-property :path (org-element-context))
		     current-prefix-arg))
  (cl-destructuring-bind (project fpath link-target) (nb-parse-path path)
    (let* ((projects (remove nil (append (projectile-relevant-known-projects)
					 (list
					  (when (projectile-project-p)
					    (projectile-project-root))))))
	   ;; These are projects that match the project spec
	   (project-candidates (-filter (lambda (p)
					  (string-match (concat project "/\\'") p))
					projects))
	   ;; These are projects that match the spec, and that have the file we want.
	   (candidates (-filter (lambda (p)
				  (file-exists-p (expand-file-name fpath p)))
				project-candidates)))
      (cond
       ((null project-candidates)
	(error "%s is not a known project" project))
       ((null candidates)
	(error "%s was not found in %s\nproject-candidates: %S\ncandidates: %s" fpath project project-candidates candidates))
       ;; one project, and the file exists
       ((and (= 1 (length candidates))
	     (file-exists-p (expand-file-name fpath (car candidates))))
	(org-mark-ring-push)
	(if new-frame
	    (find-file-other-frame (expand-file-name fpath (car candidates)))
	  (find-file-other-window (expand-file-name fpath (car candidates)))))
       ;; multiple matches, select project interactively
       (t
	(org-mark-ring-push)
	(if new-frame
	    (find-file-other-frame (expand-file-name fpath (completing-read "Project: " candidates)))
	  (find-file-other-window (expand-file-name fpath (completing-read "Project: " candidates))))))
      ;; If we get here, we have not errored and should have opened a file. Now,
      ;; link-target the end link.
      (cond
       ((eq major-mode 'org-mode)
	(when (not (or (null link-target) (string= "" link-target)))
	  (cond
	   ((string-match "\\<[0-9]+\\>" link-target)
	    (forward-line (- (string-to-number link-target) 1)))
	   ((string-match "\\<c\\([0-9]+\\)\\>" link-target)
	    (goto-char (string-to-number (match-string 1 link-target))))
	   (t
	    (org-link-open-from-string (format "[[%s]]" link-target)))))
	(org-show-entry))
       ;; everything else
       (t
	(cond
	 ;; if it is just a number it is a line number
	 ((string-match "\\<[0-9]*\\>" link-target)
	  (forward-line (- (string-to-number link-target) 1)))
	 ;; a pattern like c23 means go to char 23
	 ((string-match "c\\([0-9]*\\)" link-target)
	  (goto-char (string-to-number (match-string 1 link-target))))
	 ;; Everything else means search for it. I don't know why I have to
	 ;; use goto-char here. I thought it should just go.
	 (t
	  (goto-char (re-search-forward link-target nil 'mv)))))))))


(defun nb-follow-other-frame (path)
  "Follow path in other frame."
  (interactive (list (org-element-property :path (org-element-context))))
  (nb-follow-other path t))

(defun nb-follow-sys (path)
  "Open the project at PATH with a system program."
  (interactive (list (org-element-property :path (org-element-context))))
  (cl-destructuring-bind (project fpath link-target) (nb-parse-path path)
    (let* ((projects (remove nil (append (projectile-relevant-known-projects)
					 (list
					  (when (projectile-project-p)
					    (projectile-project-root))))))
	   ;; These are projects that match the project spec
	   (project-candidates (-filter (lambda (p)
					  (string-match (concat project "/\\'") p))
					projects))
	   ;; These are projects that match the spec, and that have the file we want.
	   (candidates (-filter (lambda (p)
				  (file-exists-p (expand-file-name fpath p)))
				project-candidates)))
      (cond
       ((null project-candidates)
	(error "%s is not a known project" project))
       ((null candidates)
	(error "%s was not found in %s\nproject-candidates: %S\ncandidates: %s" fpath project project-candidates candidates))
       ;; one project, and the file exists
       ((and (= 1 (length candidates))
	     (file-exists-p (expand-file-name fpath (car candidates))))
	(org-mark-ring-push)
	;; use system if possible.
	(org-open-file (expand-file-name fpath (car candidates)) '(16)))
       ;; multiple matches, select project interactively
       (t
	(error "no match found"))))))

(defun nb-store-link ()
  "Store a project link to a file in a project."
  (if (or (null (buffer-file-name)) (not (projectile-project-p)))
      nil
    (let* ((root (projectile-project-root))
	   (current-file (buffer-file-name))
	   (project (car (last (f-split (projectile-project-root)))))
	   (relpath (file-relative-name current-file root))
	   (link-target (format "c%s" (point))))

      (org-link-store-props
       :type "nb"
       ;; Note I use the concat here just to avoid fontifying errors in the link
       ;; in the org file.
       :link (format (concat "nb:" "%s::%s::%s") project relpath link-target)
       :description "")
      (format (concat "nb:" "%s::%s::%s") project relpath link-target))))

(defun nb-complete-link ()
  "Create a link with completion."
  ;; Pick a project
  (let* ((project-root  (projectile-completing-read
			 "Project: "
			 (projectile-relevant-known-projects)
			 :initial-input (projectile-project-root (projectile-project-name))))
	 (project (projectile-project-name project-root))
	 (file (completing-read "File: " (projectile-project-files project-root))))
    (format "nb:%s::%s" project file)))


(defun nb-insert-link ()
  "Insert a link with completion."
  (interactive)
  (insert (nb-complete-link)))

(defun nb-link-face (path)
  "Compute a face for the link.
If everything is in order it is an 'org-link.
If there are multiple projects it will be orange.
If we can't find a project or file, it will be red."
  ;; Something in here modifies the match-data which will mess up fontification.
  ;; We save it to avoid that.
  (save-match-data
    (let* ((parts (nb-parse-path path))
	   (project (nth 0 parts))
	   (fpath (nth 1 parts))
	   (follow (nth 2 parts))
	   (projects (append (projectile-relevant-known-projects)
			     (list (projectile-project-root))))
	   (project-candidates (-filter (lambda (p)
					  (string-match project (or p "")))
					projects))
	   ;; These are projects that match the spec, and that have the file we want.
	   (candidates (-filter (lambda (p)
				  (file-exists-p (expand-file-name fpath (or p ""))))
				project-candidates)))
      (cond
       ;; No project
       ((null candidates)
        '(:foreground "red"))
       ;; one project, and the file exists
       ((= 1 (length candidates))
	'(:foreground "darkviolet"))
       ;; Multiple projects seem to match.
       ((> (length candidates) 1)
        '(:foreground "orange"))))))

(defun nb-link-tooltip (_win _obj position)
  "A tooltip for the nb links."
  (save-match-data
    (save-excursion
      (goto-char position)
      (let ((path (org-element-property :path (org-element-context))))
  	(cl-destructuring-bind (project fpath follow) (nb-parse-path path)
  	  (let* ((projects (append (projectile-relevant-known-projects)
				   (list (projectile-project-root))))
  		 (project-candidates (-filter (lambda (p)
  						(string-match project p))
  					      projects))
  		 ;; These are projects that match the spec, and that have the file we want.
  		 (candidates (-filter (lambda (p)
  					(file-exists-p (expand-file-name fpath p)))
  				      project-candidates)))
  	    (cond
  	     ((null project-candidates)
  	      (format "%s is not a known project." project))

  	     ((null candidates)
  	      (format "%s not found in %s." fpath project))

  	     ;; There is one project, and the file is in it.
  	     ((= 1 (length candidates))
  	      ;; Show the path
  	      (expand-file-name fpath (car candidates)))

  	     ;; Multiple projects. We don't check for file existence
  	     ((> (length candidates) 1)
  	      (format "Multiple projects have %s: %S" fpath candidates))

  	     (t
  	      "Not sure what is going on with this one."))))))))

(defun nb-activate-link (start end path bracketp)
  "Activate a project link.
This is used to put image overlays on links.
START and END are the positions of the link.
PATH is the link PATH.
BRACKETP is non-nil for bracketed links."
  (cl-destructuring-bind (project fpath link-target) (nb-parse-path path)
    (if (and (string-match (image-file-name-regexp) fpath)
  	     (not (ov-at start)))
  	;; Find the image
  	(let* ((projects (remove nil (append (projectile-relevant-known-projects)
  					     (list
  					      (when (projectile-project-p)
  						(projectile-project-root))))))
  	       ;; These are projects that match the project spec
  	       (project-candidates (-filter (lambda (p)
  					      (string-match (concat project "/\\'") p))
  					    projects))
  	       ;; These are projects that match the spec, and that have the file we want.
  	       (candidates (-filter (lambda (p)
  				      (file-exists-p (expand-file-name fpath p)))
  				    project-candidates))
  	       (img-file (when (and (= 1 (length candidates))
  				    (file-exists-p (expand-file-name fpath (car candidates))))
  			   (expand-file-name fpath (car candidates)))))
  	  (when img-file
  	    (let* ((ov (make-overlay start end))
  		   (lnk (org-element-context))
  		   (parent (org-element-property :parent lnk))
  		   (ao (when parent (org-element-property :attr_org parent)))
  		   (width (when ao
  		   	    (plist-get
  		   	     (org-export-read-attribute :attr_org  parent) :width)))
  		   (img-file (if width
  		   		 (funcall  org-inline-image-resize-function img-file width)
  		   	       img-file))
  		   (img (create-image (or img-file )
  		   		      nil
  		   		      nil
  		   		      :width width)))

  	      (overlay-put ov 'display img)
  	      (overlay-put ov 'help-echo (expand-file-name fpath (car candidates)))
  	      (overlay-put ov 'face 'default)
  	      (overlay-put ov 'org-image-overlay t)
  	      (overlay-put ov 'modification-hooks
  	      		   (list 'org-display-inline-remove-overlay))
  	      (push ov org-inline-image-overlays)))))))

(defun nb-link-bash ()
  "Open the nb link at point in bash."
  (interactive)
  (let* ((link (org-element-context))
	 (path (org-element-property :path link)))
    (when (and (eq 'link (car link))
	       (string= "nb" (org-element-property :type link)))
      (cl-destructuring-bind (project fpath link-target) (nb-parse-path path)
	(let* ((projects (remove nil (append (projectile-relevant-known-projects)
					     (list
					      (when (projectile-project-p)
						(projectile-project-root))))))
	       ;; These are projects that match the project spec
	       (project-candidates (-filter (lambda (p)
					      (string-match (concat project "/\\'") p))
					    projects))
	       ;; These are projects that match the spec, and that have the file we want.
	       (candidates (-filter (lambda (p)
				      (file-exists-p (expand-file-name fpath p)))
				    project-candidates)))
	  (if (= 1 (length candidates))
	      (bash (expand-file-name (car candidates)))
	    (bash (read-string "Project: " candidates))))))))

(defun nb-link-explorer ()
  "Open the nb link at point in explorer/finder."
  (interactive)
  (let* ((link (org-element-context))
	 (path (org-element-property :path link)))
    (when (and (eq 'link (car link))
	       (string= "nb" (org-element-property :type link)))
      (cl-destructuring-bind (project fpath link-target) (nb-parse-path path)
	(let* ((projects (remove nil (append (projectile-relevant-known-projects)
					     (list
					      (when (projectile-project-p)
						(projectile-project-root))))))
	       ;; These are projects that match the project spec
	       (project-candidates (-filter (lambda (p)
					      (string-match (concat project "/\\'") p))
					    projects))
	       ;; These are projects that match the spec, and that have the file we want.
	       (candidates (-filter (lambda (p)
				      (file-exists-p (expand-file-name fpath p)))
				    project-candidates)))
	  (if (= 1 (length candidates))
	      (explorer (expand-file-name (car candidates)))
	    (explorer (read-string "Project: " candidates))))))))

(defun nb-link-projectile-find-file ()
  "Open the nb link at point with projectile."
  (interactive)
  (let* ((link (org-element-context))
	 (path (org-element-property :path link)))
    (when (and (eq 'link (car link))
	       (string= "nb" (org-element-property :type link)))
      (cl-destructuring-bind (project fpath link-target) (nb-parse-path path)
	(let* ((projects (remove nil (append (projectile-relevant-known-projects)
					     (list
					      (when (projectile-project-p)
						(projectile-project-root))))))
	       ;; These are projects that match the project spec
	       (project-candidates (-filter (lambda (p)
					      (string-match (concat project "/\\'") p))
					    projects))
	       ;; These are projects that match the spec, and that have the file we want.
	       (candidates (-filter (lambda (p)
				      (file-exists-p (expand-file-name fpath p)))
				    project-candidates)))
	  (if (= 1 (length candidates))
	      (let ((default-directory (expand-file-name (car candidates))))
		(projectile-completing-read "Find file: "
                                            (projectile-project-files
					     (projectile-project-root))
					    :initial-input fpath))))))))

(defun nb-event (event)
  "EVENT is from a mouse click.
We use this with C-mouse-1 on a link."
  (interactive "e")
  (with-selected-window (nth 0 (cadr event))
    (goto-char (nth 1 (cadr event)))
    (nb-hydra/body)))

(defvar nb-link-map (let ((map (copy-keymap org-mouse-map)))
		      (define-key map (kbd "M-o") 'nb-follow-other)
		      (define-key map (kbd "M-O") 'nb-follow-other-frame)
		      (define-key map (kbd "M-s") 'nb-follow-sys)
		      (define-key map (kbd "M-b") 'nb-link-bash)
		      (define-key map (kbd "M-e") 'nb-link-explorer)
		      (define-key map (kbd "M-f") 'nb-link-projectile-find-file)
		      (define-key map (kbd "M-h") 'nb-hydra/body)
		      (define-key map (kbd "<C-mouse-1>") 'nb-event)
		      map)
  "Key bindings for notebook links")

(org-link-set-parameters
 "nb"
 :follow #'nb-follow
 :store #'nb-store-link
 :complete #'nb-complete-link
 :help-echo #'nb-link-tooltip
 :activate-func #'nb-activate-link
 :face #'nb-link-face
 :keymap nb-link-map)

(defhydra nb-hydra (:hint nil :color blue)
  "scimax-notebook
"

  ("n" nb-new "new notebook" :column "navigation")
  ("o" nb-open "open notebook" :column "navigation")
  ("f" projectile-find-file-dwim "find-file" :column "navigation")
  ("d" projectile-find-dir "find dir" :column "navigation")
  ("D" projectile-dired "open root in dired" :column "navigation")
  ("c" nb-contacts "Notebook contact" :column "navigation")

  ("sa" nb-search-all "search all files" :column "search")
  ("ss" nb-search "search some files" :column "search")
  ("sb" projectile-multi-occur "search nb buffers" :column "search")
  ("st" nb-search-title "Search by title/date" :column "search")

  ("a" nb-agenda "notebook agenda" :column "utilities")
  ("t" nb-set-tags "set tags" :column "utilities")
  ("b" bash "bash" :column "utilities")
  ("e" explorer "explorer" :column "utilities")
  ("p" hydra-projectile/body "projectile" :column "utilities")
  ("r" nb-archive "make archive" :column "utilities")

  ("l" nb-insert-link "Insert link" :column "link")
  ("C-<return>" nb-follow-other "open other window" :column "link")
  ("S-<return>" nb-follow-other-frame "Open other frame" :column "link")
  ("M-<return>" nb-follow-sys "Open with system program" :column "link"))

;; * The end

(provide 'scimax-notebook)

;;; scimax-notebook.el ends here
