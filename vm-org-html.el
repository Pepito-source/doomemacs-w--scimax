;;				Last Update HTML
(defun my-org-html-postamble (plist)
 (format "Last update : %s" (format-time-string "%d %b %Y")))
(setq org-html-postamble 'my-org-html-postamble)
