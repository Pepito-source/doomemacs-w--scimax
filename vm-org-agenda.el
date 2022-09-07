;;; vm-org-agenda.el -*- lexical-binding: t; -*-
;; Set agenda configuration for org-mode
;; Code


(setq org-agenda-files (append
                        '("/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/hopital/douleur/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/hopital/pharmacometrie/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/hopital/stresam/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/hopital/cannapark/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/hopital/csh/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/assos/amipbm/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/assos/fnsipbm/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/perso/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/univ/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/univ/biology/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/univ/chemistry/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/univ/conseil-scientifique/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/univ/communications/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/univ/computer-science/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/univ/funding/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/univ/teaching/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/univ/these-pharma/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/univ/these-science/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/univ/writing-articles/")
                        (scimax-journal-get-list-of-entries)))
