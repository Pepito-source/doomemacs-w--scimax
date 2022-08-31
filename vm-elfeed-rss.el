;;;Code

(use-package elfeed
  :config
  (setq elfeed-search-feed-face ":foreground #fff :weight bold"
        elfeed-feeds (quote
                       (
                        ("http://feeds.feedburner.com/acs/jmcmar" ACSJMedChem)
                        ("http://feeds.feedburner.com/acs/amclct" ACSMedChem Letters)
                        ("http://feeds.feedburner.com/acs/aptsfn" ACSPharmacolTransla)
                        ("http://feeds.feedburner.com/acs/mpohbp" ACSMolecularPharmaceutics)
                        ("http://feeds.feedburner.com/acs/jacsat" ACSJournal ACS)("http://feeds.nature.com/nchem/rss/current" NatureChemistry)
                        ("http://feeds.nature.com/nrd/rss/current" NatureDrugDiscovRev)
                        ("http://feeds.nature.com/ncomms/rss/current" NatureCom)("https://www.science.org/action/showFeed?type=etoc&feed=rss&jc=stm" ScienceTransla)
                        ("https://www.science.org/action/showFeed?type=etoc&feed=rss&jc=science" Science)
                        ("https://www.science.org/blogs/pipeline/feed" SciencePipeline)
                        ("https://www.mdpi.com/rss/journal/pharmaceuticals" Pharmaceuticals)
                        ("https://www.mdpi.com/rss/journal/molecules" Molecules)
                        ("https://www.mdpi.com/rss/journal/cancers" Cancers)
                        ("https://www.mdpi.com/rss/journal/pharmaceutics" Pharmaceutics)
                        ("https://rss.sciencedirect.com/publication/science/02235234" EurJMedChem)
                        ("https://onesearch-rss.nejm.org/api/specialty/rss?context=nejm&specialty=hematology-oncology" NEJMOncol)
                        ("https://onesearch-rss.nejm.org/api/specialty/rss?context=nejm&specialty=clinical-medicine" NEJMClinical)
                        ("https://www.reddit.com/r/linux.rss" reddit linux)
                        ("https://www.reddit.com/r/commandline.rss" reddit commandline)
                        ("https://www.reddit.com/r/distrotube.rss" reddit distrotube)
                        ("https://www.reddit.com/r/emacs.rss" reddit emacs)
                        ("https://www.gamingonlinux.com/article_rss.php" gaming linux)
                        ("https://hackaday.com/blog/feed/" hackaday linux)
                        ("https://opensource.com/feed" opensource linux)
                        ("https://linux.softpedia.com/backend.xml" softpedia linux)
                        ("https://itsfoss.com/feed/" itsfoss linux)
                        ("https://www.zdnet.com/topic/linux/rss.xml" zdnet linux)
                        ("https://www.phoronix.com/rss.php" phoronix linux)
                        ("http://feeds.feedburner.com/d0od" omgubuntu linux)
                        ("https://www.computerworld.com/index.rss" computerworld linux)
                        ("https://www.networkworld.com/category/linux/index.rss" networkworld linux)
                        ("https://www.techrepublic.com/rssfeeds/topic/open-source/" techrepublic linux)
                        ("https://betanews.com/feed" betanews linux)
                        ("http://lxer.com/module/newswire/headlines.rss" lxer linux)
                        ("https://distrowatch.com/news/dwd.xml" distrowatch linux)
                        ))))

(use-package elfeed-goodies
  :init
  (elfeed-goodies/setup)
  :config
  (setq elfeed-goodies/entry-pane-size 0.5))

(add-hook 'elfeed-show-mode-hook 'visual-line-mode)
(evil-define-key 'normal elfeed-show-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev)
(evil-define-key 'normal elfeed-search-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev)


;; drug discovery today
