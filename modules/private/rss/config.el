;  (setq rmh-elfeed-org-files '("~/SynologyDrive/rss/feeds.org"))

(use-package! elfeed)

(use-package! elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/SynologyDrive/rss/feeds.org")))

(use-package! elfeed-dashboard
  :config
  (setq elfeed-dashboard-file "~/SynologyDrive/rss/dashboard.org")
  ;; update feed counts on elfeed-quit
  (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links))

(map!
 "C-c r r" 'elfeed
 "C-c r d" 'elfeed-dashboard)
