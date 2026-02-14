;  (setq rmh-elfeed-org-files '("~/SynologyDrive/rss/feeds.org"))

;; ============================================================
;; org-excalidraw
;; ============================================================

(use-package! org-excalidraw
  :after org
  :config
  (setq org-excalidraw-directory "~/SynologyDrive/draws"
        org-excalidraw-image-width 800)
  (org-excalidraw-setup)
  (map! "C-c n g c" #'org-excalidraw-create-drawing
        "C-c n g e" #'org-excalidraw-export-all))

(use-package! anki-editor
  :after org
  :config
  (setq org-my-anki-file "~/SynologyDrive/anki/anki.org")
  (setq anki-editor-create-decks 't))

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
 "C-c r u" 'elfeed-update
 "C-c r d" 'elfeed-dashboard)
