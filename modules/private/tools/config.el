;  (setq rmh-elfeed-org-files '("~/SynologyDrive/rss/feeds.org"))

;; (use-package! org-excalidraw
;;   :config
;;   (setq org-excalidraw-directory "~/SynologyDrive/draws"))

(after! org
  ;(org-link-set-parameters "excalidraw" 'org-excalidraw-open)

  (defun org-excalidraw-open (name)
    "Open Excalidraw"
    (let* ((excalidraw-appdata-root "~/SynologyDrive/draws")
           (excalidraw-empty-file (f-join excalidraw-appdata-root ".empty.excalidraw"))
           (excalidraw-path (f-join excalidraw-appdata-root (concat name ".excalidraw")))
           (excalidraw-export-path (f-join excalidraw-appdata-root "Exports" (concat name ".svg"))))
      (unless (f-exists? excalidraw-path) (f-copy excalidraw-empty-file excalidraw-path))
      (quiet! (shell-command (concat "open " (shell-quote-argument excalidraw-path))))
      (message (concat "Openning " excalidraw-path))
      ))
)


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
