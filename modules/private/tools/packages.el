;; -*- no-byte-compile: t; -*-
;;; private/tools/packages.el

(package! anki-editor
  :recipe (:host github :repo "louietan/anki-editor"))

(package! org-excalidraw
  :recipe (:host github :repo "wdavew/org-excalidraw"))

(package! elfeed
  :recipe (:host github :repo "skeeto/elfeed"))

(package! elfeed-org
  :recipe (:host github :repo "remyhonig/elfeed-org"))

(package! elfeed-dashboard
  :recipe (:host github :repo "Manoj321/elfeed-dashboard"))
