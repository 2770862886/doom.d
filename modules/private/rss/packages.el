;; -*- no-byte-compile: t; -*-
;;; private/rss/packages.el

(package! elfeed
  :recipe (:host github :repo "skeeto/elfeed"))

(package! elfeed-org
  :recipe (:host github :repo "remyhonig/elfeed-org"))

(package! elfeed-dashboard
  :recipe (:host github :repo "Manoj321/elfeed-dashboard"))
