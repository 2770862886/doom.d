;; -*- no-byte-compile: t; -*-
;;; private/lang/packages.el

(package! adoc-mode
  :recipe (:host github :repo "sensorflo/adoc-mode"))

(package! qml-mode :recipe (:host github :repo "coldnew/qml-mode"))
(package! company-qml)

(package! protobuf-mode)
