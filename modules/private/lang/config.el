;;; private/lang/config.el -*- lexical-binding: t; -*-

(use-package! adoc-mode
  :init
  (add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))
  (add-hook 'adoc-mode-hook (lambda() (buffer-face-mode t))))

(use-package! qml-mode
  :ensure t
  :mode "\\.qml\\'"
  :config
  (add-hook 'qml-mode-hook #'lsp!)
  )

(use-package! company-qml
  :after (company qml-mode)
  :config
  (add-to-list 'company-backends 'company-qml))

(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration '(qml-mode . "qml"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "qmlls")
    :major-modes '(qml-mode)
    :server-id 'qmlls)))

(with-eval-after-load 'treesit
  (setq treesit-language-source-alist
        '((c   . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp")))))
