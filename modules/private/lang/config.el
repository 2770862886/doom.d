;;; private/lang/config.el -*- lexical-binding: t; -*-

(use-package! adoc-mode
  :init
  (add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))
  (add-hook 'adoc-mode-hook (lambda() (buffer-face-mode t))))

(use-package! qml-mode
  :mode "\\.qml\\'"
  :config
  ;; 设置缩进
  (setq qml-indent-offset 4)

  ;; 添加语法高亮关键字
  (font-lock-add-keywords
   'qml-mode
   '(("\\<\\(property\\|signal\\|readonly\\)\\>" . font-lock-keyword-face)
     ("\\<\\(Qt\\.[A-Za-z]+\\)\\>" . font-lock-constant-face)))

  ;; 配置注释样式
  (modify-syntax-entry ?/ ". 124b" qml-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" qml-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" qml-mode-syntax-table))



;; 可选：配置 company-qml
(use-package! company-qml
  :after (company qml-mode)
  :config
  (add-to-list 'company-backends 'company-qml))

;; 添加 LSP 支持（如果你使用 QML language server）
(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration '(qml-mode . "qml"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "qmlls")
    :major-modes '(qml-mode)
    :server-id 'qmlls)))
