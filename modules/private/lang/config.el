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

;; Register qmlls (the QML LSP server) with eglot.
;; Replaces the old lsp-mode `lsp-register-client' block.
(after! eglot
  (add-to-list 'eglot-server-programs '(qml-mode . ("qmlls"))))

(with-eval-after-load 'treesit
  (setq treesit-language-source-alist
        '((c   . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp")))))

(defun +cc/company-append-method-params-h (candidate)
  "Company 补全确认后，为 C++ 方法实现自动追加形参列表。"
  (when (and (stringp candidate)
             (bound-and-true-p eglot--managed-mode)
             (derived-mode-p 'c++-mode 'c++-ts-mode 'c-mode 'c-ts-mode)
             (not (looking-at-p "("))
             (save-excursion
               (let ((p (point)))
                 (beginning-of-line)
                 (re-search-forward "::" p t))))
    (when-let* ((lsp-item (get-text-property 0 'eglot--lsp-item candidate))
                (kind (plist-get lsp-item :kind))
                ((eql kind 2))
                (fmt (or (plist-get lsp-item :insertTextFormat) 1))
                ((eql fmt 1))
                (label (or (plist-get lsp-item :label) ""))
                ((string-match "(.*" label)))
      (insert (match-string 0 label)))))

(after! company
  (add-hook 'company-after-completion-hook #'+cc/company-append-method-params-h))

;; C/C++ 中禁用 electric-indent 的"回溯重缩进上一行"行为，
;; 仅保留 newline-and-indent（只缩进新行），格式化统一交给 clang-format。
(defun +cc-safe-newline-h ()
  (setq-local electric-indent-inhibit t)
  (local-set-key (kbd "RET") #'newline-and-indent)
  (local-set-key (kbd "C-j") #'newline-and-indent))

(add-hook 'c-mode-hook    #'+cc-safe-newline-h)
(add-hook 'c++-mode-hook  #'+cc-safe-newline-h)
(add-hook 'c-ts-mode-hook   #'+cc-safe-newline-h)
(add-hook 'c++-ts-mode-hook #'+cc-safe-newline-h)

;; Protobuf：.proto 与 .pb.txt/.pbtxt 文本格式共用同一模式
(use-package! protobuf-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.pb\\.txt\\'" . protobuf-mode))
  (add-to-list 'auto-mode-alist '("\\.pbtxt\\'" . protobuf-mode))
  (add-hook 'protobuf-mode-hook
            (lambda ()
              (c-add-style "my-protobuf"
                           '((c-basic-offset . 2)
                             (indent-tabs-mode . nil))
                           t))))
