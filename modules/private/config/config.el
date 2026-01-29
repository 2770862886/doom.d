;;; config/config.el -*- lexical-binding: t; -*-

(setq indent-tabs-mode nil
      show-trailing-whitespace t
      tab-width 2
      c-basic-offset 2
      scroll-step 1
      scroll-margin 3
      scroll-conservatively 10000)

(setq-default tab-width 2)

(setq consult-locate-command "mdfind -name")

(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(use-package! move-dup
  :config
  (map!
   "M-<up>" 'move-dup-move-lines-up
   "M-<down>" 'move-dup-move-lines-down
   "C-c u" 'move-dup-duplicate-up
   "C-c d" 'move-dup-duplicate-down))

(use-package! goto-chg
  :config
  (map!
   "C-," 'goto-last-change
   "C-." 'goto-last-change-reverse))

(eval-after-load "dired"
  '(progn
     (defadvice dired-advertised-find-file (around dired-subst-directory activate)
       "Replace current buffer if file is a directory."
       (interactive)
       (let* ((orig (current-buffer))
              ;; (filename (dired-get-filename))
              (filename (dired-get-filename t t))
              (bye-p (file-directory-p filename)))
                ad-do-it
                (when (and bye-p (not (string-match "[/\\\\]\\.$" filename)))
               (kill-buffer orig))))))

;; we want dired not not make always a new buffer if visiting a directory
;; but using only one dired buffer for all directories.
(defadvice dired-advertised-find-file (around dired-subst-directory activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let ((orig (current-buffer))
        (filename (dired-get-filename)))
    ad-do-it
    (when (and (file-directory-p filename)
               (not (eq (current-buffer) orig)))
      (kill-buffer orig))))

(eval-after-load "dired"
  ;; don't remove `other-window', the caller expects it to be there
  '(defun dired-up-directory (&optional other-window)
     "Run Dired on parent directory of current directory."
     (interactive "P")
     (let* ((dir (dired-current-directory))
            (orig (current-buffer))
            (up (file-name-directory (directory-file-name dir))))
            (or (dired-goto-file (directory-file-name dir))
                ;; Only try dired-goto-subdir if buffer has more than one dir.
                (and (cdr dired-subdir-alist)
                     (dired-goto-subdir up))
                (progn
                  (kill-buffer orig)
                  (dired up)
                  (dired-goto-file dir))))))

(map!
 "C-c C-m" 'execute-extended-command
 "C-x 2" (split-window-func-with-other-buffer 'split-window-vertically)
 "C-x 3" (split-window-func-with-other-buffer 'split-window-horizontally)
 "C-c C-k" 'kill-region
 "C-x /" 'comment-or-uncomment-region
 "S-<backspace>" 'doom/backward-kill-to-bol-and-indent
 "S-SPC" 'set-mark-command
 "C-w" 'backward-kill-word
 "C-c C-p" 'org-mark-ring-goto
 "C-c p w" '+vertico/project-search
 "C-c o a" 'treemacs-select-window
 "C-c n i" 'inbox
 "C-c n w" 'work
 "C-c n r u" 'org-roam-ui-mode
 "C-:" 'avy-goto-char-2
 "C-\"" 'avy-goto-word-or-subword-1
 "C-c c b" 'lsp-treemacs-symbols
 "C-c c f" 'lsp-format-region
 "M-n" 'scroll-up-line
 "M-p" 'scroll-down-line)

(defun my/remote-docker-clangd ()
  "Return the command to start clangd in the remote docker container."
  '("ssh" "vm" "docker" "exec" "-i" "zbx_bde-liangchao-pnc-algo-dev" "clangd"))

(add-hook 'lsp-mode-hook
          (lambda ()
            (when (file-remote-p default-directory)
              (setq-local lsp-buffer-uri-fn
                          (lambda (buffer)
                            (with-current-buffer buffer
                              (tramp-file-name-localname (tramp-dissect-file-name default-directory))))))))

(use-package lsp-mode
  :commands lsp
  :hook (prog-mode . lsp)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection 'my/remote-docker-clangd)
                    :major-modes '(c-mode c++-mode)
                    :priority -1
                    :remote? t
                    :server-id 'clangd-remote-test))
  (add-to-list
   'lsp-language-id-configuration
   '(emacs-lisp-mode . "emacs-lisp"))
)

(use-package vterm
  :config
   (setq vterm-shell "zsh"))

(use-package! whitespace
  :init
  (setq whitespace-style '(face spaces tabs space-mark tab-mark trailing))
  (setq whitespace-display-mappings
        '((space-mark   ?\     [?\u00B7]     [?.])  ; 空格显示成中心点
          (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t])))
  :config
  (global-whitespace-mode +1))

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(setq projectile-indexing-method 'alien)
(setq projectile-verbose t)
(setq tramp-verbose 10)
(setq tramp-default-method "scp")
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq tramp-persistency-file-name nil)
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)
