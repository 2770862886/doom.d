;;; config/config.el -*- lexical-binding: t; -*-

(setq indent-tabs-mode nil
      show-trailing-whitespace t
      tab-width 4
      scroll-step 1
      scroll-margin 3
      scroll-conservatively 10000)

(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

(use-package! winum
  :init (winum-mode)
  :config
  (map!
   "C-`" 'winum-select-window-by-number
   "M-0" 'winum-select-window-0-or-10
   "M-1" 'winum-select-window-1
   "M-2" 'winum-select-window-2
   "M-3" 'winum-select-window-3
   "M-4" 'winum-select-window-4
   "M-5" 'winum-select-window-5
   "M-6" 'winum-select-window-6
   "M-7" 'winum-select-window-7
   "M-8" 'winum-select-window-8
   "M-9" 'winum-select-window-9))

(use-package! move-dup
  :config
  (map!
   "M-<up>" 'move-dup-move-lines-up
   "M-<down>" 'move-dup-move-lines-down
   "C-c u" 'move-dup-duplicate-up
   "C-c d" 'move-dup-duplicate-down))

(use-package! immortal-scratch
  :init
  (add-hook! 'after-init-hook #'immortal-scratch-mode)
  (setq initial-scratch-message
        (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n")))

(use-package! adoc-mode
  :init
  (add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))
  (add-hook 'adoc-mode-hook (lambda() (buffer-face-mode t))))

(use-package! goto-chg
  :config
  (map!
   "C-," 'goto-last-change
   "C-." 'goto-last-change-reverse))

(use-package! org-excalidraw
  :config
  (setq org-excalidraw-directory "~/SynologyDrive/draws"))

(use-package! anki-editor
  :after org
  :config
  (setq org-my-anki-file "~/SynologyDrive/anki/anki.org")
  (setq anki-editor-create-decks 't)
  )

;; (use-package! tmtxt-async-tasks)
;; (use-package! tmtxt-dired-async
;;   :config
;;   (map!
;;    "C-c C-r" 'tda/rsync
;;    "C-c C-z" 'tda/zip
;;    "C-c C-u" 'tda/unzip
;;    "C-c C-a" 'tda/rsync-multiple-mark-file
;;    "C-c C-e" 'tda/rsync-multiple-empty-list
;;    "C-c C-d" 'tda/rsync-multiple-remove-item
;;    "C-c C-v" 'tda/rsync-multiple
;;    "C-c C-l" 'tda/download-clipboard-link-to-current-dir
;;    "C-c C-q" 'tda/download-to-current-dir
;;    "C-c C-s" 'tmtxt/dired-async-get-file-size
;;    ))

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
 "C-x C-m" 'execute-extended-command
 "S-<backspace>" 'doom/backward-kill-to-bol-and-indent
 "S-SPC" 'set-mark-command
 "C-w" 'backward-kill-word
 "C-x C-k" 'kill-region
 "C-x 2" (split-window-func-with-other-buffer 'split-window-vertically)
 "C-x 3" (split-window-func-with-other-buffer 'split-window-horizontally)
 "C-c k" 'revert-buffer-with-coding-system
 "C-:" 'avy-goto-char-2
 "C-\"" 'avy-goto-word-or-subword-1
 "C-x /" 'comment-or-uncomment-region
 "C-c C-p" 'org-mark-ring-goto
 "C-c p w" '+vertico/project-search
 "C-c o s" 'treemacs-select-window
 "C-c n r u" 'org-roam-ui-mode
 )
