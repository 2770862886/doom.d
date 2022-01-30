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

(defun edit-current-file-as-root ()
  "Edit the file that is associated with the current buffer as root"
  (interactive)
  (let ((filep (buffer-file-name)))
    (if filep (find-file (concat "/sudo::" filep))
      (message "Current buffer does not have an associated file."))))

(global-set-key (kbd "C-c o e") 'edit-current-file-as-root)

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

(map!
 "C-x C-m" 'execute-extended-command
 "S-<backspace>" 'doom/backward-kill-to-bol-and-indent
 "C-w" 'backward-kill-word
 "S-SPC" 'set-mark-command
 "C-x C-k" 'kill-region
 "C-x 2" (split-window-func-with-other-buffer 'split-window-vertically)
 "C-x 3" (split-window-func-with-other-buffer 'split-window-horizontally)
 "C-c p w" 'projectile-ag
 "C-c k" 'revert-buffer-with-coding-system
 "C-:" 'avy-goto-char-2
 "C-\"" 'avy-goto-word-or-subword-1
 "C-x /" 'comment-or-uncomment-region
 )
