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
 "C-x C-m" 'execute-extended-command
 "C-x 2" (split-window-func-with-other-buffer 'split-window-vertically)
 "C-x 3" (split-window-func-with-other-buffer 'split-window-horizontally)
 "C-x C-k" 'kill-region
 "C-x /" 'comment-or-uncomment-region
 "S-<backspace>" 'doom/backward-kill-to-bol-and-indent
 "S-SPC" 'set-mark-command
 "C-w" 'backward-kill-word
 "C-c C-p" 'org-mark-ring-goto
 "C-c k" 'revert-buffer-with-coding-system
 "C-c p w" '+vertico/project-search
 "C-c o s" 'treemacs-select-window
 "C-c n i" 'inbox
 "C-c n r u" 'org-roam-ui-mode
 "C-:" 'avy-goto-char-2
 "C-\"" 'avy-goto-word-or-subword-1
 )
