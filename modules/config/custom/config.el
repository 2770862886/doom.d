;;; config/config.el -*- lexical-binding: t; -*-

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

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
   :gi "C-`" #'winum-select-window-by-number
   :gi "M-0" #'winum-select-window-by-0-or-10
   :gi "M-1" #'winum-select-window-by-1
   :gi "M-2" #'winum-select-window-by-2
   :gi "M-3" #'winum-select-window-by-3
   :gi "M-4" #'winum-select-window-by-4
   :gi "M-5" #'winum-select-window-by-5
   :gi "M-6" #'winum-select-window-by-6))

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

;;(use-package! dired+
;;  :config
;;  (diredp-toggle-find-file-reuse-dir 1))

(map!
 "C-x C-m" 'counsel-M-x
 "C-M-<backspace>" 'kill-back-to-indentation
 "C-w" 'backward-kill-word
 "S-SPC" 'set-mark-command
 "C-x C-k" 'kill-region
 "C-x 2" (split-window-func-with-other-buffer 'split-window-vertically)
 "C-x 3" (split-window-func-with-other-buffer 'split-window-horizontally)
 "C-c p w" 'projectile-ag
 "C-c k" 'revert-buffer-with-coding-system
 )
