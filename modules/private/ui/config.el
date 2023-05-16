;;; private/ui/config.el -*- lexical-binding: t; -*-

(use-package! immortal-scratch
  :init
  (add-hook! 'after-init-hook #'immortal-scratch-mode)
  (setq initial-scratch-message
        (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n")))

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

(use-package! pulsar
  :init
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-magenta)

  (pulsar-global-mode 1)

  (add-hook! '(imenu-after-jump-hook
               better-jumper-post-jump-hook)
             #'pulsar-pulse-line)

  (add-hook 'doom-switch-window-hook #'pulsar-pulse-line)
  (add-hook 'org-follow-link-hook #'pulsar-pulse-line)

  (advice-add #'save-place-find-file-hook :after #'pulsar-pulse-line)

  (let ((map global-map))
    (define-key map (kbd "C-h y") #'pulsar-pulse-line)
    (define-key map (kbd "C-h h") #'pulsar-highlight-line)))

(map!
 "C-x w o" 'ace-swap-window)
