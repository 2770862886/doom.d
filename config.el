;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Albert Leung"
      user-mail-address "albert.leung@live.cn")

(setq fancy-splash-image "~/.doom.d/logo/logo2.png")
(setq-default initial-scratch-message
              (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(cond
 (IS-MAC
  (setq mac-command-modifier      'meta
        ns-command-modifier       'meta
        mac-option-modifier       'super
        ns-option-modifier        'super
        ;; Free up the right option for character composition
        mac-right-option-modifier 'none
        ns-right-option-modifier  'none))
 (IS-WINDOWS
  (setq w32-lwindow-modifier 'super
        w32-rwindow-modifier 'super)))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; M-Plus font repo [http://sourceforge.jp/projects/mplus-fonts/]
(setq doom-font (font-spec :family "Sarasa Mono SC" :size 14)
      doom-variable-pitch-font (font-spec :family "Sarasa Mono SC" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 1)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-env-version t)
  (doom-modeline-irc-stylize 'identity)
  (doom-modeline-github-timer nil)
  (doom-modeline-gnus-timer nil))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/SynologyDrive/Org/")
(setq anki-directory "~/SynologyDrive/Anki/")
(setq template-directory "~/SynologyDrive/templates/")

(setq org-reverse-note-order t)

(after! org
  (org-babel-do-load-languages
     'org-babel-load-languages
     (append org-babel-load-languages
       '((C             . t)
         (dot           . t)
         (emacs-lisp    . t)
         (gnuplot       . t)
         (latex         . t)
         (ledger        . t)
         (python        . t)
         (ruby          . t)
         (scheme        . t)
         (sh            . t)))
     )

  (setq org-capture-templates
        `(("i" "Inbox" entry (file+headline ,(concat org-directory "inbox.org") "Inbox")
           "* TODO %?\n %i")
          ("t" "Task" entry (file+headligne ,(concat org-directory "task.org") "Tasks")
           "* TODO %?\n %i")
          ("p" "Project" entry (file+headline ,(concat org-directory "task.org") "Projects")
           (file (concat template-directory "newprojecttemplate.org")))
          ("s" "Someday" entry (file+headline ,(concat org-directory "someday.org") "Someday")
           "* %?\n %i")
          ("b" "Maybe" entry (file+headline ,(concat org-directory "someday.org") "Maybe")
           "* %?\n %i")
          ("n" "Note" entry (file+headline ,(concat org-directory "note.org") "Notes")
           "* %?\n %i")
          ("g" "Goal" entry (file+headline ,(concat org-directory "goals.org") "Goals")
           "* %?\n %i")
          ("j" "Journal" entry (file+olp+datetree ,(concat org-directory "journal.org"))
           "* %U - %^{heading}\n %?")
          ("w" "Anki Words" entry (file+headline org-my-anki-file "Vocabulary")
               "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Vocabulary\n:END:\n** Front\n%?\n** Back\n%x\n")
          ("c" "Anki cloze" entry (file+headline org-my-anki-file "Dispatch Shelf")
               "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Mega\n:END:\n** Text\n%x\n** Extra\n")
          ("d" "Review: Daily Review" entry (file+olp+datetree "/tmp/reviews.org")
           (file (concat template-directory "dailyreviewtemplate.org")))
          ("w" "Review: Weekly Review" entry (file+olp+datetree "/tmp/reviews.org")
           (file (concat template-directory "weeklyreviewtemplate.org")))
          ("m" "Review: Monthly Review" entry (file+olp+datetree "/tmp/reviews.org")
           (file (concat template-directory "monthlyreviewtemplate.org")))
          ("y" "Review: Annual Review" entry (file+olp+datetree "/tmp/reviews.org")
           (file (concat template-directory "annualreviewtemplate.org")))
        ))

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
                (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)"))))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "DeepSkyBlue3" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("DELEGATED" :foreground "purple" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("PROJECT" :foreground "DeepPink3" :weight bold))))

  (setq org-tag-alist '((:startgroup . nil)
                        ("@office" . ?o)
                        ("@home" . ?h)
                        (:endgroup . nil)
                        (:startgroup . nil)
                        ("computer" . ?c)
                        ("laptop" . ?l)
                        ("phone" . ?p)
                        ("xbox" . ?x)
                        (:endgroup . nil)
                        ("baby" . ?b)
                        ("download" . ?d)
                        ("emacs" . ?e)
                        ("play" . ?y)
                        ("read" . ?r)
                        ("study" . ?s)
                        ("watch" . ?t)
                        ("work" . ?w)
                        ))

  ;; effort estimate
  (setq org-columns-default-format "%38ITEM(Details) %TAGS(Context) %7TODO(To Do) %5Effort(Time){:} %6CLOCKSUM{Total}")
  ;; (setq org-columns-default-format  "%70ITEM(Task) %TAGS(Context) %10Effort(Effort){:} %10CLOCKSUM")
  (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                      ("STYLE_ALL" . "habit"))))
  (setq org-archive-location (concat "archive/archive-"
                                     (format-time-string "%Y%m" (current-time))
                                     ".org_archive::"))

  ;; #### GTD & Notes configuration
  ;; by liangchao, 2019.7.9
  (setq org-agenda-files
        (list (concat org-directory "inbox.org")
              (concat org-directory "task.org")
              (concat org-directory "work.org")
              (concat org-directory "tickler.org")
              (concat org-directory "someday.org")
              (concat org-directory "goals.org")))

  (setq org-agenda-text-search-extra-files
        (list (concat org-directory "someday.org")
              (concat org-directory "note.org")))

  ;; (setq org-refile-targets
  ;;       '(
  ;;         ((concat org-directory "task.org") :maxlevel . 3)
  ;;         ((concat org-directory "work.org") :maxlevel . 3)
  ;;         ((concat org-directory "someday.org") :level . 1)
  ;;         ((concat org-directory "tickler.org") :maxlevel . 2)
  ;;         ))

  (setq org-archive-location (concat "archive/archive-"
                                     (format-time-string "%Y%m" (current-time)) ".org_archive::"))

  (setq org-publish-project-alist
        '(("org-notes"
           :base-directory "~/notes/tech/"
           :base-extension "org"
           :publishing-directory "~/notes/public/html"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :auto-preamble t)
          ("org-static"
           :base-directory "~/notes/tech"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/note/public/html"
           :recursive t
           :publishing-function org-publish-attachment)
          ("org" :components ("org-notes" "org-static")))))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/SynologyDrive/Org/roam")
  (org-roam-complete-everywhere t)
  (org-roam-capture-templates '(
                ("d" "default" plain "%?"
                 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                    "#+TITLE: ${title}\n")
                 :unnarrowed t)
                ("b" "book reading" plain "%?"
                 :target (file+head "book/%<%Y%m%d%H%M%S>-${slug}.org"
                                    "#+TITLE: ${title}\n")
                 :unnarrowed t)
                 ("r" "bibliograph reference" plain "%?"
                  :target (file+head "reference/${citekey}.org"
                                     "#+TITLE: ${title}\n")
                 :unnarrowed t)
                ))
  :bind (("C-c n b" . org-roam-buffer-toggle)
         ("C-c n r b" . org-roam-update-org-id-locations)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package! org-ref
  :after org
  :config
  (setq org-ref-default-bibliography '("~/SynologyDrive/Org/roam/.library.bib")
        org-ref-notes-function 'orb-edit-note
        org-ref-completion-library 'org-ref-ivy-cite
        org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex)
  (setq bibtex-completion-df-extension '(".pdf" ".djvu")
        bibtex-completion-bibliography '("~/SynologyDrive/Org/roam/.reference.bib")
        bibtex-completion-notes-path "~/SynologyDrive/Org/roam/reference"
        bibtex-completion-pdf-field "file"))

(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref)
  (setq orb-preformat-keywords
        '("citekey" "title" "url" "author-or-editor" "keywords" "file" "year")
        orb-process-file-keyword t
        orb-attached-file-extensions '("pdf")
        orb-note-actions-interface 'default))
;;自动创建笔记的创建时间和修改时间
;; (use-package! org-roam-timestamps
;;   :after org-roam
;;   :config
;;   (org-roam-timestamps-mode))
;;跨文件的引用，能够实现笔记的一处修改，处处修改。
(use-package! org-transclusion
  :after org-roam)

 (use-package deft
    :config
    (setq deft-directory org-directory
          deft-recursive t
          deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
          deft-use-filename-as-title t))

(after! rjsx-mode
  (setq js2-basic-offset 4))

;; #### Add global function for org interactive function
(defun inbox ()
  "Used to open inbox org file."
  (interactive)
  (find-file (concat org-directory "inbox.org")))

;; by liangchao, 2018.2.28
(defun todo ()
  "Used to open todos org file."
  (interactive)
  (find-file (concat org-directory "task.org")))

(defun work ()
  "Used to open work related org file, which is add to gitignore."
  (interactive)
  (find-file (concat org-directory "work.org")))

(defun note ()
  "Used to open note, which is not belong to todos and work."
  (interactive)
  (find-file (concat org-directory "note.org")))

(defun anki ()
  "Used to open default anki file."
  (interactive)
  (find-file "~/SynologyDrive/anki/anki.org"))

(require 'exec-path-from-shell)
(when (display-graphic-p)
  (exec-path-from-shell-initialize))
;; ####

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(setq gdb-many-windows t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
