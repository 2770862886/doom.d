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
        mac-option-modifier       'none
        ns-option-modifier        'none
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
(setq doom-font (font-spec :family "M+ 1mn" :size 14)
      doom-variable-pitch-font (font-spec :family "M+ 1mn" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/CloudStation/Org/")

(setq org-reverse-note-order t)

(after! deft
  (setq deft-recursive t)
  )

(after! org
  (setq org-capture-templates
        `(("i" "Inbox" entry (file+headline ,(concat org-directory "inbox.org") "Inbox")
           "* TODO %?\n %i")
          ("t" "Task" entry (file+headligne ,(concat org-directory "task.org") "Tasks")
           "* TODO %?\n %i")
          ("p" "Project" entry (file+headline ,(concat org-directory "task.org") "Projects")
           (file "~/notes/templates/newprojecttemplate.org"))
          ("s" "Someday" entry (file+headline ,(concat org-directory "somedaymaybe.org") "Someday")
           "* %?\n %i")
          ("y" "Maybe" entry (file+headline ,(concat org-directory "somedaymaybe.org") "Maybe")
           "* %?\n %i")
          ("n" "Note" entry (file+headline ,(concat org-directory "note.org") "Notes")
           "* %?\n %i")
          ("g" "Goal" entry (file+headline ,(concat org-directory "goals.org") "Goals")
           "* %?\n %i")
          ("d" "Review: Daily Review" entry (file+olp+datetree "/tmp/reviews.org")
           (file "~/notes/templates/dailyreviewtemplate.org"))
          ("w" "Review: Weekly Review" entry (file+olp+datetree "/tmp/reviews.org")
           (file "~/notes/templates/weeklyreviewtemplate.org"))
          ("m" "Review: Monthly Review" entry (file+olp+datetree "/tmp/reviews.org")
           (file "~/notes/templates/monthlyreviewtemplate.org"))
          ("j" "Journal" entry (file+olp+datetree ,(concat org-directory "journal.org"))
           "* %U - %^{heading}\n %?")))

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
                (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)"))))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("STARTED" :foreground "DeepSkyBlue3" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
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

  ;; #### GTD & Notes configuration
  ;; by liangchao, 2019.7.9
  (setq org-agenda-files
        (list (concat org-directory "inbox.org")
              (concat org-directory "task.org")
              (concat org-directory "agenda.org")
              (concat org-directory "goals.org")))

  (setq org-agenda-text-search-extra-files
        (list (concat org-directory "somedaymaybe.org")
              (concat org-directory "note.org"))))

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
;; ####

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

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
