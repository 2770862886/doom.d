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
(setq doom-theme 'doom-monokai-spectrum)

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
(setq org-directory "~/SynologyDrive/org/")
(setq anki-directory "~/SynologyDrive/anki/")
(setq template-directory "~/SynologyDrive/templates/")

(setq org-reverse-note-order t)

(after! org
  (org-babel-do-load-languages
     'org-babel-load-languages
     (append org-babel-load-languages
       '((C             . t)
         (dot           . t)
         (emacs-lisp    . t)
         (mermaid       . t)
         (gnuplot       . t)
         (latex         . t)
         (ledger        . t)
         (python        . t)
         (racket        . t)
         (ruby          . t)
         (scheme        . t)
         (sh            . t)))
     )

  (setq org-capture-templates
        `(("i" "Inbox" entry (file+headline ,(concat org-directory "inbox.org") "Inbox")
           "* TODO %?\n %i")
          ("t" "Task" entry (file+headline ,(concat org-directory "task.org") "Tasks")
           "* TODO %?\n %i")
          ("p" "Project" entry (file+headline ,(concat org-directory "task.org") "Projects")
           (file (concat template-directory "newprojecttemplate.org")))
          ("s" "Someday" entry (file+headline ,(concat org-directory "someday.org") "Someday")
           "* %?\n %i")
          ("b" "Maybe" entry (file+headline ,(concat org-directory "someday.org") "Maybe")
           "* %?\n %i")
          ;; "n" Note → 已迁移到 org-roam；新建笔记请用 C-c n n (org-roam-capture)
          ("g" "Goal" entry (file+headline ,(concat org-directory "goals.org") "Goals")
           "* %?\n %i")
          ("v" "Anki Words" entry (file+headline org-my-anki-file "Vocabulary")
               "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Vocabulary\n:END:\n** Front\n%?\n** Back\n%x\n")
          ("c" "Anki cloze" entry (file+headline org-my-anki-file "Dispatch Shelf")
               "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Mega\n:END:\n** Text\n%x\n** Extra\n")
          ("j" "Journal (→ 今日 daily note)" entry (file my/today-daily-file)
           "* %<%H:%M> %?\n- 做了啥：\n- 学到了：\n- 明天要：")
          ("l" "Log (事件记录)" entry (file+datetree ,(concat org-directory "log.org"))
           "* %<%H:%M> %? %^g\n%a")
          ("d" "Review: Daily Review" entry (file+olp+datetree ,(concat org-directory "journal/reviews.org") "Daily Reviews")
           (file ,(concat template-directory "dailyreviewtemplate.org")))
          ("w" "Review: Weekly Review" entry (file+olp+datetree ,(concat org-directory "journal/reviews.org") "Weekly Reviews")
           (file ,(concat template-directory "weeklyreviewtemplate.org")))
          ("m" "Review: Monthly Review" entry (file+olp+datetree ,(concat org-directory "journal/reviews.org") "Monthly Reviews")
           (file ,(concat template-directory "monthlyreviewtemplate.org")))
          ("y" "Review: Annual Review" entry (file+olp+datetree ,(concat org-directory "journal/reviews.org") "Annual Reviews")
           (file ,(concat template-directory "annualreviewtemplate.org")))
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
              (concat org-directory "work.org")
              (concat org-directory "task.org")
              (concat org-directory "tickler.org")
              (concat org-directory "someday.org")
              (concat org-directory "goals.org")))

  (setq org-agenda-text-search-extra-files
        (list (concat org-directory "someday.org")
              (concat org-directory "log.org")))

  (setq org-refile-targets
        `((,(concat org-directory "task.org") :maxlevel . 3)
          (,(concat org-directory "work.org") :maxlevel . 3)
          (,(concat org-directory "someday.org") :level . 1)
          (,(concat org-directory "tickler.org") :maxlevel . 2)))
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps t
        org-refile-allow-creating-parent-nodes 'confirm)

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
          ("org" :components ("org-notes" "org-static"))))

  ;; Habit tracking settings
  (setq org-habit-show-habits-only-for-today nil)
  (setq org-habit-show-all-today t)
  (setq org-habit-graph-column 60)
  (setq org-habit-preceding-days 28)
  (setq org-habit-following-days 7))

(use-package emacsql :ensure t)
(use-package emacsql-sqlite3 :ensure t)

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (concat org-directory "roam/"))
  (org-roam-db-location (concat org-roam-directory (cond (IS-LINUX "org-roam.db")
                                                         (IS-MAC "org-roam-osx.db")
                                                         (IS-WINDOWS "org-roam-win.db"))))
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
  (org-roam-dailies-capture-templates '(
                ("d" "default" entry "* %?\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:END:"
                 :target (file+head "%<%Y-%m-%d>.org"
                                    "#+TITLE: Daily Note - %<%Y-%m-%d>\n#+STARTUP: content\n#+TAGS: fleeting(f) question(q) todo(t) idea(i) link(l)\n")
                 :unnarrowed t)))
  :bind (("C-c n b" . org-roam-buffer-toggle)
         ("C-c n r b" . org-roam-update-org-id-locations)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  ;; org-roam-node-find (C-c n f) 候选项带上 tag，可直接输入 tag 模糊过滤（路径 1）
  (setq org-roam-node-display-template "${title:*} ${tags:30}")
  (org-roam-db-autosync-enable))

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(after! citar
  (setq! citar-bibliography '(~/SynologyDrive/org/roam/.library.bib))
  (setq! citar-notes-paths '(~/SynologyDrive/org/roam))

  (setq citar-templates
        '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
          (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
          (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
          (note . "Notes on ${author editor} (${year issued date}) ${title}")))

  (setq citar-symbols
        `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
          (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
          (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))

  (setq citar-symbol-separator "  ")
  (map! :leader :prefix "n"
        "b" #'citar-insert-citation))

;; automatically add timestamp for creation and modification.
(use-package! org-roam-timestamps
   :after org-roam
   :config
   (org-roam-timestamps-mode))

;; references through files
(use-package! org-transclusion
  :after org-roam)

(use-package! websocket
  :after org-roam)

 (use-package deft
    :config
    (setq deft-directory org-directory
          deft-recursive t
          deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
          deft-use-filename-as-title t))

(after! rjsx-mode
  (setq js2-basic-offset 4))

(use-package! ob-racket
  :after racket-mode-hook)

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
  (find-file anki-directory))

(defun my/today-daily-file ()
  "Return the path to today's org-roam daily note, creating a stub if absent.
Used as the capture target for the journal (\"j\") template so that daily
reflection lands in the same place as org-roam-dailies (roam/daily/YYYY-MM-DD.org)."
  (let* ((daily-dir (file-name-as-directory (concat org-roam-directory "daily/")))
         (path (expand-file-name (format-time-string "%Y-%m-%d.org") daily-dir)))
    (unless (file-exists-p path)
      (make-directory daily-dir t)
      (with-temp-file path
        (insert (concat "#+TITLE: Daily - " (format-time-string "%Y-%m-%d") "\n"
                        "#+STARTUP: content\n"
                        "#+TAGS: fleeting(f) question(q) todo(t) idea(i) link(l)\n"))))
    path))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(require 'exec-path-from-shell)
(when (display-graphic-p)
  (exec-path-from-shell-initialize))
;; ####

(setq conda-anaconda-home (expand-file-name "~/miniconda3")) ; 根据你的安装路径修改
(setq conda-env-home-directory (expand-file-name "~/miniconda3/envs"))

(use-package! conda
  :config
  (conda-env-initialize-interactive-shells) ; 初始化交互式 shell
  (conda-env-initialize-eshell)             ; 初始化 eshell
  (setq conda-env-autoactivate-mode t))     ; 自动激活项目环境

(after! conda
  (conda-env-activate "emacs"))

(after! lsp-pyright
  (setq lsp-pyright-python-executable-cmd "python"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(setq gdb-many-windows t)

;; (after! vterm
;;   :config
;;   (setopt vterm-keymap-exceptions
;;           (append vterm-keymap-exceptions
;;                  '("M-1" "M-2" "M-3" "M-4" "M-5" "M-6" "M-7" "M-8" "M-9"))))

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

;; #### Dictionary / 查词 (sdcv 离线 + osx-dictionary 兜底)
;; by liangchao, 2026.06.24
;;
;; 前置准备:
;;   • sdcv:        brew install sdcv ; 把 StarDict 词典(.ifo/.idx/.dz)放到 ~/.stardict/dic/
;;                  词典书名以 `sdcv -l` 的输出为准; 默认显示所有词典,
;;                  想精简可取消下面 sdcv-dictionary-*-list 的注释并填准确书名
;;   • osx-dictionary: 首次用 M-x osx-dictionary-install-cli 安装命令行辅助
(use-package! sdcv
  :commands (sdcv-search-input sdcv-search-pointer)
  :config
  (setq sdcv-say-word-p t                                  ;; 朗读单词
        sdcv-dictionary-data-dir (expand-file-name "~/.stardict/dic"))
  ;; 默认显示 ~/.stardict/dic 下全部词典;
  ;; 装好词典后跑 `sdcv -l` 拿到准确书名, 取消下面注释并填入即可精简显示:
  ;; (setq sdcv-dictionary-simple-list   '("朗道英汉字典5.0")
  ;;       sdcv-dictionary-complete-list '("朗道英汉字典5.0" "Collins"))
  )

(use-package! osx-dictionary
  :commands (osx-dictionary-search-input osx-dictionary-search-pointer)
  ;; 想要 buffer 内单词高亮, 取消下一行注释 (osx-dictionary-mode 1)
  )

;; 查词并把释义复制到剪贴板, 配合 Anki capture 模板 "v" 粘贴
(defun my/sdcv-pointer-then-copy ()
  "查光标处单词并把 *sdcv* 缓冲区内容复制到剪贴板(便于 Anki capture 粘贴)."
  (interactive)
  (call-interactively #'sdcv-search-pointer)
  (let ((buf (get-buffer "* sdcv *")))
    (when buf
      (with-current-buffer buf
        (clipboard-kill-ring-save (point-min) (point-max))))))

;; 按键 (C-c d 前缀, 全局生效, 无需 leader)
(which-key-add-key-based-replacements "C-c d" "dictionary/查词")
(map! :desc "查词 sdcv(输入)"      "C-c d i" #'sdcv-search-input
      :desc "查词 sdcv(光标处)"    "C-c d p" #'sdcv-search-pointer
      :desc "查词 osx(输入)"       "C-c d I" #'osx-dictionary-search-input
      :desc "查词 osx(光标处)"     "C-c d P" #'osx-dictionary-search-pointer
      :desc "查词→复制(给 Anki)"  "C-c d a" #'my/sdcv-pointer-then-copy)

;; #### Password Store → 改用 doom :tools pass 模块 (init.el: (pass +auth))
;; 仅保留模块未提供的个性化设置：
(after! password-store
  (setq password-store-password-length 20))   ; 模块默认 12，改 20
(after! auth-source-pass
  (setq auth-sources '(password-store)))      ; 仅从 pass 取密，.authinfo.gpg 退役
;; 打开密码库：全局单键 C-c z (Emacs 约定 C-c <字母> 预留给用户; 避开 projectile 的 C-c p)
(map! :desc "密码库 pass" "C-c z" #'pass)

;; Start Emacs Server
(after! server
  (unless (server-running-p)
    (server-start)))

;; NOTE: There was a spurious face-inheritance cycle in doom-themes that
;;   crashed company-box ("Face inheritance results in inheritance cycle:
;;   gnus-group-news-low"). The fix is applied directly to
;;   ~/.emacs.d/.local/straight/repos/themes/doom-themes-base.el (one line
;;   edit), where doom-themes redefined
;;     gnus-group-news-low-empty :inherit gnus-group-news-low
;;   in a way that formed a cycle with Emacs 31's built-in face spec. It now
;;   inherits `gnus-group-mail-1-empty' (matching its sibling empty faces).
;;   If doom-themes is reinstalled (doom sync --rebuild), this edit will be
;;   lost and needs to be reapplied.)
