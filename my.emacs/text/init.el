;; -*- coding: utf-8 -*-

;; =ox-twbs=，使用框架，输出更为漂亮的html

;; org-mode存在bug，在refile的时候会报错
;; 解决方案如下，删除所有编译后的文件
;; $ cd ~/.emacs.d/elpa
;; $ find org*/*.elc -print0 | xargs -0 rm

(defun pkg/org/setup/time ()
  (setq org-display-custom-times nil
        org-deadline-warning-days 1)
  (setq-default org-display-custom-times org-display-custom-times)
  (use-package org-clock
    :defer t
    :init
    (org-clock-persistence-insinuate)
    :config
    (setq org-clock-idle-time 10
          org-clock-persist nil
          org-clock-persist-file (pkg/org/get-file "clock.el")
          org-clock-persist-query-save nil
          org-clock-persist-query-resume t))
  (bind-keys :map org-mode-map
             ("C-c ." . nil) ("C-c !" . nil) ("C-c C-s" . nil) ("C-c C-d" . nil)
             ("C-c C-x C-d" . nil) ("C-c C-x C-e" . nil) ("C-c C-x C-i" . nil)
             ("C-c C-x C-j" . nil) ("C-c C-x C-o" . nil) ("C-c C-x C-q" . nil)
             ("C-c C-x C-r" . nil) ("C-c C-x C-t" . nil) ("C-c C-x C-x" . nil)
             ("C-c C-x C-z" . nil))
  (defhydra pkg/hydra/group/org/timestamp
    (:timeout pkg/hydra/timeout-sec :exit t)
    ("t" org-time-stamp                   "active   " :column "timestamp insert")
    ("T" org-time-stamp-inactive          "inactive "                           )
    ("s" org-schedule                     "scheduled"                           )
    ("d" org-deadline                     "deadline "                           )
    ("o" org-clock-in                     "start    " :column "clocking        ")
    ("p" org-clock-out                    "stop     "                           )
    ("P" org-clock-cancel                 "cancel   "                           )
    ("e" org-clock-modify-effort-estimate "estimate "                           )
    ("l" org-clock-display                "summary  " :column "clock management")
    ("L" org-clock-report                 "report   "                           )
    ("u" org-resolve-clocks               "resolve  "                           )))

(defun pkg/org/setup/todo ()
  (setq org-use-fast-todo-selection t
        org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "PENDING(p)" "|"
                    "CANCELLED(c)" "DONE(d)")))
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "red" :weight bold)
          ("DOING" :foreground "dodger blue" :weight bold)
          ("PENDING" :foreground "blue violet" :weight bold)
          ("CANCELLED" :foreground "forest green" :weight bold)
          ("DONE" :foreground "dark green" :weight bold)))
  (setq org-todo-state-tags-triggers
        '(("TODO" ("PENDING") ("CANCELLED"))
          ("DOING" ("PENDING") ("CANCELLED"))
          ("PENDING" ("PENDING" . t) ("CANCELLED"))
          ("CANCELLED" ("PENDING") ("CANCELLED" . t))
          ("DONE" ("PENDING") ("CANCELLED")))))

(defun pkg/org/setup/capture ()
  (setq org-default-notes-file (pkg/org/get-file "capture.org"))
  (use-package org-capture
    :defer t
    :config
    (setq org-capture-templates
          `(("t" "task" entry (file+headline "" "Tasks")
             "* TODO %?\t%^G\nCAPTURED: %U\n%l\n%i\n" :clock-resume t)
            ("n" "note" entry (file+headline "" "Notes")
             "* %?\t%^G\nCAPTURED: %U\n%l\n%i\n" :clock-resume t)
            ("c" "calendar" entry (file+datetree ,(pkg/org/get-file "calendar.org"))
             "* %?\nDURING: %T--%T\nCAPTURED: %U\n%i\n" :clock-resume t)
            ("j" "journey" entry (file+datetree ,(pkg/org/get-file "journey.org"))
             "* %?\nCAPTURED: %U\n%i\n" :clock-resume t))
          org-capture-bookmark t
          org-capture-use-agenda-date nil))
  (setq org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets `((nil :maxlevel . 4)
                             (,(pkg/org/get-file "gtd.org") :maxlevel . 4)
                             (,(pkg/org/get-file "future.org") :maxlevel . 3)
                             (,(pkg/org/get-file "note.org") :maxlevel . 3))))

(defun pkg/org/setup/agenda ()
  (setq org-agenda-files (list org-directory)
        org-agenda-text-search-extra-files nil
        org-agenda-skip-unavailable-files nil
        org-agenda-diary-file (pkg/org/get-file "journey.org")))

(use-package org
  :defer t
  :commands (org-store-link
             org-capture
             org-agenda)
  :preface
  (defun pkg/org/start ())
  (defun pkg/org/get-file (file)
    (let ((path (my/concat-directory-file org-directory file)))
      (when path (pkg/files/ignore-readonly-path path))
      path))
  :if (pkg/package/enabled-p 'org)
  :init
  (setq org-use-extra-keys nil
        org-replace-disputed-keys nil)
  (my/add-mode-hook "org" #'pkg/org/start)
  :config
  (setq org-startup-indented t
        org-startup-folded t
        org-startup-truncated nil
        org-startup-with-beamer-mode nil
        org-startup-align-all-tables nil
        org-startup-shrink-all-tables nil
        org-startup-with-latex-preview t
        org-startup-with-inline-images nil
        org-hide-block-startup nil)
  (setq org-adapt-indentation t
        org-support-shift-select nil
        org-M-RET-may-split-line '((default . nil))
        org-insert-heading-respect-content nil
        org-tab-follows-link nil
        org-return-follows-link nil
        org-use-speed-commands t
        org-enable-priority-commands nil)
  (setq org-cycle-global-at-bob t
        org-cycle-emulate-tab t
        org-cycle-separator-lines 1
        org-cycle-level-after-item/entry-creation t)
  (use-package ob-core
    :defer t
    :config
    (setq org-confirm-babel-evaluate nil
          org-babel-no-eval-on-ctrl-c-ctrl-c nil
          org-babel-uppercase-example-markers t))
  (use-package org-src
    :defer t
    :config
    ;; TODO: org-src-lang-modes
    (setq org-src-window-setup 'split-window-below
          org-src-ask-before-returning-to-edit-buffer nil
          org-edit-src-persistent-message t
          org-src-preserve-indentation t
          org-src-tab-acts-natively t
          org-edit-src-turn-on-auto-save nil
          org-edit-src-auto-save-idle-delay 0))
  ;; =====================================================================================
  (setq org-directory (my/set-user-emacs-file "my.org/")
        org-src-fontify-natively t
        org-use-sub-superscripts t
        org-indirect-buffer-display 'dedicated-frame
        org-ellipsis "↷")
  (dolist (lang '((latex . t) (matlab . t)
                  (lisp . t) (sh . t)
                  (C . nil) (C++ . nil) (java . nil)
                  (python . t) (js . nil) (haskell . nil)))
    (add-to-list 'org-babel-load-languages lang))
  ;; =====================================================================================
  (use-package org-indent
    :diminish (org-indent-mode)
    :defer t)
  (bind-keys :map org-mode-map
             ("M-<tab>" . nil)
             ("C-c [" . nil) ;; (org-agenda-file-to-front)
             ("C-c ]" . nil) ;; (org-remove-file)
             ("C-c ;" . nil) ;; (org-toggle-comment)
             ("C-'" . nil) ("C-," . nil) ;; (org-cycle-agenda-files)
             )
  ;; =====================================================================================
  (pkg/org/setup/time)
  (pkg/org/setup/todo)
  (pkg/org/setup/capture)
  (pkg/org/setup/agenda))

(use-package org-bullets
  :after (org)
  :defer t
  :preface
  (defun pkg/org-bullets/start ()
    (org-bullets-mode 1))
  :if (pkg/package/enabled-p 'org-bullets)
  :init
  (my/add-mode-hook "org" #'pkg/org-bullets/start))

(use-package org-pomodoro
  :after (org)
  :defer t
  :if (pkg/package/enabled-p 'org-pomodoro)
  :config
  (setq org-pomodoro-format "%s"
        org-pomodoro-time-format "%.2m:%.2s"
        org-pomodoro-ask-upon-killing t
        org-pomodoro-play-sounds nil
        org-pomodoro-clock-break nil))

(use-package idle-org-agenda
  ;; :after (org-agenda)
  :defer t
  :commands (idle-org-agenda-mode)
  :preface
  (defun pkg/idle-org-agenda/start ()
    (idle-org-agenda-mode 1))
  :if (pkg/package/enabled-p 'idle-org-agenda)
  :init
  (my/add-mode-hook "init" #'pkg/idle-org-agenda/start)
  :config
  (setq idle-org-agenda-interval (* 60 10)))

;; =======================================================================================
(defun jiw ()
  (org-clock-into-drawer t
                         org-clock-out-when-done t
                         org-clock-rounding-minutes 0
                         org-clock-out-remove-zero-time-clocks nil
                         org-clock-in-switch-to-state nil
                         org-clock-out-switch-to-state nil
                         org-clock-history-length 5
                         org-clock-goto-may-find-recent-task t
                         org-clock-heading-function nil
                         org-clock-string-limit 0
                         org-clock-in-resume nil
                         org-clock-sound nil
                         org-clock-mode-line-total 'auto
                         org-clock-task-overrun-text nil
                         org-show-notification-handler nil
                         org-clocktable-defaults
                         org-clock-clocktable-formatter 'org-clocktable-write-default
                         org-clock-clocktable-language-setup
                         org-clock-clocktable-default-properties '(:maxlevel 2 :scope file)

                         org-clock-auto-clock-resolution 'when-no-clock-is-running
                         org-clock-report-include-clocking-task nil
                         org-clock-resolve-expert nil
                         org-clock-continuously nil
                         org-clock-total-time-cell-format "*%s*"
                         org-clock-file-time-cell-format "*%s*"
                         org-clock-clocked-in-display 'mode-line
                         org-clock-frame-title-format '(t org-mode-line-string)
                         org-clock-x11idle-program-name "x11idle"
                         org-clock-goto-before-context 2
                         org-clock-display-default-range 'thisyear))

(defun wo ()
  (setq org-clone-delete-id nil
        org-export-backends '(ascii html icalendar latex odt)

        org-loop-over-headlines-in-active-region nil
        org-ellipsis nil
        org-closed-keep-when-no-todo nil
        org-show-context-detail '((agenda . local))

        org-bookmark-names-plist



        org-odd-levels-only nil
        org-special-ctrl-a/e nil
        org-special-ctrl-k nil
        org-ctrl-k-protect-subtree nil
        org-special-ctrl-o t
        org-catch-invisible-edits nil
        org-yank-folded-subtrees t
        org-yank-adjusted-subtrees nil

        org-enable-fixed-width-editor t
        org-highlight-sparse-tree-matches t
        org-remove-highlights-with-change t
        org-occur-case-fold-search t
        org-self-insert-cluster-for-undo nil
        org-table-tab-recognizes-table.el t
        org-link-parameters
        org-link-abbrev-alist nil
        org-descriptive-links t
        org-link-file-path-type 'adaptive
        org-highlight-links '(bracket angle plain radio tag date footnote)
        org-make-link-description-function nil
        org-url-hexify-p t
        org-email-link-description-format "Email %c: %.30s"
        org-from-is-user-regexp
        org-context-in-file-links t
        org-keep-stored-link-after-insertion nil
        org-link-translation-function nil

        org-mark-ring-length 4
        org-link-search-must-match-exact-headline 'query-to-create
        org-link-frame-setup
        org-display-internal-link-with-indirect-buffer nil
        org-open-non-existing-files nil
        org-open-directory-means-index-dot-org nil
        org-confirm-shell-link-function 'yes-or-no-p
        org-confirm-shell-link-not-regexp ""
        org-confirm-elisp-link-function 'yes-or-no-p
        org-confirm-elisp-link-not-regexp ""
        org-doi-server-url "http://dx.doi.org/"

        org-reverse-note-order nil
        org-log-refile nil

        org-provide-todo-statistics t
        org-hierarchical-todo-statistics t
        org-enforce-todo-dependencies nil
        org-enforce-todo-checkbox-dependencies nil
        org-treat-insert-todo-heading-as-state-change nil
        org-treat-S-cursor-todo-selection-as-state-change t
        org-todo-state-tags-triggers nil
        org-log-done nil
        org-log-reschedule nil
        org-log-redeadline nil
        org-log-note-clock-out nil
        org-log-done-with-time t
        org-log-note-headings
        org-log-into-drawer nil
        org-log-state-notes-insert-after-drawers nil
        org-log-states-order-reversed t
        org-todo-repeat-to-state nil
        org-log-repeat 'time

        org-time-stamp-rounding-minutes '(0 5)

        org-deadline-warning-days 14
        org-scheduled-delay-days 0
        org-read-date-prefer-future t
        org-agenda-jump-prefer-future 'org-read-date-prefer-future
        org-read-date-force-compatible-dates t
        org-read-date-display-live t
        org-read-date-popup-calendar t
        org-extend-today-until 0
        org-use-effective-time nil
        org-use-last-clock-out-time-as-effective-time nil
        org-edit-timestamp-down-means-later nil
        org-calendar-follow-timestamp-change t
        org-tag-alist nil
        org-tag-persistent-alist nil
        org-complete-tags-always-offer-all-agenda-tags nil
        org-use-fast-tag-selection 'auto
        org-fast-tag-selection-single-key nil
        org-tags-column -77
        org-auto-align-tags t
        org-use-tag-inheritance t
        org-tags-exclude-from-inheritance nil
        org-tags-match-list-sublevels t
        org-tags-sort-function nil
        org-property-format "%-10s %s"
        org-properties-postprocess-alist nil
        org-use-property-inheritance nil
        org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS"
        org-columns-ellipses ".."
        org-global-properties nil

        org-format-latex-options
        org-format-latex-signal-error t
        org-latex-to-mathml-jar-file nil
        org-latex-to-mathml-convert-command nil
        org-preview-latex-default-process 'dvipng
        org-preview-latex-process-alist
        org-preview-latex-image-directory "ltximg/"
        org-format-latex-header "\\documentclass{article}"
        org-latex-default-packages-alist
        org-latex-packages-alist nil
        org-level-color-stars-only nil
        org-hide-leading-stars nil
        org-hidden-keywords nil
        org-custom-properties nil
        org-fontify-done-headline nil
        org-fontify-emphasized-text t
        org-fontify-whole-heading-line nil
        org-highlight-latex-and-related nil
        org-hide-emphasis-markers nil
        org-hide-macro-markers nil

        org-pretty-entities nil
        org-pretty-entities-include-sub-superscripts t

        org-emphasis-alist
        org-archive-location "%s_archive::"
        org-agenda-skip-archived-trees t
        org-columns-skip-archived-trees t
        org-cycle-open-archived-trees nil
        org-sparse-tree-open-archived-trees nil
        org-sparse-tree-default-date-type nil
        org-group-tags t
        org-allow-promoting-top-level-subtree nil

        org-structure-template-alist
        org-track-ordered-property-with-tag nil
        org-image-actual-width t
        org-agenda-inhibit-startup nil
        org-agenda-ignore-properties nil)
  (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+"))))


(use-package deft
  :if (pkg/package/enabled-p 'deft)
  :config
  (setq deft-directory (my/get-user-emacs-file)
        deft-recursive t
        deft-extensions '("txt" "md" "org")
        deft-use-filename-as-title t))

(use-package pdf-tools
  :defer t
  :if (pkg/package/enabled-p 'pdf-tools)
  :init
  (pdf-loader-install) ;; (pdf-tools-install)
  :config
  (setq pdf-cache-image-limit 32
        pdf-view-continuous t
        pdf-view-use-imagemagick t
        pdf-view-midnight-colors (cons (face-attribute 'default :foreground)
                                       (face-attribute 'default :background)))
  (setq pdf-tools-enabled-modes '(pdf-outline-minor-mode
                                  pdf-history-minor-mode
                                  pdf-links-minor-mode
                                  pdf-misc-minor-mode
                                  pdf-misc-size-indication-minor-mode
                                  pdf-annot-minor-mode
                                  pdf-sync-minor-mode
                                  pdf-cache-prefetch-minor-mode
                                  pdf-occur-global-minor-mode))
  (when (my/theme/style-p 'dark)
    (add-to-list 'pdf-tools-enabled-modes 'pdf-view-dark-minor-mode)
    (add-to-list 'pdf-tools-enabled-modes 'pdf-view-midnight-minor-mode)))

;; $ sudo apt install djvulibre-bin djview
(use-package djvu
  :defer t
  :commands (djvu-find-file)
  :if (pkg/package/enabled-p 'djvu)
  :config
  (setq djvu-djview-command (my/locate-exec "djview")
        djvu-djview-options nil
        djvu-buffer-name-extensions '("" "-text*" "-outline*" "-bookmark*"
                                      "-annotation*" "-shared*")))


(provide 'my/text/init)
