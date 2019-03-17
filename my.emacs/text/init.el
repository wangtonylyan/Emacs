;; -*- coding: utf-8 -*-

(use-package org
  :defer t
  :commands (org-store-link
             org-capture
             org-agenda)
  :preface
  (defun pkg/org/start ())
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
        org-startup-align-all-tables t
        org-startup-shrink-all-tables t
        org-startup-with-latex-preview t
        org-startup-with-inline-images nil)
  (setq org-use-speed-commands t
        org-speed-commands-user nil
        org-support-shift-select nil
        org-cycle-separator-lines 1
        org-M-RET-may-split-line '((default . nil))
        org-insert-heading-respect-content nil
        org-tab-follows-link nil
        org-return-follows-link nil)
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
          org-src-preserve-indentation t
          org-src-ask-before-returning-to-edit-buffer nil
          org-edit-src-persistent-message t
          org-src-tab-acts-natively t
          org-edit-src-turn-on-auto-save nil
          org-edit-src-auto-save-idle-delay 0))
  ;; =====================================================================================

  (setq org-directory (my/set-user-emacs-file "org/")
        org-default-notes-file (my/concat-directory-file org-directory "notes")
        org-src-fontify-natively t
        org-use-sub-superscripts t
        org-adapt-indentation nil)
  (setq org-agenda-files org-directory
        org-agenda-file-regexp "\\`[^.].*\\.org\\'"
        org-agenda-text-search-extra-files nil
        org-agenda-skip-unavailable-files nil
        org-agenda-diary-file 'diary-file)
  (dolist (lang '((latex . t) (matlab . t)
                  (lisp . t) (sh . t)
                  (C . nil) (C++ . nil) (java . nil)
                  (python . t) (js . nil) (haskell . nil)))
    (add-to-list 'org-babel-load-languages lang)))

(use-package org-bullets
  :after (org)
  :defer t
  :preface
  (defun pkg/org-bullets/start ()
    (org-bullets-mode 1))
  :if (pkg/package/enabled-p 'org-bullets)
  :init
  (my/add-mode-hook "org" #'pkg/org-bullets/start))


;; =======================================================================================
(defun wo ()
  (setq org-todo-keywords
        '((sequence "NEW(n)" "TODO(t)" "DOING(i)" "PEND(p)"
                    "|" "CANCEL(c)" "DONE(d)")
          (sequence "REPORT" "BUG" "DUPLICATED" "KNOWN" "DELAY" "VERIFY"
                    "|" "NONBUG" "DELEGATED" "FIXED")
          (type "HOME(h)" "WORK(w)" "SCHOOL(s)")))
  (setq org-todo-keyword-faces

        (let ((lvl1-1 '(:background "red" :foreground "black" :weight bold))
              (lvl1-2 '(:background "orange red" :foreground "black" :weight bold))
              (lvl1-3 '(:background "orange" :foreground "black" :weight bold))
              (lvl1-4 '(:background "gold" :foreground "black" :weight bold))
              (lvl1-5 '(:background "yellow" :foreground "black" :weight bold))

              (lvl2-1 '(:background "yellow" :foreground "black" :weight bold))
              (lvl2-1 '(:background "yellow" :foreground "black" :weight bold))
              (lvl2-1 '(:background "yellow" :foreground "black" :weight bold))
              (lvl2-1 '(:background "yellow" :foreground "black" :weight bold))
              (lvl2-1 '(:background "yellow" :foreground "black" :weight bold)))
          '(("NEW" . nil)
            ("TODO" . (:background "red" :foreground "black" :weight bold))
            ("DOING" . (:background "blue" :foreground "black" :weight bold))
            ("PEND" . (:background "dodger blue" :foreground "black" :weight bold))
            ("CANCEL" . (:background "dark green" :foreground "black" :weight bold))
            ("DONE" . (:background "dark green" :foreground "black" :weight bold))
            ("REPORT" . (:background "red" :foreground "black" :weight bold))
            ("BUG" . (:background "red" :foreground "black" :weight bold))
            ("DUPLICATED" . (:background "blue" :foreground "black" :weight bold))
            ("KNOWN" . (:background "blue" :foreground "black" :weight bold))
            ("DELAY" . (:background "dodger blue" :foreground "black" :weight bold))
            ("VERIFY" . (:background "dodger blue" :foreground "black" :weight bold))
            ("NONBUG" . (:background "dark green" :foreground "black" :weight bold))
            ("DELEGATED" . (:background "dark green" :foreground "black" :weight bold))
            ("FIXED" . (:background "dark green" :foreground "black" :weight bold)))))
  (setq org-clone-delete-id nil
        org-export-backends '(ascii html icalendar latex odt)

        org-loop-over-headlines-in-active-region nil
        org-ellipsis nil
        org-closed-keep-when-no-todo nil
        org-show-context-detail '((agenda . local))
        org-indirect-buffer-display 'other-window

        org-bookmark-names-plist

        org-cycle-skip-children-state-if-no-children t
        org-cycle-max-level nil
        org-hide-block-startup nil

        (setq org-cycle-global-at-bob nil
              org-cycle-emulate-tab 'never)

        org-cycle-level-after-item/entry-creation t

        org-odd-levels-only nil
        org-special-ctrl-a/e nil
        org-special-ctrl-k nil
        org-ctrl-k-protect-subtree nil
        org-special-ctrl-o t
        org-catch-invisible-edits nil
        org-yank-folded-subtrees t
        org-yank-adjusted-subtrees nil

        org-blank-before-new-entry '((heading . auto))
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
        org-file-apps
        org-doi-server-url "http://dx.doi.org/"

        org-reverse-note-order nil
        org-log-refile nil
        org-refile-targets nil
        org-refile-target-verify-function nil
        org-refile-use-cache nil
        org-refile-use-outline-path nil
        org-outline-path-complete-in-steps t
        org-refile-allow-creating-parent-nodes nil
        org-refile-active-region-within-subtree nil
        org-todo-keywords '((sequence "TODO" "DONE"))
        org-todo-interpretation 'sequence
        org-use-fast-todo-selection t
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
        org-enable-priority-commands t
        org-highest-priority ?A
        org-lowest-priority ?C
        org-default-priority ?B
        org-priority-start-cycle-with-default t
        org-get-priority-function nil
        org-time-stamp-rounding-minutes '(0 5)
        org-display-custom-times nil
        org-time-stamp-custom-formats
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

  (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))

  (let ((my-org-file-task (concat org-directory "task.org")))
    (setq org-capture-templates
          '(
            ("t" "Templates for task")
            ("tn" "new" entry (file+datetree ,my-org-file-task) "* NEW %? %T %^G\n")
            ("tt" "todo" entry (file+datetree ,my-org-file-task) "* TODO %? %T %^G\n")
            ("ti" "doing" entry (file+datetree ,my-org-file-task) "* DOING %? %T %^G\n")
            ("tp" "pend" entry (file+datetree ,my-org-file-task) "* PEND %? %T %^G\n")
            ("tc" "cancel" entry (file+datetree ,my-org-file-task) "* CANCEL %? %T %^G\n")
            ("td" "done" entry (file+datetree ,my-org-file-task) "* DONE %? %T %^G\n")

            ("o" "Templates for note")
            ("oo" "basic" entry (file+datetree (concat org-directory "note.org")) "* NOTE %? %T %^G\n")
            ("c" "Templates for calendar")
            ("cc" "basic" entry (file+datetree (concat org-directory "task.org")) "* CALENDAR %? %T %^G\n")
            ("p" "Templates for project")
            ("pp" "basic" entry (file+datetree (concat org-directory "project.org")) "* PROJECT %? %T %^G\n")))))

(use-package pdf-tools
  :defer t
  :if (pkg/package/enabled-p 'pdf-tools)
  :init
  ;; (pdf-tools-install)
  :config
  (setq pdf-view-continuous t))


(provide 'my/text/init)
