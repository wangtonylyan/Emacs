;; =============================================================================
;; org-mode
;; -----------------------------------------------------------------------------
;; (add-to-list 'org-babel-load-languages '(sh . t))
;; (add-to-list 'org-babel-load-languages '(ruby . t))
;; (org-babel-do-load-languages 'org-babel-load-languages '((sh . t)))

(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))
(setq org-src-fontify-natively t)
(setq org-directory "~/.emacs.d/org/")
(setq org-default-notes-file (concat org-directory "default.org"))
(setq org-todo-keywords
      '(
        (sequence "NEW(n)" "TODO(t)" "DOING(i)" "PEND(p)" "|" "CANCEL(c)" "DONE(d)")
        (type "HOME(h)" "WORK(w)")
        ))
(setq org-todo-keyword-faces
      '(
        ("NEW" . (:background "orange" :foreground "black" :weight bold))
        ("TODO" . (:background "yellow" :foreground "black" :weight bold))
        ("DOING" . (:background "red" :foreground "black" :weight bold))
        ("PEND" . (:background "pink" :foreground "black" :weight bold))
        ("CANCEL" . (:background "lime green" :foreground "black" :weight bold))
        ("DONE" . (:background "green" :foreground "black" :weight bold))
        ))
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
          ("pp" "basic" entry (file+datetree (concat org-directory "project.org")) "* PROJECT %? %T %^G\n")
          )))

(add-hook 'org-mode-hook
          (lambda ()
            (progn
              (setq truncate-lines nil)
              (org-indent-mode t))))
