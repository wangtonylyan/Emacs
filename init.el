;; -*- coding: utf-8 -*-

;; 'emacs-major-version, 'emacs-minor-version

;; prefix convention
;; my :: global
;; my/prog, my/prog-cc, ... :: file-local
;; pvt :: private
;; pkg :: package

;; file, path ::
;; directory :: 以斜杠结尾
;; dwim :: Do What I Mean

(defun my/map (func seq)
  (delq nil (mapcar func seq)))

(defun my/mapcar (func seq)
  (car (my/map func seq)))

(defalias 'my/get-file-directory 'file-name-directory)

(defun my/get-file-path (file)
  (when (stringp file)
    (let ((dir (my/get-file-directory file)))
      (when dir (directory-file-name dir)))))

(defun my/concat-directory-file (dir file)
  (let ((dir (when (stringp dir)
               (file-name-as-directory dir))))
    (concat dir file)))

;; 'nil = '(), satisfy both (string-or-null-p) and (listp)
(defun my/directory-exists-p (file &optional dirs)
  (cond
   ((string-or-null-p dirs) ;; include the case of '()
    (let ((file (my/concat-directory-file dirs file)))
      (when (and (file-directory-p file)
                 (file-accessible-directory-p file))
        (expand-file-name (file-name-as-directory file)))))
   ((listp dirs)
    (my/map (lambda (dir)
              (my/directory-exists-p file dir))
            dirs))))

(defun my/path-exists-p (file &optional dirs)
  (let ((dirs (my/directory-exists-p file dirs)))
    (if (listp dirs) ;; include the case of 'nil
        (my/map (lambda (dir)
                  (when dir (directory-file-name dir)))
                dirs)
      (directory-file-name dirs))))

(defun my/file-exists-p (file &optional dirs)
  (cond
   ((string-or-null-p dirs) ;; in 'default-directory by default
    (let ((file (my/concat-directory-file dirs file)))
      (when (file-regular-p file)
        (expand-file-name file))))
   ((listp dirs)
    (my/map (lambda (dir)
              (my/file-exists-p file dir))
            dirs))))

(defun my/exists-p (file &optional dirs)
  (or (my/directory-exists-p file dirs)
      (my/file-exists-p file dirs)))

(defun my/locate (type dir file add)
  (let ((file (my/concat-directory-file dir file)))
    (cond
     ;; 1. locate directory or file, in 'load-path by default
     ((equal type 'exist)
      (let ((file (or (my/exists-p file)
                      (car (my/exists-p file load-path)))))
        (when add
          (cond ((my/directory-exists-p file)
                 (add-to-list 'load-path (my/path-exists-p file)))
                ((my/file-exists-p file)
                 (add-to-list 'load-path (my/get-file-path file)))))
        file))
     ;; 2. locate readable file, in 'load-path by default
     ((equal type 'file)
      (let ((file (or (my/file-exists-p file)
                      (locate-file file load-path)))) ;; as-is the 'file
        (when (and file (file-readable-p file))
          (when add (add-to-list 'load-path (my/get-file-path file)))
          file)))
     ;; 3. locate executable file, in 'exec-path by default
     ((equal type 'exec)
      (let ((file (or (my/file-exists-p file)
                      (executable-find file))))
        (when (and file (file-executable-p file))
          (when add (add-to-list 'exec-path (my/get-file-path file)))
          file)))
     (t (user-error "*my/locate* TYPE=%s FILE=%s" type file)))))

(defun my/locate-file (file &optional dir add)
  (my/locate 'file dir file add))

(defun my/locate-exec (file &optional dir add)
  (my/locate 'exec dir file add))

;; 目前暂没有对于该函数实现上的额外需求
;; (load)本身的实现逻辑就类似于(my/locate-file)
(defun my/load-file (file &optional dir) ;; TODO
  (load (my/concat-directory-file dir file) t))

(defun my/load-init-file (feature)
  (let ((prefix "my/"))
    (if (and (symbolp feature)
             (string-match (concat "^" prefix "\\.*")
                           (symbol-name feature)))
        (let* ((name (substring (symbol-name feature) (length prefix)))
               (file (my/set-user-emacs-file name t)))
          (require feature file))
      (user-error "*my/load-init-file* illegal feature %s" feature))))

(defalias 'my/minor-mode-on-p 'bound-and-true-p)

(defconst my/mode-hook-dict
  (let ((dict (make-hash-table :test 'equal)))
    (mapc (lambda (tup) (puthash (car tup) (cadr tup) dict))
          '(("my/prog/init"       my/prog/start-hook/init      )
            ("my/prog/completion" my/prog/start-hook/completion)
            ("my/prog/cpp"        my/prog/start-hook/cpp       )
            ("my/prog/python"     my/prog/start-hook/python    )
            ;; [Emacs]
            ("init"           after-init-hook              )
            ("capf"           completion-at-point-functions)
            ;; [Editing]
            ("text"           text-mode-hook               )
            ("org"            org-mode-hook                )
            ;; [Programming]
            ("prog"           prog-mode-hook               )
            ("yas"            yas-minor-mode-hook          ) ;; when enabled as local mode
            ("YAS"            yas-global-mode-hook         ) ;; when enabled as global mode
            ("flycheck"       flycheck-mode-hook           )
            ("company"        company-mode-hook            )
            ("SEMANTIC"       semantic-init-hook           ) ;; when enabled as global mode
            ("semantic"       semantic-init-mode-hook      ) ;; mode-local hook
            ;; [Language]
            ("lisp"           lisp-mode-hook               )
            ("elisp"          emacs-lisp-mode-hook         )
            ("ilisp"          lisp-interaction-mode-hook   )
            ("slime"          slime-mode-hook              )
            ("scheme"         scheme-mode-hook             )
            ("CC"             c-initialization-hook        )
            ("cc"             c-mode-common-hook           )
            ("c"              c-mode-hook                  )
            ("c++"            c++-mode-hook                )
            ("objc"           objc-mode-hook               )
            ("irony"          irony-mode-hook              )
            ("ycmd"           ycmd-mode-hook               )
            ("python"         python-mode-hook             )
            ("elpy"           elpy-mode-hook               )
            ("sml"            sml-mode-hook                )
            ("haskell"        haskell-mode-hook            )
            ;; [Others]
            ("DIRED"          dired-load-hook              )
            ("dired"          dired-mode-hook              )
            ("w3m"            w3m-mode-hook                )))
    dict))

(defun my/get-mode-hook (mode)
  (let ((hook (gethash mode my/mode-hook-dict)))
    (unless hook (user-error "*my/get-mode-hook* MODE=%s" mode))
    hook))

(defun my/add-mode-hook (mode func &optional local)
  (add-hook (my/get-mode-hook mode) func :append local))

(defun my/push-mode-hook (mode func &optional local)
  (add-hook (my/get-mode-hook mode) func nil local))

(defun my/del-mode-hook (mode func &optional local)
  (remove-hook (my/get-mode-hook mode) func local))

(defun my/add-modes-hook (list)
  (dolist (elem list)
    (my/add-mode-hook (car elem) (cadr elem) (caddr elem))))

(defun my/run-mode-hook (mode)
  (run-hooks (my/get-mode-hook mode)))

(defun my/find-file-read-only ()
  (let ((file (my/file-exists-p buffer-file-name))
        (skiplist `(".*-autoloads.el$"
                    ,(my/set-user-emacs-file "\\..*/.*")
                    ,(file-truename (my/set-user-emacs-file "\\..*/.*")))))
    (unless (or (not file)
                (my/map (lambda (regexp)
                          (string-match-p regexp file))
                        skiplist))
      (read-only-mode 1))))

(defun my/save-point (func)
  (let ((line (line-number-at-pos)))
    (funcall func)
    (goto-char (point-min))
    (when (>= line 1) (forward-line (- line 1)))
    (recenter-top-bottom)))

(defun my/reformat-current-file ()
  (interactive)
  (defun my/reformat-wrapper (func)
    (let ((state buffer-read-only))
      (when state (read-only-mode -1))
      (delete-trailing-whitespace) ;; 删除每行末尾的空格
      (when (< (- (point-max) (point-min)) (* 1024 1024))
        (funcall func))
      ;; 每次保存buffer时都将删除现有的改动高亮，替换成以下两个hook无法生效，原因未知
      ;; write-content-functions或write-file-functions
      (highlight-changes-remove-highlight (point-min) (point-max))
      (when state (read-only-mode 1))))
  (defun my/reformat ()
    (let* ((file (my/locate-file buffer-file-name))
           (mode (when file (assoc-default file auto-mode-alist
                                           'string-match))))
      (cond
       ((and (provided-mode-derived-p mode 'c-mode)
             (derived-mode-p 'c-mode))
        (my/reformat-current-file/c))
       ((and (provided-mode-derived-p mode 'lisp-mode 'emacs-lisp-mode)
             (derived-mode-p 'lisp-mode 'emacs-lisp-mode))
        (my/reformat-current-file/lisp))
       (t nil))))
  (my/reformat-wrapper 'my/reformat))

(defun my/reformat-current-file/c ()
  (interactive)
  (my/save-point
   (lambda ()
     (let ((cfg (my/get-user-emacs-file "my.config/uncrustify.c.cfg"))
           (cmd (my/locate-exec "uncrustify" "/usr/local/bin/")))
       (when (and cfg cmd)
         (shell-command-on-region (point-min) (point-max)
                                  (concat cmd " -l C -c " cfg " --no-backup")
                                  t t "*Shell Error Output*"))))))

(defun my/reformat-current-file/lisp ()
  (interactive)
  (indent-region (point-min) (point-max)))


(cond ;; os-related
 ((eq system-type 'windows-nt)
  (mapc (lambda (dir)
          (let ((path (my/path-exists-p dir)))
            (when path (add-to-list 'exec-path path))))
        '("D:/softwares"))
  (let ((path (my/locate-exec "cmdproxy.exe"
                              "Emacs25/libexec/emacs/24.5/i686-pc-mingw32")))
    (when path
      (setq shell-file-name path
            shell-command-switch "-c")))
  (my/locate-exec "git.exe" "Git" t)
  (or (my/locate-exec "python.exe" "Python3" t)
      (my/locate-exec "python.exe" "Python" t))
  (setq inhibit-compacting-font-caches t))
 ((eq system-type 'gnu/linux)
  (mapc (lambda (dir)
          (let ((path (my/path-exists-p dir)))
            (when path (add-to-list 'exec-path path))))
        '("~/.local/bin"))
  (let ((path (my/locate-exec "bash"))) ;; not recommend "zsh"
    (when path
      (setq shell-file-name path)))))


;; (setq user-init-file "~/.emacs.d/init.el")
;; (load user-init-file)
(setq default-directory (my/directory-exists-p "~/")
      user-emacs-directory (my/directory-exists-p "~/.emacs.d/")
      command-line-default-directory default-directory)
(setq-default default-directory default-directory
              user-emacs-directory user-emacs-directory)

(defconst my/self-emacs-directory
  (my/locate 'exist user-emacs-directory "my.emacs/" t))

(defun my/set-user-emacs-file (file &optional self)
  (let ((dir (if self my/self-emacs-directory
               user-emacs-directory)))
    (when dir (my/concat-directory-file dir file))))

(defun my/get-user-emacs-file (&optional file self)
  (my/exists-p (my/set-user-emacs-file file self)))

(defconst my/private-emacs-directory
  (my/get-user-emacs-file ".private/"))

(defun my/get-private-emacs-file (file)
  (let ((dir my/private-emacs-directory))
    (when dir (my/exists-p file dir))))

(my/load-file (my/get-private-emacs-file "init.el"))
;; *************************** sample code in .private/init.el ***************************
;; (defvar pvt/project/root-directories '("~/Projects/" "~/projects/"))
;; (defvar pvt/project/ede-config-file-names '("ede-projects.el"))
;; ***************************************************************************************

(defconst pvt/project/root-directories
  (when (boundp 'pvt/project/root-directories)
    (my/map 'my/directory-exists-p pvt/project/root-directories)))

(defconst pvt/project/ede-config-files
  (when (and pvt/project/root-directories
             (boundp 'pvt/project/ede-config-file-names))
    (mapcan (lambda (file)
              (my/file-exists-p file pvt/project/root-directories))
            pvt/project/ede-config-file-names)))

;; 指定由(customize)写入配置信息的文件，随后每当Emacs自动写入时就不会再修改当前文件了
(setq custom-file (my/get-user-emacs-file "custom.el"))
(my/load-file custom-file)


;; 加载其他配置文件
(mapc #'my/load-init-file '(my/init/package
                            my/init/emacs
                            my/init/interface
                            my/init/visual
                            my/init/editing
                            my/init/utility

                            my/prog/init ;; prog-mode, asn1-mode
                            my/prog/completion
                            my/prog/cpp ;; cc-mode
                            my/prog/python ;; python-mode
                            ;; my/prog/functional ;; lisp-mode, sml-mode, haskell-mode
                            ;; my/prog/web ;; web-mode

                            my/keys/init
                            my/keys/buffer
                            my/keys/directory
                            my/keys/misc
                            my/keys/programming))
;; "text-tex" ;; tex-mode, latex-mode
;; "web-browser" ;; web browser


(message "emacs init time = %s" (emacs-init-time))


(provide 'my/init)
