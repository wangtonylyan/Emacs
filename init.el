;; -*- coding: utf-8 -*-

;; 'emacs-major-version, 'emacs-minor-version

;; prefix convention
;; my :: global
;; my/prog, my/prog-cc, ... :: file-local
;; pvt :: private
;; pkg :: package

;; file, path :: 不以斜杠结尾
;; directory :: 以斜杠结尾
;; dwim :: Do What I Mean

(defun my/listp (list)
  (and list (listp list)))

(defun my/find-dict (fkey dict)
  (when (my/listp dict)
    (let ((first (car dict)))
      (if (and (consp first) (funcall fkey (car first)))
          (cdr first) (my/find-dict fkey (cdr dict))))))

(defun my/find-dict-by-key (key dict)
  (my/find-dict (lambda (x) (eq x key)) dict))

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

(defmacro my/save-point (func)
  `(let ((line (line-number-at-pos)))
     ,func
     (goto-char (point-min))
     (when (>= line 1) (forward-line (- line 1)))
     (recenter-top-bottom)))


(defun my/get-hook (dict mode)
  (let ((hook (gethash mode dict)))
    (unless hook (user-error "*my/get-hook* MODE=%s" mode))
    hook))

(defun my/push-hook (dict mode func &optional local)
  (add-hook (my/get-hook dict mode) func nil local))

(defun my/add-hook (dict mode func &optional local)
  (add-hook (my/get-hook dict mode) func :append local))

(defun my/add-hooks (dict list)
  (dolist (elem list)
    (my/add-hook dict (car elem) (cadr elem) (caddr elem))))

(defun my/del-hook (dict mode func &optional local)
  (remove-hook (my/get-hook dict mode) func local))

(defun my/run-hook (dict mode)
  (run-hooks (my/get-hook dict mode)))

(defmacro my/create-hook-interface (name dict)
  (list 'progn
        (list 'defun (intern (concat "my/get-" name "-hook"))
              (list 'mode)
              (list 'my/get-hook dict 'mode))
        (list 'defun (intern (concat "my/push-" name "-hook"))
              (list 'mode 'func '&optional 'local)
              (list 'my/push-hook dict 'mode 'func 'local))
        (list 'defun (intern (concat "my/add-" name "-hook"))
              (list 'mode 'func '&optional 'local)
              (list 'my/add-hook dict 'mode 'func 'local))
        (list 'defun (intern (concat "my/add-" name "-hooks"))
              (list 'list)
              (list 'my/add-hooks dict 'list))
        (list 'defun (intern (concat "my/del-" name "-hook"))
              (list 'mode 'func '&optional 'local)
              (list 'my/del-hook dict 'mode 'func 'local))
        (list 'defun (intern (concat "my/run-" name "-hook"))
              (list 'mode)
              (list 'my/run-hook dict 'mode))))

(defun my/create-hook (list)
  (let ((dict (make-hash-table :test 'equal)))
    (mapc (lambda (tup) (puthash (car tup) (cadr tup) dict)) list)
    dict))

(defconst my/mode-hook-dict
  (my/create-hook
   '(("my/prog/init"     my/prog/start-hook/init    )
     ("my/prog/complete" my/prog/start-hook/complete)
     ("my/prog/cpp"      my/prog/start-hook/cpp     )
     ("my/prog/python"   my/prog/start-hook/python  )
     ;; [Emacs]
     ("init"             after-init-hook            )
     ;; [Editing]
     ("text"             text-mode-hook             )
     ("ORG"              org-load-hook              )
     ("org"              org-mode-hook              )
     ;; [Programming]
     ("prog"             prog-mode-hook             )
     ("yas"              yas-minor-mode-hook        ) ;; when enabled as local mode
     ("YAS"              yas-global-mode-hook       ) ;; when enabled as global mode
     ("flycheck"         flycheck-mode-hook         )
     ("company"          company-mode-hook          )
     ("SEMANTIC"         semantic-init-hook         ) ;; when enabled as global mode
     ("semantic"         semantic-init-mode-hook    ) ;; mode-local hook
     ;; [Language]
     ("lisp"             lisp-mode-hook             )
     ("elisp"            emacs-lisp-mode-hook       )
     ("ilisp"            lisp-interaction-mode-hook )
     ("slime"            slime-mode-hook            )
     ("scheme"           scheme-mode-hook           )
     ("CC"               c-initialization-hook      )
     ("cc"               c-mode-common-hook         )
     ("c"                c-mode-hook                )
     ("c++"              c++-mode-hook              )
     ("objc"             objc-mode-hook             )
     ("irony"            irony-mode-hook            )
     ("ycmd"             ycmd-mode-hook             )
     ("python"           python-mode-hook           )
     ("elpy"             elpy-mode-hook             )
     ("sml"              sml-mode-hook              )
     ("haskell"          haskell-mode-hook          )
     ;; [Others]
     ("DIRED"            dired-load-hook            )
     ("dired"            dired-mode-hook            )
     ("w3m"              w3m-mode-hook              ))))

(defconst my/package-hook-dict
  (my/create-hook
   '(("minibuffer/capf"   completion-at-point-functions)
     ("projectile/switch" pkg/projectile/switch-hook   ))))

(my/create-hook-interface "mode" my/mode-hook-dict)
(my/create-hook-interface "pkg" my/package-hook-dict)


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
  (setq inhibit-compacting-font-caches t))
 ((eq system-type 'gnu/linux)
  (mapc (lambda (dir)
          (let ((path (my/path-exists-p dir)))
            (when path (add-to-list 'exec-path path))))
        '("~/.local/bin"))
  (let ((path (my/locate-exec "bash"))) ;; not recommend "zsh"
    (when path
      (setq shell-file-name path)))))

(defun my/select-by-system (tuples)
  (if (nlistp tuples)
      (user-error "*my/select-system* no info for current system '%s" system-type)
    (if (eq system-type (caar tuples))
        (eval (cadar tuples))
      (my/select-system (cdr tuples)))))

(defconst my/bin/python-interpreter
  (my/select-by-system
   '((gnu/linux (or (my/locate-exec "python3")
                    (my/locate-exec "python")))
     (windows-nt (or (my/locate-exec "python.exe" "Python3" t)
                     (my/locate-exec "python.exe" "Python" t))))))

(defconst my/bin/git-command
  (my/select-by-system
   '((gnu/linux (my/locate-exec "git"))
     (windows-nt (my/locate-exec "git.exe" "Git" t)))))


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

(defun my/get-private-config (symbol validate read &optional default keep)
  (let ((config (when (and (symbolp symbol) (boundp symbol)
                           (funcall validate (symbol-value symbol)))
                  (symbol-value symbol))))
    (unless keep (unintern symbol nil))
    (if config
        (let ((result (funcall read config)))
          (if (funcall validate result) result default))
      default)))

(my/load-file (my/get-private-emacs-file "init.el"))
;; *********************************** sample code ***************************************
;; 详见于(my/get-user-emacs-file "my.config/private.el")
;; ***************************************************************************************

(defconst my/project/root-directories
  (my/get-private-config 'pvt/project/root-directories #'my/listp
                         (lambda (dirs) (my/map 'my/directory-exists-p dirs))))

(defconst my/project/ede-config-files
  (my/get-private-config 'pvt/project/ede-config-files #'my/listp
                         (lambda (files)
                           (when my/project/root-directories
                             (mapcan
                              (lambda (file)
                                (my/file-exists-p file my/project/root-directories))
                              files)))))


;; 指定由(customize)写入配置信息的文件，随后每当Emacs自动写入时就不会再修改当前文件了
(setq custom-file (my/get-user-emacs-file "custom.el"))
(my/load-file custom-file)


;; 加载其他配置文件
(mapc #'my/load-init-file '(my/init/package
                            my/init/emacs
                            my/init/utility
                            my/init/edit
                            my/init/visual
                            my/init/gui
                            my/prog/init ;; prog-mode, asn1-mode
                            my/prog/complete
                            my/prog/cpp ;; cc-mode
                            my/prog/python ;; python-mode
                            my/prog/function ;; sml-mode, haskell-mode
                            ;; my/prog/web ;; web-mode
                            my/text/init ;; org-mode, tex-mode, latex-mode
                            ;; my/text/web ;; web browser
                            my/keys/init
                            my/keys/buffer
                            my/keys/directory
                            my/keys/misc
                            my/keys/program
                            ;; my/keys/text
                            my/elpa/init))


(message "emacs init time = %s" (emacs-init-time))


(provide 'my/init)
