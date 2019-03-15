;; -*- coding: utf-8 -*-

(defun my/prog/python/add-hook (func)
  (my/add-mode-hook "my/prog/python" func))

(defun my/prog/python/run-hook ()
  (my/run-mode-hook "my/prog/python"))

(my/add-mode-hook "python" #'my/prog/python/run-hook)


;; 'my/bin/python-interpreter
;; 0. 'my/bin/python-virtualenv
;; 1. 'python-shell-interpreter
;; 2. 'pyvenv-virtualenvwrapper-python
;; 3. 'elpy-rpc-python-command

;; 'my/bin/python-virtualenv
;; 1. python-environment-virtualenv
;; 2. venv-virtualenv-command
;; 3. jedi:environment-virtualenv

;; virtualenv directory
;; 1. python-environment-directory
;; 2. venv-location
;;    venv-dirlookup-names
;; 3. jedi:environment-root

(defconst my/bin/python-virtualenv
  (when (my/locate-exec "virtualenv")
    (list (my/locate-exec "virtualenv")
          "--python" my/bin/python-interpreter
          "--no-site-packages")))

(defconst my/project/python-global-virtualenvs
  (my/get-private-config 'pvt/project/python-global-virtualenvs #'my/listp
                         (lambda (dirs) (my/map 'my/directory-exists-p dirs))
                         '("~/.virtualenvs/")))

(defconst my/project/python-local-virtualenvs
  '(".venv" ".pyenv" ".pyvenv"))


(use-package python
  :defer t
  :preface
  (defun pkg/python/start ())
  :if (pkg/package/enabled-p 'python)
  :init
  (my/prog/python/add-hook #'pkg/python/start)
  :config
  (setq python-shell-interpreter my/bin/python-interpreter
        python-shell-interpreter-args "-i"
        python-shell-interpreter-interactive-arg "-i"
        python-shell-buffer-name "*Python shell*"
        ;; python-shell-process-environment
        ;; python-shell-virtualenv-root
        python-indent-offset 4
        python-indent-guess-indent-offset t
        python-indent-guess-indent-offset-verbose t)
  (my/del-mode-hook "python" #'wisent-python-default-setup)
  (bind-keys :map python-mode-map
             ("<backspace>" . python-indent-dedent-line-backspace)))

(use-package python-mode
  :defer t
  :if (pkg/package/enabled-p 'python-mode))


(use-package python-environment ;; required by 'jedi-core
  :after (:or python python-mode)
  :defer t
  :if my/bin/python-virtualenv
  :config
  (setq python-environment-virtualenv my/bin/python-virtualenv
        python-environment-directory (car my/project/python-global-virtualenvs)
        python-environment-default-root-name "emacs"))

(use-package pyvenv ;; required by 'elpy
  :after (:or python python-mode)
  :defer t
  :commands (pyvenv-workon pyvenv-activate)
  :preface
  (defun pkg/pyvenv/start ()
    ;; (setq pyvenv-workon nil)
    ;; (setq pyvenv-activate nil)
    )
  :if my/bin/python-virtualenv ;; maybe implicitly required
  :init
  (when (pkg/package/enabled-p 'pyvenv) ;; if explicitly enabled
    (my/prog/python/add-hook #'pkg/pyvenv/start))
  :config
  (setq pyvenv-virtualenvwrapper-python my/bin/python-interpreter
        pyvenv-exec-shell shell-file-name
        pyvenv-tracking-ask-before-change t))

(use-package auto-virtualenv
  :after (:or python python-mode)
  :defer t
  :preface
  (defun pkg/auto-virtualenv/start ()
    (auto-virtualenv-set-virtualenv))
  :if (and my/bin/python-virtualenv
           (pkg/package/enabled-p '(python
                                    pyvenv ;; should be explicitly enabled
                                    auto-virtualenv)))
  :init
  (my/prog/python/add-hook #'pkg/auto-virtualenv/start))

(use-package virtualenvwrapper
  :after (:or python python-mode)
  :defer t
  :commands (venv-workon)
  :if my/bin/python-virtualenv
  :init
  (when (pkg/package/enabled-p 'virtualenvwrapper)
    (use-package projectile
      :defer t
      :config
      (my/add-pkg-hook "projectile/switch" #'venv-projectile-auto-workon)))
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-virtualenv-command (car my/bin/python-virtualenv)
        venv-location (car my/project/python-global-virtualenvs)
        venv-dirlookup-names my/project/python-local-virtualenvs
        venv-workon-cd t)
  (setq mode-line-format (add-to-list 'mode-line-format
                                      '(:exec venv-current-name)))
  (setq-default mode-line-format mode-line-format))

(use-package auto-virtualenvwrapper
  :after (:or python python-mode)
  :defer t
  :preface
  (defun pkg/auto-virtualenvwrapper/start ()
    (auto-virtualenvwrapper-activate))
  :if (and my/bin/python-virtualenv
           (pkg/package/enabled-p '(python
                                    virtualenvwrapper
                                    auto-virtualenvwrapper)))
  :init
  (my/prog/python/add-hook #'pkg/auto-virtualenvwrapper/start))

(use-package pyenv-mode
  :after (:or python python-mode)
  :defer t
  :if my/bin/python-virtualenv)


(use-package flycheck-pyflakes
  :after ((:or python python-mode) flycheck)
  :if (and (pkg/package/enabled-p 'flycheck-pyflakes)
           (my/locate-exec "pyflakes")))

(use-package flycheck-pycheckers
  :after ((:or python python-mode) flycheck)
  :if (pkg/package/enabled-p 'flycheck-pycheckers)
  :config
  (setq flycheck-pycheckers-max-line-length 100)
  (flycheck-pycheckers-setup))


(use-package jedi-core
  :after (:or python python-mode)
  :defer t
  :preface
  (defun pkg/jedi-core/start ()
    (jedi:setup))
  :init
  ;; 目前由于使用了:after (python)，因此只能在打开.py文件后再执行以下命令
  ;; (jedi:install-server) ;; 安装后需要交互式地手动执行一次
  ;; (jedi:show-version-info), (jedi:show-setup-info)
  (when (pkg/package/enabled-p 'jedi-core)
    (my/prog/python/add-hook #'pkg/jedi-core/start))
  :config
  (setq jedi:environment-virtualenv nil ;; inherit 'python-environment
        jedi:environment-root nil ;; inherit 'python-environment
        jedi:complete-on-dot t
        jedi:tooltip-method nil))

(use-package company-jedi
  :after ((:or python python-mode) company)
  :commands (company-jedi)
  :if (pkg/package/enabled-p 'company-jedi))

(use-package jedi
  :after ((:or python python-mode) auto-complete)
  :defer t
  :preface
  (defun pkg/jedi/start ()
    (jedi:ac-setup))
  :if (pkg/package/enabled-p 'jedi))


(use-package anaconda-mode
  :diminish (anaconda-mode)
  :after (:or python python-mode)
  :defer t
  :preface
  (defun pkg/anaconda-mode/start ()
    (anaconda-mode 1)
    (anaconda-eldoc-mode 1))
  :if (pkg/package/enabled-p '(python anaconda-mode))
  :init
  (my/prog/python/add-hook #'pkg/anaconda-mode/start)
  :config
  (setq anaconda-mode-installation-directory
        (my/set-user-emacs-file ".anaconda/")))

(use-package company-anaconda
  :after ((:or python python-mode) company anaconda-mode)
  :commands (company-anaconda)
  :if (pkg/package/enabled-p 'company-anaconda))

(use-package elpy
  :after (:or python python-mode)
  :defer t
  :preface
  (defun pkg/elpy/start ()
    ;; (elpy-mode 1) ;; 由(elpy-enable)追加
    )
  :if (pkg/package/enabled-p 'elpy)
  :init
  (my/prog/python/add-hook #'pkg/elpy/start)
  :config
  ;; (elpy-config)
  (elpy-enable)
  (setq elpy-rpc-python-command my/bin/python-interpreter
        elpy-rpc-backend "jedi" ;; or "rope"
        elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))
  (setq-default elpy-rpc-python-command elpy-rpc-python-command
                elpy-rpc-backend elpy-rpc-backend)
  (when (pkg/package/enabled-p 'flycheck)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))
  ;; (elpy-use-ipython) or (elpy-use-cpython)
  (bind-keys :map elpy-mode-map
             ("C-c C-c" . elpy-shell-send-region-or-buffer)))


(provide 'my/prog/python)
