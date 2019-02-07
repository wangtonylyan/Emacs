;; -*- coding: utf-8 -*-

(require 'my/prog)

(defun my/prog-py/add-start-hook (func)
  (my/add-mode-hook "my/prog-py" func))

(defun my/prog-py/run-start-hook ()
  (my/run-mode-hook "my/prog-py"))


;; =============================================================================
;; 关于Emacs对于Python支持方面的介绍可参考如下：
;; https://wiki.python.org/moin/EmacsEditor
;; http://emacswiki.org/emacs/PythonProgrammingInEmacs
;; https://realpython.com/blog/python/emacs-the-best-python-editor/

;; =============================================================================
;; python.el
;; Emacs内置了该插件，并将其作为对python-mode主模式的默认支持
;; 实现理念是简洁，尽可能地依赖并融合于Emacs中已有的功能
;; -----------------------------------------------------------------------------
(defun pkg/python/init ()
  (let ((exe "python3")) ;; python或python3
    (when (eq system-type 'windows-nt)
      (when (my/locate-exec "python.exe" "python3" t)
        (setq exe "python.exe")))
    (use-package python
      :if (my/locate-exec exe)
      ;; 此函数的执行就是在(load 'python)之后，因此:init与:config的效果理应是等价的
      ;; 今后可以使用:mode改进当前实现
      :init
      (setq python-shell-interpreter exe
            python-shell-interpreter-args "-i"
            ;; python-shell-interpreter-args "--pylab=osx --pdb --nosep --classic"
            ;; python-shell-prompt-regexp ">>> "
            ;; python-shell-prompt-output-regexp ""
            ;; python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
            ;; python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
            ;; python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
            )
      (my/prog-py/add-start-hook #'pkg/python/start)
      :config
      (bind-keys :map python-mode-map
                 ("<backspace>" . python-indent-dedent-line-backspace))
      (my/del-mode-hook "python" #'wisent-python-default-setup))))

(defun pkg/python/start ()
  )


;; =============================================================================
;; python-mode.el
;; 该插件用于完全代替python.el，其优点是能够支持单元测试、IPython等额外功能
;; 实现设计理念是大而全，尽可能地不依赖并独立于其他工具
;; https://launchpad.net/python-mode
;; https://github.com/emacsmirror/python-mode
;; -----------------------------------------------------------------------------

;; 下述插件都依赖于额外的Python库的支持，需要首先安装对应的Python库才能够正常使用
;; ELPY与Ropemacs这两者所提供的作用类似，一般互斥使用
;; 两者都提供了对于Python解释器的调用，以及一些Python库的额外支持

;; =============================================================================
;; ELPY (Emacs Lisp Python Environment)
;; https://github.com/jorgenschaefer/elpy
;; 必要的Python库：jedi, flake8, autopep8, virtualenv
;; 可选的Python库：virtualenvwrapper
;; -----------------------------------------------------------------------------
(defun pkg/elpy/init ()
  (use-package elpy
    :if (my/package-enabled-p 'elpy)
    :commands (elpy-enable elpy-mode)
    :init
    (eval-after-load 'python '(elpy-enable))
    (my/prog-py/add-start-hook #'pkg/elpy/start)
    :config ;; (elpy-config)
    (bind-keys :map elpy-mode-map
               ("C-c C-c" . elpy-shell-send-region-or-buffer))
    (setq elpy-rpc-python-command python-shell-interpreter
          elpy-rpc-backend "jedi" ;; 支持jedi和rope这两个库
          elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))
    (setq-default elpy-rpc-python-command elpy-rpc-python-command
                  elpy-rpc-backend elpy-rpc-backend)

    ;; (elpy-use-ipython) ;; (elpy-use-cpython) ;; 指定Python解释器
    ;; elpy默认支持并使用Emacs内置的flymake，但可随意地切换成flycheck
    (with-eval-after-load 'flycheck
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      (my/add-mode-hook "elpy" #'flycheck-mode-on-safe))
    (use-package py-autopep8
      :if (and (my-func-package-enabled-p 'py-autopep8)
               (executable-find "autopep8"))
      :commands (py-autopep8-enable-on-save)
      :init
      (my/add-mode-hook "elpy" #'py-autopep8-enable-on-save))
    ;; ELPY所默认依赖的插件，基于Python库virtualenvwrapper
    (use-package pyvenv
      ;; 目前使用virtualenvwrapper插件替代
      :disabled t)))

(defun pkg/elpy/start ()
  ;; (elpy-mode 1) ;; 由(elpy-enable)追加
  )

;; =============================================================================
;; Ropemacs
;; 依赖的python库：rope, pymacs, ropemacs
;; 其中pymacs库在安装后会自动生成pymacs.el插件，需要将该插件加入至'load-path中
;; 而ropemacs库则为Emacs提供了一个使用rope库的子模式ropemacs-mode
;; 该子模式会随pymacs.el插件的加载而被自动关联至'python-mode-hook中
;; 即(add-hook 'python-mode-hook 'ropemacs-mode)
;; 每当roepmacs-mode启用时，一个pymacs客户进程和一个Python解释器将被启动，并彼此相连
;; 随后Emacs将通过与该客户进程进行通信，从而获取rope库的相关支持
;; -----------------------------------------------------------------------------
(defun my-plugin-ropemacs-init ()
  (use-package ropemacs
    :if (my-func-package-enabled-p 'ropemacs)
    :demand t
    :commands (ropemacs-mode)
    :init
    (let ((path (concat package-user-dir "/ropemacs")))
      (when (file-directory-p path)
        (add-to-list 'load-path path t)))
    (add-hook 'my-prog-py-mode-start-hook 'my-plugin-ropemacs-start t)
    :config
    (when (and (require 'pymacs nil t)
               (pymacs-load "ropemacs" "rope-" t))
      ;; ropemacs模式中的自动补全快捷键与auto-complete不同，默认为M-/，可作为后者的补充
      ;; 因为后者在某些情况下必须至少输入一个字符，而此时就可以使用前者
      (setq ropemacs-confirm-saving t
            ropemacs-enable-autoimport t
            ropemacs-autoimport-modules '("os" "sys" "inspect")))))

(defun my-plugin-ropemacs-start ()
  ;; (ropemacs-mode 1) ;;无需手动启用
  (when (boundp 'ac-sources)
    (set (make-local-variable 'ac-sources)
         (add-to-list 'ac-sources 'ac-source-ropemacs t))))

;; =============================================================================
;; 插件virtualenvwrapper是Python同名库的Elisp实现，可完全替代后者
;; 插件auto-virtualenvwrapper则是对于插件virtualenvwrapper的封装
(defun my-plugin-auto-virtualenvwrapper-init ()
  (use-package auto-virtualenvwrapper
    :if (my-func-package-enabled-p 'auto-virtualenvwrapper)
    :ensure virtualenvwrapper
    :commands (auto-virtualenvwrapper-activate
               venv-projectile-auto-workon venv-workon venv-lsvirtualenv)
    :init
    ;; note that setting `venv-location` is not necessary if you
    ;; use the default location (`~/.virtualenvs`), or if the
    ;; the environment variable `WORKON_HOME` points to the right place
    ;; (setq venv-location '("/path/")) ;; (venv-set-location)
    (setq venv-dirlookup-names '(".venv" "venv"))
    (with-eval-after-load 'projectile
      ;; 自动启用此插件的前提是，使用projectile切换项目后必须首先打开.py文件
      ;; 否则就需要手动执行(venv-workon)等命令
      (add-hook 'projectile-find-file-hook
                ;; (venv-projectile-auto-workon)
                'auto-virtualenvwrapper-activate t))
    (add-hook 'my-prog-py-mode-start-hook 'my-plugin-auto-virtualenvwrapper-start t)
    :config
    (venv-initialize-interactive-shells)
    (venv-initialize-eshell)))

(defun my-plugin-auto-virtualenvwrapper-start ()
  )

;; =============================================================================
;; =============================================================================
(defun my/prog-py/init ()
  (pkg/python/init)
  (pkg/elpy/init)
  ;; (my-plugin-ropemacs-init)
  ;; (my-plugin-auto-virtualenvwrapper-init)
  (my/add-mode-hook "python" #'my/prog-py/start))

(defun my/prog-py/start ()
  (prettify-symbols-mode 1)
  (my/prog-py/run-start-hook))

(eval-after-load 'python '(my/prog-py/init))

(provide 'my/prog-py)
