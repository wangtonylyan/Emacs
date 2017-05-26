(require 'my-prog)

(defvar my-prog-py-mode-start-hook '())

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
(defun my-plugin-python-init ()
  (let ((exe "python3")) ;; python或python3
    (when (eq system-type 'windows-nt)
      (when (my-func-executable-find exe "python.exe" t)
        (setq exe "python.exe")))
    (when (and (executable-find exe)
               (require 'python nil t))
      (remove-hook 'python-mode-hook 'wisent-python-default-setup)
      (setq python-shell-interpreter exe
            python-shell-interpreter-args "-i"
            ;; python-shell-prompt-regexp ""
            ;; python-shell-prompt-output-regexp ""
            ;; python-shell-completion-setup-code ""
            ;; python-shell-completion-module-string-code ""
            ;; python-shell-completion-string-code ""
            )
      (add-hook 'my-prog-py-mode-start-hook 'my-plugin-python-start t))))

(defun my-plugin-python-start ()
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
;; 依赖的python库：flake8, jedi
;; -----------------------------------------------------------------------------
(defun my-plugin-elpy-init ()
  (when (and (member 'elpy package-selected-packages)
             (require 'elpy nil t))
    ;; 常用快捷键：
    ;; C-c C-c用于调用Python解释器
    ;; elpy默认支持并使用Emacs内置的flymake，但可随意地切换成flycheck
    (with-eval-after-load 'flycheck
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      (add-hook 'elpy-mode-hook 'flycheck-mode-on-safe t))
    (when (and (member 'py-autopep8 package-selected-packages)
               (executable-find "autopep8")
               (require 'py-autopep8 nil t))
      (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save t))
    (setq elpy-rpc-backend "jedi" ;; 支持rope和jedi这两个库
          )
    ;; 指定Python解释器
    ;; (elpy-use-ipython) ;; (elpy-use-cpython)
    ;; (elpy-enable)
    (add-hook 'my-prog-py-mode-start-hook 'my-plugin-elpy-start t)))

(defun my-plugin-elpy-start ()
  (elpy-mode 1))

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
  (when (member 'ropemacs package-selected-packages)
    (let ((path (concat package-user-dir "/ropemacs")))
      (when (file-directory-p path)
        (add-to-list 'load-path path t)))
    (when (and (require 'pymacs nil t)
               (pymacs-load "ropemacs" "rope-" t))
      (setq ropemacs-confirm-saving t
            ropemacs-enable-autoimport t
            ropemacs-autoimport-modules '("os" "sys" "inspect"))
      ;; ropemacs模式中的自动补全快捷键与auto-complete不同，默认为M-/，可作为后者的补充
      ;; 因为后者在某些情况下必须至少输入一个字符，而此时就可以使用前者
      (add-hook 'my-prog-py-mode-start-hook 'my-plugin-ropemacs-start t))))

(defun my-plugin-ropemacs-start ()
  ;; (ropemacs-mode 1) ;; 无需手动启用
  (when (and (my-func-minor-mode-on-p auto-complete-mode)
             (boundp 'ac-sources) (boundp 'my-prog-ac-sources))
    (setq ac-sources
          (add-to-list my-prog-ac-sources '(ac-source-ropemacs) t))))

;; =============================================================================
;; =============================================================================
(defun my-prog-py-mode-init ()
  (my-plugin-python-init)
  (my-plugin-elpy-init)
  (my-plugin-ropemacs-init)
  (add-hook 'python-mode-hook 'my-prog-py-mode-start t))

(defun my-prog-py-mode-start ()
  ;; 将lambda显示为λ
  (prettify-symbols-mode 1)
  (setq prettify-symbols-alist '(("lambda" . 955)))
  (run-hooks 'my-prog-py-mode-start-hook))

;; (add-hook 'prog-mode-hook 'my-prog-py-mode-init t)
(eval-after-load 'python '(my-prog-py-mode-init))

(provide 'my-prog-py)
