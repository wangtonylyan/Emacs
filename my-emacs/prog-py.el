(require 'my-prog)
;; =============================================================================
;; 关于Emacs对于Python支持方面的介绍可参考如下：
;; https://wiki.python.org/moin/EmacsEditor
;; http://emacswiki.org/emacs/PythonProgrammingInEmacs
;; 总之，Emacs中的Python环境大致可分为以下四种
;; 其中后两种是作为前两种的功能补充或替换，而并没有提供一套完整齐全的功能：
;; 1) python.el
;; 该插件由Emacs社区维护，已集成于Emacs24以上版本中，作为默认的python-mode支持
;; 设计理念是简洁，尽可能地依赖并契合于Emacs中已有的功能
;; 2) python-mode.el
;; 该插件由Python社区维护，其优点包括了能够支持单元测试、IPython等
;; 设计理念是大而全，尽可能地不依赖并独立于其他工具
;; 3) Elpy
;; https://github.com/jorgenschaefer/elpy
;; 可使用Rope或Jedi作为底层支持
;; 4) Ropemacs
;; 使用Rope和Pymacs作为底层支持
;; -----------------------------------------------------------------------------
;; python-mode.el
;; https://launchpad.net/python-mode
;; https://github.com/emacsmirror/python-mode
;; -----------------------------------------------------------------------------
;; Ropemacs
;; https://github.com/python-rope
;; 其底层基于Rope和Pymacs，安装步骤为首先安装Rope和Pymacs，最后再安装Ropemacs
;; 虽安装于Python中，但会在加载时执行Elisp程序，从而为Emacs提供支持
;; [shell]$ python setup.py install
;; -----------------------------------------------------------------------------
;; Rope
;; https://github.com/python-rope/rope
;; Rope是一个支持代码重构的Python开发库，提供缩进、补全、跳转等功能
;; [shell]$ python setup.py install
;; 配套地还需安装Ropemode，其作为Rope的使用接口
;; https://github.com/python-rope/ropemode
;; [shell]$ python setup.py install
;; -----------------------------------------------------------------------------
;; Pymacs
;; version: 0.25
;; https://github.com/pinard/Pymacs
;; Pymacs是一款作用如同IPython client的Emacs插件
;; 其会启动并链接至一个Python解释器，将之作为后台负责执行和计算的kernel
;; 安装步骤主要参照于其Makefile
;; [shell]$ python pppp -C ppppconfig.py Pymacs.py.in pppp.rst.in pymacs.el.in pymacs.rst.in contrib tests
;; [shell]$ python setup.py install
;; 随后会在当前目录下生成pymacs.el文件，将其移至Emacs的load-path中即可
;; -----------------------------------------------------------------------------
(defun my-plugin-ropemacs-init ()
  (let ((path (concat package-user-dir "/ropemacs")))
    (when (file-directory-p path)
      (add-to-list 'load-path path)))
  (when (and (require 'pymacs nil t)
             (pymacs-load "ropemacs" "rope-" t))
    (setq ropemacs-confirm-saving t)
    (setq ropemacs-enable-autoimport t)
    (setq ropemacs-autoimport-modules '("os" "sys" "inspect")))
  ) ;; end of my-plugin-ropemacs-init()

(defun my-plugin-ropemacs-start ()
  ;; 已自动将(ropemacs-mode)加入python-mode-hook中，因此无需手动启用
  (when (fboundp 'ropemacs-mode)
    (when (boundp 'ac-sources)
      (setq ac-sources
            (append my-prog-ac-sources '(ac-source-ropemacs)))))
  ;; Ropemacs代码补全的快捷键与auto-complete不同，默认为M-/，可作为后者的补充
  ;; 因为后者在某些情况下必须至少输入一个字符，而此时就可以使用前者
  ) ;; end of my-plugin-ropemacs-start()

;; =============================================================================
(defun my-python-mode-init ()
  (let ((exec "python3"))
    (when (eq system-type 'windows-nt)
      (let* ((path (executable-find (concat exec "/python.exe")))
             (dir (when path (file-name-directory path))))
        (when dir
          (unless (member dir exec-path)
            (add-to-list 'exec-path dir t))
          (setq exec "python.exe"))))
    (when (executable-find exec)
      ;; 设置python-mode
      (when (require 'python nil t)
        (remove-hook 'python-mode-hook 'wisent-python-default-setup)
        (setq python-shell-interpreter exec
              python-shell-interpreter-args "-i"
              ;; python-shell-prompt-regexp ""
              ;; python-shell-prompt-output-regexp ""
              ;; python-shell-completion-setup-code ""
              ;; python-shell-completion-module-string-code ""
              ;; python-shell-completion-string-code ""
              ))
      ;; Ropemacs
      (my-plugin-ropemacs-init))))

(defun my-python-mode-start ()
  ;; 将lambda显示为λ
  (prettify-symbols-mode t)
  (setq prettify-symbols-alist '(("lambda" . 955)))
  (when (fboundp 'ropemacs-mdoe)
    (my-plugin-ropemacs-start)))

(eval-after-load 'python ;; /lisp/progmodes/python.el
  '(progn
     (my-python-mode-init)
     (add-hook 'python-mode-hook 'my-python-mode-start)))

(provide 'my-prog-py)
