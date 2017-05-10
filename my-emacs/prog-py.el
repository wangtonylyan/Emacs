(require 'my-prog)
;; =============================================================================
;; ����Emacs����Python֧�ַ���Ľ��ܿɲο����£�
;; https://wiki.python.org/moin/EmacsEditor
;; http://emacswiki.org/emacs/PythonProgrammingInEmacs
;; ��֮��Emacs�е�Python�������¿ɷ�Ϊ��������
;; ���к���������Ϊǰ���ֵĹ��ܲ�����滻������û���ṩһ��������ȫ�Ĺ��ܣ�
;; 1) python.el
;; �ò����Emacs����ά�����Ѽ�����Emacs24���ϰ汾�У���ΪĬ�ϵ�python-mode֧��
;; ��������Ǽ�࣬�����ܵ�������������Emacs�����еĹ���
;; 2) python-mode.el
;; �ò����Python����ά�������ŵ�������ܹ�֧�ֵ�Ԫ���ԡ�IPython��
;; ��������Ǵ��ȫ�������ܵز���������������������
;; 3) Elpy
;; https://github.com/jorgenschaefer/elpy
;; ��ʹ��Rope��Jedi��Ϊ�ײ�֧��
;; 4) Ropemacs
;; ʹ��Rope��Pymacs��Ϊ�ײ�֧��
;; -----------------------------------------------------------------------------
;; python-mode.el
;; https://launchpad.net/python-mode
;; https://github.com/emacsmirror/python-mode
;; -----------------------------------------------------------------------------
;; Ropemacs
;; https://github.com/python-rope
;; ��ײ����Rope��Pymacs����װ����Ϊ���Ȱ�װRope��Pymacs������ٰ�װRopemacs
;; �䰲װ��Python�У������ڼ���ʱִ��Elisp���򣬴Ӷ�ΪEmacs�ṩ֧��
;; [shell]$ python setup.py install
;; -----------------------------------------------------------------------------
;; Rope
;; https://github.com/python-rope/rope
;; Rope��һ��֧�ִ����ع���Python�����⣬�ṩ��������ȫ����ת�ȹ���
;; [shell]$ python setup.py install
;; ���׵ػ��谲װRopemode������ΪRope��ʹ�ýӿ�
;; https://github.com/python-rope/ropemode
;; [shell]$ python setup.py install
;; -----------------------------------------------------------------------------
;; Pymacs
;; version: 0.25
;; https://github.com/pinard/Pymacs
;; Pymacs��һ��������ͬIPython client��Emacs���
;; ���������������һ��Python����������֮��Ϊ��̨����ִ�кͼ����kernel
;; ��װ������Ҫ��������Makefile
;; [shell]$ python pppp -C ppppconfig.py Pymacs.py.in pppp.rst.in pymacs.el.in pymacs.rst.in contrib tests
;; [shell]$ python setup.py install
;; �����ڵ�ǰĿ¼������pymacs.el�ļ�����������Emacs��load-path�м���
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
  ;; ���Զ���(ropemacs-mode)����python-mode-hook�У���������ֶ�����
  (when (fboundp 'ropemacs-mode)
    (when (boundp 'ac-sources)
      (setq ac-sources
            (append my-prog-ac-sources '(ac-source-ropemacs)))))
  ;; Ropemacs���벹ȫ�Ŀ�ݼ���auto-complete��ͬ��Ĭ��ΪM-/������Ϊ���ߵĲ���
  ;; ��Ϊ������ĳЩ����±�����������һ���ַ�������ʱ�Ϳ���ʹ��ǰ��
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
      ;; ����python-mode
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
  ;; ��lambda��ʾΪ��
  (prettify-symbols-mode t)
  (setq prettify-symbols-alist '(("lambda" . 955)))
  (when (fboundp 'ropemacs-mdoe)
    (my-plugin-ropemacs-start)))

(eval-after-load 'python ;; /lisp/progmodes/python.el
  '(progn
     (my-python-mode-init)
     (add-hook 'python-mode-hook 'my-python-mode-start)))

(provide 'my-prog-py)
