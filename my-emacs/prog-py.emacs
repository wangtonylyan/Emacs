;===========================================================================
; Ropemacs
;===========================================================================
; ����Emacs����Python֧�ַ���Ľ��ܿɲο����£�
; https://wiki.python.org/moin/EmacsEditor
; http://emacswiki.org/emacs/PythonProgrammingInEmacs
; ��֮��Emacs�е�Python������Ҫ�ɷ�Ϊ����
; ���к���������Ϊǰ���ֵĹ��ܲ�����滻������û���ṩһ��������ȫ�Ĺ��ܣ�
; 1) python.el
; �ò����Emacs����ά�����Ѽ�����Emacs24���ϰ汾�У���ΪĬ�ϵ�python-mode֧��
; ʵ�������Ǽ�࣬�����ܵ�������������Emacs�����еĹ���
; 2) python-mode.el
; �ò����Python����ά�������ŵ�������ܹ�֧�ֵ�Ԫ���ԡ�IPython��
; ʵ�������Ǵ��ȫ�������ܵز���������������������
; 3) Elpy
; https://github.com/jorgenschaefer/elpy
; ��ʹ��Rope��Jedi��Ϊ�ײ�֧��
; 4) Ropemacs
; ʹ��Rope��Pymacs��Ϊ�ײ�֧��


;===========================================================================
; python-mode.el
;===========================================================================
; https://launchpad.net/python-mode
; https://github.com/emacsmirror/python-mode



;===========================================================================
; Ropemacs
;===========================================================================
; https://github.com/python-rope
; ��ײ����Rope��Pymacs����װ����Ϊ���Ȱ�װRope��Pymacs������ٰ�װRopemacs
; [shell]$ python setup.py install
;---------------------------------------------------------------------------
; Rope
;---------------------------------------------------------------------------
; https://github.com/python-rope/rope
; Rope��һ��֧�ִ����ع���Python�����⣬�ṩ��������ȫ����ת�ȹ���
; [shell]$ python setup.py install
; ���׵ػ��谲װRopemode������ΪRope��ʹ�ýӿ�
; https://github.com/python-rope/ropemode
; [shell]$ python setup.py install
;---------------------------------------------------------------------------
; Pymacs
;---------------------------------------------------------------------------
; version: 0.25
; https://github.com/pinard/Pymacs
; Pymacs��һ������������SLIME��Emacs�����������������һ��Python����������
; ��װ������Ҫ��������Makefile
; [shell]$ python pppp -C ppppconfig.py Pymacs.py.in pppp.rst.in pymacs.el.in pymacs.rst.in contrib tests
; [shell]$ python setup.py install
; �����ڵ�ǰĿ¼������pymacs.el�ļ�����������Emacs��load-path�м���
;===========================================================================
(defun my-plugin-ropemacs-init ()
  (add-to-list 'load-path (concat my-emacs-plugin-load-path "ropemacs"))
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-confirm-saving t)
  (setq ropemacs-enable-autoimport t)
  (setq ropemacs-autoimport-modules '("os"))
  ) ;end of my-plugin-ropemacs-init()

(defun my-plugin-ropemacs-start ()
  ; ���Զ���(ropemacs-mode)����python-mode-hook�У���������ֶ�����
  (when (boundp 'ac-sources)
    (setq ac-sources
          (append my-prog-ac-sources '(ac-source-ropemacs))))
  ;; Ropemacs���벹ȫ�Ŀ�ݼ���auto-complete��ͬ��Ĭ��ΪM-/������Ϊ���ߵĲ���
  ;; ��Ϊ������ĳЩ����±�����������һ���ַ�������ʱ�Ϳ���ʹ��ǰ��
  ) ;end of my-plugin-ropemacs-start()


;===========================================================================
;===========================================================================
(defun my-python-mode-init ()
  (when (eq system-type 'windows-nt)
    (setq my-python-interpreter-path (concat my-emacs-exec-bin-path "/programming/python2/"))
    (add-to-list 'exec-path my-python-interpreter-path)
    )
  (when (executable-find "python")
    ;; ����python-mode
    (require 'python)
    (remove-hook 'python-mode-hook 'wisent-python-default-setup)
    (setq python-shell-interpreter "python"
          python-shell-interpreter-args "-i"
;          python-shell-prompt-regexp ""
;          python-shell-prompt-output-regexp ""
;          python-shell-completion-setup-code ""
;          python-shell-completion-module-string-code ""
;          python-shell-completion-string-code ""
          )
    ;; lambda-mode
    (require 'lambda-mode)
    (add-hook 'python-mode-hook #'lambda-mode 1)
    (setq lambda-regex "lambda ")
    (setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))
    ;; Ropemacs
    (my-plugin-ropemacs-init)
    ))
(defun my-python-mode-start ()
  (when (fboundp 'ropemacs-mdoe)
    (my-plugin-ropemacs-start)
    ))
(eval-after-load 'python ;/lisp/progmodes/python.el
  '(progn
     (my-python-mode-init)
     (add-hook 'python-mode-hook 'my-python-mode-start)
     ))
