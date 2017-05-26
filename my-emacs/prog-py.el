(require 'my-prog)

(defvar my-prog-py-mode-start-hook '())

;; =============================================================================
;; ����Emacs����Python֧�ַ���Ľ��ܿɲο����£�
;; https://wiki.python.org/moin/EmacsEditor
;; http://emacswiki.org/emacs/PythonProgrammingInEmacs
;; https://realpython.com/blog/python/emacs-the-best-python-editor/

;; =============================================================================
;; python.el
;; Emacs�����˸ò������������Ϊ��python-mode��ģʽ��Ĭ��֧��
;; ʵ�������Ǽ�࣬�����ܵ��������ں���Emacs�����еĹ���
;; -----------------------------------------------------------------------------
(defun my-plugin-python-init ()
  (let ((exe "python3")) ;; python��python3
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
;; �ò��������ȫ����python.el�����ŵ����ܹ�֧�ֵ�Ԫ���ԡ�IPython�ȶ��⹦��
;; ʵ����������Ǵ��ȫ�������ܵز���������������������
;; https://launchpad.net/python-mode
;; https://github.com/emacsmirror/python-mode
;; -----------------------------------------------------------------------------

;; ��������������ڶ����Python���֧�֣���Ҫ���Ȱ�װ��Ӧ��Python����ܹ�����ʹ��
;; ELPY��Ropemacs���������ṩ���������ƣ�һ�㻥��ʹ��
;; ���߶��ṩ�˶���Python�������ĵ��ã��Լ�һЩPython��Ķ���֧��

;; =============================================================================
;; ELPY (Emacs Lisp Python Environment)
;; https://github.com/jorgenschaefer/elpy
;; ������python�⣺flake8, jedi
;; -----------------------------------------------------------------------------
(defun my-plugin-elpy-init ()
  (when (and (member 'elpy package-selected-packages)
             (require 'elpy nil t))
    ;; ���ÿ�ݼ���
    ;; C-c C-c���ڵ���Python������
    ;; elpyĬ��֧�ֲ�ʹ��Emacs���õ�flymake������������л���flycheck
    (with-eval-after-load 'flycheck
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      (add-hook 'elpy-mode-hook 'flycheck-mode-on-safe t))
    (when (and (member 'py-autopep8 package-selected-packages)
               (executable-find "autopep8")
               (require 'py-autopep8 nil t))
      (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save t))
    (setq elpy-rpc-backend "jedi" ;; ֧��rope��jedi��������
          )
    ;; ָ��Python������
    ;; (elpy-use-ipython) ;; (elpy-use-cpython)
    ;; (elpy-enable)
    (add-hook 'my-prog-py-mode-start-hook 'my-plugin-elpy-start t)))

(defun my-plugin-elpy-start ()
  (elpy-mode 1))

;; =============================================================================
;; Ropemacs
;; ������python�⣺rope, pymacs, ropemacs
;; ����pymacs���ڰ�װ����Զ�����pymacs.el�������Ҫ���ò��������'load-path��
;; ��ropemacs����ΪEmacs�ṩ��һ��ʹ��rope�����ģʽropemacs-mode
;; ����ģʽ����pymacs.el����ļ��ض����Զ�������'python-mode-hook��
;; ��(add-hook 'python-mode-hook 'ropemacs-mode)
;; ÿ��roepmacs-mode����ʱ��һ��pymacs�ͻ����̺�һ��Python�������������������˴�����
;; ���Emacs��ͨ����ÿͻ����̽���ͨ�ţ��Ӷ���ȡrope������֧��
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
      ;; ropemacsģʽ�е��Զ���ȫ��ݼ���auto-complete��ͬ��Ĭ��ΪM-/������Ϊ���ߵĲ���
      ;; ��Ϊ������ĳЩ����±�����������һ���ַ�������ʱ�Ϳ���ʹ��ǰ��
      (add-hook 'my-prog-py-mode-start-hook 'my-plugin-ropemacs-start t))))

(defun my-plugin-ropemacs-start ()
  ;; (ropemacs-mode 1) ;; �����ֶ�����
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
  ;; ��lambda��ʾΪ��
  (prettify-symbols-mode 1)
  (setq prettify-symbols-alist '(("lambda" . 955)))
  (run-hooks 'my-prog-py-mode-start-hook))

;; (add-hook 'prog-mode-hook 'my-prog-py-mode-init t)
(eval-after-load 'python '(my-prog-py-mode-init))

(provide 'my-prog-py)
