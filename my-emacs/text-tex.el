(require 'my-init)

(defvar my-text-tex-mode-start-hook '())
(defvar my-text-tex-mode-start-mode 'tex-mode)

;; =============================================================================
(defun my-plugin-auctex-init ()
  (when (member 'auctex package-selected-packages)
    ;; �ò���ڰ�װ��ͻ���(package-initialize)��ִ�ж����Զ�����
    ;; �������Ҳ��������·�ʽ�����¼���
    ;; (load "auctex.el" t)
    (when (eq system-type 'windows-nt)
      (when (my-func-executable-find "MiKTeX/miktex/bin/x64"
                                     "miktex-texworks.exe")
        (require 'tex-mik nil t)))
    (setq TeX-auto-save t
          TeX-parse-self t)
    ;; (setq-default TeX-master nil)
    (add-hook 'my-text-tex-mode-start-hook 'my-plugin-auctex-start t)
    (setq my-text-tex-mode-start-mode 'TeX-mode)))

(defun my-plugin-auctex-start ()
  (reftex-mode t)
  (TeX-fold-mode t))

;; =============================================================================
(defun my-text-tex-mode-init ()
  (my-plugin-auctex-init)
  (if (eq my-text-tex-mode-start-mode 'TeX-mode)
      (add-hook 'TeX-mode-hook 'my-text-tex-mode-start t)
    (add-hook 'tex-mode-hook 'my-text-tex-mode-start t)))

(defun my-text-tex-mode-start ()
  (font-lock-mode 1)
  (linum-mode 1)
  (run-hooks 'my-text-tex-mode-start-hook))

;; (add-hook 'text-mode-hook 'my-text-tex-mode-init t)
(eval-after-load 'tex '(my-text-tex-mode-init))

(provide 'my-text-tex)
