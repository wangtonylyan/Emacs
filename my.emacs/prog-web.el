;; -*- coding: utf-8 -*-

(defvar my-prog-web-mode-start-hook '())

(defun my-plugin-web-mode-init ()
  (use-package web-mode
    :if (my-func-package-enabled-p 'web-mode)
    ;; ("M-;" . comment/uncomment)
    :init
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)) ;; html
    (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode)) ;; php
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode)) ;; php
    (add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode)) ;; php template
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)) ;; django
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)) ;; ruby
    (add-to-list 'auto-mode-alist '("\\.rhtml\\'" . web-mode)) ;; ruby
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (setq web-mode-engines-alist '(("php" . "\\.php\\'")
                                   ("php" . "\\.phtml\\'")
                                   ("django" . "\\.djhtml\\'")
                                   ("erb" . "\\.erb\\'")
                                   ("erb" . "\\.rhtml\\'")
                                   ("jsp" . "\\.jsp\\'"))
          web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-style-padding 1
          web-mode-script-padding 1
          web-mode-block-padding 0
          web-mode-comment-style 2
          web-mode-enable-auto-pairing t
          web-mode-enable-css-colorization t
          web-mode-enable-block-face t
          web-mode-enable-part-face t
          web-mode-enable-comment-keywords t
          web-mode-enable-heredoc-fontification t
          web-mode-enable-current-element-highlight t
          web-mode-enable-current-column-highlight t)
    (add-hook 'my-prog-web-mode-start-hook 'my-plugin-web-mode-start t)))

(defun my-plugin-web-mode-start ()
  )

;; =============================================================================
(defun my-prog-web-mode-init ()
  (my-plugin-web-mode-init)
  (add-hook 'prog-mode-hook 'my-prog-web-mode-start t))

(defun my-prog-web-mode-start ()
  (run-hooks 'my-prog-web-mode-start-hook))

(add-hook 'after-init-hook 'my-prog-web-mode-init t)

(provide 'my/prog-web)
