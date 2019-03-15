;; -*- coding: utf-8 -*-

(let ((path (my/path-exists-p (my/get-user-emacs-file "my.elpa")))
      (exclude "^[^.]")) ;; excluding ".", "..", and dotfiles
  (dolist (dir (directory-files path t exclude t))
    (add-to-list 'load-path dir t)))


(use-package awesome-tab
  :after (tabbar)
  :if (pkg/package/enabled-p 'awesome-tab)
  :config
  (setq awesome-tab-background-color (face-background 'default)
        awesome-tab-style "wave"
        awesome-tab-height 22
        awesome-tab-label-fixed-length 0
        awesome-tab-display-sticky-function-name nil)
  (set-face-attribute 'awesome-tab-default nil
                      :background (face-background 'default)
                      :foreground (face-background 'default))
  (set-face-attribute 'awesome-tab-unselected nil
                      :background (face-background 'default))
  (set-face-attribute 'awesome-tab-selected nil
                      :background (face-background 'hl-line))
  ;; (add-to-list 'awesometab-hide-tabs-hooks)
  (define-key awesome-tab-mode-map awesome-tab-prefix-key nil)
  (awesome-tab-mode 1)
  (use-package awesome-tab
    :after (helm)
    :if (pkg/package/enabled-p 'helm)
    :config
    (awesome-tab-build-helm-source)
    (add-to-list 'helm-mini-default-sources 'helm-source-awesome-tab-group)))


(provide 'my/elpa/init)
