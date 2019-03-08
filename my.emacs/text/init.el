;; -*- coding: utf-8 -*-

(use-package pdf-tools
  :defer t
  :if (pkg/package/enabled-p 'pdf-tools)
  :init
  ;; (pdf-tools-install)
  :config
  (setq pdf-view-continuous t))

(use-package org
  :defer t
  :config
  (setq org-src-fontify-natively t)
  (my/add-mode-hook "org" #'org-indent-mode))


(provide 'my/text/init)
