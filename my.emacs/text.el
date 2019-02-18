;; -*- coding: utf-8 -*-

(use-package pdf-tools
  :if (my/package-enabled-p 'pdf-tools)
  :init
  (setq pdf-view-continuous t)
  :config
  (pdf-tools-install))

(use-package org
  :commands (org-capture
             org-agenda)
  :init
  (setq org-src-fontify-natively t)
  :config
  (my/add-mode-hook "org" #'org-indent-mode))

(use-package markdown-mode
  :commands (markdown-mode
             gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :preface
  (defun pkg/markdown-mode/command ()
    (or (my/locate-exec "multimarkdown")
        (my/locate-exec "markdown")))
  :if (and (my/package-enabled-p 'markdown-mode)
           (pkg/markdown-mode/command))
  :init
  (setq markdown-command (pkg/markdown-mode/command))
  :config
  (use-package markdown-preview-mode
    :if (my/package-enabled-p 'markdown-preview-mode)
    ))

(use-package auctex
  :if (my/package-enabled-p 'auctex)
  )

(provide 'my/text)
