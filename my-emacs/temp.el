;; 重构
;; 1. 检查包含以下字符串的符号名
;;    my-, plugin
;;    user-emacs, private
;;    add-hook, cc

;; this is a template for 'use-package
(use-package package
  :disabled t
  :diminish (mode1
             mode2)
  :ensure t
  ;; 1. list dependent packages not shown in (package-list-packages)
  :requires (package1
             package2)
  :after (package1
          package2)
  :demand t
  ;; 1. mostly used in init-keys.el file only
  ;; 2. this package is required by some others
  :defer t
  ;; 1. list commands that are bound in init-keys.el file
  ;; 2. non-autoload commands
  :commands (command1
             command2)
  ;; 1. mostly this should be defined in init-keys.el
  :bind (("key1" . command1)
         ([remap command1] . command2))
  :hook ((before-init . pkg/package/init)
         (after-init . pkg/package/start))
  :preface
  ;; When this function is added to 'after-init-hook, it should either directly load this package,
  ;; or register some triggers for loading this package.
  ;; In the former case, the remaining procedure in this function is executed right after :config part.
  (defun pkg/package/start ()
    nil)
  :if (my/package-enabled-p 'package)
  ;; 1. executed during emacs initialization, i.e. loading init.el file
  :init
  (setq customized-config nil)
  ;; 1. executed after this package has been loaded or whenever reloaded
  :config)



(use-package w3m
  :preface
  (defvar pkg/w3m/exists-p
    (if (eq system-type 'windows-nt)
        (my/locate-exec "w3m.exe" "w3m" t)
      (my/locate-exec "w3m")))
  :if (and (my/package-enabled-p 'w3m) pkg/w3m/exists-p)
  :config
  (setq w3m-home-page "http://www.baidu.com/"
        w3m-command-arguments '("-cookie" "-F")
        w3m-quick-start t
        w3m-use-cookies t
        w3m-use-favicon t
        w3m-use-symbol t
        w3m-default-display-inline-images t
        w3m-show-graphic-icons-in-header-line nil
        w3m-show-graphic-icons-in-mode-line nil)
  (setq browse-url-browser-function 'w3m-browse-url)
  (my/add-mode-hook "w3m" #'visual-line-mode))

(use-package erc
  :if (my/package-enabled-p 'erc)
  :config
  (bind-keys :map erc-mode-map
             ;; ("<return>" . nil)
             ("C-<return>" . erc-send-current-line))
  (setq erc-autojoin-channels-alist nil ;; '(("freenode.net" "#emacs"))
        erc-interpret-mirc-color t
        erc-kill-buffer-on-part t))

(use-package circe
  :if (my/package-enabled-p 'circe)
  :config
  (setq circe-network-options '(("Freenode" ;; http://freenode.net/
                                 :nick ""
                                 :sasl-username ""
                                 :sasl-password ""
                                 :channels ("#emacs" "#c_lang_cn")))))


(defun my-func-prog-mode-beautify ()
  (interactive)
  (cond
   ((or (eq major-mode 'c-mode)
        (eq major-mode 'c++-mode))
    (let ((exe "uncrustify")
          (cfg "~/.uncrustify/alps.cfg"))
      (if (and (executable-find exe)
               (file-exists-p cfg))
          (message (shell-command-to-string
                    (concat exe " -l C -c " cfg " --no-backup " buffer-file-name)))
        (message "uncrustify unsupported!"))))
   ((eq major-mode 'python-mode)
    (if (and (my/package-enabled-p 'py-autopep8)
             (executable-find "autopep8")
             (fboundp 'py-autopep8-buffer))
        (py-autopep8-buffer)
      (message "autopep8 unsupported!")))
   ((derived-mode-p 'web-mode)
    (if (my/package-enabled-p 'web-beautify)
        (web-beautify-html)
      (message "html-beautify unsupported!")))
   (t (message "current major mode unsupported!"))))


;; replace built-in CEDET with an external one, if exists
;; $ git clone http://git.code.sf.net/p/cedet/git cedet
;; $ make
;; CEDET及其现状的介绍
;; https://www.emacswiki.org/emacs/CollectionOfEmacsDevelopmentEnvironmentTools
;; https://stackoverflow.com/questions/12711765/status-of-cedet-and-ecb-in-emacs-24-2
;; http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
(when (boundp 'my-private-project-root-directory)
  (let* ((path (file-name-as-directory
                (concat my-private-project-root-directory "cedet")))
         (file (concat path "cedet-devel-load.el")))
    (when (file-exists-p file)
      (load-file file)
      (add-to-list 'load-path (concat path "contrib"))
      (add-to-list 'Info-directory-list (concat path "doc/info/")))))



# http_proxy=http://CHT1HTSH3191:Alps1911@10.25.71.1:8080
# https_proxy=https://CHT1HTSH3191:Alps1911@10.25.71.1:8080


[http]
proxy = http://CHT1HTSH3191:Alps1911@10.25.71.1:8080
sslverify = false
[https]
proxy = https://CHT1HTSH3191:Alps1911@10.25.71.1:8080
