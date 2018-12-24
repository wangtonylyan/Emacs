;; 重构
;; 1. 检查包含以下字符串的符号名
;;    my-, plugin
;;    user-emacs, private
;;    add-hook, cc



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
