(let ((def-threshold gc-cons-threshold))
  ;; this makes garbage collection less frequent,
  ;; which speeds up init by about 0.5 seconds.
  (setq gc-cons-threshold 80000000)
  (load "~/.emacs.d/my.emacs/init.el")
  (setq gc-cons-threshold def-threshold))

(message "emacs init time = %s" (emacs-init-time))
