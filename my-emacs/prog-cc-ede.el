(require 'my-prog-cc)
;===========================================================================
;; 1)对于复杂的项目，应利用ede-new新建并利用Project.ede文件定制
;ede-project-directories的作用：
;1)一方面是缩小ede搜索的范围以提高性能
;2)另一方面是限制ede-new可以生效的范围
(setq ede-project-directories '())
(add-to-list 'ede-project-directories (expand-file-name "~/projects"))

;===========================================================================
;; 2)对于简单的项目，应利用以下函数新建并定制
(let ((root "~/projects/workspace-eclipse/evolution/evolution/evo_btappl/.project"))
  (when (file-exists-p root)
    (ede-cpp-root-project "evo_btappl"
                          :file root
;                          :include-path '("/include")
;                          :system-include-path '("" "")
;                          :spp-table '(("MACRO1" . "VALUE1"))
                          )))

(provide 'my-prog-cc-ede)
