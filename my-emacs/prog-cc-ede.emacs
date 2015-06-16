;===========================================================================
;; 1)对于复杂的项目，应利用ede-new新建并利用Project.ede文件定制
;ede-project-directories的作用：
;1)一方面是缩小ede搜索的范围以提高性能
;2)另一方面是限制ede-new可以生效的范围
(setq ede-project-directories '())
(add-to-list 'ede-project-directories (expand-file-name "~/projects"))


;===========================================================================
;; 2)对于简单的项目，应利用以下函数新建并定制
;  (ede-cpp-root-project "EVOLUTION"
;           :file "~/projects/EVOLUTION/EVOLUTION.org"
;           :include-path '("/include")
;           :system-include-path '("" "")
;           :spp-table '(("MACRO1" . "VALUE1"))
;           )
