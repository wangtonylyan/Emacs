(provide 'my-prog-cc-ede)
(eval-when-compile
  (require 'my-prog-cc)
  )
;===========================================================================
;; 1)���ڸ��ӵ���Ŀ��Ӧ����ede-new�½�������Project.ede�ļ�����
;ede-project-directories�����ã�
;1)һ��������Сede�����ķ�Χ���������
;2)��һ����������ede-new������Ч�ķ�Χ
(setq ede-project-directories '())
(add-to-list 'ede-project-directories (expand-file-name "~/projects"))


;===========================================================================
;; 2)���ڼ򵥵���Ŀ��Ӧ�������º����½�������
(let ((root "~/projects/workspace-eclipse/evolution/evolution/evo_btappl/.project"))
  (when (file-exists-p root)
    (ede-cpp-root-project "evo_btappl"
                          :file root
;                          :include-path '("/include")
;                          :system-include-path '("" "")
;                          :spp-table '(("MACRO1" . "VALUE1"))
                          )))
