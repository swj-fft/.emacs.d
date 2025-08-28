;;; -*- lexical-binding: t -*-

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")))
(setq package-enable-at-startup t)

(unless (package-installed-p 'use-package)
  (package-install 'use-package)) ; Manual refresh only if needed
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; UI and theme
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)) ; Minimize buffer name display

;; Git integration
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package markdown-mode
  :mode "\\.md\\'")

;; Org-mode
(use-package org
  :config
  (setq org-startup-indented t
        org-startup-folded t))

;; Keybinding helpers
(use-package which-key
  :config
  (which-key-mode))

;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file :noerror))

;; 确保 undo-tree 安装
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode ;; 在 modeline 中隐藏 undo-tree 的标志
  :init
  ;; 启用全局 undo-tree 模式
  (global-undo-tree-mode 1)
  :custom
  ;; 禁用自动保存历史到文件，防止生成 ~undo-tree~ 文件
  (undo-tree-auto-save-history nil)
  ;; 增加撤销历史的上限，适应现代硬件
  (undo-tree-history-limit 1000000)
  ;; 优化性能，启用压缩历史记录
  (undo-tree-enable-undo-history-compression t)
  ;; 设置可视化界面的默认宽度
  (undo-tree-visualizer-window-width 40)
  ;; 启用时间戳显示，便于追踪编辑历史
  (undo-tree-visualizer-timestamps t)
  :bind
  ;; 现代快捷键绑定，模仿常见编辑器的 undo/redo
  (("C-x C-u" . undo-tree-visualize))
  :config
  ;; 优化可视化界面的显示
  (setq undo-tree-visualizer-diff t) ;; 显示每个节点的差异
  ;; 防止 undo-tree 与某些模式（如 org-mode）冲突
  (add-hook 'undo-tree-visualizer-mode-hook
            (lambda ()
              (setq-local global-hl-line-mode nil))))

(use-package goto-chg
  :ensure t
  :bind (("C--" . goto-last-change)
         ("C-=" . goto-last-change-reverse)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines) ;; 每行添加光标
         ("C->" . mc/mark-next-like-this) ;; 下一个匹配
         ("C-<" . mc/mark-previous-like-this) ;; 上一个匹配
         ("C-c C-<" . mc/mark-all-like-this)) ;; 所有匹配
  :config
  ;; 优化性能：限制光标数量
  (setq mc/max-cursors 100)
  ;; 与 evil-mode 兼容
  (when (featurep 'evil)
    (add-to-list 'mc/unsupported-minor-modes 'evil-mode)))

(require 'project-conf)
(require 'copilot-conf)
(require 'bazel-conf)
(require 'completion-conf)
(require 'format-conf)
(require 'move-text-conf)

(provide 'packages)
