;;; -*- lexical-binding: t -*-

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode ;; 隐藏 modeline 标志
  :config
  (editorconfig-mode 1) ;; 全局启用
  ;; 确保 editorconfig 覆盖默认缩进设置
  (add-hook 'c++-mode-hook #'editorconfig-apply))

;;; Projectile 配置
(use-package projectile
  :diminish projectile-mode ; 隐藏模式行中的 Projectile 标志
  :config
  (projectile-mode +1) ; 全局启用 Projectile
  (setq projectile-project-search-path '("~/Projects/")) ; 设置项目根目录
  (setq projectile-enable-caching t) ; 启用缓存以提高性能
  (setq projectile-switch-project-action #'projectile-dired) ; 切换项目时打开 Dired
  :bind-keymap
  ("C-c p" . projectile-command-map) ; Projectile 命令前缀
  :init
  (when (file-directory-p "~/Projects")
    (setq projectile-known-projects-file
          (expand-file-name "~/.emacs.d/projectile-bookmarks.eld"))))

(use-package treemacs
  :ensure t
  :config
  ;; 自定义函数：选择并聚焦 Treemacs 窗口
  (defun treemacs-select-and-focus ()
    "Select or open the Treemacs window and ensure the cursor is in the Treemacs buffer."
    (interactive)
    (unless (treemacs-get-local-window)
      (treemacs)) ; 打开 Treemacs 如果它尚未存在
    (treemacs-select-window)
    (when (treemacs-get-local-window)
      (select-window (treemacs-get-local-window))))

  ;; 自定义函数：移动到上一行并聚焦 Treemacs
  (defun treemacs-previous-line-and-focus ()
    "Move to the previous line in Treemacs and focus the Treemacs window."
    (interactive)
    (treemacs-select-and-focus)
    (when (eq major-mode 'treemacs-mode)
      (treemacs-previous-line 1)))

  ;; 自定义函数：移动到下一行并聚焦 Treemacs
  (defun treemacs-next-line-and-focus ()
    "Move to the next line in Treemacs and focus the Treemacs window."
    (interactive)
    (treemacs-select-and-focus)
    (when (eq major-mode 'treemacs-mode)
      (treemacs-next-line 1)))

  ;; 可选：启用 follow-mode 自动跟踪当前文件
  (treemacs-follow-mode t)

  ;; 全局键绑定，适用于任何缓冲区
  :bind
  (("M-0" . treemacs-select-window)
   ("C-c t" . treemacs))
  :bind
  (:map global-map
        ("M-p" . treemacs-previous-line-and-focus)
        ("M-n" . treemacs-next-line-and-focus)))

;;; Centaur Tabs 配置
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-style "wave" ; 标签页样式
        centaur-tabs-height 32 ; 标签高度
        centaur-tabs-set-icons t ; 启用图标（需要 all-the-icons）
        centaur-tabs-set-bar 'under ; 高亮条位置
        centaur-tabs-set-modified-marker t ; 显示修改标记
        centaur-tabs-cycle-scope 'tabs ; 标签循环范围
        centaur-tabs-excluded-prefixes '("*" " *") ; 忽略特定缓冲区
        centaur-tabs-group-by-projectile-project t) ; 按 Projectile 项目分组
  (centaur-tabs-headline-match) ; 匹配标题样式
  :bind
  ("M-h" . centaur-tabs-backward) ; 上一标签
  ("M-l" . centaur-tabs-forward) ; 下一标签
  :hook
  (dired-mode . centaur-tabs-local-mode) ; 在 Dired 中禁用标签
  (treemacs-mode . centaur-tabs-local-mode)) ; 在 Treemacs 中禁用标签

;;; 安装 all-the-icons 以支持 Centaur Tabs 和 Treemacs 的图标
(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))

(provide 'project-conf)
