;;; -*- lexical-binding: t -*-

;; Define global exclude patterns
(defconst my/exclude-directories
  '(".git"
    "__pycache__"
    "bazel-bin"
    "bazel-out"
    "bazel-testlogs"
    "bazel-*") ; Wildcard for bazel-<workspace> symlinks
  "List of directories to exclude from indexing and navigation.")

(defconst my/exclude-files
  '("*.pyc"
    "*.o"
    "*.a"
    "*.so"
    "bazel-*") ; Bazel-related files
  "List of file patterns to exclude from indexing and navigation.")

;; Package management
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")))
(setq package-enable-at-startup t)
;; No package-initialize or package-refresh-contents to avoid automatic updates

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package)) ; Manual refresh only if needed
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; General settings
(setq inhibit-startup-screen t
      initial-scratch-message nil
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-saves/" t))
      create-lockfiles nil
      header-line-format nil) ; Disable header line to avoid duplication
(make-directory "~/.emacs.d/backups" t)
(make-directory "~/.emacs.d/auto-saves" t)
(tool-bar-mode -1)
(menu-bar-mode -1)
;; (scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(column-number-mode 1)
(show-paren-mode 1)

;; UI and theme
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)) ; Minimize buffer name display


;; Completion framework: Vertico and friends
(use-package vertico
  :init
  (vertico-mode 1)
  :config
  (setq vertico-cycle t
        vertico-resize nil)
  :bind (("C-s" . isearch-forward)
         ("M-x" . execute-extended-command)
         ("C-x C-f" . find-file)))

(use-package marginalia
  :init
  (marginalia-mode 1)
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

;; Consult for advanced buffer switching
(use-package consult
  :ensure t
  :config
  (setq consult-projectile-use-projectile t)) ; Integrate with projectile


;; Code completion
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2)
  (add-to-list 'company-backends 'company-capf))

;; GitHub Copilot
(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :config
  (setq copilot-ignored-paths (append my/exclude-directories my/exclude-files)))

;; Git integration
(use-package magit
  :bind ("C-x g" . magit-status))


;; LSP for programming
(use-package lsp-mode
  :hook ((prog-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c++-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config
  (dolist (dir my/exclude-directories)
    (add-to-list 'lsp-file-watch-ignored-directories (concat "[/\\]" dir "$")))
  (dolist (file my/exclude-files)
    (add-to-list 'lsp-file-watch-ignored-files (concat "[/\\]" file "$")))
  (setq lsp-idle-delay 0.5
        lsp-disabled-clients '(pyls)
        lsp-python-ms-executable nil
        lsp-warn-no-matched-clients nil)
  (setq lsp-clients-python-server 'ruff)
  (add-to-list 'lsp-language-id-configuration '(python-mode . "python"))
  (add-to-list 'lsp-language-id-configuration '(c++-mode . "cpp"))
  (add-to-list 'lsp-language-id-configuration '(c++-mode . "cc"))
  (add-to-list 'lsp-language-id-configuration '(c-mode . "c")))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-doc-enable t))

;; Language-specific settings
(use-package python-mode
  :mode "\\.py\\'"
  :hook (python-mode . lsp-deferred))

(use-package cc-mode
  :mode (("\\.cpp\\'" . c++-mode)
         ("\\.h\\'" . c++-mode)
         ("\\.c\\'" . c-mode))
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)))

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

;; Load custom keybindings
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(when (file-exists-p (expand-file-name "lisp/keybindings.el" user-emacs-directory))
  (require 'keybindings))

;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file :noerror))

;; Performance tweaks
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Treemacs configuration
(use-package treemacs
  :ensure t
  :defer t
  :bind
  (("M-0" . treemacs-select-window)
   ("C-c t" . treemacs))
  :bind (:map treemacs-mode-map
              ("M-j" . treemacs-previous-entry)
              ("M-p" . treemacs-next-entry))
  :config
  (setq treemacs-use-git-mode 'simple)
  (setq treemacs-no-png-images t) ; Terminal-friendly
  (treemacs-follow-mode t)
  ;; Disable line numbers in Treemacs
  (add-hook 'treemacs-mode-hook
            (lambda () (display-line-numbers-mode -1)))
  ;; Apply exclude patterns
  (when (boundp 'treemacs-file-ignore-regexps)
    (dolist (dir my/exclude-directories)
      (add-to-list 'treemacs-file-ignore-regexps (concat dir "\\'")))
    (dolist (file my/exclude-files)
      (add-to-list 'treemacs-file-ignore-regexps (concat file "\\'")))))

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

;;; Treemacs 配置
(use-package treemacs
  :defer t
  :config
  (progn
    (setq treemacs-collapse-dirs (if treemacs-python-executable 3 0) ; 折叠空目录
          treemacs-file-event-delay 500 ; 降低文件事件延迟
          treemacs-git-mode 'deferred ; 使用异步 Git 模式
          treemacs-file-ignore-globs '("*.pyc" "node_modules" "__pycache__") ; 忽略特定文件
          treemacs-sorting 'alphabetic-asc) ; 按字母排序
    (treemacs-follow-mode t) ; 自动跟随当前缓冲区
    (treemacs-filewatch-mode t) ; 启用文件系统监控
    (treemacs-git-commit-diff-mode t)) ; 显示 Git 提交差异
  :bind
  (:map global-map
        ("M-0" . treemacs-select-window) ; 切换到 Treemacs 窗口
        ("C-x t t" . treemacs) ; 打开/切换 Treemacs
        ("C-x t p" . treemacs-projectile)) ; 集成 Projectile
  :hook
  (treemacs-mode . (lambda () (display-line-numbers-mode -1)))) ; 禁用 Treemacs 中的行号

;;; Treemacs 与 Projectile 集成
(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

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

(provide 'init)
;;; init.el ends here
