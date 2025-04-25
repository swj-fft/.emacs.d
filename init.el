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

;; Native tab-line-mode for buffer tabs
;; (use-package tab-line
;;   :ensure nil
;;   :config
;;   (global-tab-line-mode 1)
;;   (setq tab-line-background-color "#282c34"
;;         tab-line-separator " | "
;;         tab-line-close-button-show nil
;;         tab-line-new-button-show nil
;;         tab-line-tab-name-function #'tab-line-tab-name-buffer) ; Show only buffer name
;;   (defun my-tab-line-buffer-groups ()
;;     "Group buffers for tab-line."
;;     (list
;;      (cond
;;       ((string-match-p "^\\*.*\\*$" (buffer-name)) "Emacs")
;;       ((eq major-mode 'dired-mode) "Dired")
;;       (t "User"))))
;;   (setq tab-line-tabs-buffer-group-function #'my-tab-line-buffer-groups)
;;   :bind
;;   (("C-<tab>" . tab-line-switch-to-next-tab)
;;    ("C-S-<tab>" . tab-line-switch-to-prev-tab)))

;; Native tab-bar-mode for workspace tabs (optional, disable if not needed)
(use-package tab-bar
  :ensure nil
  :config
  (tab-bar-mode 1) ; Set to -1 to disable if causing duplication
  (setq tab-bar-show t
        tab-bar-new-tab-to 'right
        tab-bar-tab-hints t)
  :bind
  (("M-h" . tab-bar-switch-to-prev-tab)
   ("M-l" . tab-bar-switch-to-next-tab)
   ("C-c t n" . tab-bar-new-tab)
   ("C-c t c" . tab-bar-close-tab)))

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

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'projectile
    (require 'treemacs-projectile))
  :bind
  (("M-0" . treemacs-select-window)
   ("C-c t" . treemacs))
  :config
  (setq treemacs-use-git-mode 'simple) ; 简化 Git 集成，减少依赖
  (treemacs-follow-mode t)) ; 自动跟踪当前文件

;; Configure treemacs-projectile
(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile)
  :demand t)

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (("M-0" . treemacs-select-window)
   ("C-c t" . treemacs))
  :config
  (setq treemacs-use-git-mode 'simple)
  (treemacs-follow-mode t))

;; Project management
(use-package projectile
  :config
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))
(with-eval-after-load 'projectile
  (add-to-list 'projectile-known-projects "~/.emacs.d"))

;; Ensure projectile is installed and enabled
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  ;; Add directories to globally ignore
  (dolist (dir '(".git"
                 "__pycache__"
                 "bazel-bin"
                 "bazel-out"
                 "bazel-testlogs"
                 "bazel-*")) ; Wildcard for bazel-<workspace> symlinks
    (add-to-list 'projectile-globally-ignored-directories dir))
  ;; Add file patterns to globally ignore
  (dolist (file '("*.pyc"
                  "*.o"
                  "*.a"
                  "*.so"
                  "bazel-*")) ; Ignore bazel-related files
    (add-to-list 'projectile-globally-ignored-files file))
  ;; Optional: Optimize indexing for large projects
  (setq projectile-indexing-method 'hybrid)
  ;; Auto-start Treemacs when switching projects
  (add-hook 'projectile-after-switch-project-hook
            (lambda ()
              (treemacs-display-current-project-exclusively))))

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

(provide 'init)
;;; init.el ends here
