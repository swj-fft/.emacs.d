;;; init.el --- Modern Emacs configuration with Vertico and Copilot -*- lexical-binding: t; -*-

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

;; Install quelpa and quelpa-use-package for non-MELPA packages
(use-package quelpa
  :ensure t
  :config
  (setq quelpa-upgrade-p nil)) ; Disable automatic upgrades

(use-package quelpa-use-package
  :ensure t
  :init
  (require 'quelpa-use-package))

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
(scroll-bar-mode -1)
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
(use-package tab-line
  :ensure nil
  :config
  (global-tab-line-mode 1)
  (setq tab-line-background-color "#282c34"
        tab-line-separator " | "
        tab-line-close-button-show nil
        tab-line-new-button-show nil
        tab-line-tab-name-function #'tab-line-tab-name-buffer) ; Show only buffer name
  (defun my-tab-line-buffer-groups ()
    "Group buffers for tab-line."
    (list
     (cond
      ((string-match-p "^\\*.*\\*$" (buffer-name)) "Emacs")
      ((eq major-mode 'dired-mode) "Dired")
      (t "User"))))
  (setq tab-line-tabs-buffer-group-function #'my-tab-line-buffer-groups)
  :bind
  (("C-<tab>" . tab-line-switch-to-next-tab)
   ("C-S-<tab>" . tab-line-switch-to-prev-tab)))

;; Native tab-bar-mode for workspace tabs (optional, disable if not needed)
(use-package tab-bar
  :ensure nil
  :config
  (tab-bar-mode 1) ; Set to -1 to disable if causing duplication
  (setq tab-bar-show t
        tab-bar-new-tab-to 'right
        tab-bar-tab-hints t)
  :bind
  (("C-c t n" . tab-bar-new-tab)
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
         ("C-x b" . switch-to-buffer)
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

;; Code completion
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2)
  (add-to-list 'company-backends 'company-capf))

;; GitHub Copilot
(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("*.el" "dist")
                   :upgrade nil) ; Disable automatic upgrades
  :hook (prog-mode . copilot-mode)
  :config
  (setq copilot-idle-delay 0.5
        copilot-server-executable (or (executable-find "copilot-language-server")
                                      "/usr/lib/node_modules/@github/copilot-language-server"))
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("C-<tab>" . copilot-accept-completion-by-word)
              ("C-TAB" . copilot-accept-completion-by-word)))

;; Git integration
(use-package magit
  :bind ("C-x g" . magit-status))

;; Project management
(use-package projectile
  :config
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; LSP for programming
(use-package lsp-mode
  :hook ((prog-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c++-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-idle-delay 0.5
        lsp-disabled-clients '(pyls)
        lsp-python-ms-executable nil
        lsp-warn-no-matched-clients nil
        lsp-file-watch-ignored-directories
        '("[/\\\\]\\.git$"
          "[/\\\\]\\.hg$"
          "[/\\\\]\\.svn$"
          "[/\\\\]venv$"
          "[/\\\\]\\.venv$"
          "[/\\\\]env$"
          "[/\\\\]node_modules$"
          "[/\\\\]__pycache__$"
          "[/\\\\]\\.cache$"
          "[/\\\\]\\.tox$"
          "[/\\\\]build$"
          "[/\\\\]dist$"
          "[/\\\\]target$"
          "[/\\\\]bazel-.*$")
        lsp-file-watch-ignored-files
        '("[/\\\\]\\.pyc$"
          "[/\\\\]\\.#"
          "[/\\\\]#.*#$"
          "[/\\\\]\\.lock$"
          "[/\\\\]\\.o$"
          "[/\\\\]\\.a$"))
  (setq lsp-clients-python-server 'ruff)
  (add-to-list 'lsp-language-id-configuration '(python-mode . "python"))
  (add-to-list 'lsp-language-id-configuration '(c++-mode . "cpp"))
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

(provide 'init)
;;; init.el ends here
