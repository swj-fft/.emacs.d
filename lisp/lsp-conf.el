;;; -*- lexical-binding: t -*-

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

(provide 'lsp-conf)
