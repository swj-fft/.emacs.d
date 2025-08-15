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

(delete-selection-mode 1)

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

;; Performance tweaks
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))


(provide 'consts)
