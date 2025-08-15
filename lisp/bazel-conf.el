;;; -*- lexical-binding: t -*-

(use-package bazel
  :ensure t
  :mode (("\\BUILD\\'" . bazel-build-mode)
         ("\\WORKSPACE\\'" . bazel-workspace-mode)
         ("\\.bazelrc\\'" . bazelrc-mode)
         ("\\.bzl\\'" . bazel-starlark-mode))
  :config
  ;; 启用 buildifier 格式化（需要安装 buildifier）
  (defun bazel-format-buffer ()
    "Format the current Bazel buffer using buildifier."
    (interactive)
    (when (or (eq major-mode 'bazel-build-mode)
              (eq major-mode 'bazel-workspace-mode)
              (eq major-mode 'bazel-starlark-mode))
      (shell-command-on-region
       (point-min) (point-max)
       "buildifier --type=auto"
       (current-buffer) t)))
  :hook
  ((bazel-build-mode bazel-workspace-mode bazel-starlark-mode) .
   (lambda ()
     (add-hook 'before-save-hook #'bazel-format-buffer nil t)))
  :bind (:map bazel-mode-map
              ("C-c C-f" . bazel-format-buffer)))

(provide 'bazel-conf)
