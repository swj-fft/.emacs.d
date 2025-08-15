;;; -*- lexical-binding: t -*-

;; 配置 Google C++ 样式
(defun apply-google-c++-style ()
  "Apply the Google C++ style in c-mode or c++-mode."
  (when (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
    (c-set-style "google")
    (setq c-basic-offset 4        ; Match .clang-format IndentWidth: 4
          fill-column 120)))      ; Match .clang-format ColumnLimit: 120

(add-hook 'c-mode-hook #'apply-google-c++-style)
(add-hook 'c++-mode-hook #'apply-google-c++-style)

;; 通用格式化函数
(defun format-buffer ()
  "Format the current buffer based on its major mode."
  (interactive)
  (cond
   ;; Bazel 文件
   ((or (eq major-mode 'bazel-build-mode)
        (eq major-mode 'bazel-workspace-mode)
        (eq major-mode 'bazel-starlark-mode))
    (bazel-format-buffer))
   ;; Python 文件
   ((eq major-mode 'python-mode)
    (shell-command-on-region
     (point-min) (point-max)
     "black --quiet -"
     (current-buffer) t))
   ;; C/C++ 文件
   ((or (eq major-mode 'c-mode)
        (eq major-mode 'c++-mode))
    (shell-command-on-region
     (point-min) (point-max)
     "clang-format -style=file"
     (current-buffer) t))
   ;; JavaScript/TypeScript/HTML/CSS
   ((or (eq major-mode 'js-mode)
        (eq major-mode 'js2-mode)
        (eq major-mode 'typescript-mode)
        (eq major-mode 'web-mode))
    (shell-command-on-region
     (point-min) (point-max)
     "prettier --stdin-filepath buffer"
     (current-buffer) t))
   ;; 默认：缩进整个缓冲区
   (t
    (indent-region (point-min) (point-max))
    (message "No specific formatter for %s, applied indent-region" major-mode))))

;; 绑定 S-M-f 到全局格式化
(global-set-key (kbd "C-c f") #'format-buffer)

(provide 'format-conf)
