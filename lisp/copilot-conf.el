;;; -*- lexical-binding: t -*-

;; GitHub Copilot
(use-package copilot
  :ensure t
  :hook (prog-mode . copilot-mode) ; 在编程模式下自动启用 copilot-mode
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion) ; 接受补全建议
              ("C-g" . copilot-clear-overlay)      ; 取消补全
              ("M-n" . copilot-next-completion)    ; 下一个补全建议
              ("M-p" . copilot-previous-completion) ; 上一个补全建议
              ("C-c C-k" . copilot-complete))      ; 手动触发补全
  :config
  (setq copilot-node-executable "/usr/bin/node") ; 设置 Node.js 可执行文件路径
  (setq copilot-max-char -1) ; 禁用字符限制（默认 -1 表示无限制）
  (setq copilot-auto-complete t)) ; 启用自动补全

(provide 'copilot-conf)
