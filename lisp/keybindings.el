;;; keybindings.el --- Custom keybindings for Emacs -*- lexical-binding: t -*-

;; General keybindings
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x C-n") 'set-mark-command)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-a") 'align-regexp)

;; Custom commands
(defun prev-insert-new-line ()
  "Insert a newline before the current line and move point there."
  (interactive)
  (beginning-of-line)
  (open-line 1))

(defun back-insert-new-line ()
  "Insert a newline after the current line without indenting."
  (interactive)
  (end-of-line)
  (insert "\n"))

(defun view-in-explorer ()
  "Open the current file's directory in the system file explorer."
  (interactive)
  (let ((dir (if (buffer-file-name)
                 (file-name-directory (buffer-file-name))
               default-directory)))
    (cond
     ((eq system-type 'gnu/linux) (call-process "xdg-open" nil 0 nil dir))
     ((eq system-type 'darwin) (call-process "open" nil 0 nil dir))
     ((eq system-type 'windows-nt) (call-process "explorer" nil 0 nil dir))
     (t (message "Unsupported system type")))))

(defun insert-one-tab ()
  "Insert a literal tab character at point."
  (interactive)
  (insert "\t"))

(defun replace-last-sexp ()
  "Evaluate the last s-expression and replace it with its result."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))

;; Keybindings for custom commands
(global-set-key (kbd "<C-S-return>") 'prev-insert-new-line)
(global-set-key (kbd "<C-return>") 'back-insert-new-line)
(global-set-key (kbd "C-x C-v") 'view-in-explorer)
(global-set-key (kbd "<C-S-tab>") 'insert-one-tab)
(global-set-key (kbd "<C-backspace>") 'replace-last-sexp)

;; GUD key prefix
(setq gud-key-prefix (kbd "C-x C-g"))

(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(defun my/smart-switch-buffer ()
  "If Projectile is loaded and current buffer is in a Projectile project, use consult-project-buffer;
otherwise, use switch-to-buffer."
  (interactive)
  (if (and (featurep 'projectile)  ; 检查 projectile 是否已加载
           (projectile-project-p)
           (buffer-file-name))
      (consult-project-buffer)
    (call-interactively 'switch-to-buffer)))

;; 绑定 C-x C-b 到自定义函数
(global-set-key (kbd "C-x C-b") 'my/smart-switch-buffer)

;; eshell
(global-set-key (kbd "C-'") 'eshell)

(provide 'keybindings)

