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

(defun run-file ()
  "Run the current file based on its major mode."
  (interactive)
  (cond
   ((eq major-mode 'python-mode)
    (save-buffer)
    (shell-command (concat "python3 " (shell-quote-argument (buffer-file-name)))))
   ((eq major-mode 'c-mode)
    (save-buffer)
    (let ((output (file-name-sans-extension (buffer-file-name))))
      (shell-command (concat "gcc " (buffer-file-name) " -o " output " && ./" output))))
   ((eq major-mode 'c++-mode)
    (save-buffer)
    (let ((output (file-name-sans-extension (buffer-file-name))))
      (shell-command (concat "g++ " (buffer-file-name) " -o " output " && ./" output))))
   (t (message "No run command defined for %s" major-mode))))

;; Keybindings for custom commands
(global-set-key (kbd "<C-S-return>") 'prev-insert-new-line)
(global-set-key (kbd "<C-return>") 'back-insert-new-line)
(global-set-key (kbd "C-x C-v") 'view-in-explorer)
(global-set-key (kbd "<C-S-tab>") 'insert-one-tab)
(global-set-key (kbd "<C-backspace>") 'replace-last-sexp)
(global-set-key (kbd "<f9>") 'run-file)

;; GUD key prefix
(setq gud-key-prefix (kbd "C-x C-g"))

(provide 'keybindings)
;;; keybindings.el ends here
