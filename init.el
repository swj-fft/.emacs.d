;;; -*- lexical-binding: t -*-
;; init.el --- Emacs configuration entry point

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'consts)
(require 'keybindings)
(require 'packages)

;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file :noerror))

(provide 'init)

