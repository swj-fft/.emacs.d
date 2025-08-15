;;; -*- lexical-binding: t -*-

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

(require 'lsp-conf)

(provide 'completion-conf)
