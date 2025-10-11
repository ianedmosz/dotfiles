(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 18))
;; Set the background transparency for the currently selected frame
;;(set-frame-parameter nil 'alpha-background 95)

;; Set the background transparency for all new frames that are created
;;(add-to-list 'default-frame-alist '(alpha-background . 95))

(require 'nerd-icons)

(add-hook 'java-mode-hook (lambda () (setq lsp-headerline-breadcrumb-enable nil)))
(add-hook 'python-mode-hook (lambda () (setq lsp-headerline-breadcrumb-enable nil)))

(add-hook 'rust-mode-hook #'tree-sitter-mode)

(add-hook 'C-mode-hook #'tree-sitter-mode)

(add-hook 'python-mode-hook #'tree-sitter-mode)

(add-hook 'java-mode-hook #'tree-sitter-mode)

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(setq major-mode-remap-alist
      (append
       '((c-mode . c-ts-mode)
         (c++-mode . c++-ts-mode)
         (java-mode . java-ts-mode)
         (rust-mode . rust-ts-mode)
         (python-mode . python-ts-mode))
       major-mode-remap-alist))

;;(setq lsp-semantic-tokens-enable t)
;;(after! lsp-mode
  ;;(setq lsp-semantic-tokens-enable t)
  ;;(setq lsp-enable-semantic-highlighting t))



(global-flycheck-mode +1)

(after! lsp-mode
  (setq lsp-semantic-tokens-enable nil
    ;;    lsp-enable-semantic-highlighting t
        lsp-auto-guess-root t
        lsp-headerline-breadcrumb-enable nil
        lsp-enable-snippet t
        lsp-modeline-diagnostics-enable t
        lsp-signature-auto-activate t
        lsp-signature-render-documentation t))



(use-package lsp-pyright
    :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

(let ((npm-global-bin (expand-file-name "~/.npm-global/bin")))
  (setenv "PATH" (concat npm-global-bin ":" (getenv "PATH")))
  (add-to-list 'exec-path npm-global-bin))

(after! lsp-mode
  (setq lsp-auto-guess-root t
        lsp-headerline-breadcrumb-enable t
        lsp-enable-snippet t))

(dolist (mode '(python-mode python-ts-mode java-mode rust-mode))
  (add-hook (intern (concat (symbol-name mode) "-hook")) #'lsp-deferred))


(dolist (mode '(c-mode c++-mode c-ts-mode c++-ts-mode))
  (add-hook (intern (concat (symbol-name mode) "-hook")) #'lsp-deferred))

(after! lsp-mode
  (setq lsp-enable-on-type-formatting nil
        lsp-enable-indentation nil))

(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-show-with-mouse t)

(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable nil))

  (setq flycheck-display-errors-delay 0.1
        flycheck-indication-mode 'left-fringe
        flycheck-highlighting-mode 'lines
        flycheck-global-modes '(not org-mode text-mode)
        flycheck-check-syntax-automatically '(save mode-enabled))

(add-hook 'after-init-hook #'global-flycheck-mode)


(setq flycheck-check-syntax-automatically '(save mode-enabled idle-change new-line)
      flycheck-idle-change-delay 0.5
      flycheck-display-errors-delay 0.1)

(setq display-line-numbers-type 'relative)
(setq org-directory "~/org/")

  (doom-themes-treemacs-config)
(doom-themes-org-config)

;; Optional: Set the flavor (e.g., 'latte, 'frappe, 'macchiato, 'mocha)
(setq catppuccin-flavor 'mocha)

(setq doom-theme 'catppuccin)

(add-hook 'org-mode-hook #'(lambda () (display-line-numbers-mode 0)))

  (use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :config
  (setq
    org-auto-align-tags nil
    org-tags-column 0
    org-catch-invisible-edits 'show-and-error
    org-special-ctrl-a/e t
    org-insert-heading-respect-content t
    org-hide-emphasis-markers t
    org-pretty-entities t
    org-agenda-tags-column 0
    org-ellipsis "…"
    org-modern-star '("●" "○" "◉" "◇" "◆"))
  )


  (set-face-attribute 'default nil
                      :family "JetBrains Mono"
                      :height 120)

  (with-eval-after-load 'org
    (global-org-modern-mode))


(global-org-modern-mode)

(add-hook 'after-init-hook 'global-company-mode)
(setq company-backends '(company-files))

(setq company-backends '((company-capf :with company-files)))

(set-company-backend! 'org-mode '(company-files company-capf))
