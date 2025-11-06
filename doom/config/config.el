(setq-default evil-shift-width 4)
(add-to-list 'default-frame-alist '(alpha-background . 95))
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 22))
;; Set the background transparency for the currently selected frame
;;(set-frame-parameter nil 'alpha-background 95)

;; Set the background transparency for all new frames that are created
;;(add-to-list 'default-frame-alist '(alpha-background . 95))

(setq package-quickstart t)

(setq use-package-always-defer t)

(require 'nerd-icons)

(add-hook 'java-mode-hook (lambda () (setq lsp-headerline-breadcrumb-enable nil)))
(add-hook 'python-mode-hook (lambda () (setq lsp-headerline-breadcrumb-enable nil)))

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

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
         (python-mode . python-ts-mode)
         (json-mode . json-ts-mode)
         (css-mode . css-ts-mode)
         (html-mode . html-ts-mode)
         (kdl-mode . kdl-ts-mode))
       major-mode-remap-alist))

(global-flycheck-mode +1)

;;(after! lsp-mode
  ;;(setq lsp-semantic-tokens-enable nil
    ;;    lsp-idle-delay 0.5
      ;;   lsp-completion-provider :cape
    ;;    lsp-enable-semantic-highlighting t
       ;; lsp-auto-guess-root t
       ;; lsp-headerline-breadcrumb-enable nil
       ;; lsp-enable-snippet t
       ;; lsp-modeline-diagnostics-enable t
       ;; lsp-signature-auto-activate t
       ;; lsp-signature-render-documentation t))

;;(use-package lsp-pyright
  ;;  :hook (python-mode . (lambda ()
    ;;                      (require 'lsp-pyright)
      ;;                    (lsp-deferred))))

;;(let ((npm-global-bin (expand-file-name "~/.npm-global/bin")))
  ;;(setenv "PATH" (concat npm-global-bin ":" (getenv "PATH")))
  ;;(add-to-list 'exec-path npm-global-bin))

;;(after! lsp-mode
  ;;(setq lsp-auto-guess-root t
    ;;    lsp-headerline-breadcrumb-enable t
      ;;  lsp-enable-snippet t))

;;(dolist (mode '(python-mode python-ts-mode java-mode java-ts-mode rust-mode))
  ;;(add-hook (intern (concat (symbol-name mode) "-hook")) #'lsp-deferred))


;;(dolist (mode '(c-mode c++-mode c-ts-mode c++-ts-mode))
  ;;(add-hook (intern (concat (symbol-name mode) "-hook")) #'lsp-deferred))

;;(after! lsp-mode
  ;;(setq lsp-enable-on-type-formatting nil
    ;;    lsp-enable-indentation nil))

;;(after! lsp-java
  ;;(dolist (hook '(java-mode-hook java-ts-mode-hook))
    ;;(add-hook hook #'lsp! 'append)))

(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-show-with-mouse t)

(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable nil))

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

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(setq doom-themes-treemacs-theme "doom-colors")

(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Minimum length of prefix for auto completion.
  (corfu-popupinfo-mode t)       ;; Enable popup information
  (corfu-popupinfo-delay 0)    ;; Lower popup info delay to 0.5 seconds from 2 seconds
  (corfu-separator ?\s)          ;; Orderless field separator, Use M-SPC to enter separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  (completion-ignore-case t)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  (corfu-preview-current nil) ;; Don't insert completion without confirmation
  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))


(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :after corfu
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.

  ;; The functions that are added later will be the first in the list
  (add-hook 'completion-at-point-functions #'cape-dabbrev) ;; Complete word from current buffers
  (add-hook 'completion-at-point-functions #'cape-dict) ;; Dictionary completion
  (add-hook 'completion-at-point-functions #'cape-file) ;; Path completion
  (add-hook 'completion-at-point-functions #'cape-elisp-block) ;; Complete elisp in Org or Markdown mode
  (add-hook 'completion-at-point-functions #'cape-keyword) ;; Keyword completion

  ;;(add-hook 'completion-at-point-functions #'cape-abbrev) ;; Complete abbreviation
  ;;(add-hook 'completion-at-point-functions #'cape-history) ;; Complete from Eshell, Comint or minibuffer history
  ;;(add-hook 'completion-at-point-functions #'cape-line) ;; Complete entire line from current buffer
  ;;(add-hook 'completion-at-point-functions #'cape-elisp-symbol) ;; Complete Elisp symbol
  ;;(add-hook 'completion-at-point-functions #'cape-tex) ;; Complete Unicode char from TeX command, e.g. \hbar
  ;;(add-hook 'completion-at-point-functions #'cape-sgml) ;; Complete Unicode char from SGML entity, e.g., &alpha
  ;;(add-hook 'completion-at-point-functions #'cape-rfc1345) ;; Complete Unicode char using RFC 1345 mnemonics
  )

(use-package toc-org
  :commands toc-org-enable
  :hook (org-mode . toc-org-mode))

(require 'project)

(defun my-project-try-build-files (dir)
  "Try to find a project root in DIR based on common build files."
  (let* ((build-files '("pom.xml" "build.gradle" "settings.gradle" "build.gradle.kts" "settings.gradle.kts"))
         (root-file (cl-find-if (lambda (file) (file-exists-p (expand-file-name file dir))) build-files)))
    (when root-file
      ;; Return the project definition: (type root-directory)
      ;; We use 'transient to avoid persisting it unnecessarily.
      (list 'transient dir))))

;; Prepend your custom function to project-find-functions
;; This ensures it runs *before* the default version control check (project-try-vc)
(add-to-list 'project-find-functions #'my-project-try-build-files 't)

(use-package eglot
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (java-mode . eglot-ensure)
         (java-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (lua-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-report-progress nil))

(use-package sideline-flymake
  :hook (flymake-mode . sideline-mode)
  :custom
  (sideline-flymake-display-mode 'line) ;; Show errors on the current line
  (sideline-backends-right '(sideline-flymake)))

(use-package nerd-icons
  :if (display-graphic-p))

(use-package nerd-icons-dired
  :hook (dired-mode . (lambda () (nerd-icons-dired-mode t))))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; ===== Global indentation defaults =====
(setq-default tab-width 4
              evil-shift-width 4
              indent-tabs-mode nil
              standard-indent 4)

;; ===== Language-specific indentation =====
(setq-default js-indent-level 4
              typescript-indent-level 4
              css-indent-offset 4
              sgml-basic-offset 4)

;; ===== C / C++ =====
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))

(defun my/c-ts-mode-setup ()
  (setq-local c-ts-mode-indent-offset 4
              tab-width 4
              evil-shift-width 4))
(add-hook 'c-ts-mode-hook #'my/c-ts-mode-setup)

(defun my/eglot-c-reapply-indent (&rest _)
  (when (derived-mode-p 'c-ts-mode)
    (setq-local c-ts-mode-indent-offset 4
                tab-width 4
                evil-shift-width 4)))
(advice-add 'eglot--maybe-activate-editing-mode :after #'my/eglot-c-reapply-indent)

;; ===== Python =====
(add-hook 'python-ts-mode-hook
          (lambda ()
            (setq-local python-indent-offset 4
                        tab-width 4
                        evil-shift-width 4)))

;; ===== Java =====
(add-hook 'java-ts-mode-hook
          (lambda ()
            (setq-local java-ts-mode-indent-offset 4
                        tab-width 4
                        evil-shift-width 4)))

;; ===== Eglot: prevent LSP from overriding formatting =====
(setq eglot-ignored-server-capabilities
      '(:documentFormattingProvider :documentRangeFormattingProvider))

(use-package eglot
  :custom
  (eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider
                                       :documentRangeFormattingProvider
                                       :documentFormattingProvider)))
