;;Show at anything
(defun my/show-thing-at-point ()
  "Show info about symbol at point depending on LSP backend, manual only."
  (interactive)
  (cond
   ((bound-and-true-p eglot--managed-mode)
    (eldoc-box-help-at-point))
   ((bound-and-true-p lsp-mode)
    (lsp-ui-doc-show))
   (t
    (message "No LSP client active"))))

(map! :n "K" #'my/show-thing-at-point)

;; Treeamacs

(defun my/treemacs-current-directory ()
  "Open Treemacs at the current buffer's directory."
  (interactive)
  (treemacs-select-window)
  (treemacs-add-project-to-workspace
   "Current Dir" (file-name-directory (buffer-file-name)))
  (treemacs-refresh))

(map! :leader
      :desc "Open Treemacs at current file directory"
      "e e" #'my/treemacs-current-directory)



;; Vterm
(map! :leader
      :desc "Open vterm"
      "t t" #'vterm)



;; Normal: indent línea o bloque
(map! :n "<leader>i" #'evil-indent)

;; Visual: indent selección
(map! :v "<leader>i" #'evil-indent)

(map! :leader
      "f n" (lambda ()
              (interactive)
              (find-file (expand-file-name org-directory))))
