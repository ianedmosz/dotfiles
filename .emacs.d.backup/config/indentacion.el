(defun my-c-mode-final-fix ()
  "Aggressively overrides RET binding to ensure newline-and-indent runs."
  (interactive)
  
  (when (derived-mode-p 'c-ts-mode 'c-mode 'c++-ts-mode 'c++-mode)
    
    ;; 1. Define the universal function (similar to Doom's smart RET)
    (defun my-universal-newline-and-indent ()
      (interactive)
      ;; Abort completion explicitly
      (when (fboundp 'company-abort)
        (company-abort))
      ;; Force the Emacs standard indented newline
      (newline-and-indent))

    ;; 2. Set the function in the local buffer keymap (Highest priority)
    (define-key (current-local-map) (kbd "RET") #'my-universal-newline-and-indent)
    
    ;; 3. Overwrite the binding in Evil's local insert map for safety
    (define-key evil-insert-state-local-map (kbd "RET") #'my-universal-newline-and-indent)))

;; Apply this fix to the Major Mode hooks
(add-hook 'c-mode-hook #'my-c-mode-final-fix)
(add-hook 'c++-mode-hook #'my-c-mode-final-fix)
(add-hook 'c-ts-mode-hook #'my-c-mode-final-fix)
(add-hook 'c++-ts-mode-hook #'my-c-mode-final-fix)

;; 4. Crucial: Override the Evil RET globally to avoid default insertion
(with-eval-after-load 'evil
  (define-key evil-insert-state-map (kbd "RET") nil))

;; 5. Patch the completion map as a final safeguard
(with-eval-after-load 'company
  (when (boundp 'company-active-map)
    (define-key company-active-map (kbd "RET") #'my-universal-newline-and-indent)))
