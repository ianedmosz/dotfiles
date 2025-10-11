(defun my-force-4-space-indent ()
  "Forces all C/C++ and Evil indentation settings to 4 spaces."
  
  ;; 1. CRITICAL EVIL SETTING: This forces Evil's shift operations to 4 spaces.
  (setq evil-shift-width 4) 
  
  ;; 2. Set the primary style to 'user' to stop auto-detection of styles
  (c-set-style "user")
  
  ;; 3. GUARANTEE 4-SPACE OFFSETS for all C/C++ mode variables
  (setq c-basic-offset 4)
  (setq c-indent-level 4)
  
  ;; 4. TREE-SITTER FIX (if active)
  (setq c-ts-mode-indent-offset 4)
  (setq c++-ts-mode-indent-offset 4)

  ;; 5. VISUAL & TAB FIX
  (setq indent-tabs-mode nil) ; Use spaces
  (setq tab-width 4)          
  
  ;; 6. EVIL MODE TAB FIX: Rebind TAB in Normal/Motion State
  (define-key evil-motion-state-local-map (kbd "TAB") 'c-indent-command))

;; Run this fix on all C/C++ mode hooks with a high priority (99) to run LAST.
(add-hook 'c-mode-hook 'my-force-4-space-indent 99)
(add-hook 'c++-mode-hook 'my-force-4-space-indent 99)
(add-hook 'c-ts-mode-hook 'my-force-4-space-indent 99)
(add-hook 'c++-ts-mode-hook 'my-force-4-space-indent 99)
