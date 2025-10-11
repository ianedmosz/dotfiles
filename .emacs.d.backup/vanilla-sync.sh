#!/bin/bash

# --- Configuration ---
EMACS_SANDBOX_DIR="/home/ianedmosz/.emacs.d.backup"
EMACS_EXEC="emacs" 

# Set compilation jobs to the total number of CPU cores for maximum speed
COMP_JOBS=$(nproc) 

# --- Elisp Code for Full Sync and Aggressive Native Compilation ---
EMACS_SYNC_EVAL="(progn
    ;; Set the directory for package loading
    (setq user-emacs-directory \"$EMACS_SANDBOX_DIR/\")
    (setq native-comp-async-jobs-number $COMP_JOBS)
    
    ;; 1. Load Straight.el's bootstrap and main file
    (let ((bootstrap-file (expand-file-name \"straight/repos/straight.el/bootstrap.el\" user-emacs-directory)))
      (load-file bootstrap-file))
    (load (expand-file-name \"straight/repos/straight.el/straight.el\" user-emacs-directory))

    ;; 2. Load Org and tangle configuration
    (require 'org) 
    (message \"Tangling and loading config.org to initialize Straight.el requests...\")
    
    (setq org-babel-default-directory user-emacs-directory)
    (setq org-confirm-babel-evaluate nil) ; Prevent prompts
    
    (org-babel-tangle-file \"config.org\" (expand-file-name \"init.el\" user-emacs-directory) 'nil) 
    
    ;; *** DEFINITIVE FIX: Wrap tangled content for safe loading (resolves 'End of file during parsing') ***
    (message \"Loading corrected configuration content...\")
    (let ((init-path (expand-file-name \"init.el\" user-emacs-directory)))
      ;; Wrap content in a lambda for safe evaluation
      (with-temp-buffer
        (insert \"(lambda () \") 
        (insert-file-contents init-path)
        (insert \")\\n\")       
        (eval-buffer)))
    
    ;; 3. THE SYNC FIX: Force package processing
    (message \"Starting package installation/sync...\")
    (package-install-selected-packages t)
    
    ;; 4. Aggressively initiate native compilation for the entire config
    (message \"Starting aggressive native compilation on %d cores...\" native-comp-async-jobs-number)
    (native-compile-async user-emacs-directory 'recursively)
    
    ;; 5. Clean up old compiled files (Conditional call to fix 'void-function' error)
    (when (fboundp 'native-comp-cleanup)
      (native-comp-cleanup))
    
    (kill-emacs))"

# --- Execution (remains the same) ---

echo "Starting FULL Sync and AGGRESSIVE Native Compilation of $EMACS_SANDBOX_DIR..."
echo "Using $COMP_JOBS CPU cores (System load will be high)..."

# Run Emacs in batch mode to execute the sync/compilation
"$EMACS_EXEC" --batch --eval "$EMACS_SYNC_EVAL" 

if [ $? -eq 0 ]; then
    echo "Full Sync and Compilation initiated successfully."
    echo "The heavy work is now happening in the background. Check *Async-native-compile-log* when running emacs-vanilla."
else
    echo "Sync/Compilation failed. Check logs."
fi
