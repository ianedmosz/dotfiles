#!/bin/bash

# --- Configuration ---
EMACS_SANDBOX_DIR="/home/ianedmosz/.emacs.d.backup"
EMACS_EXEC="emacs" 

# --- Execution ---
echo "Starting Emacs with $EMACS_SANDBOX_DIR..."
# The --init-directory flag is the essential part.
exec "$EMACS_EXEC" --init-directory "$EMACS_SANDBOX_DIR" "$@"
