;;; config/tree.el -*- lexical-binding: t; -*-

(setq treesit-language-source-alist
      '((c       . ("https://github.com/tree-sitter/tree-sitter-c" "master"))
        (cpp     . ("https://github.com/tree-sitter/tree-sitter-cpp" "master"))
        (java    . ("https://github.com/tree-sitter/tree-sitter-java" "master"))
        (python  . ("https://github.com/tree-sitter/tree-sitter-python" "master"))
        (rust    . ("https://github.com/tree-sitter/tree-sitter-rust" "master"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master"))))



(use-package! tree-sitter
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))
