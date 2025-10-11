;; ====================================
;; CRÍTICO: Cargar straight y Org ANTES de config.org
;; ====================================
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Cargar Org ANTES de org-babel-load-file
(straight-use-package 'org)

;; ====================================
;; Tu código original continúa aquí
;; ====================================
(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))
;;Config
(load "~/.emacs.d.backup/config/themes.el")
(load "~/.emacs.d.backup/config/evil_set.el")
(load "~/.emacs.d.backup/config/indentacion.el")
(custom-set-variables
 '(c-ts-mode-indent-offset 4)
 '(inhibit-startup-screen t)
 '(warning-suppress-types '((use-package) (emacs))))  ; Solo agregué (emacs) aquí
(custom-set-faces)
