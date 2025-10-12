#!/bin/bash

# =============================================================================
# Script estilo "doom sync" para Emacs con straight.el
# Sincroniza paquetes y compila nativamente sin tocar init.el
# =============================================================================

set -e  # Salir si hay errores

# --- Configuración ---
EMACS_SANDBOX_DIR="/home/ianedmosz/.emacs.d.backup"
EMACS_EXEC="emacs"
COMP_JOBS=$(nproc)
CONFIG_ORG="$EMACS_SANDBOX_DIR/config.org"
INIT_FILE="$EMACS_SANDBOX_DIR/init.el"

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
NC='\033[0m' # No Color

# --- Funciones auxiliares ---
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_sync() {
    echo -e "${MAGENTA}[SYNC]${NC} $1"
}

# Banner estilo doom
echo -e "${MAGENTA}"
cat << "EOF"
╔═══════════════════════════════════════╗
║     EMACS VANILLA SYNC v1.0           ║
║  Sincronización y Compilación Nativa  ║
╚═══════════════════════════════════════╝
EOF
echo -e "${NC}"

# --- Validaciones previas ---
if [ ! -d "$EMACS_SANDBOX_DIR" ]; then
    log_error "El directorio $EMACS_SANDBOX_DIR no existe"
    exit 1
fi

if [ ! -f "$INIT_FILE" ]; then
    log_error "No se encontró init.el en $EMACS_SANDBOX_DIR"
    log_error "Crea un init.el que cargue tu config.org"
    exit 1
fi

# --- Código Elisp para Sync y Compilación (SIN TANGLE) ---
read -r -d '' EMACS_SYNC_EVAL <<'EOF' || true
(let ((user-emacs-directory (expand-file-name (getenv "EMACS_DIR")))
      (native-jobs (string-to-number (getenv "COMP_JOBS"))))
  
  (message "")
  (message "════════════════════════════════════════")
  (message "  Iniciando Sync y Compilación Nativa  ")
  (message "════════════════════════════════════════")
  (message "Directorio: %s" user-emacs-directory)
  (message "CPU cores: %d" native-jobs)
  (message "")
  
  ;; Configurar compilación nativa
  (when (and (fboundp 'native-comp-available-p)
             (native-comp-available-p))
    (setq native-comp-async-jobs-number native-jobs)
    (setq native-comp-speed 2)
    (setq native-comp-async-report-warnings-errors nil)
    (message "✓ Compilación nativa habilitada con %d jobs" native-jobs))
  
  ;; ========================================================================
  ;; PASO 1: BOOTSTRAP STRAIGHT.EL
  ;; ========================================================================
  (message "")
  (message "► Paso 1: Bootstrapping straight.el...")
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 6))
    
    (unless (file-exists-p bootstrap-file)
      (message "  Instalando straight.el por primera vez...")
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    
    (load bootstrap-file nil 'nomessage))
  
  (unless (fboundp 'straight-use-package)
    (message "✗ ERROR: straight-use-package no disponible")
    (kill-emacs 1))
  
  (message "✓ Straight.el bootstrapped correctamente")
  
  ;; ========================================================================
  ;; PASO 2: CARGAR CONFIGURACIÓN (init.el carga config.org internamente)
  ;; ========================================================================
  (message "")
  (message "► Paso 2: Cargando configuración...")
  
  ;; Cargar org para poder procesar config.org
  (straight-use-package 'org)
  (require 'org)
  (setq org-confirm-babel-evaluate nil)
  
  ;; Cargar config.org directamente (como lo haría init.el)
  (let ((config-org (expand-file-name "config.org" user-emacs-directory)))
    (if (file-exists-p config-org)
        (condition-case err
            (progn
              (message "  Procesando config.org...")
              (org-babel-load-file config-org)
              (message "✓ Configuración cargada desde config.org"))
          (error 
           (message "✗ ERROR al cargar config.org: %s" (error-message-string err))
           (kill-emacs 1)))
      (message "✗ config.org no encontrado")
      (kill-emacs 1)))
  
  ;; ========================================================================
  ;; PASO 3: SINCRONIZAR PAQUETES
  ;; ========================================================================
  (message "")
  (message "► Paso 3: Sincronizando paquetes...")
  (condition-case err
      (progn
        ;; Actualizar repositorios
        (when (fboundp 'straight-pull-all)
          (message "  Actualizando repositorios...")
          (straight-pull-all))
        
        ;; Reconstruir paquetes modificados
        (when (fboundp 'straight-rebuild-all)
          (message "  Reconstruyendo paquetes...")
          (straight-rebuild-all))
        
        (message "✓ Sincronización completada"))
    (error 
     (message "⚠ Advertencia durante sync: %s" (error-message-string err))))
  
  ;; ========================================================================
  ;; PASO 4: COMPILACIÓN NATIVA AGRESIVA
  ;; ========================================================================
  (when (and (fboundp 'native-comp-available-p)
             (native-comp-available-p))
    (message "")
    (message "► Paso 4: Compilación nativa agresiva...")
    
    ;; Compilar todos los paquetes de straight
    (let ((straight-build-dir (expand-file-name "straight/build" user-emacs-directory)))
      (when (file-directory-p straight-build-dir)
        (message "  Compilando %d paquetes..." 
                 (length (directory-files straight-build-dir nil "^[^.]")))
        (native-compile-async straight-build-dir 'recursively)))
    
    ;; Compilar config.org a .elc
    (let ((config-org (expand-file-name "config.org" user-emacs-directory))
          (config-el (expand-file-name "config.el" user-emacs-directory)))
      (when (file-exists-p config-el)
        (message "  Compilando config.el...")
        (native-compile-async config-el)))
    
    ;; Compilar otros archivos .el en el directorio
    (dolist (file (directory-files user-emacs-directory t "\\.el$"))
      (let ((basename (file-name-nondirectory file)))
        (unless (member basename '("init.el" "config.el" "early-init.el"))
          (message "  Compilando %s..." basename)
          (native-compile-async file))))
    
    ;; Limpiar compilaciones antiguas
    (when (fboundp 'native-compile-prune-cache)
      (message "  Limpiando cache antigua...")
      (native-compile-prune-cache))
    
    (message "✓ Compilación nativa iniciada en background"))
  
  ;; ========================================================================
  ;; RESUMEN FINAL
  ;; ========================================================================
  (message "")
  (message "════════════════════════════════════════")
  (message "  ✓ Sync completado exitosamente")
  (message "════════════════════════════════════════")
  (message "")
  (message "La compilación nativa continúa en background.")
  (message "Monitorea el progreso en *Async-native-compile-log*")
  (message "")
  (kill-emacs 0))
EOF

# --- Ejecución ---
log_sync "Iniciando sincronización..."
log_info "Directorio: $EMACS_SANDBOX_DIR"
log_info "CPU cores: $COMP_JOBS"
log_warning "Esto puede tomar varios minutos..."
echo ""

# Exportar variables de entorno
export EMACS_DIR="$EMACS_SANDBOX_DIR"
export COMP_JOBS="$COMP_JOBS"

# Ejecutar Emacs en modo batch
if "$EMACS_EXEC" --batch --eval "$EMACS_SYNC_EVAL" 2>&1 | tee /tmp/emacs-sync.log; then
    echo ""
    log_success "════════════════════════════════════════"
    log_success "  Sync completado correctamente"
    log_success "════════════════════════════════════════"
    echo ""
    log_info "Compilación nativa activa en background"
    log_info "Logs: /tmp/emacs-sync.log"
    echo ""
else
    echo ""
    log_error "════════════════════════════════════════"
    log_error "  El sync falló"
    log_error "════════════════════════════════════════"
    log_error "Revisa los logs: cat /tmp/emacs-sync.log"
    exit 1
fi

