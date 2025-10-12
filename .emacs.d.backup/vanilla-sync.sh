#!/bin/bash

# =============================================================================
# Script de Sync y Compilación Nativa Agresiva para Emacs
# Similar a doom sync pero para configuraciones con straight.el
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

# --- Validaciones previas ---
if [ ! -d "$EMACS_SANDBOX_DIR" ]; then
    log_error "El directorio $EMACS_SANDBOX_DIR no existe"
    exit 1
fi

if [ ! -f "$CONFIG_ORG" ]; then
    log_warning "No se encontró config.org en $EMACS_SANDBOX_DIR"
fi

# --- Código Elisp para Sync y Compilación ---
read -r -d '' EMACS_SYNC_EVAL <<'EOF' || true
(let ((user-emacs-directory (expand-file-name (getenv "EMACS_DIR")))
      (native-jobs (string-to-number (getenv "COMP_JOBS")))
      (config-org (expand-file-name "config.org" (getenv "EMACS_DIR")))
      (init-file (expand-file-name "init.el" (getenv "EMACS_DIR"))))
  
  (message "=== Iniciando Sync y Compilación Nativa ===")
  (message "Directorio: %s" user-emacs-directory)
  (message "CPU cores: %d" native-jobs)
  
  ;; Configurar compilación nativa
  (when (and (fboundp 'native-comp-available-p)
             (native-comp-available-p))
    (setq native-comp-async-jobs-number native-jobs)
    (setq native-comp-speed 2)
    (setq native-comp-async-report-warnings-errors nil)
    (message "Compilación nativa habilitada con %d jobs" native-jobs))
  
  ;; Verificar que straight.el esté disponible
  (let ((bootstrap-file 
         (expand-file-name "straight/repos/straight.el/bootstrap.el" 
                          user-emacs-directory)))
    (unless (file-exists-p bootstrap-file)
      (message "ERROR: No se encontró straight.el bootstrap")
      (kill-emacs 1))
    
    ;; Cargar straight.el
    (load bootstrap-file nil 'nomessage)
    (message "Straight.el cargado correctamente"))
  
  ;; Cargar straight.el principal
  (require 'straight)
  
  ;; Si existe config.org, procesarlo
  (when (file-exists-p config-org)
    (message "Procesando config.org...")
    (require 'org)
    (setq org-confirm-babel-evaluate nil)
    
    ;; Tangle config.org a init.el
    (condition-case err
        (progn
          (org-babel-tangle-file config-org init-file)
          (message "Config.org tangled exitosamente"))
      (error 
       (message "Error al hacer tangle: %s" (error-message-string err))
       (kill-emacs 1))))
  
  ;; Cargar init.el de forma segura
  (when (file-exists-p init-file)
    (message "Cargando configuración desde init.el...")
    (condition-case err
        (load init-file nil 'nomessage)
      (error 
       (message "Error al cargar init.el: %s" (error-message-string err))
       (kill-emacs 1))))
  
  ;; Ejecutar straight-check-all para sincronizar paquetes
  (message "Sincronizando paquetes con straight.el...")
  (condition-case err
      (progn
        ;; Freezar el estado actual de los paquetes
        (when (fboundp 'straight-freeze-versions)
          (straight-freeze-versions))
        
        ;; Actualizar repositorios si es necesario
        (when (fboundp 'straight-pull-all)
          (straight-pull-all))
        
        (message "Sincronización de paquetes completada"))
    (error 
     (message "Advertencia durante sync: %s" (error-message-string err))))
  
  ;; Compilación nativa agresiva
  (when (and (fboundp 'native-comp-available-p)
             (native-comp-available-p))
    (message "Iniciando compilación nativa agresiva...")
    
    ;; Compilar directorio de straight
    (let ((straight-dir (expand-file-name "straight/build" user-emacs-directory)))
      (when (file-directory-p straight-dir)
        (message "Compilando paquetes de straight...")
        (native-compile-async straight-dir 'recursively)))
    
    ;; Compilar configuración principal
    (when (file-exists-p init-file)
      (message "Compilando init.el...")
      (native-compile-async init-file))
    
    ;; Limpiar archivos antiguos
    (when (fboundp 'native-compile-prune-cache)
      (message "Limpiando cache antigua...")
      (native-compile-prune-cache))
    
    (message "Compilación nativa iniciada en background"))
  
  (message "=== Proceso completado exitosamente ===")
  (kill-emacs 0))
EOF

# --- Ejecución ---
log_info "Iniciando SYNC completo y compilación nativa agresiva"
log_info "Directorio: $EMACS_SANDBOX_DIR"
log_info "Usando $COMP_JOBS cores de CPU (la carga del sistema será alta)"
echo ""

# Exportar variables de entorno para el código elisp
export EMACS_DIR="$EMACS_SANDBOX_DIR"
export COMP_JOBS="$COMP_JOBS"

# Ejecutar Emacs en modo batch
if "$EMACS_EXEC" --batch --eval "$EMACS_SYNC_EVAL" 2>&1 | tee /tmp/emacs-sync.log; then
    echo ""
    log_success "Sync y compilación iniciados exitosamente"
    log_info "La compilación nativa continúa en background"
    log_info "Puedes monitorear el progreso en *Async-native-compile-log*"
    echo ""
    log_info "Para ver los logs completos: cat /tmp/emacs-sync.log"
else
    echo ""
    log_error "El proceso falló. Revisa los logs en /tmp/emacs-sync.log"
    exit 1
fi
