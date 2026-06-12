# =============================================================
# verify_setup.R
# Script de verificación funcional del entorno — Fase 1
# Ejecutar antes de iniciar cualquier desarrollo
# =============================================================

cat('Verificando entorno de desarrollo...\n\n')

# 1. Versión de R
stopifnot('R >= 4.2.0 requerido' = getRversion() >= '4.2.0')
cat('✔ R version:', as.character(getRversion()), '\n')

# 2. Paquetes críticos
pkgs <- c('renv', 'here', 'tidyverse', 'quantmod', 'tidyquant',
          'xts', 'zoo', 'forecast', 'tseries', 'rugarch',
          'FinTS', 'strucchange', 'shiny', 'bslib', 'plotly',
          'DT', 'testthat', 'rmarkdown')

for (pkg in pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste('Paquete no encontrado:', pkg))
  }
  cat('✔ Paquete OK:', pkg, '\n')
}

# 3. Estructura de carpetas
dirs_required <- c('data/raw', 'data/processed', 'R/functions/boxjenkins',
                   'R/scripts', 'outputs/tables', 'outputs/models',
                   'app/modules', 'notebooks', 'tests')

for (d in dirs_required) {
  if (!dir.exists(d)) stop(paste('Carpeta no encontrada:', d))
  cat('✔ Carpeta OK:', d, '\n')
}

# 4. Git
git_path <- Sys.which('git')
stopifnot('Git no encontrado en PATH' = nchar(git_path) > 0)
cat('✔ Git encontrado en:', git_path, '\n')

# 5. Archivo de configuración
stopifnot('00_config.R no encontrado' = file.exists('R/scripts/00_config.R'))
cat('✔ 00_config.R presente\n')

cat('\n================================================\n')
cat('✔ Entorno verificado correctamente. Fase 1 completada.\n')
cat('================================================\n')
