# ============================================================
# Preparar Dashboard Shiny EDA 911 CDMX - Pipeline automático
#
# Propósito:
# Preparar la carpeta dashboard_911/ para ejecución local o despliegue
# en shinyapps.io, copiando únicamente archivos agregados generados por
# el EDA:
#   - CSV de diagnóstico y tareas -> data/
#   - PNG de tareas -> www/
#   - app.R actualizado -> dashboard_911/app.R
#
# Este script NO copia ni carga los datasets originales de llamadas 911.
#
# Uso recomendado:
#   source("preparar_dashboard_shiny_911_auto.R")
#
# O uso explícito:
#   preparar_dashboard_shiny_911(
#     root = "D:/DISCO C/Antonio Toro/UIA-MAI/5. Lenguajes de ciencia de datos avanzado/Unidad4/App_R_tidyr_lubridate_dplyr_markdown"
#   )
# ============================================================

# ------------------------------------------------------------
# 1. Bitácora y utilidades generales
# ------------------------------------------------------------

#' Registrar evento del pipeline
#'
#' Imprime mensajes de bitácora con fecha, nivel y descripción.
#'
#' @param nivel character. Nivel del evento: INFO, WARN o ERROR.
#' @param mensaje character. Mensaje descriptivo del evento.
#' @return NULL de forma invisible.
log_event <- function(nivel, mensaje) {
  cat(sprintf("[%s] [%s] %s\n", Sys.time(), nivel, mensaje))
  invisible(NULL)
}

#' Obtener ruta del script actual
#'
#' Intenta detectar la ruta del archivo R cuando se ejecuta con source().
#' Si no puede detectarla, devuelve NA_character_.
#'
#' @return character. Ruta del script o NA_character_.
obtener_ruta_script <- function() {
  ruta <- tryCatch(
    {
      frames <- sys.frames()

      for (i in rev(seq_along(frames))) {
        if (!is.null(frames[[i]]$ofile)) {
          return(normalizePath(frames[[i]]$ofile, winslash = "/", mustWork = FALSE))
        }
      }

      NA_character_
    },
    error = function(e) {
      NA_character_
    }
  )

  ruta
}

#' Resolver carpeta raíz del proyecto
#'
#' Usa, en orden de prioridad:
#' 1) argumento root si se proporciona;
#' 2) carpeta donde se encuentra este script;
#' 3) directorio de trabajo actual.
#'
#' @param root character o NULL. Carpeta raíz del proyecto.
#' @return character. Ruta normalizada de la carpeta raíz.
resolver_root <- function(root = NULL) {
  if (!is.null(root) && !is.na(root) && nzchar(root)) {
    if (!dir.exists(root)) {
      stop("La carpeta root no existe: ", root)
    }

    return(normalizePath(root, winslash = "/", mustWork = TRUE))
  }

  ruta_script <- obtener_ruta_script()

  if (!is.na(ruta_script) && nzchar(ruta_script)) {
    root_detectado <- dirname(ruta_script)

    if (dir.exists(root_detectado)) {
      return(normalizePath(root_detectado, winslash = "/", mustWork = TRUE))
    }
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

#' Formatear lista de archivos faltantes
#'
#' Prepara una cadena legible con archivos faltantes.
#'
#' @param faltantes character vector. Archivos faltantes.
#' @return character. Texto formateado.
formatear_faltantes <- function(faltantes) {
  if (length(faltantes) == 0) {
    return("Ninguno")
  }

  paste(faltantes, collapse = "\n - ")
}

# ------------------------------------------------------------
# 2. Especificación de archivos requeridos
# ------------------------------------------------------------

#' Obtener especificación de archivos requeridos
#'
#' Define los CSV y PNG que la app Shiny debe tener disponibles.
#'
#' @return list con vectores csv y png.
obtener_archivos_requeridos_shiny <- function() {
  list(
    csv = c(
      "01_diagnostico_calidad_general.csv",
      "01_diagnostico_por_archivo.csv",
      "01_diagnostico_rango_fechas.csv",
      "02_tarea1_conteo_mes_alcaldia_categoria.csv",
      "02_tarea1_promedio_mensual_por_alcaldia_categoria.csv",
      "03_tarea2_dia_semana_maximo.csv",
      "03_tarea2_incidentes_por_dia_semana.csv",
      "04_tarea3_distribucion_hora_categoria.csv",
      "04_tarea3_hora_pico_por_categoria.csv",
      "05_tarea4_duraciones_negativas.csv",
      "05_tarea4_resumen_tiempo_cierre.csv",
      "06_tarea5_distribucion_categorias.csv",
      "06_tarea5_porcentaje_falsa_alarma.csv"
    ),
    png = c(
      "02_tarea1_promedio_mensual_alcaldia_categoria.png",
      "03_tarea2_incidentes_por_dia_semana.png",
      "04_tarea3_distribucion_horaria_categorias.png",
      "05_tarea4_histograma_duracion_minutos.png",
      "06_tarea5_porcentaje_por_clasificacion.png"
    )
  )
}

# ------------------------------------------------------------
# 3. Detección y validación de carpetas
# ------------------------------------------------------------

#' Detectar carpeta de salidas del EDA
#'
#' Busca de forma recursiva la carpeta salidas_911_eda_* más reciente
#' dentro de la carpeta raíz del proyecto.
#'
#' @param root character. Carpeta raíz del proyecto.
#' @return character. Ruta de la carpeta de salidas detectada.
detectar_carpeta_salidas <- function(root) {
  candidatas <- list.dirs(path = root, recursive = TRUE, full.names = TRUE)
  candidatas <- candidatas[grepl("^salidas_911_eda_", basename(candidatas))]

  if (length(candidatas) == 0) {
    stop(
      "No se encontró ninguna carpeta salidas_911_eda_* dentro de: ", root, "\n",
      "Ejecuta primero eda_911_cdmx_v4.R o indica carpeta_salidas manualmente."
    )
  }

  info <- file.info(candidatas)
  carpeta <- rownames(info)[which.max(info$mtime)]

  log_event("INFO", paste("Carpeta de salidas detectada:", carpeta))
  normalizePath(carpeta, winslash = "/", mustWork = TRUE)
}

#' Resolver carpeta de salidas
#'
#' Usa la carpeta indicada si existe; de lo contrario, intenta detectarla.
#'
#' @param root character. Carpeta raíz del proyecto.
#' @param carpeta_salidas character o NULL. Carpeta de salidas.
#' @return character. Ruta normalizada de carpeta de salidas.
resolver_carpeta_salidas <- function(root, carpeta_salidas = NULL) {
  if (!is.null(carpeta_salidas) && !is.na(carpeta_salidas) && nzchar(carpeta_salidas)) {
    ruta <- if (dirname(carpeta_salidas) == ".") {
      file.path(root, carpeta_salidas)
    } else {
      carpeta_salidas
    }

    if (!dir.exists(ruta)) {
      stop("La carpeta de salidas no existe: ", ruta)
    }

    return(normalizePath(ruta, winslash = "/", mustWork = TRUE))
  }

  detectar_carpeta_salidas(root)
}

#' Crear estructura del dashboard
#'
#' Crea dashboard_911/, data/ y www/. Puede limpiar data/ y www/ antes
#' de copiar archivos nuevos.
#'
#' @param root character. Carpeta raíz del proyecto.
#' @param carpeta_dashboard character. Carpeta destino.
#' @param limpiar logical. Si TRUE, limpia data/ y www/.
#' @return list con rutas dashboard_dir, data_dir y www_dir.
crear_estructura_dashboard <- function(root, carpeta_dashboard = "dashboard_911", limpiar = TRUE) {
  dashboard_dir <- if (dirname(carpeta_dashboard) == ".") {
    file.path(root, carpeta_dashboard)
  } else {
    carpeta_dashboard
  }

  dashboard_dir <- normalizePath(dashboard_dir, winslash = "/", mustWork = FALSE)

  data_dir <- file.path(dashboard_dir, "data")
  www_dir <- file.path(dashboard_dir, "www")

  dir.create(dashboard_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(www_dir, recursive = TRUE, showWarnings = FALSE)

  if (isTRUE(limpiar)) {
    for (archivo in list.files(data_dir, full.names = TRUE, all.files = FALSE)) {
      if (file.exists(archivo)) {
        unlink(archivo, recursive = TRUE, force = TRUE)
      }
    }

    for (archivo in list.files(www_dir, full.names = TRUE, all.files = FALSE)) {
      if (file.exists(archivo)) {
        unlink(archivo, recursive = TRUE, force = TRUE)
      }
    }

    log_event("INFO", "Carpetas data/ y www/ limpiadas.")
  }

  list(
    dashboard_dir = dashboard_dir,
    data_dir = data_dir,
    www_dir = www_dir
  )
}

# ------------------------------------------------------------
# 4. Copia de archivos
# ------------------------------------------------------------

#' Copiar un conjunto de archivos
#'
#' Copia archivos desde una carpeta origen hacia una carpeta destino.
#'
#' @param archivos character vector. Nombres de archivos a copiar.
#' @param origen_dir character. Carpeta origen.
#' @param destino_dir character. Carpeta destino.
#' @param tipo character. Etiqueta para bitácora.
#' @return character vector. Archivos faltantes.
copiar_archivos <- function(archivos, origen_dir, destino_dir, tipo = "archivo") {
  faltantes <- character(0)

  for (archivo in archivos) {
    origen <- file.path(origen_dir, archivo)
    destino <- file.path(destino_dir, archivo)

    if (!file.exists(origen)) {
      faltantes <- c(faltantes, archivo)
      log_event("WARN", paste("No se encontró", tipo, ":", origen))
    } else {
      ok <- file.copy(origen, destino, overwrite = TRUE)

      if (isTRUE(ok)) {
        log_event("INFO", paste("Copiado", tipo, ":", archivo))
      } else {
        faltantes <- c(faltantes, archivo)
        log_event("WARN", paste("No se pudo copiar", tipo, ":", archivo))
      }
    }
  }

  faltantes
}

#' Detectar app.R de origen
#'
#' Prioriza la app con diagnóstico y, si no existe, busca app.R en la raíz
#' o app.R dentro del dashboard.
#'
#' @param root character. Carpeta raíz del proyecto.
#' @param dashboard_dir character. Carpeta del dashboard.
#' @param app_origen character o NULL. Ruta explícita de app de origen.
#' @return character. Ruta del app.R a copiar.
detectar_app_origen <- function(root, dashboard_dir, app_origen = NULL) {
  candidatos <- character(0)

  if (!is.null(app_origen) && !is.na(app_origen) && nzchar(app_origen)) {
    candidatos <- c(candidatos, app_origen)
  }

  candidatos <- c(
    candidatos,
    file.path(root, "app_shiny_911_resumen_diagnostico.R"),
    file.path(root, "app_con_footer_911.R"),
    file.path(root, "app_shiny_911_footer_integrado.R"),
    file.path(root, "app.R"),
    file.path(dashboard_dir, "app.R")
  )

  for (candidato in candidatos) {
    if (!is.na(candidato) && nzchar(candidato) && file.exists(candidato)) {
      ruta <- normalizePath(candidato, winslash = "/", mustWork = TRUE)
      log_event("INFO", paste("app.R de origen detectado:", ruta))
      return(ruta)
    }
  }

  stop(
    "No se encontró app.R de origen. Coloca app_shiny_911_resumen_diagnostico.R ",
    "o app.R en la carpeta raíz del proyecto."
  )
}

#' Copiar app.R al dashboard
#'
#' Copia la app de origen como app.R dentro del dashboard. Si origen y destino
#' son el mismo archivo, no hace nada.
#'
#' @param app_origen character. Ruta del app de origen.
#' @param dashboard_dir character. Carpeta del dashboard.
#' @return character. Ruta destino de app.R.
copiar_app_dashboard <- function(app_origen, dashboard_dir) {
  destino <- file.path(dashboard_dir, "app.R")

  origen_norm <- normalizePath(app_origen, winslash = "/", mustWork = TRUE)
  destino_norm <- normalizePath(destino, winslash = "/", mustWork = FALSE)

  if (identical(origen_norm, destino_norm)) {
    log_event("INFO", "El app.R ya está en la carpeta del dashboard; no se copia.")
    return(destino_norm)
  }

  ok <- file.copy(origen_norm, destino, overwrite = TRUE)

  if (!isTRUE(ok)) {
    stop("No se pudo copiar app.R hacia: ", destino)
  }

  log_event("INFO", paste("app.R copiado a:", destino))
  normalizePath(destino, winslash = "/", mustWork = TRUE)
}

# ------------------------------------------------------------
# 5. Validación final y README
# ------------------------------------------------------------

#' Validar estructura final del dashboard
#'
#' Verifica que app.R, data/ y www/ existan, y reporta archivos faltantes.
#'
#' @param estructura list. Rutas del dashboard.
#' @param archivos list. CSV y PNG requeridos.
#' @return TRUE si la estructura mínima existe.
validar_estructura_final <- function(estructura, archivos) {
  app_path <- file.path(estructura$dashboard_dir, "app.R")

  if (!file.exists(app_path)) {
    stop("No existe app.R en el dashboard: ", app_path)
  }

  if (!dir.exists(estructura$data_dir)) {
    stop("No existe data/: ", estructura$data_dir)
  }

  if (!dir.exists(estructura$www_dir)) {
    stop("No existe www/: ", estructura$www_dir)
  }

  csv_faltantes <- archivos$csv[!file.exists(file.path(estructura$data_dir, archivos$csv))]
  png_faltantes <- archivos$png[!file.exists(file.path(estructura$www_dir, archivos$png))]

  if (length(csv_faltantes) > 0) {
    log_event("WARN", paste("CSV faltantes:\n -", formatear_faltantes(csv_faltantes)))
  }

  if (length(png_faltantes) > 0) {
    log_event("WARN", paste("PNG faltantes:\n -", formatear_faltantes(png_faltantes)))
  }

  if (length(csv_faltantes) == 0 && length(png_faltantes) == 0) {
    log_event("INFO", "Estructura final validada: todos los archivos requeridos están presentes.")
  }

  TRUE
}

#' Escribir README del dashboard
#'
#' Genera un README.md con instrucciones de ejecución y despliegue.
#'
#' @param dashboard_dir character. Carpeta del dashboard.
#' @return character. Ruta del README creado.
escribir_readme_dashboard <- function(dashboard_dir) {
  ruta <- file.path(dashboard_dir, "README.md")

  lineas <- c(
    "# Dashboard Shiny EDA 911 CDMX",
    "",
    "Este dashboard usa únicamente salidas agregadas del pipeline EDA.",
    "",
    "## Estructura",
    "",
    "```text",
    "dashboard_911/",
    "├── app.R",
    "├── data/   # CSV de diagnóstico y tareas 1 a 5",
    "└── www/    # PNG de tareas 1 a 5",
    "```",
    "",
    "## Ejecución local",
    "",
    "```r",
    "shiny::runApp('dashboard_911')",
    "```",
    "",
    "## Despliegue",
    "",
    "```r",
    "rsconnect::deployApp('dashboard_911')",
    "```",
    "",
    "No se requieren datasets originales dentro del dashboard."
  )

  writeLines(lineas, ruta, useBytes = TRUE)
  log_event("INFO", paste("README creado:", ruta))
  ruta
}

# ------------------------------------------------------------
# 6. Pipeline principal
# ------------------------------------------------------------

#' Preparar dashboard Shiny 911
#'
#' Construye una carpeta lista para ejecutar o desplegar en Shiny.
#'
#' @param root character o NULL. Carpeta raíz del proyecto.
#' @param carpeta_salidas character o NULL. Carpeta salidas_911_eda_*.
#' @param carpeta_dashboard character. Carpeta destino del dashboard.
#' @param app_origen character o NULL. Ruta del app.R a copiar.
#' @param limpiar logical. Si TRUE, limpia data/ y www/.
#' @return character. Ruta del dashboard preparado.
preparar_dashboard_shiny_911 <- function(
  root = NULL,
  carpeta_salidas = NULL,
  carpeta_dashboard = "dashboard_911",
  app_origen = NULL,
  limpiar = TRUE
) {
  tryCatch(
    {
      cat("\n============================================================\n")
      cat("PREPARACION AUTOMATICA DASHBOARD SHINY 911 CDMX\n")
      cat("============================================================\n")

      root <- resolver_root(root)
      log_event("INFO", paste("Root del proyecto:", root))

      archivos <- obtener_archivos_requeridos_shiny()
      carpeta_salidas <- resolver_carpeta_salidas(root, carpeta_salidas)
      estructura <- crear_estructura_dashboard(root, carpeta_dashboard, limpiar = limpiar)

      faltantes_csv <- copiar_archivos(
        archivos = archivos$csv,
        origen_dir = carpeta_salidas,
        destino_dir = estructura$data_dir,
        tipo = "CSV"
      )

      faltantes_png <- copiar_archivos(
        archivos = archivos$png,
        origen_dir = carpeta_salidas,
        destino_dir = estructura$www_dir,
        tipo = "PNG"
      )

      app_detectada <- detectar_app_origen(root, estructura$dashboard_dir, app_origen)
      copiar_app_dashboard(app_detectada, estructura$dashboard_dir)
      escribir_readme_dashboard(estructura$dashboard_dir)

      validar_estructura_final(estructura, archivos)

      cat("\nDashboard preparado en:\n")
      cat(estructura$dashboard_dir, "\n\n")
      cat("Ejecuta:\n")
      cat(sprintf("shiny::runApp('%s')\n", estructura$dashboard_dir))
      cat("\nArchivos CSV faltantes:", length(faltantes_csv), "\n")
      cat("Archivos PNG faltantes:", length(faltantes_png), "\n")
      cat("============================================================\n\n")

      estructura$dashboard_dir
    },
    error = function(e) {
      log_event("ERROR", e$message)
      stop(e)
    }
  )
}

# ------------------------------------------------------------
# 7. Ejecución automática opcional al hacer source()
# ------------------------------------------------------------

#' Ejecutar preparación automática si el usuario hace source()
#'
#' En sesiones interactivas, intenta preparar el dashboard automáticamente.
#' Si falla, muestra el error y deja disponible la función para ejecución manual.
#'
#' @return NULL de forma invisible.
main <- function() {
  tryCatch(
    {
      preparar_dashboard_shiny_911()
      invisible(NULL)
    },
    error = function(e) {
      log_event("ERROR", paste("Ejecución automática no completada:", e$message))
      log_event(
        "INFO",
        paste(
          "Puedes ejecutar manualmente:",
          "preparar_dashboard_shiny_911(root = 'RUTA_DE_TU_PROYECTO')"
        )
      )
      invisible(NULL)
    }
  )
}

if (interactive()) {
  main()
}
