# ============================================================
# pipeline_shiny_911_PORTABLE.R
#
# Propósito:
#   Probar y ejecutar localmente la aplicación Shiny del proyecto
#   911 CDMX desde un pipeline portable.
#
# Principio de portabilidad:
#   Este script NO contiene rutas absolutas ni depende del disco D:.
#   Trabaja en la carpeta donde se encuentre el script, salvo que
#   el usuario indique explícitamente otra raíz mediante:
#     - argumento root
#     - variable de entorno SHINY_911_ROOT
#
# Uso recomendado:
#   1) Copiar este archivo a la raíz del proyecto.
#   2) Ejecutar:
#        source("pipeline_shiny_911_PORTABLE.R")
#
# Salida esperada:
#   Aplicación Shiny en:
#        http://127.0.0.1:3838/
#
# Variables de entorno opcionales:
#   SHINY_911_ROOT       Ruta raíz del proyecto.
#   SHINY_911_AUTO       TRUE/FALSE. Ejecutar automáticamente al hacer source().
#   SHINY_911_RUN        TRUE/FALSE. Si TRUE abre Shiny; si FALSE solo valida.
#   SHINY_911_PORT       Puerto local. Por defecto 3838.
#   SHINY_911_INSTALL    TRUE/FALSE. Instalar paquetes faltantes. Por defecto FALSE.
#
# Nota:
#   Este pipeline NO ejecuta EDA, NO crea index.Rmd y NO renderiza HTML.
#   Solo valida y ejecuta dashboard_911/app.R con sus insumos.
# ============================================================

#' Registrar eventos del pipeline Shiny
#'
#' @param nivel character. Nivel del evento: INFO, OK, WARN o ERROR.
#' @param mensaje character. Mensaje que se mostrará en consola.
#' @return NULL invisible.
log_shiny_911 <- function(nivel, mensaje) {
  cat(sprintf("[%s] [%s] %s\n", Sys.time(), nivel, mensaje))
  invisible(NULL)
}

#' Convertir texto de entorno a valor lógico
#'
#' @param valor character. Valor textual.
#' @param default logical. Valor por defecto.
#' @return logical. Valor convertido.
as_logical_shiny_911 <- function(valor, default = FALSE) {
  tryCatch(
    {
      if (missing(valor) || is.null(valor) || !nzchar(valor)) {
        return(default)
      }

      valor_normalizado <- toupper(trimws(as.character(valor)))

      if (valor_normalizado %in% c("TRUE", "T", "1", "YES", "Y", "SI", "S")) {
        return(TRUE)
      }

      if (valor_normalizado %in% c("FALSE", "F", "0", "NO", "N")) {
        return(FALSE)
      }

      default
    },
    error = function(e) {
      log_shiny_911("WARN", paste("No se pudo convertir valor lógico:", e$message))
      default
    }
  )
}

#' Convertir texto de entorno a puerto
#'
#' @param valor character. Valor textual del puerto.
#' @param default integer. Puerto por defecto.
#' @return integer. Puerto validado.
as_port_shiny_911 <- function(valor, default = 3838L) {
  tryCatch(
    {
      if (missing(valor) || is.null(valor) || !nzchar(valor)) {
        return(as.integer(default))
      }

      puerto <- suppressWarnings(as.integer(valor))

      if (is.na(puerto) || puerto < 1024L || puerto > 65535L) {
        log_shiny_911("WARN", paste("Puerto inválido; se usará:", default))
        return(as.integer(default))
      }

      puerto
    },
    error = function(e) {
      log_shiny_911("WARN", paste("No se pudo convertir puerto:", e$message))
      as.integer(default)
    }
  )
}

#' Detectar la ruta del archivo cuando se ejecuta con source()
#'
#' @return character. Ruta del script o cadena vacía si no puede detectarse.
detectar_script_path_shiny_911 <- function() {
  tryCatch(
    {
      frames <- sys.frames()

      for (i in rev(seq_along(frames))) {
        posible <- frames[[i]]$ofile

        if (!is.null(posible) && length(posible) == 1L && nzchar(posible)) {
          return(normalizePath(posible, winslash = "/", mustWork = FALSE))
        }
      }

      ""
    },
    error = function(e) {
      log_shiny_911("WARN", paste("No se pudo detectar ruta del script:", e$message))
      ""
    }
  )
}

#' Inferir raíz portable del proyecto
#'
#' Prioridad:
#'   1. Variable de entorno SHINY_911_ROOT.
#'   2. Carpeta donde está este script.
#'   3. getwd() como respaldo.
#'
#' @return character. Ruta raíz normalizada.
inferir_root_shiny_911 <- function() {
  tryCatch(
    {
      root_env <- Sys.getenv("SHINY_911_ROOT", unset = "")

      if (nzchar(root_env)) {
        root <- normalizePath(root_env, winslash = "/", mustWork = FALSE)
        log_shiny_911("INFO", paste("Raíz tomada desde SHINY_911_ROOT:", root))
        return(root)
      }

      script_path <- detectar_script_path_shiny_911()

      if (nzchar(script_path)) {
        root <- normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE)
        log_shiny_911("INFO", paste("Raíz inferida desde ubicación del script:", root))
        return(root)
      }

      root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
      log_shiny_911("INFO", paste("Raíz inferida desde getwd():", root))
      root
    },
    error = function(e) {
      log_shiny_911("ERROR", paste("No se pudo inferir la raíz:", e$message))
      stop(e$message)
    }
  )
}

#' Resolver raíz del proyecto
#'
#' @param root character | NULL. Ruta raíz opcional.
#' @return character. Ruta raíz normalizada.
resolver_root_shiny_911 <- function(root = NULL) {
  tryCatch(
    {
      if (missing(root) || is.null(root) || !nzchar(root)) {
        return(inferir_root_shiny_911())
      }

      normalizePath(root, winslash = "/", mustWork = FALSE)
    },
    error = function(e) {
      log_shiny_911("ERROR", paste("No se pudo resolver raíz:", e$message))
      stop(e$message)
    }
  )
}

#' Validar o instalar paquetes requeridos
#'
#' @param paquetes character. Vector de paquetes.
#' @param instalar logical. Si TRUE instala paquetes faltantes.
#' @return NULL invisible.
validar_paquetes_shiny_911 <- function(paquetes, instalar = FALSE) {
  tryCatch(
    {
      faltantes <- character(0)

      for (paquete in paquetes) {
        if (!requireNamespace(paquete, quietly = TRUE)) {
          faltantes <- c(faltantes, paquete)
        }
      }

      if (length(faltantes) == 0L) {
        log_shiny_911("OK", "Paquetes requeridos disponibles.")
        return(invisible(NULL))
      }

      if (!isTRUE(instalar)) {
        stop(
          "Faltan paquetes: ",
          paste(faltantes, collapse = ", "),
          ". Instálalos o ejecuta con SHINY_911_INSTALL=TRUE."
        )
      }

      for (paquete in faltantes) {
        log_shiny_911("INFO", paste("Instalando paquete faltante:", paquete))
        install.packages(paquete, repos = "https://cloud.r-project.org")
      }

      log_shiny_911("OK", "Paquetes faltantes instalados.")
      invisible(NULL)
    },
    error = function(e) {
      log_shiny_911("ERROR", paste("Error validando paquetes:", e$message))
      stop(e$message)
    }
  )
}

#' Construir rutas principales de la app Shiny
#'
#' @param root character. Ruta raíz del proyecto.
#' @return list. Rutas de app, datos y recursos.
construir_rutas_shiny_911 <- function(root) {
  tryCatch(
    {
      list(
        root = root,
        app_dir = file.path(root, "dashboard_911"),
        app_r = file.path(root, "dashboard_911", "app.R"),
        data_dir = file.path(root, "dashboard_911", "data"),
        www_dir = file.path(root, "dashboard_911", "www")
      )
    },
    error = function(e) {
      log_shiny_911("ERROR", paste("Error construyendo rutas:", e$message))
      stop(e$message)
    }
  )
}

#' Validar estructura física de Shiny
#'
#' @param rutas list. Rutas principales.
#' @return NULL invisible.
validar_estructura_shiny_911 <- function(rutas) {
  tryCatch(
    {
      requeridas <- c(
        app_dir = rutas$app_dir,
        app_r = rutas$app_r,
        data_dir = rutas$data_dir,
        www_dir = rutas$www_dir
      )

      for (nombre in names(requeridas)) {
        ruta <- requeridas[[nombre]]

        if (!file.exists(ruta)) {
          stop("Falta ruta requerida [", nombre, "]: ", ruta)
        }

        log_shiny_911("OK", paste("Existe:", ruta))
      }

      invisible(NULL)
    },
    error = function(e) {
      log_shiny_911("ERROR", paste("Estructura inválida:", e$message))
      stop(e$message)
    }
  )
}

#' Validar archivos CSV y PNG requeridos por tareas 1 a 5
#'
#' @param rutas list. Rutas principales.
#' @return list. Resumen de archivos.
validar_insumos_shiny_911 <- function(rutas) {
  tryCatch(
    {
      csv_requeridos <- c(
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
      )

      png_requeridos <- c(
        "02_tarea1_promedio_mensual_alcaldia_categoria.png",
        "03_tarea2_incidentes_por_dia_semana.png",
        "04_tarea3_distribucion_horaria_categorias.png",
        "05_tarea4_histograma_duracion_minutos.png",
        "06_tarea5_porcentaje_por_clasificacion.png"
      )

      csv_rutas <- file.path(rutas$data_dir, csv_requeridos)
      png_rutas <- file.path(rutas$www_dir, png_requeridos)

      csv_faltantes <- csv_requeridos[!file.exists(csv_rutas)]
      png_faltantes <- png_requeridos[!file.exists(png_rutas)]

      csv_existentes <- list.files(rutas$data_dir, pattern = "\\.csv$", full.names = FALSE)
      png_existentes <- list.files(rutas$www_dir, pattern = "\\.png$", full.names = FALSE)

      log_shiny_911("INFO", paste("CSV encontrados:", length(csv_existentes)))
      log_shiny_911("INFO", paste("PNG encontrados:", length(png_existentes)))

      if (length(csv_faltantes) > 0L) {
        stop("Faltan CSV requeridos: ", paste(csv_faltantes, collapse = ", "))
      }

      if (length(png_faltantes) > 0L) {
        stop("Faltan PNG requeridos: ", paste(png_faltantes, collapse = ", "))
      }

      log_shiny_911("OK", "Insumos CSV/PNG requeridos completos.")

      list(
        csv = csv_existentes,
        png = png_existentes,
        csv_requeridos = csv_requeridos,
        png_requeridos = png_requeridos
      )
    },
    error = function(e) {
      log_shiny_911("ERROR", paste("Insumos inválidos:", e$message))
      stop(e$message)
    }
  )
}

#' Validar sintaxis de app.R
#'
#' @param rutas list. Rutas principales.
#' @return NULL invisible.
validar_sintaxis_app_shiny_911 <- function(rutas) {
  tryCatch(
    {
      parse(rutas$app_r)
      log_shiny_911("OK", "app.R no tiene errores de sintaxis.")
      invisible(NULL)
    },
    error = function(e) {
      log_shiny_911("ERROR", paste("Error de sintaxis en app.R:", e$message))
      stop(e$message)
    }
  )
}

#' Mostrar resumen de validación
#'
#' @param rutas list. Rutas principales.
#' @param insumos list. Resultado de validación de insumos.
#' @param port integer. Puerto de ejecución.
#' @return NULL invisible.
mostrar_resumen_shiny_911 <- function(rutas, insumos, port) {
  cat("\n============================================================\n")
  cat("RESUMEN PIPELINE SHINY 911\n")
  cat("============================================================\n")
  cat("Raíz del proyecto : ", rutas$root, "\n", sep = "")
  cat("App directory     : ", rutas$app_dir, "\n", sep = "")
  cat("app.R             : ", rutas$app_r, "\n", sep = "")
  cat("CSV requeridos    : ", length(insumos$csv_requeridos), "\n", sep = "")
  cat("PNG requeridos    : ", length(insumos$png_requeridos), "\n", sep = "")
  cat("Puerto            : ", port, "\n", sep = "")
  cat("URL local         : http://127.0.0.1:", port, "/\n", sep = "")
  cat("============================================================\n\n")
  invisible(NULL)
}

#' Validar proyecto Shiny 911
#'
#' @param root character | NULL. Ruta raíz del proyecto.
#' @param instalar_paquetes logical. Si TRUE instala paquetes faltantes.
#' @param port integer. Puerto local.
#' @return list. Rutas e insumos validados.
validar_pipeline_shiny_911 <- function(root = NULL, instalar_paquetes = FALSE, port = 3838L) {
  tryCatch(
    {
      root <- resolver_root_shiny_911(root)
      rutas <- construir_rutas_shiny_911(root)

      validar_paquetes_shiny_911(
        paquetes = c("shiny", "DT"),
        instalar = instalar_paquetes
      )

      validar_estructura_shiny_911(rutas)
      insumos <- validar_insumos_shiny_911(rutas)
      validar_sintaxis_app_shiny_911(rutas)
      mostrar_resumen_shiny_911(rutas, insumos, port)

      list(
        root = root,
        rutas = rutas,
        insumos = insumos,
        port = port
      )
    },
    error = function(e) {
      log_shiny_911("ERROR", paste("Validación del pipeline fallida:", e$message))
      stop(e$message)
    }
  )
}

#' Ejecutar Shiny local desde pipeline
#'
#' @param root character | NULL. Ruta raíz del proyecto.
#' @param port integer. Puerto local.
#' @param launch_browser logical. Si TRUE abre navegador.
#' @param instalar_paquetes logical. Si TRUE instala paquetes faltantes.
#' @param run_app logical. Si TRUE ejecuta Shiny; si FALSE solo valida.
#' @return invisible NULL.
ejecutar_pipeline_shiny_911 <- function(
    root = NULL,
    port = 3838L,
    launch_browser = TRUE,
    instalar_paquetes = FALSE,
    run_app = TRUE
) {
  tryCatch(
    {
      validacion <- validar_pipeline_shiny_911(
        root = root,
        instalar_paquetes = instalar_paquetes,
        port = port
      )

      if (!isTRUE(run_app)) {
        log_shiny_911("OK", "Validación completa. Shiny no se ejecutó porque run_app = FALSE.")
        return(invisible(validacion))
      }

      log_shiny_911("INFO", paste("Iniciando Shiny desde:", validacion$rutas$app_dir))
      log_shiny_911("INFO", paste0("URL esperada: http://127.0.0.1:", port, "/"))

      shiny::runApp(
        appDir = validacion$rutas$app_dir,
        host = "127.0.0.1",
        port = port,
        launch.browser = launch_browser
      )

      invisible(validacion)
    },
    error = function(e) {
      log_shiny_911("ERROR", paste("Ejecución del pipeline Shiny fallida:", e$message))
      stop(e$message)
    }
  )
}

#' Ejecutar automáticamente el pipeline Shiny
#'
#' @return invisible NULL.
ejecutar_auto_pipeline_shiny_911 <- function() {
  tryCatch(
    {
      auto <- as_logical_shiny_911(
        Sys.getenv("SHINY_911_AUTO", unset = "TRUE"),
        default = TRUE
      )

      if (!isTRUE(auto)) {
        log_shiny_911("INFO", "Ejecución automática desactivada.")
        return(invisible(NULL))
      }

      port <- as_port_shiny_911(
        Sys.getenv("SHINY_911_PORT", unset = "3838"),
        default = 3838L
      )

      instalar <- as_logical_shiny_911(
        Sys.getenv("SHINY_911_INSTALL", unset = "FALSE"),
        default = FALSE
      )

      run_app <- as_logical_shiny_911(
        Sys.getenv("SHINY_911_RUN", unset = "TRUE"),
        default = TRUE
      )

      ejecutar_pipeline_shiny_911(
        root = NULL,
        port = port,
        launch_browser = TRUE,
        instalar_paquetes = instalar,
        run_app = run_app
      )
    },
    error = function(e) {
      log_shiny_911("ERROR", paste("Error en ejecución automática:", e$message))
      stop(e$message)
    }
  )
}

ejecutar_auto_pipeline_shiny_911()
