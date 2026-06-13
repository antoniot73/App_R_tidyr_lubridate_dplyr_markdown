# ============================================================
# orquestar_proyecto_911.R
#
# Propósito:
#   Ejecutar de forma secuencial y controlada los pipelines oficiales
#   del proyecto 911 CDMX, evitando conflictos de servidores Shiny y
#   permitiendo continuar cuando los archivos requeridos ya existen.
#
# Pipelines coordinados:
#   1. eda_911_cdmx_v4.R
#   2. crear_app_shiny_911.R
#   3. preparar_dashboard_shiny_911.R
#   4. crear_index_rmd_911.R
#   5. render_rmarkdown_github_pages_911.R
#   6. probar_shiny_local_911.R  # solo desde menú local
#
# Principio de operación:
#   - No depende de C:, D: ni rutas absolutas.
#   - Usa la carpeta donde esté este script como raíz del proyecto,
#     salvo que se indique otra con ORQ_911_ROOT o argumento root.
#   - Carga los pipelines sin ejecutar automáticamente sus servidores.
#   - probar_shiny_local_911.R no corre automáticamente; solo se invoca desde el menú local.
#
# Uso local recomendado:
#   source("orquestar_proyecto_911.R")
#
# Ejecutar Shiny al final:
#   Sys.setenv(ORQ_911_RUN_SHINY = "TRUE")
#   source("orquestar_proyecto_911.R")
#
# Solo validar Shiny sin abrir servidor:
#   Sys.setenv(ORQ_911_RUN_SHINY = "FALSE")
#   source("orquestar_proyecto_911.R")
# ============================================================

# ------------------------------------------------------------
# 1. Bitácora y utilidades generales
# ------------------------------------------------------------

#' Registrar eventos del orquestador
#'
#' @param nivel character. Nivel del evento: INFO, OK, WARN, SKIP o ERROR.
#' @param mensaje character. Mensaje para consola.
#' @return NULL invisible.
log_orq_911 <- function(nivel, mensaje) {
  cat(sprintf("[%s] [%s] %s\n", Sys.time(), nivel, mensaje))
  invisible(NULL)
}

#' Convertir texto de entorno a lógico
#'
#' @param valor character. Valor textual.
#' @param default logical. Valor por defecto.
#' @return logical. Valor convertido.
as_logical_orq_911 <- function(valor, default = FALSE) {
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
      log_orq_911("WARN", paste("No se pudo convertir valor lógico:", e$message))
      default
    }
  )
}

#' Convertir texto de entorno a puerto
#'
#' @param valor character. Valor textual.
#' @param default integer. Puerto por defecto.
#' @return integer. Puerto validado.
as_port_orq_911 <- function(valor, default = 3838L) {
  tryCatch(
    {
      if (missing(valor) || is.null(valor) || !nzchar(valor)) {
        return(as.integer(default))
      }

      puerto <- suppressWarnings(as.integer(valor))

      if (is.na(puerto) || puerto < 1024L || puerto > 65535L) {
        log_orq_911("WARN", paste("Puerto inválido; se usará:", default))
        return(as.integer(default))
      }

      puerto
    },
    error = function(e) {
      log_orq_911("WARN", paste("No se pudo convertir puerto:", e$message))
      as.integer(default)
    }
  )
}

#' Detectar ruta del script cuando se ejecuta con source()
#'
#' @return character. Ruta del script o cadena vacía.
detectar_script_path_orq_911 <- function() {
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
      log_orq_911("WARN", paste("No se pudo detectar ruta del script:", e$message))
      ""
    }
  )
}

#' Inferir raíz portable del proyecto
#'
#' @return character. Ruta raíz normalizada.
inferir_root_orq_911 <- function() {
  tryCatch(
    {
      root_env <- Sys.getenv("ORQ_911_ROOT", unset = "")

      if (nzchar(root_env)) {
        root <- normalizePath(root_env, winslash = "/", mustWork = FALSE)
        log_orq_911("INFO", paste("Raíz tomada desde ORQ_911_ROOT:", root))
        return(root)
      }

      script_path <- detectar_script_path_orq_911()

      if (nzchar(script_path)) {
        root <- normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE)
        log_orq_911("INFO", paste("Raíz inferida desde ubicación del orquestador:", root))
        return(root)
      }

      root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
      log_orq_911("INFO", paste("Raíz inferida desde getwd():", root))
      root
    },
    error = function(e) {
      log_orq_911("ERROR", paste("No se pudo inferir raíz:", e$message))
      stop(e$message)
    }
  )
}

#' Resolver raíz del proyecto
#'
#' @param root character | NULL. Ruta raíz opcional.
#' @return character. Ruta raíz normalizada.
resolver_root_orq_911 <- function(root = NULL) {
  tryCatch(
    {
      if (missing(root) || is.null(root) || !nzchar(root)) {
        return(inferir_root_orq_911())
      }

      normalizePath(root, winslash = "/", mustWork = FALSE)
    },
    error = function(e) {
      log_orq_911("ERROR", paste("No se pudo resolver raíz:", e$message))
      stop(e$message)
    }
  )
}

# ------------------------------------------------------------
# 2. Catálogo de archivos esperados
# ------------------------------------------------------------

#' Obtener nombres oficiales de pipelines
#'
#' @return character. Vector de archivos R oficiales.
obtener_pipelines_oficiales_911 <- function() {
  c(
    "eda_911_cdmx_v4.R",
    "crear_app_shiny_911.R",
    "preparar_dashboard_shiny_911.R",
    "crear_index_rmd_911.R",
    "render_rmarkdown_github_pages_911.R",
    "probar_shiny_local_911.R"
  )
}

#' Obtener CSV requeridos por R Markdown y Shiny
#'
#' @return character. Vector de CSV requeridos.
obtener_csv_requeridos_911 <- function() {
  c(
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
}

#' Obtener PNG requeridos por R Markdown y Shiny
#'
#' @return character. Vector de PNG requeridos.
obtener_png_requeridos_911 <- function() {
  c(
    "02_tarea1_promedio_mensual_alcaldia_categoria.png",
    "03_tarea2_incidentes_por_dia_semana.png",
    "04_tarea3_distribucion_horaria_categorias.png",
    "05_tarea4_histograma_duracion_minutos.png",
    "06_tarea5_porcentaje_por_clasificacion.png"
  )
}

#' Construir rutas principales del proyecto
#'
#' @param root character. Ruta raíz.
#' @return list. Rutas principales.
construir_rutas_orq_911 <- function(root) {
  tryCatch(
    {
      list(
        root = root,
        dashboard = file.path(root, "dashboard_911"),
        data = file.path(root, "dashboard_911", "data"),
        www = file.path(root, "dashboard_911", "www"),
        docs = file.path(root, "docs"),
        docs_data = file.path(root, "docs", "data"),
        docs_www = file.path(root, "docs", "www"),
        app_r = file.path(root, "dashboard_911", "app.R"),
        index_rmd = file.path(root, "dashboard_911", "index.Rmd"),
        dashboard_html = file.path(root, "dashboard_911", "index.html"),
        docs_html = file.path(root, "docs", "index.html")
      )
    },
    error = function(e) {
      log_orq_911("ERROR", paste("No se pudieron construir rutas:", e$message))
      stop(e$message)
    }
  )
}

# ------------------------------------------------------------
# 3. Validaciones de existencia
# ------------------------------------------------------------

#' Validar que existan los pipelines oficiales
#'
#' @param root character. Ruta raíz.
#' @return logical. TRUE si todos existen.
validar_pipelines_oficiales_911 <- function(root) {
  tryCatch(
    {
      pipelines <- obtener_pipelines_oficiales_911()
      rutas <- file.path(root, pipelines)
      faltantes <- pipelines[!file.exists(rutas)]

      if (length(faltantes) > 0L) {
        stop("Faltan pipelines oficiales: ", paste(faltantes, collapse = ", "))
      }

      for (pipeline in pipelines) {
        log_orq_911("OK", paste("Pipeline disponible:", pipeline))
      }

      TRUE
    },
    error = function(e) {
      log_orq_911("ERROR", e$message)
      stop(e$message)
    }
  )
}

#' Validar si todos los archivos requeridos existen en una carpeta
#'
#' @param carpeta character. Carpeta a validar.
#' @param archivos character. Archivos requeridos.
#' @return logical. TRUE si todos existen.
archivos_completos_911 <- function(carpeta, archivos) {
  tryCatch(
    {
      if (!dir.exists(carpeta)) {
        return(FALSE)
      }

      all(file.exists(file.path(carpeta, archivos)))
    },
    error = function(e) {
      log_orq_911("WARN", paste("No se pudo validar carpeta:", carpeta, e$message))
      FALSE
    }
  )
}

#' Obtener archivos faltantes en una carpeta
#'
#' @param carpeta character. Carpeta a validar.
#' @param archivos character. Archivos requeridos.
#' @return character. Archivos faltantes.
obtener_faltantes_911 <- function(carpeta, archivos) {
  tryCatch(
    {
      if (!dir.exists(carpeta)) {
        return(archivos)
      }

      archivos[!file.exists(file.path(carpeta, archivos))]
    },
    error = function(e) {
      log_orq_911("WARN", paste("No se pudieron obtener faltantes:", e$message))
      archivos
    }
  )
}

#' Validar si dashboard_911/data y dashboard_911/www están completos
#'
#' @param rutas list. Rutas principales.
#' @return logical. TRUE si están completos.
dashboard_insumos_completos_911 <- function(rutas) {
  csv_ok <- archivos_completos_911(rutas$data, obtener_csv_requeridos_911())
  png_ok <- archivos_completos_911(rutas$www, obtener_png_requeridos_911())
  isTRUE(csv_ok && png_ok)
}

#' Validar si docs está completo para GitHub Pages
#'
#' @param rutas list. Rutas principales.
#' @return logical. TRUE si docs está completo.
docs_completos_911 <- function(rutas) {
  html_ok <- file.exists(rutas$docs_html)
  csv_ok <- archivos_completos_911(rutas$docs_data, obtener_csv_requeridos_911())
  png_ok <- archivos_completos_911(rutas$docs_www, obtener_png_requeridos_911())
  isTRUE(html_ok && csv_ok && png_ok)
}

#' Detectar carpeta de salida EDA completa
#'
#' @param root character. Ruta raíz.
#' @return character | NA. Carpeta completa más reciente.
detectar_salida_eda_completa_911 <- function(root) {
  tryCatch(
    {
      candidatos <- list.dirs(root, recursive = FALSE, full.names = TRUE)
      candidatos <- candidatos[grepl("salidas_911_eda", basename(candidatos), fixed = TRUE)]

      if (length(candidatos) == 0L) {
        return(NA_character_)
      }

      candidatos <- candidatos[order(file.info(candidatos)$mtime, decreasing = TRUE)]

      for (candidato in candidatos) {
        csv_ok <- archivos_completos_911(candidato, obtener_csv_requeridos_911())
        png_ok <- archivos_completos_911(candidato, obtener_png_requeridos_911())

        if (isTRUE(csv_ok && png_ok)) {
          return(normalizePath(candidato, winslash = "/", mustWork = TRUE))
        }
      }

      NA_character_
    },
    error = function(e) {
      log_orq_911("WARN", paste("No se pudo detectar salida EDA completa:", e$message))
      NA_character_
    }
  )
}

#' Mostrar diagnóstico breve de insumos
#'
#' @param rutas list. Rutas principales.
#' @return NULL invisible.
mostrar_estado_insumos_911 <- function(rutas) {
  tryCatch(
    {
      faltantes_csv <- obtener_faltantes_911(rutas$data, obtener_csv_requeridos_911())
      faltantes_png <- obtener_faltantes_911(rutas$www, obtener_png_requeridos_911())

      log_orq_911("INFO", paste("CSV faltantes en dashboard:", length(faltantes_csv)))
      log_orq_911("INFO", paste("PNG faltantes en dashboard:", length(faltantes_png)))

      invisible(NULL)
    },
    error = function(e) {
      log_orq_911("WARN", paste("No se pudo mostrar estado de insumos:", e$message))
      invisible(NULL)
    }
  )
}

# ------------------------------------------------------------
# 4. Carga controlada de pipelines sin autoejecución
# ------------------------------------------------------------

#' Cargar un pipeline en entorno aislado, cortando autoejecución
#'
#' @param path character. Ruta del archivo R.
#' @param patrones_corte character. Patrones que indican inicio de autoejecución.
#' @return environment. Entorno con funciones cargadas.
cargar_pipeline_controlado_911 <- function(path, patrones_corte) {
  tryCatch(
    {
      if (!file.exists(path)) {
        stop("No existe pipeline: ", path)
      }

      lineas <- readLines(path, warn = FALSE, encoding = "UTF-8")
      indices <- integer(0)

      for (patron in patrones_corte) {
        encontrados <- grep(patron, lineas)
        if (length(encontrados) > 0L) {
          indices <- c(indices, encontrados[1L])
        }
      }

      if (length(indices) > 0L) {
        corte <- min(indices)
        if (corte > 1L) {
          lineas <- lineas[seq_len(corte - 1L)]
        }
      }

      env <- new.env(parent = globalenv())
      eval(parse(text = lineas), envir = env)

      log_orq_911("OK", paste("Pipeline cargado sin autoejecución:", basename(path)))
      env
    },
    error = function(e) {
      log_orq_911("ERROR", paste("Error cargando pipeline controlado:", basename(path), e$message))
      stop(e$message)
    }
  )
}

#' Ejecutar un paso con control de salto
#'
#' @param nombre character. Nombre del paso.
#' @param saltar logical. Si TRUE se omite.
#' @param razon_skip character. Razón para omitir.
#' @param accion function. Acción a ejecutar.
#' @return list. Resultado del paso.
ejecutar_paso_orq_911 <- function(nombre, saltar, razon_skip, accion) {
  tryCatch(
    {
      cat("\n------------------------------------------------------------\n")
      cat("PASO:", nombre, "\n")
      cat("------------------------------------------------------------\n")

      if (isTRUE(saltar)) {
        log_orq_911("SKIP", razon_skip)
        return(list(nombre = nombre, estado = "SKIP", resultado = NULL))
      }

      resultado <- accion()
      log_orq_911("OK", paste("Paso completado:", nombre))
      list(nombre = nombre, estado = "OK", resultado = resultado)
    },
    error = function(e) {
      log_orq_911("ERROR", paste("Paso fallido:", nombre, "-", e$message))
      stop(e$message)
    }
  )
}

# ------------------------------------------------------------
# 5. Pasos del orquestador
# ------------------------------------------------------------

#' Ejecutar EDA si faltan salidas agregadas
#'
#' @param root character. Ruta raíz.
#' @param rutas list. Rutas principales.
#' @param forzar logical. Si TRUE ejecuta aunque existan salidas.
#' @param instalar_paquetes logical. Si TRUE permite instalación de paquetes.
#' @return list. Resultado del paso.
paso_eda_911 <- function(root, rutas, forzar = FALSE, instalar_paquetes = TRUE) {
  salida_eda <- detectar_salida_eda_completa_911(root)
  dashboard_ok <- dashboard_insumos_completos_911(rutas)

  saltar <- !isTRUE(forzar) && (isTRUE(dashboard_ok) || !is.na(salida_eda))

  ejecutar_paso_orq_911(
    nombre = "1. EDA 911 CDMX",
    saltar = saltar,
    razon_skip = "Ya existen insumos EDA completos o dashboard_911/data y www completos.",
    accion = function() {
      env <- cargar_pipeline_controlado_911(
        path = file.path(root, "eda_911_cdmx_v4.R"),
        patrones_corte = c("^# 10\\. EJECUCION", "^main\\(\\)")
      )

      if (!exists("main", envir = env, inherits = FALSE)) {
        stop("El pipeline EDA no expone la función main().")
      }

      resultado <- env$main(
        directorio_datos = root,
        instalar_paquetes = instalar_paquetes
      )

      salida <- detectar_salida_eda_completa_911(root)

      if (is.na(salida)) {
        stop("EDA ejecutado, pero no se detectó una carpeta salidas_911_eda_* completa.")
      }

      list(resultado = resultado, salida_eda = salida)
    }
  )
}

#' Crear app.R si falta o se fuerza
#'
#' @param root character. Ruta raíz.
#' @param rutas list. Rutas principales.
#' @param forzar logical. Si TRUE sobrescribe.
#' @return list. Resultado del paso.
paso_crear_app_911 <- function(root, rutas, forzar = FALSE) {
  saltar <- !isTRUE(forzar) && file.exists(rutas$app_r)

  ejecutar_paso_orq_911(
    nombre = "2. Crear app.R de Shiny",
    saltar = saltar,
    razon_skip = "dashboard_911/app.R ya existe.",
    accion = function() {
      env <- cargar_pipeline_controlado_911(
        path = file.path(root, "crear_app_shiny_911.R"),
        patrones_corte = c("^ejecutar_auto_crear_app_shiny_911\\(\\)")
      )

      if (!exists("crear_app_shiny_911", envir = env, inherits = FALSE)) {
        stop("El pipeline crear_app_shiny_911.R no expone crear_app_shiny_911().")
      }

      env$crear_app_shiny_911(
        root = root,
        overwrite = TRUE
      )

      if (!file.exists(rutas$app_r)) {
        stop("No se generó dashboard_911/app.R.")
      }

      rutas$app_r
    }
  )
}

#' Preparar dashboard si faltan CSV o PNG
#'
#' @param root character. Ruta raíz.
#' @param rutas list. Rutas principales.
#' @param forzar logical. Si TRUE vuelve a preparar.
#' @param limpiar_dashboard logical. Si TRUE limpia data/ y www/ antes de copiar.
#' @return list. Resultado del paso.
paso_preparar_dashboard_911 <- function(root, rutas, forzar = FALSE, limpiar_dashboard = FALSE) {
  saltar <- !isTRUE(forzar) && dashboard_insumos_completos_911(rutas)

  ejecutar_paso_orq_911(
    nombre = "3. Preparar dashboard_911/data y dashboard_911/www",
    saltar = saltar,
    razon_skip = "dashboard_911/data y dashboard_911/www ya tienen todos los insumos requeridos.",
    accion = function() {
      if (!file.exists(rutas$app_r)) {
        stop("No existe app.R antes de preparar dashboard. Ejecuta crear_app_shiny_911.R primero.")
      }

      env <- cargar_pipeline_controlado_911(
        path = file.path(root, "preparar_dashboard_shiny_911.R"),
        patrones_corte = c("^# 7\\. Ejecución automática", "^if \\(interactive\\(\\)\\)")
      )

      if (!exists("preparar_dashboard_shiny_911", envir = env, inherits = FALSE)) {
        stop("El pipeline preparar_dashboard_shiny_911.R no expone preparar_dashboard_shiny_911().")
      }

      salida_eda <- detectar_salida_eda_completa_911(root)

      if (is.na(salida_eda)) {
        stop("No se encontró carpeta salidas_911_eda_* completa para preparar dashboard.")
      }

      env$preparar_dashboard_shiny_911(
        root = root,
        carpeta_salidas = salida_eda,
        carpeta_dashboard = "dashboard_911",
        app_origen = rutas$app_r,
        limpiar = limpiar_dashboard
      )

      if (!dashboard_insumos_completos_911(rutas)) {
        mostrar_estado_insumos_911(rutas)
        stop("La preparación del dashboard terminó, pero siguen faltando insumos.")
      }

      rutas$dashboard
    }
  )
}

#' Crear index.Rmd si falta o se fuerza
#'
#' @param root character. Ruta raíz.
#' @param rutas list. Rutas principales.
#' @param forzar logical. Si TRUE sobrescribe.
#' @return list. Resultado del paso.
paso_crear_index_911 <- function(root, rutas, forzar = FALSE) {
  saltar <- !isTRUE(forzar) && file.exists(rutas$index_rmd)

  ejecutar_paso_orq_911(
    nombre = "4. Crear index.Rmd",
    saltar = saltar,
    razon_skip = "dashboard_911/index.Rmd ya existe.",
    accion = function() {
      env <- cargar_pipeline_controlado_911(
        path = file.path(root, "crear_index_rmd_911.R"),
        patrones_corte = c("^ejecutar_auto_index_rmd_911\\(\\)")
      )

      if (!exists("crear_index_rmd_911", envir = env, inherits = FALSE)) {
        stop("El pipeline crear_index_rmd_911.R no expone crear_index_rmd_911().")
      }

      env$crear_index_rmd_911(
        root = root,
        overwrite = TRUE
      )

      if (!file.exists(rutas$index_rmd)) {
        stop("No se generó dashboard_911/index.Rmd.")
      }

      rutas$index_rmd
    }
  )
}

#' Renderizar R Markdown y preparar docs/
#'
#' @param root character. Ruta raíz.
#' @param rutas list. Rutas principales.
#' @param forzar logical. Si TRUE renderiza aunque docs exista.
#' @param abrir_html logical. Si TRUE abre HTML local.
#' @return list. Resultado del paso.
paso_render_911 <- function(root, rutas, forzar = FALSE, abrir_html = FALSE) {
  saltar <- !isTRUE(forzar) && docs_completos_911(rutas)

  ejecutar_paso_orq_911(
    nombre = "5. Render R Markdown y actualizar docs/",
    saltar = saltar,
    razon_skip = "docs/index.html, docs/data y docs/www ya existen completos.",
    accion = function() {
      if (!file.exists(rutas$index_rmd)) {
        stop("No existe dashboard_911/index.Rmd.")
      }

      if (!dashboard_insumos_completos_911(rutas)) {
        stop("No están completos dashboard_911/data y dashboard_911/www.")
      }

      env <- cargar_pipeline_controlado_911(
        path = file.path(root, "render_rmarkdown_github_pages_911.R"),
        patrones_corte = c("^# Ejecución directa", "^tryCatch\\(")
      )

      if (!exists("render_rmarkdown_github_pages_911", envir = env, inherits = FALSE)) {
        stop("El pipeline render_rmarkdown_github_pages_911.R no expone render_rmarkdown_github_pages_911().")
      }

      env$render_rmarkdown_github_pages_911(
        root = root,
        actualizar_docs = TRUE,
        abrir_html = abrir_html
      )

      if (!docs_completos_911(rutas)) {
        stop("Render ejecutado, pero docs/ no quedó completo.")
      }

      rutas$docs_html
    }
  )
}

#' Validar o ejecutar Shiny local al final
#'
#' @param root character. Ruta raíz.
#' @param rutas list. Rutas principales.
#' @param ejecutar_shiny logical. Si TRUE abre servidor Shiny.
#' @param port integer. Puerto local.
#' @param instalar_paquetes logical. Si TRUE instala paquetes faltantes.
#' @return list. Resultado del paso.
paso_shiny_local_911 <- function(root,
                                 rutas,
                                 ejecutar_shiny = FALSE,
                                 port = 3838L,
                                 instalar_paquetes = FALSE) {
  ejecutar_paso_orq_911(
    nombre = "6. Validar Shiny local",
    saltar = FALSE,
    razon_skip = "",
    accion = function() {
      if (!file.exists(rutas$app_r)) {
        stop("No existe dashboard_911/app.R.")
      }

      if (!dashboard_insumos_completos_911(rutas)) {
        stop("No están completos los insumos de Shiny.")
      }

      env <- cargar_pipeline_controlado_911(
        path = file.path(root, "probar_shiny_local_911.R"),
        patrones_corte = c("^ejecutar_auto_pipeline_shiny_911\\(\\)")
      )

      if (!exists("ejecutar_pipeline_shiny_911", envir = env, inherits = FALSE)) {
        stop("El pipeline probar_shiny_local_911.R no expone ejecutar_pipeline_shiny_911().")
      }

      env$ejecutar_pipeline_shiny_911(
        root = root,
        port = port,
        launch_browser = TRUE,
        instalar_paquetes = instalar_paquetes,
        run_app = ejecutar_shiny
      )

      if (isTRUE(ejecutar_shiny)) {
        "Servidor Shiny iniciado. Detener con Stop/Esc en RStudio."
      } else {
        "Shiny validado sin abrir servidor."
      }
    }
  )
}


# ------------------------------------------------------------
# 6. Menú local de despliegues
# ------------------------------------------------------------

#' Abrir despliegue local estático del reporte R Markdown
#'
#' Abre en navegador local el archivo HTML preparado para GitHub Pages.
#' Prioriza docs/index.html y, si no existe, usa dashboard_911/index.html.
#'
#' @param rutas list. Rutas principales del proyecto.
#' @return character. Ruta HTML abierta.
abrir_markdown_local_911 <- function(rutas) {
  tryCatch(
    {
      html_docs <- rutas$docs_html
      html_dashboard <- rutas$dashboard_html

      if (file.exists(html_docs)) {
        html_objetivo <- normalizePath(html_docs, winslash = "/", mustWork = TRUE)
      } else if (file.exists(html_dashboard)) {
        html_objetivo <- normalizePath(html_dashboard, winslash = "/", mustWork = TRUE)
      } else {
        stop("No existe docs/index.html ni dashboard_911/index.html. Ejecuta primero el render R Markdown.")
      }

      log_orq_911("INFO", paste("Abriendo reporte R Markdown local:", html_objetivo))
      utils::browseURL(html_objetivo)

      html_objetivo
    },
    error = function(e) {
      log_orq_911("ERROR", paste("No se pudo abrir Markdown local:", e$message))
      stop(e$message)
    }
  )
}

#' Obtener ruta del archivo de estado del servidor Shiny local
#'
#' @param rutas list. Rutas principales.
#' @return character. Ruta del archivo RDS con PID y puerto.
obtener_pid_file_shiny_911 <- function(rutas) {
  file.path(rutas$root, ".orq_911_shiny_local.pid.rds")
}

#' Obtener ruta del log del servidor Shiny local en segundo plano
#'
#' @param rutas list. Rutas principales.
#' @return character. Ruta del archivo log.
obtener_log_file_shiny_911 <- function(rutas) {
  file.path(rutas$root, ".orq_911_shiny_local.log")
}

#' Validar disponibilidad del paquete callr
#'
#' @param instalar_paquetes logical. Si TRUE instala callr cuando falte.
#' @return logical. TRUE si callr está disponible.
asegurar_callr_orq_911 <- function(instalar_paquetes = FALSE) {
  tryCatch(
    {
      if (requireNamespace("callr", quietly = TRUE)) {
        return(TRUE)
      }

      if (!isTRUE(instalar_paquetes)) {
        stop(
          "Para levantar Shiny sin bloquear el menú se requiere el paquete 'callr'. ",
          "Instálalo con install.packages('callr') o ejecuta el orquestador con instalar_paquetes = TRUE."
        )
      }

      log_orq_911("INFO", "Instalando paquete requerido: callr")
      install.packages("callr", dependencies = TRUE)

      if (!requireNamespace("callr", quietly = TRUE)) {
        stop("No se pudo instalar o cargar el paquete callr.")
      }

      TRUE
    },
    error = function(e) {
      log_orq_911("ERROR", paste("No está disponible callr:", e$message))
      stop(e$message)
    }
  )
}

#' Leer estado del servidor Shiny local
#'
#' @param rutas list. Rutas principales.
#' @return list | NULL. Estado guardado o NULL.
leer_estado_shiny_local_911 <- function(rutas) {
  tryCatch(
    {
      pid_file <- obtener_pid_file_shiny_911(rutas)

      if (!file.exists(pid_file)) {
        return(NULL)
      }

      readRDS(pid_file)
    },
    error = function(e) {
      log_orq_911("WARN", paste("No se pudo leer estado Shiny:", e$message))
      NULL
    }
  )
}

#' Verificar si un proceso está activo por PID
#'
#' @param pid integer. Identificador de proceso.
#' @return logical. TRUE si el proceso parece activo.
proceso_activo_orq_911 <- function(pid) {
  tryCatch(
    {
      pid <- suppressWarnings(as.integer(pid))

      if (is.na(pid) || pid <= 0L) {
        return(FALSE)
      }

      if (.Platform$OS.type == "windows") {
        comando_ps <- sprintf(
          "if (Get-Process -Id %d -ErrorAction SilentlyContinue) { exit 0 } else { exit 1 }",
          pid
        )

        estado <- suppressWarnings(
          system2(
            "powershell",
            args = c("-NoProfile", "-ExecutionPolicy", "Bypass", "-Command", comando_ps),
            stdout = FALSE,
            stderr = FALSE
          )
        )

        return(identical(as.integer(estado), 0L))
      }

      estado <- suppressWarnings(
        system2(
          "kill",
          args = c("-0", as.character(pid)),
          stdout = FALSE,
          stderr = FALSE
        )
      )

      identical(as.integer(estado), 0L)
    },
    error = function(e) {
      log_orq_911("WARN", paste("No se pudo verificar PID:", e$message))
      FALSE
    }
  )
}

#' Obtener estado textual del servidor Shiny local
#'
#' @param rutas list. Rutas principales.
#' @return character. Estado resumido.
estado_shiny_local_911 <- function(rutas) {
  tryCatch(
    {
      estado <- leer_estado_shiny_local_911(rutas)

      if (is.null(estado)) {
        return("Sin servidor Shiny registrado por el orquestador.")
      }

      activo <- proceso_activo_orq_911(estado$pid)

      if (isTRUE(activo)) {
        return(sprintf(
          "Servidor Shiny activo | PID: %s | Puerto: %s | URL: http://127.0.0.1:%s/",
          estado$pid,
          estado$port,
          estado$port
        ))
      }

      "Existe registro previo, pero el proceso Shiny ya no está activo."
    },
    error = function(e) {
      paste("No se pudo consultar estado Shiny:", e$message)
    }
  )
}

#' Guardar estado del servidor Shiny local
#'
#' @param rutas list. Rutas principales.
#' @param pid integer. PID del proceso.
#' @param port integer. Puerto usado.
#' @return character. Ruta del archivo de estado.
guardar_estado_shiny_local_911 <- function(rutas, pid, port) {
  tryCatch(
    {
      pid_file <- obtener_pid_file_shiny_911(rutas)

      estado <- list(
        pid = as.integer(pid),
        port = as.integer(port),
        url = sprintf("http://127.0.0.1:%s/", as.integer(port)),
        started_at = Sys.time()
      )

      saveRDS(estado, pid_file)
      pid_file
    },
    error = function(e) {
      log_orq_911("WARN", paste("No se pudo guardar estado Shiny:", e$message))
      ""
    }
  )
}

#' Detener servidor Shiny local iniciado por el orquestador
#'
#' @param rutas list. Rutas principales.
#' @return logical. TRUE si se detuvo o no había proceso activo.
detener_shiny_local_menu_911 <- function(rutas) {
  tryCatch(
    {
      estado <- leer_estado_shiny_local_911(rutas)
      pid_file <- obtener_pid_file_shiny_911(rutas)

      if (is.null(estado)) {
        log_orq_911("SKIP", "No hay servidor Shiny registrado por el orquestador.")
        return(TRUE)
      }

      pid <- suppressWarnings(as.integer(estado$pid))

      if (!proceso_activo_orq_911(pid)) {
        log_orq_911("SKIP", "El PID registrado ya no está activo.")
        if (file.exists(pid_file)) {
          unlink(pid_file, force = TRUE)
        }
        return(TRUE)
      }

      if (.Platform$OS.type == "windows") {
        system2(
          "taskkill",
          args = c("/PID", as.character(pid), "/T", "/F"),
          stdout = TRUE,
          stderr = TRUE
        )
      } else {
        system2(
          "kill",
          args = c("-TERM", as.character(pid)),
          stdout = FALSE,
          stderr = FALSE
        )
      }

      Sys.sleep(1)

      if (file.exists(pid_file)) {
        unlink(pid_file, force = TRUE)
      }

      log_orq_911("OK", paste("Servidor Shiny detenido. PID:", pid))
      TRUE
    },
    error = function(e) {
      log_orq_911("ERROR", paste("No se pudo detener Shiny:", e$message))
      stop(e$message)
    }
  )
}

#' Crear función interna para cargar probar_shiny_local_911.R sin autoejecución
#'
#' Esta función se ejecuta dentro de un proceso R de fondo creado con callr.
#'
#' @return function. Función ejecutable por callr.
crear_funcion_shiny_background_911 <- function() {
  function(root, port, instalar_paquetes) {
    log_local <- function(nivel, mensaje) {
      cat(sprintf("[%s] [%s] %s\n", Sys.time(), nivel, mensaje))
      flush.console()
      invisible(NULL)
    }

    cargar_pipeline_controlado_local <- function(path, patrones_corte) {
      if (!file.exists(path)) {
        stop("No existe pipeline: ", path)
      }

      lineas <- readLines(path, warn = FALSE, encoding = "UTF-8")
      indices <- integer(0)

      for (patron in patrones_corte) {
        encontrados <- grep(patron, lineas)
        if (length(encontrados) > 0L) {
          indices <- c(indices, encontrados[[1]])
        }
      }

      if (length(indices) > 0L) {
        corte <- min(indices)
        if (corte > 1L) {
          lineas <- lineas[seq_len(corte - 1L)]
        }
      }

      env <- new.env(parent = globalenv())
      eval(parse(text = lineas), envir = env)
      env
    }

    root <- normalizePath(root, winslash = "/", mustWork = TRUE)
    path_pipeline <- file.path(root, "probar_shiny_local_911.R")

    log_local("INFO", paste("Proceso Shiny de fondo iniciado en raíz:", root))
    log_local("INFO", paste("Puerto:", port))

    env <- cargar_pipeline_controlado_local(
      path = path_pipeline,
      patrones_corte = c("^ejecutar_auto_pipeline_shiny_911\\(\\)")
    )

    if (!exists("ejecutar_pipeline_shiny_911", envir = env, inherits = FALSE)) {
      stop("probar_shiny_local_911.R no expone ejecutar_pipeline_shiny_911().")
    }

    env$ejecutar_pipeline_shiny_911(
      root = root,
      port = as.integer(port),
      launch_browser = FALSE,
      instalar_paquetes = isTRUE(instalar_paquetes),
      run_app = TRUE
    )
  }
}

#' Validar Shiny local desde el menú sin levantar servidor
#'
#' Ejecuta probar_shiny_local_911.R con run_app = FALSE. Esta validación
#' solo se invoca desde el menú local y no forma parte del flujo automático.
#'
#' @param root character. Ruta raíz del proyecto.
#' @param rutas list. Rutas principales.
#' @param port integer. Puerto local.
#' @param instalar_paquetes logical. Si TRUE instala paquetes faltantes.
#' @return character. Mensaje de validación.
validar_shiny_local_menu_911 <- function(root,
                                         rutas,
                                         port = 3838L,
                                         instalar_paquetes = FALSE) {
  tryCatch(
    {
      if (!file.exists(rutas$app_r)) {
        stop("No existe dashboard_911/app.R.")
      }

      if (!dashboard_insumos_completos_911(rutas)) {
        stop("No están completos dashboard_911/data y dashboard_911/www.")
      }

      env <- cargar_pipeline_controlado_911(
        path = file.path(root, "probar_shiny_local_911.R"),
        patrones_corte = c("^ejecutar_auto_pipeline_shiny_911\\(\\)")
      )

      if (!exists("ejecutar_pipeline_shiny_911", envir = env, inherits = FALSE)) {
        stop("El pipeline probar_shiny_local_911.R no expone ejecutar_pipeline_shiny_911().")
      }

      env$ejecutar_pipeline_shiny_911(
        root = root,
        port = port,
        launch_browser = FALSE,
        instalar_paquetes = instalar_paquetes,
        run_app = FALSE
      )

      log_orq_911("OK", "Shiny validado desde menú sin levantar servidor.")
      "Shiny validado desde menú sin levantar servidor."
    },
    error = function(e) {
      log_orq_911("ERROR", paste("No se pudo validar Shiny desde menú:", e$message))
      stop(e$message)
    }
  )
}


#' Levantar despliegue local de Shiny en segundo plano desde el menú
#'
#' Inicia la app Shiny en un proceso R separado para que el menú del
#' orquestador no pierda control de la consola.
#'
#' @param root character. Ruta raíz del proyecto.
#' @param rutas list. Rutas principales.
#' @param port integer. Puerto local.
#' @param instalar_paquetes logical. Si TRUE instala paquetes faltantes.
#' @return character. URL local abierta.
levantar_shiny_local_menu_911 <- function(root,
                                          rutas,
                                          port = 3838L,
                                          instalar_paquetes = FALSE) {
  tryCatch(
    {
      if (!file.exists(rutas$app_r)) {
        stop("No existe dashboard_911/app.R.")
      }

      if (!dashboard_insumos_completos_911(rutas)) {
        stop("No están completos dashboard_911/data y dashboard_911/www.")
      }

      asegurar_callr_orq_911(instalar_paquetes = instalar_paquetes)

      estado <- leer_estado_shiny_local_911(rutas)

      if (!is.null(estado) && proceso_activo_orq_911(estado$pid)) {
        url_existente <- sprintf("http://127.0.0.1:%s/", estado$port)
        log_orq_911(
          "SKIP",
          paste("Ya existe un servidor Shiny activo. PID:", estado$pid, "| URL:", url_existente)
        )
        utils::browseURL(url_existente)
        return(url_existente)
      }

      pid_file <- obtener_pid_file_shiny_911(rutas)
      log_file <- obtener_log_file_shiny_911(rutas)

      if (file.exists(pid_file)) {
        unlink(pid_file, force = TRUE)
      }

      if (file.exists(log_file)) {
        unlink(log_file, force = TRUE)
      }

      proceso <- callr::r_bg(
        func = crear_funcion_shiny_background_911(),
        args = list(
          root = root,
          port = as.integer(port),
          instalar_paquetes = isTRUE(instalar_paquetes)
        ),
        stdout = log_file,
        stderr = log_file,
        supervise = FALSE
      )

      pid <- proceso$get_pid()
      guardar_estado_shiny_local_911(rutas, pid = pid, port = port)

      url <- sprintf("http://127.0.0.1:%s/", as.integer(port))

      log_orq_911("OK", paste("Servidor Shiny lanzado en segundo plano. PID:", pid))
      log_orq_911("INFO", paste("Log Shiny:", normalizePath(log_file, winslash = "/", mustWork = FALSE)))
      log_orq_911("INFO", paste("URL local:", url))
      log_orq_911("INFO", "El menú seguirá disponible. Usa la opción 4 para detener Shiny.")

      Sys.sleep(2)
      utils::browseURL(url)

      url
    },
    error = function(e) {
      log_orq_911("ERROR", paste("No se pudo levantar Shiny local en segundo plano:", e$message))
      stop(e$message)
    }
  )
}

#' Mostrar log del servidor Shiny local
#'
#' @param rutas list. Rutas principales.
#' @return character. Ruta del log.
mostrar_log_shiny_local_911 <- function(rutas) {
  tryCatch(
    {
      log_file <- obtener_log_file_shiny_911(rutas)

      if (!file.exists(log_file)) {
        log_orq_911("SKIP", "Todavía no existe log del servidor Shiny local.")
        return(log_file)
      }

      log_orq_911("INFO", paste("Log Shiny:", normalizePath(log_file, winslash = "/", mustWork = FALSE)))
      utils::file.show(log_file, title = "Log Shiny local 911")
      log_file
    },
    error = function(e) {
      log_orq_911("ERROR", paste("No se pudo mostrar log Shiny:", e$message))
      stop(e$message)
    }
  )
}


#' Mostrar menú local de despliegues
#'
#' Permite abrir localmente el reporte HTML de R Markdown o levantar
#' la aplicación Shiny. El menú solo se muestra en sesiones interactivas.
#'
#' @param root character. Ruta raíz del proyecto.
#' @param rutas list. Rutas principales.
#' @param port integer. Puerto local de Shiny.
#' @param instalar_paquetes logical. Si TRUE instala paquetes faltantes.
#' @param habilitar_menu logical. Si TRUE muestra el menú.
#' @return NULL invisible.
mostrar_menu_despliegues_locales_911 <- function(root,
                                                 rutas,
                                                 port = 3838L,
                                                 instalar_paquetes = FALSE,
                                                 habilitar_menu = TRUE) {
  tryCatch(
    {
      if (!isTRUE(habilitar_menu)) {
        log_orq_911("SKIP", "Menú local desactivado.")
        return(invisible(NULL))
      }

      if (!interactive()) {
        log_orq_911("SKIP", "Sesión no interactiva; no se muestra menú local.")
        return(invisible(NULL))
      }

      repeat {
        cat("\n============================================================\n")
        cat("MENÚ LOCAL DE DESPLIEGUES 911\n")
        cat("============================================================\n")
        cat("1. Abrir reporte R Markdown local: docs/index.html\n")
        cat("2. Validar app Shiny local sin levantar servidor\n")
        cat("3. Levantar app Shiny local en segundo plano: dashboard_911/app.R\n")
        cat("4. Detener servidor Shiny local iniciado por el orquestador\n")
        cat("5. Consultar estado del servidor Shiny local\n")
        cat("0. Salir sin abrir despliegues\n")
        cat("============================================================\n")

        opcion <- trimws(readline("Selecciona una opción [0-5]: "))

        if (identical(opcion, "1")) {
          abrir_markdown_local_911(rutas)
          next
        }

        if (identical(opcion, "2")) {
          validar_shiny_local_menu_911(
            root = root,
            rutas = rutas,
            port = port,
            instalar_paquetes = instalar_paquetes
          )
          next
        }

        if (identical(opcion, "3")) {
          levantar_shiny_local_menu_911(
            root = root,
            rutas = rutas,
            port = port,
            instalar_paquetes = instalar_paquetes
          )
          next
        }

        if (identical(opcion, "4")) {
          detener_shiny_local_menu_911(rutas)
          next
        }

        if (identical(opcion, "5")) {
          log_orq_911("INFO", estado_shiny_local_911(rutas))
          next
        }

        if (identical(opcion, "0") || identical(toupper(opcion), "Q")) {
          log_orq_911("OK", "Menú local cerrado por el usuario.")
          break
        }

        log_orq_911("WARN", "Opción inválida. Usa 0, 1, 2, 3, 4 o 5.")
      }

      invisible(NULL)
    },
    error = function(e) {
      log_orq_911("ERROR", paste("Error en menú local:", e$message))
      stop(e$message)
    }
  )
}


# ------------------------------------------------------------
# 7. Orquestador principal
# ------------------------------------------------------------

#' Orquestar proyecto 911 completo
#'
#' @param root character | NULL. Ruta raíz del proyecto.
#' @param forzar_eda logical. Si TRUE ejecuta EDA aunque existan salidas.
#' @param forzar_preparar logical. Si TRUE vuelve a preparar dashboard.
#' @param forzar_index logical. Si TRUE sobrescribe index.Rmd.
#' @param forzar_render logical. Si TRUE vuelve a renderizar HTML.
#' @param forzar_app logical. Si TRUE sobrescribe app.R.
#' @param limpiar_dashboard logical. Si TRUE limpia data/ y www/ al preparar.
#' @param abrir_html logical. Si TRUE abre HTML después de renderizar.
#' @param ejecutar_shiny logical. Conservado por compatibilidad; no ejecuta Shiny automáticamente.
#' @param mostrar_menu logical. Si TRUE muestra un menú local al final.
#' @param port integer. Puerto local de Shiny.
#' @param instalar_paquetes logical. Si TRUE permite instalar paquetes requeridos.
#' @return list. Resumen de ejecución.
orquestar_proyecto_911 <- function(root = NULL,
                                   forzar_eda = FALSE,
                                   forzar_preparar = FALSE,
                                   forzar_index = FALSE,
                                   forzar_render = FALSE,
                                   forzar_app = FALSE,
                                   limpiar_dashboard = FALSE,
                                   abrir_html = FALSE,
                                   ejecutar_shiny = FALSE,
                                   mostrar_menu = TRUE,
                                   port = 3838L,
                                   instalar_paquetes = TRUE) {
  tryCatch(
    {
      root <- resolver_root_orq_911(root)
      rutas <- construir_rutas_orq_911(root)

      cat("\n============================================================\n")
      cat("ORQUESTADOR PROYECTO 911 CDMX\n")
      cat("============================================================\n")
      cat("Raíz del proyecto:", root, "\n")
      cat("Ejecutar Shiny directo al final:", ejecutar_shiny, "\n")
      cat("Mostrar menú local al final:", mostrar_menu, "\n")
      cat("Nota: probar_shiny_local_911.R solo se ejecuta desde el menú.\n")
      cat("Puerto Shiny:", port, "\n")
      cat("============================================================\n\n")

      validar_pipelines_oficiales_911(root)

      resultados <- list()

      resultados$eda <- paso_eda_911(
        root = root,
        rutas = rutas,
        forzar = forzar_eda,
        instalar_paquetes = instalar_paquetes
      )

      resultados$app <- paso_crear_app_911(
        root = root,
        rutas = rutas,
        forzar = forzar_app
      )

      resultados$preparar <- paso_preparar_dashboard_911(
        root = root,
        rutas = rutas,
        forzar = forzar_preparar,
        limpiar_dashboard = limpiar_dashboard
      )

      resultados$index <- paso_crear_index_911(
        root = root,
        rutas = rutas,
        forzar = forzar_index
      )

      resultados$render <- paso_render_911(
        root = root,
        rutas = rutas,
        forzar = forzar_render,
        abrir_html = abrir_html
      )

      resultados$shiny <- list(
        nombre = "6. Shiny local",
        estado = "MENU",
        resultado = "probar_shiny_local_911.R no se ejecuta automáticamente; usar menú local."
      )

      if (isTRUE(ejecutar_shiny)) {
        log_orq_911(
          "WARN",
          "ORQ_911_RUN_SHINY ya no ejecuta Shiny automáticamente. Usa el menú local opción 3 o 4."
        )
      }

      mostrar_menu_despliegues_locales_911(
        root = root,
        rutas = rutas,
        port = port,
        instalar_paquetes = instalar_paquetes,
        habilitar_menu = mostrar_menu
      )

      cat("\n============================================================\n")
      cat("ORQUESTACIÓN TERMINADA\n")
      cat("============================================================\n")
      cat("HTML GitHub Pages:", rutas$docs_html, "\n")
      cat("App Shiny:", rutas$app_r, "\n")
      cat("Nota Shiny: probar_shiny_local_911.R se ejecuta solo desde el menú local.\n")
      cat("Datos Shiny:", rutas$data, "\n")
      cat("Gráficos Shiny:", rutas$www, "\n")
      cat("============================================================\n\n")

      invisible(resultados)
    },
    error = function(e) {
      log_orq_911("ERROR", paste("Orquestación detenida:", e$message))
      stop(e$message)
    }
  )
}

# ------------------------------------------------------------
# 8. Ejecución automática controlada
# ------------------------------------------------------------

#' Ejecutar orquestador automáticamente
#'
#' @return list | NULL. Resultado de la orquestación si se ejecuta.
ejecutar_auto_orquestador_911 <- function() {
  tryCatch(
    {
      auto <- as_logical_orq_911(
        Sys.getenv("ORQ_911_AUTO", unset = "TRUE"),
        default = TRUE
      )

      if (!isTRUE(auto)) {
        log_orq_911("INFO", "Ejecución automática desactivada.")
        return(invisible(NULL))
      }

      orquestar_proyecto_911(
        root = NULL,
        forzar_eda = as_logical_orq_911(Sys.getenv("ORQ_911_FORZAR_EDA", unset = "FALSE")),
        forzar_preparar = as_logical_orq_911(Sys.getenv("ORQ_911_FORZAR_PREPARAR", unset = "FALSE")),
        forzar_index = as_logical_orq_911(Sys.getenv("ORQ_911_FORZAR_INDEX", unset = "FALSE")),
        forzar_render = as_logical_orq_911(Sys.getenv("ORQ_911_FORZAR_RENDER", unset = "FALSE")),
        forzar_app = as_logical_orq_911(Sys.getenv("ORQ_911_FORZAR_APP", unset = "FALSE")),
        limpiar_dashboard = as_logical_orq_911(Sys.getenv("ORQ_911_LIMPIAR_DASHBOARD", unset = "FALSE")),
        abrir_html = as_logical_orq_911(Sys.getenv("ORQ_911_ABRIR_HTML", unset = "FALSE")),
        ejecutar_shiny = FALSE,
        mostrar_menu = as_logical_orq_911(Sys.getenv("ORQ_911_MENU", unset = "TRUE"), default = TRUE),
        port = as_port_orq_911(Sys.getenv("ORQ_911_PORT", unset = "3838")),
        instalar_paquetes = as_logical_orq_911(Sys.getenv("ORQ_911_INSTALL", unset = "TRUE"), default = TRUE)
      )
    },
    error = function(e) {
      log_orq_911("ERROR", paste("Error en ejecución automática:", e$message))
      stop(e$message)
    }
  )
}

ejecutar_auto_orquestador_911()
