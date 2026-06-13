# ============================================================
# Render R Markdown para GitHub Pages - App_R_tidyr_lubridate_dplyr_markdown
#
# Propósito:
# Genera dashboard_911/index.html a partir de dashboard_911/index.Rmd
# después de ejecutar la etapa de preparación de insumos.
#
# Uso recomendado:
#   source("render_rmarkdown_github_pages_911.R")
#
# Secuencia recomendada:
#   1) source("run_pipeline_911_shiny.R")
#   2) run_pipeline_911_shiny(ejecutar_eda = TRUE, ejecutar_shiny = FALSE)
#   3) source("render_rmarkdown_github_pages_911.R")
# ============================================================

#' Registrar evento de renderizado
#'
#' @param nivel character. Nivel del evento.
#' @param mensaje character. Mensaje del evento.
#' @return NULL de forma invisible.
log_render <- function(nivel, mensaje) {
  cat(sprintf("[%s] [%s] %s\n", Sys.time(), nivel, mensaje))
  invisible(NULL)
}

#' Obtener ruta del script actual
#'
#' @return character con la ruta del script o NA_character_.
obtener_ruta_script_render <- function() {
  tryCatch(
    {
      frames <- sys.frames()

      for (i in rev(seq_along(frames))) {
        if (!is.null(frames[[i]]$ofile)) {
          return(normalizePath(frames[[i]]$ofile, winslash = "/", mustWork = FALSE))
        }
      }

      NA_character_
    },
    error = function(e) NA_character_
  )
}

#' Resolver raíz del proyecto
#'
#' @param root character o NULL. Carpeta raíz del proyecto.
#' @return character con ruta normalizada.
resolver_root_render <- function(root = NULL) {
  if (!is.null(root) && nzchar(root)) {
    if (!dir.exists(root)) {
      stop("La carpeta root no existe: ", root)
    }

    return(normalizePath(root, winslash = "/", mustWork = TRUE))
  }

  ruta_script <- obtener_ruta_script_render()

  if (!is.na(ruta_script) && nzchar(ruta_script)) {
    return(dirname(ruta_script))
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

#' Validar paquete requerido
#'
#' @param paquete character. Nombre del paquete.
#' @return TRUE si está instalado.
validar_paquete_render <- function(paquete) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    stop(
      "Falta instalar el paquete requerido: ", paquete,
      ". Ejecuta install.packages('", paquete, "')."
    )
  }

  TRUE
}

#' Validar insumos del reporte estático
#'
#' @param dashboard_dir character. Carpeta dashboard_911.
#' @return list con rutas y archivos detectados.
validar_insumos_rmarkdown <- function(dashboard_dir) {
  index_rmd <- file.path(dashboard_dir, "index.Rmd")
  data_dir <- file.path(dashboard_dir, "data")
  www_dir <- file.path(dashboard_dir, "www")

  if (!dir.exists(dashboard_dir)) {
    stop("No existe la carpeta dashboard_911/: ", dashboard_dir)
  }

  if (!file.exists(index_rmd)) {
    stop("No existe index.Rmd: ", index_rmd)
  }

  if (!dir.exists(data_dir)) {
    stop("No existe data/. Ejecuta primero preparar_dashboard_shiny_911.R.")
  }

  if (!dir.exists(www_dir)) {
    stop("No existe www/. Ejecuta primero preparar_dashboard_shiny_911.R.")
  }

  csv <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
  png <- list.files(www_dir, pattern = "\\.png$", full.names = TRUE)

  if (length(csv) == 0) {
    stop("No hay CSV en data/. Ejecuta primero el pipeline de preparación.")
  }

  if (length(png) == 0) {
    stop("No hay PNG en www/. Ejecuta primero el pipeline de preparación.")
  }

  log_render("INFO", paste("CSV detectados:", length(csv)))
  log_render("INFO", paste("PNG detectados:", length(png)))

  list(
    index_rmd = index_rmd,
    data_dir = data_dir,
    www_dir = www_dir,
    csv = csv,
    png = png
  )
}


#' Copiar directorio de forma recursiva y segura
#'
#' @param origen character. Carpeta origen.
#' @param destino character. Carpeta destino.
#' @return TRUE de forma invisible.
copiar_directorio_render <- function(origen, destino) {
  if (!dir.exists(origen)) {
    stop("No existe el directorio origen: ", origen)
  }

  if (dir.exists(destino)) {
    unlink(destino, recursive = TRUE, force = TRUE)
  }

  dir.create(destino, recursive = TRUE, showWarnings = FALSE)

  archivos <- list.files(origen, all.files = TRUE, recursive = TRUE, full.names = TRUE, no.. = TRUE)

  for (archivo in archivos) {
    relativo <- substring(archivo, nchar(origen) + 2)
    destino_archivo <- file.path(destino, relativo)

    if (dir.exists(archivo)) {
      dir.create(destino_archivo, recursive = TRUE, showWarnings = FALSE)
    } else {
      dir.create(dirname(destino_archivo), recursive = TRUE, showWarnings = FALSE)
      file.copy(archivo, destino_archivo, overwrite = TRUE)
    }
  }

  invisible(TRUE)
}

#' Preparar carpeta docs/ para GitHub Pages desde dashboard_911/
#'
#' @param root character. Carpeta raíz del proyecto.
#' @param dashboard_dir character. Carpeta dashboard_911.
#' @return character. Ruta de docs/index.html.
preparar_docs_desde_dashboard_render <- function(root, dashboard_dir) {
  docs_dir <- file.path(root, "docs")
  docs_html <- file.path(docs_dir, "index.html")
  dashboard_html <- file.path(dashboard_dir, "index.html")

  if (!file.exists(dashboard_html)) {
    stop("No existe el HTML renderizado en dashboard_911/: ", dashboard_html)
  }

  dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)

  file.copy(dashboard_html, docs_html, overwrite = TRUE)

  data_origen <- file.path(dashboard_dir, "data")
  www_origen <- file.path(dashboard_dir, "www")

  if (dir.exists(data_origen)) {
    copiar_directorio_render(data_origen, file.path(docs_dir, "data"))
  }

  if (dir.exists(www_origen)) {
    copiar_directorio_render(www_origen, file.path(docs_dir, "www"))
  }

  writeLines("", file.path(docs_dir, ".nojekyll"))

  if (!file.exists(docs_html)) {
    stop("No se pudo crear docs/index.html.")
  }

  log_render("INFO", paste("docs/index.html actualizado:", docs_html))
  docs_html
}

#' Abrir HTML local en navegador
#'
#' @param html_path character. Ruta del HTML.
#' @return NULL de forma invisible.
abrir_html_local_render <- function(html_path) {
  if (!file.exists(html_path)) {
    stop("No existe el HTML local para abrir: ", html_path)
  }

  log_render("INFO", paste("Abriendo HTML local:", html_path))
  utils::browseURL(html_path)

  invisible(NULL)
}


#' Renderizar reporte R Markdown para GitHub Pages
#'
#' @param root character o NULL. Carpeta raíz del proyecto App_R_tidyr_lubridate_dplyr_markdown.
#' @param actualizar_docs logical. Si TRUE, copia index.html, data/ y www/ a docs/.
#' @param abrir_html logical. Si TRUE, abre localmente docs/index.html si existe; si no, dashboard_911/index.html.
#' @return character. Ruta del HTML generado.
render_rmarkdown_github_pages_911 <- function(root = NULL,
                                             actualizar_docs = TRUE,
                                             abrir_html = TRUE) {
  validar_paquete_render("rmarkdown")
  validar_paquete_render("knitr")
  validar_paquete_render("readr")
  validar_paquete_render("dplyr")

  root <- resolver_root_render(root)
  dashboard_dir <- file.path(root, "dashboard_911")
  insumos <- validar_insumos_rmarkdown(dashboard_dir)

  output_html <- file.path(dashboard_dir, "index.html")

  log_render("INFO", paste("Raíz del proyecto:", root))
  log_render("INFO", paste("Renderizando:", insumos$index_rmd))

  tryCatch(
    {
      rmarkdown::render(
        input = insumos$index_rmd,
        output_file = "index.html",
        output_dir = dashboard_dir,
        knit_root_dir = dashboard_dir,
        envir = new.env(parent = globalenv()),
        quiet = FALSE
      )

      if (!file.exists(output_html)) {
        stop("El render terminó, pero no se encontró index.html: ", output_html)
      }

      log_render("INFO", paste("HTML generado:", output_html))

      html_para_abrir <- output_html

      if (isTRUE(actualizar_docs)) {
        html_para_abrir <- preparar_docs_desde_dashboard_render(
          root = root,
          dashboard_dir = dashboard_dir
        )
      }

      if (isTRUE(abrir_html)) {
        abrir_html_local_render(html_para_abrir)
      }

      output_html
    },
    error = function(e) {
      stop("Error al renderizar index.Rmd: ", e$message)
    }
  )
}

# ------------------------------------------------------------
# Ejecución directa al hacer source()
# ------------------------------------------------------------

tryCatch(
  {
    render_rmarkdown_github_pages_911()
  },
  error = function(e) {
    log_render("ERROR", e$message)
    stop(e)
  }
)
