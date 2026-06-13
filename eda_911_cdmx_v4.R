# ============================================================
# EDA 911 CDMX
# Analisis exploratorio con dplyr, tidyr y lubridate
# Version v4: ajuste visual de graficas de tareas 2, 4 y 5
# Datasets:
#   - llamadas_911_2021_s1.csv
#   - llamadas_911_2021_s2.csv
#   - llamadas_911_2022_s1.csv
#   - diccionario-de-datos-911-diccionario-de-datos.csv
#
# Propósito:
#   Resolver las cinco tareas de la actividad:
#   1) Incidentes por mes, alcaldia y promedio por categoria.
#   2) Dia de la semana con mas incidentes, con eje Y legible en el grafico.
#   3) Distribucion por hora para DELITO, EMERGENCIA y URGENCIAS MEDICAS.
#   4) Tiempo entre creacion y cierre.
#   5) Porcentaje de llamadas clasificadas como FALSA ALARMA.
# ============================================================

# ------------------------------------------------------------
# 0. CONFIGURACION GENERAL
# ------------------------------------------------------------

#' Registrar eventos del proceso
#'
#' Imprime una bitacora simple con fecha-hora, nivel y mensaje.
#'
#' @param nivel character. Nivel del evento: INFO, WARN o ERROR.
#' @param mensaje character. Descripcion del evento.
#' @return No devuelve valor explicito; imprime en consola.
log_event <- function(nivel, mensaje) {
  cat(sprintf("[%s] [%s] %s\n", Sys.time(), nivel, mensaje))
}


#' Obtener directorio del script
#'
#' Intenta identificar la carpeta donde se encuentra este archivo R.
#' Si no puede determinarla, devuelve el directorio de trabajo actual.
#'
#' @return character. Directorio base recomendado para buscar los CSV.
obtener_directorio_script <- function() {
  ruta_script <- NA_character_

  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  idx <- grep(file_arg, args)

  if (length(idx) > 0) {
    ruta_script <- normalizePath(sub(file_arg, "", args[idx[1]]), winslash = "/", mustWork = FALSE)
  }

  if (is.na(ruta_script)) {
    ruta_script <- tryCatch(
      {
        ofile <- sys.frames()[[1]]$ofile
        if (!is.null(ofile)) {
          normalizePath(ofile, winslash = "/", mustWork = FALSE)
        } else {
          NA_character_
        }
      },
      error = function(e) NA_character_
    )
  }

  if (is.na(ruta_script) && requireNamespace("rstudioapi", quietly = TRUE)) {
    ruta_script <- tryCatch(
      {
        contexto <- rstudioapi::getActiveDocumentContext()
        if (!is.null(contexto$path) && nzchar(contexto$path)) {
          normalizePath(contexto$path, winslash = "/", mustWork = FALSE)
        } else {
          NA_character_
        }
      },
      error = function(e) NA_character_
    )
  }

  if (!is.na(ruta_script) && nzchar(ruta_script)) {
    return(dirname(ruta_script))
  }

  getwd()
}

#' Validar e instalar paquetes requeridos
#'
#' Revisa si los paquetes necesarios estan disponibles. Si instalar = TRUE,
#' intenta instalarlos desde CRAN cuando no se encuentran.
#'
#' @param paquetes character vector. Nombres de paquetes requeridos.
#' @param instalar logical. Indica si se deben instalar paquetes faltantes.
#' @return TRUE si los paquetes quedan disponibles; lanza error si falla.
validar_paquetes <- function(paquetes, instalar = TRUE) {
  for (pkg in paquetes) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      if (isTRUE(instalar)) {
        log_event("WARN", paste("Paquete faltante. Intentando instalar:", pkg))
        install.packages(pkg, dependencies = TRUE)
      } else {
        stop(sprintf("Falta instalar el paquete: %s", pkg))
      }
    }

    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("No fue posible cargar o instalar el paquete: %s", pkg))
    }
  }

  log_event("INFO", "Paquetes validados correctamente.")
  TRUE
}

#' Cargar paquetes requeridos
#'
#' Carga en memoria los paquetes necesarios para ejecutar el analisis.
#'
#' @param paquetes character vector. Nombres de paquetes a cargar.
#' @return TRUE si todos los paquetes se cargan correctamente.
cargar_paquetes <- function(paquetes) {
  for (pkg in paquetes) {
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE)
    )
  }

  log_event("INFO", "Paquetes cargados correctamente.")
  TRUE
}

#' Crear carpeta de salida
#'
#' Crea una carpeta para tablas y graficos generados por el analisis.
#'
#' @param directorio_base character. Carpeta base donde se creara la salida.
#' @return character. Ruta de la carpeta de salida.
crear_directorio_salida <- function(directorio_base) {
  fecha <- format(Sys.Date(), "%Y%m%d")
  out_dir <- file.path(directorio_base, paste0("salidas_911_eda_", fecha))

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  log_event("INFO", paste("Carpeta de salida:", out_dir))
  out_dir
}

#' Resolver ruta de archivo
#'
#' Busca un archivo en el directorio indicado. Si no existe y la sesion es
#' interactiva, permite seleccionarlo manualmente.
#'
#' @param nombre_archivo character. Nombre esperado del archivo.
#' @param directorio_datos character. Carpeta donde se buscaran los datos.
#' @return character. Ruta completa del archivo.
resolver_ruta_archivo <- function(nombre_archivo, directorio_datos) {
  ruta <- file.path(directorio_datos, nombre_archivo)

  if (file.exists(ruta)) {
    return(ruta)
  }

  mensaje <- paste(
    "No se encontro el archivo", nombre_archivo,
    "en", directorio_datos
  )
  log_event("WARN", mensaje)

  if (interactive()) {
    cat("\nSelecciona manualmente el archivo: ", nombre_archivo, "\n", sep = "")
    ruta_manual <- file.choose()

    if (!file.exists(ruta_manual)) {
      stop(sprintf("La ruta seleccionada no existe: %s", ruta_manual))
    }

    return(ruta_manual)
  }

  stop(mensaje)
}

#' Guardar tabla CSV
#'
#' Escribe una tabla en formato CSV dentro de la carpeta de salida.
#'
#' @param tabla data.frame. Tabla a guardar.
#' @param ruta character. Ruta final del archivo CSV.
#' @return character. Ruta del archivo guardado.
guardar_csv <- function(tabla, ruta) {
  tryCatch(
    {
      readr::write_csv(tabla, ruta, na = "")
      log_event("INFO", paste("CSV guardado:", ruta))
      ruta
    },
    error = function(e) {
      log_event("ERROR", paste("No se pudo guardar CSV:", e$message))
      NULL
    }
  )
}

#' Guardar grafico ggplot
#'
#' Guarda un grafico en PNG con dimensiones estandar.
#'
#' @param grafico ggplot. Objeto grafico.
#' @param ruta character. Ruta de salida.
#' @param ancho numeric. Ancho en pulgadas.
#' @param alto numeric. Alto en pulgadas.
#' @return character. Ruta del archivo guardado o NULL si falla.
guardar_grafico <- function(grafico, ruta, ancho = 10, alto = 6) {
  tryCatch(
    {
      ggplot2::ggsave(
        filename = ruta,
        plot = grafico,
        width = ancho,
        height = alto,
        dpi = 150
      )
      log_event("INFO", paste("Grafico guardado:", ruta))
      ruta
    },
    error = function(e) {
      log_event("ERROR", paste("No se pudo guardar grafico:", e$message))
      NULL
    }
  )
}

#' Formatear numero para consola
#'
#' Convierte numeros a texto con separador de miles y decimales controlados.
#'
#' @param x numeric. Valor numerico.
#' @param digitos integer. Numero de decimales.
#' @return character. Valor formateado.
formatear_numero <- function(x, digitos = 2) {
  ifelse(
    is.na(x),
    "NA",
    format(
      round(x, digitos),
      nsmall = digitos,
      big.mark = ",",
      trim = TRUE
    )
  )
}


#' Formatear valores enteros para ejes de graficos
#'
#' Convierte valores numericos a texto sin notacion cientifica y con
#' separador de miles. Es util para ejes de frecuencia con conteos altos.
#'
#' @param x numeric. Vector de valores del eje.
#' @return character. Valores formateados para etiquetas del eje.
formatear_eje_entero <- function(x) {
  ifelse(
    is.na(x),
    "NA",
    format(
      round(x, 0),
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    )
  )
}


#' Formatear porcentajes para graficos
#'
#' Convierte un valor numerico en porcentaje legible, conservando separador
#' de miles y control de decimales.
#'
#' @param x numeric. Vector de porcentajes.
#' @param digitos integer. Numero de decimales.
#' @return character. Valores formateados con simbolo porcentual.
formatear_porcentaje <- function(x, digitos = 2) {
  paste0(formatear_numero(x, digitos), "%")
}

# ------------------------------------------------------------
# 1. CARGA Y PREPARACION DE DATOS
# ------------------------------------------------------------

#' Normalizar texto categorico
#'
#' Convierte texto a mayusculas, elimina acentos cuando es posible,
#' recorta espacios externos y compacta espacios internos.
#'
#' @param x vector. Texto a normalizar.
#' @return character vector. Texto normalizado.
normalizar_texto <- function(x) {
  x <- as.character(x)

  # Limpieza defensiva para archivos con acentos o codificaciones mixtas.
  # sub = "" evita que iconv devuelva NA ante bytes no convertibles.
  x <- suppressWarnings(iconv(x, from = "", to = "UTF-8", sub = ""))
  x <- trimws(x)

  x_ascii <- tryCatch(
    {
      suppressWarnings(iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = ""))
    },
    error = function(e) x
  )

  x <- toupper(x_ascii)
  x <- gsub("[^A-Z0-9]+", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)

  x[is.na(x) | x == ""] <- NA_character_
  x
}

#' Crear etiqueta legible de categoria
#'
#' Devuelve etiquetas con acentos para reporte y graficos.
#'
#' @param categoria character vector. Categoria normalizada.
#' @return character vector. Categoria con etiqueta legible.
crear_etiqueta_categoria <- function(categoria) {
  dplyr::case_when(
    categoria == "URGENCIAS MEDICAS" ~ "URGENCIAS MÉDICAS",
    categoria == "URGENCIA MEDICA" ~ "URGENCIAS MÉDICAS",
    categoria == "FALTA CIVICA" ~ "FALTA CÍVICA",
    TRUE ~ categoria
  )
}


#' Normalizar clasificacion 911
#'
#' Estandariza las categorias esperadas en la actividad, incluso cuando
#' hay variantes por acentos o codificacion.
#'
#' @param x vector. Clasificacion original.
#' @return character vector. Categoria estandarizada.
normalizar_categoria_911 <- function(x) {
  categoria <- normalizar_texto(x)

  dplyr::case_when(
    !is.na(categoria) & categoria == "DELITO" ~ "DELITO",
    !is.na(categoria) & categoria == "EMERGENCIA" ~ "EMERGENCIA",
    !is.na(categoria) & grepl("^SERVICIO", categoria) ~ "SERVICIO",
    !is.na(categoria) & grepl("^URGENCIA", categoria) ~ "URGENCIAS MEDICAS",
    !is.na(categoria) & grepl("^FALSA", categoria) ~ "FALSA ALARMA",
    !is.na(categoria) & grepl("^FALTA", categoria) ~ "FALTA CIVICA",
    TRUE ~ categoria
  )
}

#' Normalizar alcaldia 911
#'
#' Estandariza nombres de alcaldias para evitar separaciones por acentos,
#' puntos o variantes de codificacion.
#'
#' @param x vector. Alcaldia original.
#' @return character vector. Alcaldia estandarizada.
normalizar_alcaldia_911 <- function(x) {
  alcaldia <- normalizar_texto(x)

  dplyr::case_when(
    !is.na(alcaldia) & grepl("ALVARO", alcaldia) ~ "ALVARO OBREGON",
    !is.na(alcaldia) & grepl("AZCAPOTZALCO", alcaldia) ~ "AZCAPOTZALCO",
    !is.na(alcaldia) & grepl("BENITO", alcaldia) ~ "BENITO JUAREZ",
    !is.na(alcaldia) & grepl("COYOACAN", alcaldia) ~ "COYOACAN",
    !is.na(alcaldia) & grepl("CUAJIMALPA", alcaldia) ~ "CUAJIMALPA DE MORELOS",
    !is.na(alcaldia) & grepl("CUAUHTEMOC", alcaldia) ~ "CUAUHTEMOC",
    !is.na(alcaldia) & grepl("GUSTAVO", alcaldia) ~ "GUSTAVO A MADERO",
    !is.na(alcaldia) & grepl("IZTACALCO", alcaldia) ~ "IZTACALCO",
    !is.na(alcaldia) & grepl("IZTAPALAPA", alcaldia) ~ "IZTAPALAPA",
    !is.na(alcaldia) & grepl("MAGDALENA", alcaldia) ~ "LA MAGDALENA CONTRERAS",
    !is.na(alcaldia) & grepl("MIGUEL", alcaldia) ~ "MIGUEL HIDALGO",
    !is.na(alcaldia) & grepl("MILPA", alcaldia) ~ "MILPA ALTA",
    !is.na(alcaldia) & grepl("TLAHUAC", alcaldia) ~ "TLAHUAC",
    !is.na(alcaldia) & grepl("TLALPAN", alcaldia) ~ "TLALPAN",
    !is.na(alcaldia) & grepl("VENUSTIANO", alcaldia) ~ "VENUSTIANO CARRANZA",
    !is.na(alcaldia) & grepl("XOCHIMILCO", alcaldia) ~ "XOCHIMILCO",
    TRUE ~ alcaldia
  )
}

#' Crear fecha-hora con lubridate
#'
#' Combina columnas de fecha y hora y las convierte a POSIXct.
#' Acepta formatos ISO comunes y formato dia/mes/anio.
#'
#' @param fecha vector. Fecha en texto.
#' @param hora vector. Hora en texto.
#' @param zona_horaria character. Zona horaria para interpretar el dato.
#' @return POSIXct vector. Fecha-hora parseada.
crear_datetime <- function(fecha, hora, zona_horaria = "America/Mexico_City") {
  texto <- paste(as.character(fecha), as.character(hora))

  lubridate::parse_date_time(
    texto,
    orders = c("ymd HMS", "ymd HM", "dmy HMS", "dmy HM"),
    tz = zona_horaria,
    quiet = TRUE
  )
}

#' Crear dia de semana en espanol
#'
#' Obtiene el dia de semana a partir de una fecha, iniciando la semana en lunes.
#'
#' @param fecha Date vector. Fecha de referencia.
#' @return ordered factor. Dia de semana.
crear_dia_semana <- function(fecha) {
  dias <- c(
    "lunes", "martes", "miércoles", "jueves",
    "viernes", "sábado", "domingo"
  )

  indice <- lubridate::wday(fecha, week_start = 1)
  factor(dias[indice], levels = dias, ordered = TRUE)
}

#' Leer un archivo CSV de llamadas 911
#'
#' Lee un archivo de llamadas 911 como texto para evitar conversiones
#' indeseadas, especialmente en folio y manzana.
#'
#' @param ruta character. Ruta del archivo CSV.
#' @param periodo_archivo character. Identificador del periodo de origen.
#' @return tibble. Datos leidos con columna periodo_archivo.
leer_csv_911 <- function(ruta, periodo_archivo) {
  tryCatch(
    {
      datos <- readr::read_csv(
        file = ruta,
        col_types = readr::cols(.default = readr::col_character()),
        locale = readr::locale(encoding = "UTF-8"),
        show_col_types = FALSE
      )

      datos <- datos |>
        dplyr::mutate(periodo_archivo = periodo_archivo)

      log_event(
        "INFO",
        paste("Archivo cargado:", basename(ruta), "| filas:", nrow(datos))
      )

      datos
    },
    error = function(e) {
      stop(sprintf("No se pudo leer %s: %s", ruta, e$message))
    }
  )
}

#' Leer diccionario de datos
#'
#' Lee el diccionario de datos si esta disponible. Su uso es informativo:
#' permite contrastar la estructura esperada con la base recibida.
#'
#' @param directorio_datos character. Carpeta de datos.
#' @param out_dir character. Carpeta de salida.
#' @return tibble o NULL. Diccionario leido, si existe.
leer_diccionario <- function(directorio_datos, out_dir) {
  nombre_diccionario <- "diccionario-de-datos-911-diccionario-de-datos.csv"
  ruta <- file.path(directorio_datos, nombre_diccionario)

  if (!file.exists(ruta)) {
    log_event("WARN", "Diccionario de datos no encontrado. Se continua con los CSV de llamadas.")
    return(NULL)
  }

  diccionario <- tryCatch(
    {
      readr::read_csv(
        file = ruta,
        locale = readr::locale(encoding = "UTF-8"),
        show_col_types = FALSE
      )
    },
    error = function(e) {
      log_event("WARN", paste("No se pudo leer el diccionario:", e$message))
      NULL
    }
  )

  if (!is.null(diccionario)) {
    guardar_csv(diccionario, file.path(out_dir, "00_diccionario_911_leido.csv"))
    log_event("INFO", paste("Diccionario cargado. Filas:", nrow(diccionario)))
  }

  diccionario
}

#' Cargar datasets de llamadas 911
#'
#' Une los tres CSV de llamadas y agrega una columna de procedencia.
#'
#' @param directorio_datos character. Carpeta donde estan los archivos.
#' @return tibble. Base integrada.
cargar_datasets_911 <- function(directorio_datos) {
  archivos <- data.frame(
    periodo_archivo = c("2021_s1", "2021_s2", "2022_s1"),
    nombre_archivo = c(
      "llamadas_911_2021_s1.csv",
      "llamadas_911_2021_s2.csv",
      "llamadas_911_2022_s1.csv"
    ),
    stringsAsFactors = FALSE
  )

  lista_datos <- list()

  for (i in seq_len(nrow(archivos))) {
    ruta <- resolver_ruta_archivo(
      archivos$nombre_archivo[i],
      directorio_datos
    )

    lista_datos[[i]] <- leer_csv_911(
      ruta = ruta,
      periodo_archivo = archivos$periodo_archivo[i]
    )
  }

  datos <- dplyr::bind_rows(lista_datos)

  log_event(
    "INFO",
    paste("Base integrada creada. Filas:", nrow(datos), "| Columnas:", ncol(datos))
  )

  datos
}

#' Validar columnas requeridas
#'
#' Comprueba que la base contenga las variables necesarias para resolver
#' la actividad.
#'
#' @param datos data.frame. Base de llamadas 911.
#' @param columnas character vector. Columnas requeridas.
#' @return TRUE si la validacion es correcta; si no, lanza error.
validar_columnas <- function(datos, columnas) {
  faltantes <- setdiff(columnas, names(datos))

  if (length(faltantes) > 0) {
    stop(sprintf(
      "Faltan columnas requeridas: %s",
      paste(faltantes, collapse = ", ")
    ))
  }

  TRUE
}

#' Preparar base analitica 911
#'
#' Normaliza categorias, alcaldias y fechas; crea variables temporales
#' y calcula duracion entre creacion y cierre.
#'
#' @param datos data.frame. Base integrada de llamadas 911.
#' @return tibble. Base analitica preparada.
preparar_base_analitica <- function(datos) {
  columnas_requeridas <- c(
    "folio", "fecha_creacion", "hora_creacion",
    "fecha_cierre", "hora_cierre", "clas_con_f_alarma",
    "alcaldia_cierre", "codigo_cierre"
  )

  validar_columnas(datos, columnas_requeridas)

  datos_preparados <- datos |>
    dplyr::mutate(
      categoria = normalizar_categoria_911(clas_con_f_alarma),
      categoria_etiqueta = crear_etiqueta_categoria(categoria),
      alcaldia = normalizar_alcaldia_911(alcaldia_cierre),
      codigo_cierre_norm = normalizar_texto(codigo_cierre),
      datetime_creacion = crear_datetime(fecha_creacion, hora_creacion),
      datetime_cierre = crear_datetime(fecha_cierre, hora_cierre),
      fecha_creacion_date = as.Date(datetime_creacion),
      fecha_cierre_date = as.Date(datetime_cierre),
      mes_cierre = lubridate::floor_date(fecha_cierre_date, unit = "month"),
      anio_cierre_calc = lubridate::year(fecha_cierre_date),
      mes_cierre_calc = lubridate::month(fecha_cierre_date),
      dia_semana = crear_dia_semana(fecha_creacion_date),
      hora_reporte = lubridate::hour(datetime_creacion),
      duracion_minutos = as.numeric(
        difftime(datetime_cierre, datetime_creacion, units = "mins")
      ),
      duracion_valida = !is.na(duracion_minutos) & duracion_minutos >= 0,
      es_falsa_alarma = categoria == "FALSA ALARMA"
    )

  log_event("INFO", "Base analitica preparada con variables temporales y categoricas.")
  datos_preparados
}

# ------------------------------------------------------------
# 2. DIAGNOSTICO TECNICO DE CALIDAD
# ------------------------------------------------------------

#' Diagnosticar calidad de la base
#'
#' Genera un resumen tecnico de estructura, faltantes, duplicados,
#' rango temporal y duraciones invalidas.
#'
#' @param datos data.frame. Base analitica.
#' @param out_dir character. Carpeta de salida.
#' @return list. Tablas de diagnostico.
diagnosticar_datos <- function(datos, out_dir) {
  total_registros <- nrow(datos)
  total_columnas <- ncol(datos)

  duplicados_fila <- sum(duplicated(datos))
  duplicados_folio <- sum(duplicated(datos$folio))

  rango_fechas <- data.frame(
    variable = c("fecha_creacion", "fecha_cierre"),
    minimo = c(
      as.character(min(datos$fecha_creacion_date, na.rm = TRUE)),
      as.character(min(datos$fecha_cierre_date, na.rm = TRUE))
    ),
    maximo = c(
      as.character(max(datos$fecha_creacion_date, na.rm = TRUE)),
      as.character(max(datos$fecha_cierre_date, na.rm = TRUE))
    )
  )

  faltantes <- data.frame(
    variable = names(datos),
    faltantes = as.integer(colSums(is.na(datos))),
    porcentaje_faltantes = as.numeric(colMeans(is.na(datos)) * 100),
    stringsAsFactors = FALSE
  ) |>
    dplyr::arrange(dplyr::desc(porcentaje_faltantes))

  resumen_archivo <- datos |>
    dplyr::count(periodo_archivo, name = "registros") |>
    dplyr::arrange(periodo_archivo)

  calidad <- data.frame(
    indicador = c(
      "registros",
      "columnas",
      "duplicados_fila_completa",
      "folios_duplicados",
      "duraciones_negativas",
      "datetime_creacion_na",
      "datetime_cierre_na"
    ),
    valor = c(
      total_registros,
      total_columnas,
      duplicados_fila,
      duplicados_folio,
      sum(datos$duracion_minutos < 0, na.rm = TRUE),
      sum(is.na(datos$datetime_creacion)),
      sum(is.na(datos$datetime_cierre))
    )
  )

  guardar_csv(faltantes, file.path(out_dir, "01_diagnostico_faltantes.csv"))
  guardar_csv(rango_fechas, file.path(out_dir, "01_diagnostico_rango_fechas.csv"))
  guardar_csv(resumen_archivo, file.path(out_dir, "01_diagnostico_por_archivo.csv"))
  guardar_csv(calidad, file.path(out_dir, "01_diagnostico_calidad_general.csv"))

  cat("\n============================================================\n")
  cat("DIAGNOSTICO TECNICO DE LA BASE 911\n")
  cat("============================================================\n")
  cat("Registros integrados       :", formatear_numero(total_registros, 0), "\n")
  cat("Columnas                  :", total_columnas, "\n")
  cat("Duplicados fila completa  :", formatear_numero(duplicados_fila, 0), "\n")
  cat("Folios duplicados         :", formatear_numero(duplicados_folio, 0), "\n")
  cat("Duraciones negativas      :", sum(datos$duracion_minutos < 0, na.rm = TRUE), "\n")
  cat("Rango fecha cierre        :", rango_fechas$minimo[2], "a", rango_fechas$maximo[2], "\n")
  cat("============================================================\n\n")

  list(
    faltantes = faltantes,
    rango_fechas = rango_fechas,
    resumen_archivo = resumen_archivo,
    calidad = calidad
  )
}

# ------------------------------------------------------------
# 3. ANALISIS 1
# ------------------------------------------------------------

#' Analisis 1: incidentes por mes, alcaldia y categoria
#'
#' Agrupa incidentes por mes de cierre, alcaldia y categoria; calcula el
#' total mensual y el promedio mensual por categoria dentro de cada alcaldia.
#'
#' @param datos data.frame. Base analitica.
#' @param out_dir character. Carpeta de salida.
#' @return list. Conteos y promedios.
analisis_1_mes_alcaldia_categoria <- function(datos, out_dir) {
  log_event("INFO", "Ejecutando analisis 1: mes, alcaldia y categoria.")

  base <- datos |>
    dplyr::filter(
      !is.na(mes_cierre),
      !is.na(alcaldia),
      !is.na(categoria_etiqueta)
    )

  conteo <- base |>
    dplyr::count(
      mes_cierre,
      alcaldia,
      categoria_etiqueta,
      name = "total_incidentes"
    )

  meses <- seq(
    from = min(conteo$mes_cierre, na.rm = TRUE),
    to = max(conteo$mes_cierre, na.rm = TRUE),
    by = "month"
  )

  conteo_completo <- conteo |>
    tidyr::complete(
      mes_cierre = meses,
      alcaldia,
      categoria_etiqueta,
      fill = list(total_incidentes = 0)
    )

  resultado_mensual <- conteo_completo |>
    dplyr::group_by(alcaldia, categoria_etiqueta) |>
    dplyr::mutate(
      promedio_mensual_categoria = mean(total_incidentes, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(alcaldia, categoria_etiqueta, mes_cierre)

  promedio_categoria <- conteo_completo |>
    dplyr::group_by(alcaldia, categoria_etiqueta) |>
    dplyr::summarise(
      promedio_mensual = mean(total_incidentes, na.rm = TRUE),
      total_periodo = sum(total_incidentes, na.rm = TRUE),
      meses_observados = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(promedio_mensual))

  guardar_csv(
    resultado_mensual,
    file.path(out_dir, "02_tarea1_conteo_mes_alcaldia_categoria.csv")
  )
  guardar_csv(
    promedio_categoria,
    file.path(out_dir, "02_tarea1_promedio_mensual_por_alcaldia_categoria.csv")
  )

  top_alcaldias <- promedio_categoria |>
    dplyr::group_by(alcaldia) |>
    dplyr::summarise(total = sum(total_periodo), .groups = "drop") |>
    dplyr::slice_max(total, n = 10, with_ties = FALSE) |>
    dplyr::pull(alcaldia)

  datos_grafico <- promedio_categoria |>
    dplyr::filter(alcaldia %in% top_alcaldias)

  grafico <- ggplot2::ggplot(
    datos_grafico,
    ggplot2::aes(
      x = stats::reorder(alcaldia, promedio_mensual),
      y = promedio_mensual,
      fill = categoria_etiqueta
    )
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Promedio mensual de incidentes por alcaldía y categoría",
      subtitle = "Top 10 alcaldías por volumen total de incidentes",
      x = "Alcaldía",
      y = "Promedio mensual de incidentes",
      fill = "Categoría"
    ) +
    ggplot2::theme_minimal()

  guardar_grafico(
    grafico,
    file.path(out_dir, "02_tarea1_promedio_mensual_alcaldia_categoria.png"),
    ancho = 12,
    alto = 7
  )

  cat("\nTAREA 1 - TOP 10 PROMEDIOS MENSUALES POR ALCALDIA Y CATEGORIA\n")
  print(utils::head(promedio_categoria, 10))

  list(
    conteo_mensual = resultado_mensual,
    promedio_categoria = promedio_categoria
  )
}

# ------------------------------------------------------------
# 4. ANALISIS 2
# ------------------------------------------------------------

#' Analisis 2: dia de semana con mas incidentes
#'
#' Calcula el total de incidentes por dia de semana usando fecha de creacion,
#' que aproxima el momento en que se reporto la llamada.
#'
#' @param datos data.frame. Base analitica.
#' @param out_dir character. Carpeta de salida.
#' @return list. Conteo por dia y dia maximo.
analisis_2_dia_semana <- function(datos, out_dir) {
  log_event("INFO", "Ejecutando analisis 2: dia de semana con mas incidentes.")

  conteo_dia <- datos |>
    dplyr::filter(!is.na(dia_semana)) |>
    dplyr::count(dia_semana, name = "total_llamadas") |>
    dplyr::arrange(dplyr::desc(total_llamadas))

  dia_maximo <- conteo_dia |>
    dplyr::slice_max(total_llamadas, n = 1, with_ties = TRUE)

  guardar_csv(conteo_dia, file.path(out_dir, "03_tarea2_incidentes_por_dia_semana.csv"))
  guardar_csv(dia_maximo, file.path(out_dir, "03_tarea2_dia_semana_maximo.csv"))

  limite_y <- max(conteo_dia$total_llamadas, na.rm = TRUE) * 1.12

  grafico <- ggplot2::ggplot(
    conteo_dia,
    ggplot2::aes(x = dia_semana, y = total_llamadas)
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(
        label = format(
          total_llamadas,
          big.mark = ",",
          scientific = FALSE,
          trim = TRUE
        )
      ),
      vjust = -0.35,
      size = 3.7
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) {
        format(
          x,
          big.mark = ",",
          scientific = FALSE,
          trim = TRUE
        )
      },
      limits = c(0, limite_y),
      expand = ggplot2::expansion(mult = c(0, 0.04))
    ) +
    ggplot2::labs(
      title = "Total de llamadas 911 por día de la semana",
      subtitle = "Calculado con fecha de creación del folio",
      x = "Día de la semana",
      y = "Total de llamadas"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 10),
      axis.title.y = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold")
    )

  guardar_grafico(
    grafico,
    file.path(out_dir, "03_tarea2_incidentes_por_dia_semana.png"),
    ancho = 9,
    alto = 6
  )

  cat("\nTAREA 2 - DIA DE SEMANA CON MAS INCIDENTES\n")
  print(dia_maximo)

  list(conteo_dia = conteo_dia, dia_maximo = dia_maximo)
}

# ------------------------------------------------------------
# 5. ANALISIS 3
# ------------------------------------------------------------

#' Analisis 3: distribucion por hora y categoria prioritaria
#'
#' Calcula la distribucion horaria de llamadas para las categorias DELITO,
#' EMERGENCIA y URGENCIAS MEDICAS.
#'
#' @param datos data.frame. Base analitica.
#' @param out_dir character. Carpeta de salida.
#' @return data.frame. Conteo por categoria y hora.
analisis_3_hora_categoria <- function(datos, out_dir) {
  log_event("INFO", "Ejecutando analisis 3: distribucion horaria por categoria.")

  categorias_objetivo <- c("DELITO", "EMERGENCIA", "URGENCIAS MEDICAS")

  hora_categoria <- datos |>
    dplyr::filter(
      categoria %in% categorias_objetivo,
      !is.na(hora_reporte)
    ) |>
    dplyr::mutate(
      categoria_etiqueta = crear_etiqueta_categoria(categoria)
    ) |>
    dplyr::count(categoria_etiqueta, hora_reporte, name = "total_incidentes") |>
    tidyr::complete(
      categoria_etiqueta,
      hora_reporte = 0:23,
      fill = list(total_incidentes = 0)
    ) |>
    dplyr::arrange(categoria_etiqueta, hora_reporte)

  hora_pico <- hora_categoria |>
    dplyr::group_by(categoria_etiqueta) |>
    dplyr::slice_max(total_incidentes, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

  guardar_csv(hora_categoria, file.path(out_dir, "04_tarea3_distribucion_hora_categoria.csv"))
  guardar_csv(hora_pico, file.path(out_dir, "04_tarea3_hora_pico_por_categoria.csv"))

  grafico <- ggplot2::ggplot(
    hora_categoria,
    ggplot2::aes(
      x = hora_reporte,
      y = total_incidentes,
      group = categoria_etiqueta,
      color = categoria_etiqueta
    )
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.7) +
    ggplot2::scale_x_continuous(breaks = 0:23) +
    ggplot2::labs(
      title = "Distribución horaria de incidentes seleccionados",
      subtitle = "Categorías: DELITO, EMERGENCIA y URGENCIAS MÉDICAS",
      x = "Hora del día",
      y = "Total de incidentes",
      color = "Categoría"
    ) +
    ggplot2::theme_minimal()

  guardar_grafico(
    grafico,
    file.path(out_dir, "04_tarea3_distribucion_horaria_categorias.png"),
    ancho = 11,
    alto = 6
  )

  cat("\nTAREA 3 - HORA PICO POR CATEGORIA\n")
  print(hora_pico)

  hora_categoria
}

# ------------------------------------------------------------
# 6. ANALISIS 4
# ------------------------------------------------------------

#' Analisis 4: tiempo entre creacion y cierre
#'
#' Calcula duracion promedio, minima y maxima entre fecha-hora de creacion
#' y fecha-hora de cierre. Excluye duraciones negativas del resumen principal
#' y las reporta como inconsistencias.
#'
#' @param datos data.frame. Base analitica.
#' @param out_dir character. Carpeta de salida.
#' @return list. Resumen de duraciones e inconsistencias.
analisis_4_tiempo_cierre <- function(datos, out_dir) {
  log_event("INFO", "Ejecutando analisis 4: tiempo entre creacion y cierre.")

  duraciones_validas <- datos |>
    dplyr::filter(duracion_valida)

  inconsistencias <- datos |>
    dplyr::filter(!is.na(duracion_minutos), duracion_minutos < 0) |>
    dplyr::select(
      folio,
      periodo_archivo,
      fecha_creacion,
      hora_creacion,
      fecha_cierre,
      hora_cierre,
      duracion_minutos
    )

  resumen <- data.frame(
    metrica = c(
      "registros_totales",
      "registros_validos_duracion",
      "duraciones_negativas_excluidas",
      "duraciones_na",
      "promedio_minutos",
      "mediana_minutos",
      "minimo_minutos",
      "maximo_minutos",
      "promedio_horas",
      "maximo_dias"
    ),
    valor = c(
      nrow(datos),
      nrow(duraciones_validas),
      nrow(inconsistencias),
      sum(is.na(datos$duracion_minutos)),
      mean(duraciones_validas$duracion_minutos, na.rm = TRUE),
      stats::median(duraciones_validas$duracion_minutos, na.rm = TRUE),
      min(duraciones_validas$duracion_minutos, na.rm = TRUE),
      max(duraciones_validas$duracion_minutos, na.rm = TRUE),
      mean(duraciones_validas$duracion_minutos, na.rm = TRUE) / 60,
      max(duraciones_validas$duracion_minutos, na.rm = TRUE) / 1440
    )
  )

  guardar_csv(resumen, file.path(out_dir, "05_tarea4_resumen_tiempo_cierre.csv"))
  guardar_csv(inconsistencias, file.path(out_dir, "05_tarea4_duraciones_negativas.csv"))

  limite_p99 <- stats::quantile(
    duraciones_validas$duracion_minutos,
    probs = 0.99,
    na.rm = TRUE
  )

  datos_grafico <- duraciones_validas |>
    dplyr::filter(duracion_minutos <= limite_p99)

  grafico <- ggplot2::ggplot(
    datos_grafico,
    ggplot2::aes(x = duracion_minutos)
  ) +
    ggplot2::geom_histogram(bins = 60) +
    ggplot2::scale_x_continuous(
      labels = formatear_eje_entero
    ) +
    ggplot2::scale_y_continuous(
      labels = formatear_eje_entero
    ) +
    ggplot2::labs(
      title = "Distribución del tiempo entre creación y cierre",
      subtitle = "Visualización limitada al percentil 99; el eje Y muestra frecuencia de llamadas",
      x = "Duración entre creación y cierre (minutos)",
      y = "Frecuencia de llamadas"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 10),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 8))
    )

  guardar_grafico(
    grafico,
    file.path(out_dir, "05_tarea4_histograma_duracion_minutos.png"),
    ancho = 11,
    alto = 6.5
  )

  cat("\nTAREA 4 - TIEMPO ENTRE CREACION Y CIERRE\n")
  print(resumen)

  list(
    resumen = resumen,
    inconsistencias = inconsistencias
  )
}

# ------------------------------------------------------------
# 7. ANALISIS 5
# ------------------------------------------------------------

#' Analisis 5: porcentaje de falsa alarma
#'
#' Calcula el porcentaje de llamadas cuya clasificacion general es
#' FALSA ALARMA.
#'
#' @param datos data.frame. Base analitica.
#' @param out_dir character. Carpeta de salida.
#' @return list. Resumen general y distribucion por categoria.
analisis_5_falsa_alarma <- function(datos, out_dir) {
  log_event("INFO", "Ejecutando analisis 5: porcentaje de falsa alarma.")

  resumen_falsa <- datos |>
    dplyr::summarise(
      total_llamadas = dplyr::n(),
      llamadas_falsa_alarma = sum(es_falsa_alarma, na.rm = TRUE),
      porcentaje_falsa_alarma = 100 * llamadas_falsa_alarma / total_llamadas,
      codigo_cierre_f = sum(codigo_cierre_norm == "F", na.rm = TRUE)
    )

  distribucion_categoria <- datos |>
    dplyr::filter(!is.na(categoria_etiqueta)) |>
    dplyr::count(categoria_etiqueta, name = "total_llamadas") |>
    dplyr::mutate(
      porcentaje = 100 * total_llamadas / sum(total_llamadas)
    ) |>
    dplyr::arrange(dplyr::desc(total_llamadas))

  guardar_csv(resumen_falsa, file.path(out_dir, "06_tarea5_porcentaje_falsa_alarma.csv"))
  guardar_csv(distribucion_categoria, file.path(out_dir, "06_tarea5_distribucion_categorias.csv"))

  datos_grafico_falsa <- distribucion_categoria |>
    dplyr::mutate(
      categoria_ordenada = stats::reorder(categoria_etiqueta, porcentaje),
      etiqueta_porcentaje = formatear_porcentaje(porcentaje, 2)
    )

  grafico <- ggplot2::ggplot(
    datos_grafico_falsa,
    ggplot2::aes(
      x = categoria_ordenada,
      y = porcentaje
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(label = etiqueta_porcentaje),
      hjust = -0.08,
      size = 3.5
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      labels = formatear_porcentaje,
      expand = ggplot2::expansion(mult = c(0, 0.18))
    ) +
    ggplot2::labs(
      title = "Porcentaje de llamadas por clasificación",
      subtitle = "Barras horizontales: la categoría FALSA ALARMA se calcula desde clas_con_f_alarma",
      x = "Clasificación",
      y = "Porcentaje del total"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 10),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 8))
    )

  guardar_grafico(
    grafico,
    file.path(out_dir, "06_tarea5_porcentaje_por_clasificacion.png"),
    ancho = 10,
    alto = 6.5
  )

  cat("\nTAREA 5 - PORCENTAJE DE FALSA ALARMA\n")
  print(resumen_falsa)

  if (resumen_falsa$codigo_cierre_f == 0) {
    log_event(
      "WARN",
      "No se detectaron registros con codigo_cierre == 'F'. Se usa clas_con_f_alarma == 'FALSA ALARMA'."
    )
  }

  list(
    resumen_falsa = resumen_falsa,
    distribucion_categoria = distribucion_categoria
  )
}

# ------------------------------------------------------------
# 8. REPORTE FINAL CONSOLIDADO
# ------------------------------------------------------------

#' Crear reporte ejecutivo en texto
#'
#' Construye un resumen breve de resultados clave y lo guarda en TXT.
#'
#' @param resultados list. Resultados de las tareas.
#' @param out_dir character. Carpeta de salida.
#' @return character. Ruta del reporte generado.
crear_reporte_txt <- function(resultados, out_dir) {
  ruta <- file.path(out_dir, "07_reporte_ejecutivo_eda_911.txt")

  top_t1 <- utils::head(resultados$tarea1$promedio_categoria, 5)
  dia_max <- resultados$tarea2$dia_maximo
  hora_pico <- resultados$tarea3_hora_pico
  tiempo <- resultados$tarea4$resumen
  falsa <- resultados$tarea5$resumen_falsa

  lineas <- c(
    "REPORTE EJECUTIVO - EDA LLAMADAS 911 CDMX",
    "============================================================",
    "",
    "1) Promedios mensuales mas altos por alcaldia y categoria:",
    paste(capture.output(print(top_t1)), collapse = "\n"),
    "",
    "2) Dia de la semana con mas incidentes:",
    paste(capture.output(print(dia_max)), collapse = "\n"),
    "",
    "3) Hora pico por categoria prioritaria:",
    paste(capture.output(print(hora_pico)), collapse = "\n"),
    "",
    "4) Tiempo entre creacion y cierre:",
    paste(capture.output(print(tiempo)), collapse = "\n"),
    "",
    "5) Porcentaje de falsa alarma:",
    paste(capture.output(print(falsa)), collapse = "\n"),
    "",
    "Nota metodologica:",
    "- Para conteos mensuales se usa fecha_cierre.",
    "- Para dia de semana y hora se usa fecha_creacion.",
    "- Para falsa alarma se usa clas_con_f_alarma == 'FALSA ALARMA'."
  )

  tryCatch(
    {
      writeLines(lineas, ruta, useBytes = TRUE)
      log_event("INFO", paste("Reporte TXT guardado:", ruta))
      ruta
    },
    error = function(e) {
      log_event("ERROR", paste("No se pudo guardar reporte TXT:", e$message))
      NULL
    }
  )
}

# ------------------------------------------------------------
# 9. FUNCION PRINCIPAL
# ------------------------------------------------------------

#' Ejecutar pipeline completo del EDA 911
#'
#' Coordina la secuencia completa:
#' validacion de paquetes, carga de datos, preparacion, diagnostico,
#' cinco analisis solicitados, graficos, tablas y reporte final.
#'
#' @param directorio_datos character. Carpeta donde estan los CSV.
#' @param instalar_paquetes logical. Si TRUE, intenta instalar paquetes faltantes.
#' @return TRUE si termina correctamente; FALSE si ocurre error controlado.
main <- function(directorio_datos = obtener_directorio_script(), instalar_paquetes = TRUE) {
  tryCatch(
    {
      cat("\n============================================================\n")
      cat("EDA LLAMADAS 911 CDMX\n")
      cat("Analisis con dplyr, tidyr y lubridate\n")
      cat("============================================================\n\n")

      paquetes <- c("dplyr", "tidyr", "lubridate", "ggplot2", "readr")
      validar_paquetes(paquetes, instalar = instalar_paquetes)
      cargar_paquetes(paquetes)

      out_dir <- crear_directorio_salida(directorio_datos)

      leer_diccionario(directorio_datos, out_dir)

      datos_raw <- cargar_datasets_911(directorio_datos)
      datos_911 <- preparar_base_analitica(datos_raw)

      diagnostico <- diagnosticar_datos(datos_911, out_dir)

      tarea1 <- analisis_1_mes_alcaldia_categoria(datos_911, out_dir)
      tarea2 <- analisis_2_dia_semana(datos_911, out_dir)
      tarea3 <- analisis_3_hora_categoria(datos_911, out_dir)

      tarea3_hora_pico <- tarea3 |>
        dplyr::group_by(categoria_etiqueta) |>
        dplyr::slice_max(total_incidentes, n = 1, with_ties = FALSE) |>
        dplyr::ungroup()

      tarea4 <- analisis_4_tiempo_cierre(datos_911, out_dir)
      tarea5 <- analisis_5_falsa_alarma(datos_911, out_dir)

      resultados <- list(
        diagnostico = diagnostico,
        tarea1 = tarea1,
        tarea2 = tarea2,
        tarea3 = tarea3,
        tarea3_hora_pico = tarea3_hora_pico,
        tarea4 = tarea4,
        tarea5 = tarea5
      )

      crear_reporte_txt(resultados, out_dir)

      cat("\n============================================================\n")
      cat("EJECUCION FINALIZADA\n")
      cat("Revisa la carpeta de salida:\n")
      cat(out_dir, "\n")
      cat("============================================================\n\n")

      TRUE
    },
    error = function(e) {
      log_event("ERROR", e$message)
      if (requireNamespace("rlang", quietly = TRUE)) {
        traza <- tryCatch(rlang::last_trace(), error = function(err) NULL)
        if (!is.null(traza)) {
          print(traza)
        }
      }
      FALSE
    }
  )
}

# ------------------------------------------------------------
# 10. EJECUCION
# ------------------------------------------------------------

# Recomendacion:
# 1. Coloca este script en la misma carpeta que los CSV.
# 2. Ejecuta: source("eda_911_cdmx.R")
# 3. Si los archivos no estan en la carpeta de trabajo, R pedira seleccionarlos.

main()
