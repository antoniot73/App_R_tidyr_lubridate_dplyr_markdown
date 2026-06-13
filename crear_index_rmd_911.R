# ============================================================
# crear_index_rmd_911_PORTABLE_V2.R
#
# Propósito:
#   Crear desde cero el archivo dashboard_911/index.Rmd para
#   comunicar el análisis exploratorio del dataset 911 CDMX con
#   R Markdown, GitHub Pages y Shiny Apps.
#
# Ajuste V2:
#   Integra en las tareas 1 a 5 los segmentos:
#     - Datos utilizados
#     - Objetivos específicos
#     - Hallazgos e interpretación
#   Además agrega:
#     - Cierre integrado y reflexión inferencial
#     - Enlaces del proyecto
#     - Referencia del dataset
#
# Principio de portabilidad:
#   Este script NO contiene rutas absolutas ni depende del disco D:.
#   Trabaja en la carpeta donde se encuentre el script, salvo que
#   el usuario indique explícitamente otra raíz mediante el argumento
#   root o la variable de entorno CREAR_INDEX_RMD_911_ROOT.
#
# Uso recomendado:
#   1) Copiar este archivo a la raíz del proyecto.
#   2) Ejecutar:
#        source("crear_index_rmd_911_PORTABLE_V2.R")
#
# Para sobrescribir:
#        Sys.setenv(CREAR_INDEX_RMD_911_OVERWRITE = "TRUE")
#        source("crear_index_rmd_911_PORTABLE_V2.R")
#
# Salida:
#   dashboard_911/index.Rmd
#
# Nota:
#   Este pipeline NO ejecuta EDA, NO renderiza HTML y NO arranca Shiny.
#   Solo crea el documento fuente index.Rmd.
# ============================================================

#' Registrar eventos del pipeline
#'
#' @param nivel character. Nivel del evento: INFO, OK, WARN o ERROR.
#' @param mensaje character. Mensaje que se mostrará en consola.
#' @return NULL invisible.
log_index_911 <- function(nivel, mensaje) {
  cat(sprintf("[%s] [%s] %s\n", Sys.time(), nivel, mensaje))
  invisible(NULL)
}

#' Convertir texto de entorno a valor lógico
#'
#' @param valor character. Valor textual.
#' @param default logical. Valor por defecto.
#' @return logical. Valor convertido.
as_logical_index_911 <- function(valor, default = FALSE) {
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
      log_index_911("WARN", paste("No se pudo convertir valor lógico:", e$message))
      default
    }
  )
}

#' Detectar la ruta del archivo cuando se ejecuta con source()
#'
#' @return character. Ruta del script o cadena vacía si no puede detectarse.
detectar_script_path_index_911 <- function() {
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
      log_index_911("WARN", paste("No se pudo detectar ruta del script:", e$message))
      ""
    }
  )
}

#' Detectar la raíz del proyecto de forma portable
#'
#' Prioridad:
#'   1. Variable de entorno CREAR_INDEX_RMD_911_ROOT, si existe.
#'   2. Carpeta donde está este script, si se ejecutó con source().
#'   3. Directorio actual de R, getwd(), como respaldo.
#'
#' @return character. Ruta raíz normalizada.
inferir_root_portable_index_911 <- function() {
  tryCatch(
    {
      root_env <- Sys.getenv("CREAR_INDEX_RMD_911_ROOT", unset = "")

      if (nzchar(root_env)) {
        root <- normalizePath(root_env, winslash = "/", mustWork = FALSE)
        log_index_911("INFO", paste("Raíz tomada desde CREAR_INDEX_RMD_911_ROOT:", root))
        return(root)
      }

      script_path <- detectar_script_path_index_911()

      if (nzchar(script_path)) {
        root <- dirname(script_path)
        root <- normalizePath(root, winslash = "/", mustWork = FALSE)
        log_index_911("INFO", paste("Raíz inferida desde ubicación del script:", root))
        return(root)
      }

      root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
      log_index_911("INFO", paste("Raíz inferida desde getwd():", root))
      root
    },
    error = function(e) {
      log_index_911("ERROR", paste("No se pudo inferir raíz portable:", e$message))
      stop(e$message)
    }
  )
}

#' Resolver ruta raíz del proyecto
#'
#' @param root character | NULL. Ruta raíz opcional. Si es NULL, se infiere.
#' @return character. Ruta raíz normalizada.
resolver_root_index_911 <- function(root = NULL) {
  tryCatch(
    {
      if (missing(root) || is.null(root) || !nzchar(root)) {
        return(inferir_root_portable_index_911())
      }

      normalizePath(root, winslash = "/", mustWork = FALSE)
    },
    error = function(e) {
      log_index_911("ERROR", paste("No se pudo resolver la raíz:", e$message))
      stop(e$message)
    }
  )
}

#' Crear directorios mínimos del proyecto
#'
#' @param root character. Ruta raíz del proyecto.
#' @return list. Rutas principales.
crear_directorios_index_911 <- function(root) {
  tryCatch(
    {
      rutas <- list(
        root = root,
        dashboard = file.path(root, "dashboard_911"),
        data = file.path(root, "dashboard_911", "data"),
        www = file.path(root, "dashboard_911", "www"),
        docs = file.path(root, "docs")
      )

      for (nombre in names(rutas)) {
        ruta <- rutas[[nombre]]

        if (!dir.exists(ruta)) {
          dir.create(ruta, recursive = TRUE, showWarnings = FALSE)
          log_index_911("OK", paste("Directorio creado:", ruta))
        } else {
          log_index_911("INFO", paste("Directorio existente:", ruta))
        }
      }

      rutas
    },
    error = function(e) {
      log_index_911("ERROR", paste("Error creando directorios:", e$message))
      stop(e$message)
    }
  )
}

#' Contar insumos disponibles del dashboard
#'
#' @param rutas list. Rutas principales.
#' @return list. Conteo de CSV y PNG.
contar_insumos_index_911 <- function(rutas) {
  tryCatch(
    {
      csv <- character(0)
      png <- character(0)

      if (dir.exists(rutas$data)) {
        csv <- list.files(rutas$data, pattern = "\\.csv$", full.names = TRUE)
      }

      if (dir.exists(rutas$www)) {
        png <- list.files(rutas$www, pattern = "\\.png$", full.names = TRUE)
      }

      resumen <- list(
        n_csv = length(csv),
        n_png = length(png),
        csv = basename(csv),
        png = basename(png)
      )

      log_index_911("INFO", paste("CSV detectados en dashboard_911/data:", resumen$n_csv))
      log_index_911("INFO", paste("PNG detectados en dashboard_911/www:", resumen$n_png))

      resumen
    },
    error = function(e) {
      log_index_911("ERROR", paste("Error contando insumos:", e$message))
      stop(e$message)
    }
  )
}

#' Construir encabezado YAML y setup del R Markdown
#'
#' @return character. Líneas iniciales del documento.
construir_yaml_y_setup_index_911 <- function() {
  c(
    "---",
    'title: "Comunicación con R Markdown del análisis de datos 911 CDMX"',
    'author: "Antonio Toro"',
    'date: "`r format(Sys.Date(), \'%Y-%m-%d\')`"',
    "output:",
    "  html_document:",
    "    toc: true",
    "    toc_depth: 3",
    "    toc_float: true",
    "    number_sections: true",
    "    theme: journal",
    "    code_folding: hide",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(",
    "  echo = FALSE,",
    "  message = FALSE,",
    "  warning = FALSE,",
    "  fig.align = 'center'",
    ")",
    "",
    "leer_csv_seguro <- function(archivo) {",
    "  ruta <- file.path('data', archivo)",
    "  if (!file.exists(ruta)) {",
    "    return(NULL)",
    "  }",
    "  tryCatch(",
    "    read.csv(ruta, stringsAsFactors = FALSE, check.names = FALSE),",
    "    error = function(e) NULL",
    "  )",
    "}",
    "",
    "mostrar_tabla <- function(archivo, caption, n = 10) {",
    "  datos <- leer_csv_seguro(archivo)",
    "  if (is.null(datos)) {",
    "    cat(paste0('**Archivo no disponible:** `data/', archivo, '`'))",
    "    return(invisible(NULL))",
    "  }",
    "  knitr::kable(utils::head(datos, n), caption = caption)",
    "}",
    "",
    "insertar_png <- function(archivo) {",
    "  ruta <- file.path('www', archivo)",
    "  if (!file.exists(ruta)) {",
    "    cat(paste0('**Imagen no disponible:** `www/', archivo, '`'))",
    "    return(invisible(NULL))",
    "  }",
    "  cat(paste0('![](', ruta, '){width=100%}'))",
    "}",
    "```",
    ""
  )
}

#' Construir secciones introductorias del documento
#'
#' @return character. Líneas de contenido.
construir_intro_index_911 <- function() {
  c(
    "# Introducción",
    "",
    "Este documento presenta la comunicación técnica del análisis exploratorio del conjunto de datos de llamadas cerradas al **911 de la Ciudad de México**. El proyecto se desarrolló en R y se orientó a transformar registros administrativos masivos en resultados descriptivos, visualizaciones y tablas interpretables.",
    "",
    "El propósito del reporte es comunicar de manera reproducible los principales hallazgos del análisis, integrando narrativa técnica, resultados agregados, tablas y gráficos generados durante el procesamiento. El alcance del análisis es **exploratorio y descriptivo**, por lo que los resultados permiten identificar patrones territoriales, temporales y operativos, pero no establecer relaciones causales.",
    "",
    "# Comunicación reproducible con R Markdown",
    "",
    "R Markdown se utiliza como medio de comunicación reproducible porque permite integrar en un mismo documento la explicación del análisis, los insumos generados por R y la publicación final en formato HTML. En este proyecto, el archivo `index.Rmd` funciona como documento fuente y se renderiza como `index.html` para su publicación en GitHub Pages o Netlify.",
    "",
    "```text",
    "Datos 911",
    "  ↓",
    "Procesamiento con R",
    "  ↓",
    "CSV y PNG agregados",
    "  ↓",
    "dashboard_911/index.Rmd",
    "  ↓",
    "dashboard_911/index.html",
    "  ↓",
    "docs/index.html",
    "  ↓",
    "GitHub Pages / Netlify",
    "```",
    "",
    "# Relevancia del dataset 911 CDMX",
    "",
    "El dataset de llamadas cerradas al **911 de la Ciudad de México** constituye un registro administrativo de alto valor público sobre la demanda ciudadana de atención inmediata. Al reunir reportes relacionados con delitos, emergencias, urgencias médicas, faltas cívicas, servicios y falsas alarmas, permite identificar patrones territoriales, temporales y operativos útiles para reconocer zonas de mayor concentración, horarios críticos y necesidades prioritarias de atención.",
    "",
    "En este sentido, el dataset aporta evidencia descriptiva para apoyar diagnósticos urbanos, orientar recursos institucionales y fortalecer la toma de decisiones basada en datos.",
    "",
    "# Descripción y calidad de los datos",
    "",
    "El análisis integra los archivos públicos disponibles de llamadas cerradas al 911. La base procesada permite construir indicadores por periodo, alcaldía, categoría, día de la semana, hora del reporte y duración entre creación y cierre del folio.",
    "",
    "```{r tabla-diagnostico-general}",
    "mostrar_tabla(",
    "  archivo = '01_diagnostico_calidad_general.csv',",
    "  caption = 'Diagnóstico general de calidad del dataset 911 CDMX',",
    "  n = 20",
    ")",
    "```",
    "",
    "```{r tabla-rango-fechas}",
    "mostrar_tabla(",
    "  archivo = '01_diagnostico_rango_fechas.csv',",
    "  caption = 'Rango temporal de los archivos procesados',",
    "  n = 20",
    ")",
    "```",
    "",
    "# Metodología de procesamiento en R",
    "",
    "El procesamiento se organiza como un flujo estructurado de ciencia de datos. Primero se cargan e integran los archivos originales; después se normalizan textos y categorías; posteriormente se construyen variables temporales con `lubridate`; finalmente se agrupan los datos con `dplyr` y se completan combinaciones con `tidyr` para producir tablas y gráficos de las tareas solicitadas.",
    "",
    "```text",
    "Carga de datos",
    "  ↓",
    "Integración de archivos",
    "  ↓",
    "Limpieza y normalización",
    "  ↓",
    "Construcción de fechas y horas",
    "  ↓",
    "Agrupamientos y resúmenes",
    "  ↓",
    "Exportación de CSV y PNG",
    "  ↓",
    "Comunicación en R Markdown y Shiny",
    "```",
    ""
  )
}

#' Construir sección de una tarea analítica
#'
#' @param titulo character. Título de la tarea.
#' @param descripcion character. Descripción inicial.
#' @param datos_utilizados character. Viñetas sobre datos utilizados.
#' @param objetivos character. Viñetas de objetivos específicos.
#' @param hallazgos character. Viñetas de hallazgos e interpretación.
#' @param grafico character. Nombre del archivo PNG.
#' @param tabla_archivo character. Nombre del archivo CSV principal.
#' @param tabla_caption character. Título de la tabla.
#' @param tabla_n integer. Número de filas a mostrar.
#' @param tabla_extra_archivo character | NULL. Nombre de CSV adicional.
#' @param tabla_extra_caption character | NULL. Título de tabla adicional.
#' @param tabla_extra_n integer. Número de filas tabla adicional.
#' @return character. Líneas Markdown de la tarea.
construir_tarea_index_911 <- function(
    titulo,
    descripcion,
    datos_utilizados,
    objetivos,
    hallazgos,
    grafico,
    tabla_archivo,
    tabla_caption,
    tabla_n = 10,
    tabla_extra_archivo = NULL,
    tabla_extra_caption = NULL,
    tabla_extra_n = 10
) {
  tryCatch(
    {
      lineas <- c(
        paste0("# ", titulo),
        "",
        descripcion,
        "",
        "## Datos utilizados",
        "",
        datos_utilizados,
        "",
        "## Objetivos específicos",
        "",
        objetivos,
        "",
        "## Hallazgos e interpretación",
        "",
        hallazgos,
        "",
        "## Visualización y tabla de resultados",
        "",
        "```{r, results='asis'}",
        paste0("insertar_png('", grafico, "')"),
        "```",
        "",
        "```{r}",
        "mostrar_tabla(",
        paste0("  archivo = '", tabla_archivo, "',"),
        paste0("  caption = '", tabla_caption, "',"),
        paste0("  n = ", tabla_n),
        ")",
        "```",
        ""
      )

      if (!is.null(tabla_extra_archivo) && nzchar(tabla_extra_archivo)) {
        lineas <- c(
          lineas,
          "```{r}",
          "mostrar_tabla(",
          paste0("  archivo = '", tabla_extra_archivo, "',"),
          paste0("  caption = '", tabla_extra_caption, "',"),
          paste0("  n = ", tabla_extra_n),
          ")",
          "```",
          ""
        )
      }

      lineas
    },
    error = function(e) {
      log_index_911("ERROR", paste("Error construyendo tarea:", titulo, e$message))
      stop(e$message)
    }
  )
}

#' Construir las secciones de tareas 1 a 5
#'
#' @return character. Líneas Markdown.
construir_tareas_index_911 <- function() {
  tryCatch(
    {
      tarea_1 <- construir_tarea_index_911(
        titulo = "Tarea 1. Incidentes por mes, alcaldía y categoría",
        descripcion = "Agrupación de incidentes por mes, alcaldía y categoría; cálculo de promedio mensual por categoría.",
        datos_utilizados = c(
          "- **Insumos:** `02_tarea1_conteo_mes_alcaldia_categoria.csv` y `02_tarea1_promedio_mensual_por_alcaldia_categoria.csv`.",
          "- **Estructura:** combinaciones de `mes_cierre`, alcaldía y `categoria_etiqueta` con conteos y promedios mensuales.",
          "- **Origen:** folios cerrados del 911 CDMX previamente integrados y agregados por el pipeline EDA."
        ),
        objetivos = c(
          "- Identificar patrones territoriales y mensuales de llamadas cerradas al 911.",
          "- Comparar el volumen de incidentes por alcaldía y categoría para ubicar concentraciones relevantes.",
          "- Distinguir tendencias de volumen absoluto sin asumir causalidad ni riesgo ajustado por población."
        ),
        hallazgos = c(
          "- Iztapalapa concentra los mayores promedios mensuales, principalmente en DELITO, FALTA CÍVICA y SERVICIO.",
          "- Gustavo A. Madero y Cuauhtémoc también aparecen entre las alcaldías con mayor volumen absoluto.",
          "- El resultado debe interpretarse como concentración de llamadas, no como mayor riesgo territorial sin ajustes demográficos o urbanos."
        ),
        grafico = "02_tarea1_promedio_mensual_alcaldia_categoria.png",
        tabla_archivo = "02_tarea1_promedio_mensual_por_alcaldia_categoria.csv",
        tabla_caption = "Promedio mensual por alcaldía y categoría",
        tabla_n = 15,
        tabla_extra_archivo = "02_tarea1_conteo_mes_alcaldia_categoria.csv",
        tabla_extra_caption = "Conteo mensual por alcaldía y categoría",
        tabla_extra_n = 15
      )

      tarea_2 <- construir_tarea_index_911(
        titulo = "Tarea 2. Día de la semana con más incidentes",
        descripcion = "Identificación del día de la semana con más incidentes y total de llamadas asociado.",
        datos_utilizados = c(
          "- **Insumos:** `03_tarea2_dia_semana_maximo.csv` y `03_tarea2_incidentes_por_dia_semana.csv`.",
          "- **Estructura:** conteo de llamadas por `dia_semana` calculado desde `fecha_creacion`.",
          "- **Origen:** folios cerrados del 911 CDMX agregados por comportamiento semanal."
        ),
        objetivos = c(
          "- Determinar qué día de la semana concentra más llamadas al 911.",
          "- Reconocer comportamientos semanales útiles para planeación operativa.",
          "- Comparar la carga de llamadas entre fines de semana y días laborales."
        ),
        hallazgos = c(
          "- Domingo es el día con mayor número de llamadas, seguido por sábado.",
          "- Sábado y domingo concentran cerca de cuatro de cada diez llamadas del periodo analizado.",
          "- El patrón sugiere mayor presión operativa en fines de semana, aunque no permite establecer causalidad."
        ),
        grafico = "03_tarea2_incidentes_por_dia_semana.png",
        tabla_archivo = "03_tarea2_incidentes_por_dia_semana.csv",
        tabla_caption = "Incidentes por día de la semana",
        tabla_n = 10,
        tabla_extra_archivo = "03_tarea2_dia_semana_maximo.csv",
        tabla_extra_caption = "Día de la semana con mayor número de incidentes",
        tabla_extra_n = 10
      )

      tarea_3 <- construir_tarea_index_911(
        titulo = "Tarea 3. Distribución horaria para DELITO, EMERGENCIA y URGENCIAS MÉDICAS",
        descripcion = "Distribución horaria para las categorías DELITO, EMERGENCIA y URGENCIAS MÉDICAS.",
        datos_utilizados = c(
          "- **Insumos:** `04_tarea3_distribucion_hora_categoria.csv` y `04_tarea3_hora_pico_por_categoria.csv`.",
          "- **Estructura:** conteos por hora de 0 a 23 para DELITO, EMERGENCIA y URGENCIAS MÉDICAS.",
          "- **Origen:** registros 911 procesados con `hora_reporte` y categoría normalizada."
        ),
        objetivos = c(
          "- Analizar la distribución horaria de DELITO, EMERGENCIA y URGENCIAS MÉDICAS.",
          "- Identificar horas pico por categoría para reconocer comportamientos temporales diferenciados.",
          "- Apoyar una lectura operativa por franjas horarias, no uniforme para todas las categorías."
        ),
        hallazgos = c(
          "- DELITO presenta mayor concentración nocturna, con pico alrededor de las 21:00.",
          "- EMERGENCIA concentra mayor carga entre tarde y noche, con pico cercano a las 19:00.",
          "- URGENCIAS MÉDICAS alcanza su mayor volumen alrededor de las 14:00 y mantiene demanda relevante durante tarde y noche."
        ),
        grafico = "04_tarea3_distribucion_horaria_categorias.png",
        tabla_archivo = "04_tarea3_hora_pico_por_categoria.csv",
        tabla_caption = "Hora pico por categoría de incidente",
        tabla_n = 10,
        tabla_extra_archivo = "04_tarea3_distribucion_hora_categoria.csv",
        tabla_extra_caption = "Distribución horaria por categoría",
        tabla_extra_n = 15
      )

      tarea_4 <- construir_tarea_index_911(
        titulo = "Tarea 4. Tiempo entre creación y cierre del incidente",
        descripcion = "Tiempo entre creación y cierre del incidente. El histograma se limita al percentil 99 para mejorar legibilidad.",
        datos_utilizados = c(
          "- **Insumos:** `05_tarea4_resumen_tiempo_cierre.csv` y `05_tarea4_duraciones_negativas.csv`.",
          "- **Estructura:** métricas de duración en minutos entre `datetime_creacion` y `datetime_cierre`.",
          "- **Origen:** folios cerrados del 911 CDMX con fechas y horas validadas por el pipeline EDA."
        ),
        objetivos = c(
          "- Medir el tiempo transcurrido entre creación y cierre de los incidentes.",
          "- Detectar comportamientos atípicos, valores extremos e inconsistencias temporales.",
          "- Diferenciar el comportamiento típico del promedio afectado por casos extremos."
        ),
        hallazgos = c(
          "- La mediana representa mejor el cierre típico que el promedio, porque la distribución es asimétrica a la derecha.",
          "- Existen pocos casos con duraciones negativas, por lo que deben documentarse como inconsistencias y no como tiempos reales.",
          "- El histograma se limita al percentil 99 para mejorar la legibilidad visual y reducir distorsión por valores extremos."
        ),
        grafico = "05_tarea4_histograma_duracion_minutos.png",
        tabla_archivo = "05_tarea4_resumen_tiempo_cierre.csv",
        tabla_caption = "Resumen del tiempo entre creación y cierre",
        tabla_n = 20,
        tabla_extra_archivo = "05_tarea4_duraciones_negativas.csv",
        tabla_extra_caption = "Duraciones negativas detectadas",
        tabla_extra_n = 10
      )

      tarea_5 <- construir_tarea_index_911(
        titulo = "Tarea 5. Porcentaje de llamadas clasificadas como FALSA ALARMA",
        descripcion = "Porcentaje de llamadas clasificadas como FALSA ALARMA y distribución general por clasificación.",
        datos_utilizados = c(
          "- **Insumos:** `06_tarea5_distribucion_categorias.csv` y `06_tarea5_porcentaje_falsa_alarma.csv`.",
          "- **Estructura:** distribución porcentual por clasificación general y cálculo específico de FALSA ALARMA.",
          "- **Origen:** clasificación general normalizada de llamadas cerradas al 911 CDMX."
        ),
        objetivos = c(
          "- Calcular el porcentaje de llamadas clasificadas como FALSA ALARMA.",
          "- Comparar su peso relativo frente a SERVICIO, FALTA CÍVICA, DELITO, EMERGENCIA y URGENCIAS MÉDICAS.",
          "- Evitar confundir la clasificación general con el código de cierre del incidente."
        ),
        hallazgos = c(
          "- FALSA ALARMA representa una fracción mínima del total de llamadas.",
          "- La operación se concentra principalmente en SERVICIO, FALTA CÍVICA y DELITO.",
          "- La interpretación debe basarse en `clas_con_f_alarma`, no en una equivalencia automática con el código de cierre."
        ),
        grafico = "06_tarea5_porcentaje_por_clasificacion.png",
        tabla_archivo = "06_tarea5_porcentaje_falsa_alarma.csv",
        tabla_caption = "Porcentaje de llamadas clasificadas como falsa alarma",
        tabla_n = 10,
        tabla_extra_archivo = "06_tarea5_distribucion_categorias.csv",
        tabla_extra_caption = "Distribución general por clasificación",
        tabla_extra_n = 10
      )

      c(tarea_1, tarea_2, tarea_3, tarea_4, tarea_5)
    },
    error = function(e) {
      log_index_911("ERROR", paste("Error construyendo tareas:", e$message))
      stop(e$message)
    }
  )
}

#' Construir cierre, enlaces y referencias
#'
#' @return character. Líneas Markdown.
construir_cierre_index_911 <- function() {
  c(
    "# Cierre integrado y reflexión inferencial",
    "",
    "Los hallazgos muestran que la demanda registrada por el 911 presenta concentraciones claras en tres dimensiones: territorial, semanal y horaria. Iztapalapa, Gustavo A. Madero y Cuauhtémoc concentran los mayores volúmenes absolutos; domingo y sábado agrupan una parte importante de las llamadas; y las categorías DELITO, EMERGENCIA y URGENCIAS MÉDICAS no comparten el mismo patrón horario.",
    "",
    "De manera inferencial, aunque el análisis no permite afirmar causas, sí sugiere que la operación del servicio podría beneficiarse de estrategias diferenciadas por territorio, día de la semana, categoría y franja horaria. La evidencia apunta a que una planeación uniforme tendría menor capacidad explicativa que una asignación sensible a patrones temporales y territoriales.",
    "",
    "El tiempo de cierre también muestra asimetría: la mediana describe mejor el caso típico que el promedio, debido a valores extremos. Por ello, la toma de decisiones debería considerar medidas robustas y documentar inconsistencias temporales antes de interpretar los indicadores como tiempos reales de atención.",
    "",
    "# Publicación del análisis: GitHub Pages y Shiny Apps",
    "",
    "La publicación estática del reporte se realiza mediante GitHub Pages o Netlify a partir del archivo `docs/index.html`. Esta salida comunica formalmente el análisis en un formato web navegable.",
    "",
    "La aplicación Shiny se plantea como complemento interactivo. Su función es presentar los mismos insumos agregados mediante pestañas, KPIs, tablas y gráficos. En la arquitectura del proyecto, Shiny no debe recalcular los millones de registros originales; debe consumir únicamente los CSV y PNG generados previamente.",
    "",
    "```text",
    "EDA en R",
    "  ↓",
    "dashboard_911/data/*.csv",
    "dashboard_911/www/*.png",
    "  ↓",
    "dashboard_911/app.R",
    "  ↓",
    "Shiny local / shinyapps.io",
    "```",
    "",
    "# Enlaces del proyecto",
    "",
    "- **Datasets 911:**<br>",
    "  <https://datos.cdmx.gob.mx/dataset/llamadas-numero-de-atencion-a-emergencias-911>",
    "- **Repositorio:**<br>",
    "  <https://github.com/antoniot73/App_R_tidyr_lubridate_dplyr_markdown>",
    "- **Datasets CDMX:**<br>",
    "  <https://datos.cdmx.gob.mx/>",
    "",
    "# Cierre técnico",
    "",
    "El proyecto transforma un conjunto masivo de registros administrativos del 911 en evidencia descriptiva organizada. R permite procesar y resumir los datos; R Markdown permite documentar y publicar los resultados; y Shiny permite explorar los productos analíticos mediante una interfaz interactiva.",
    "",
    "```text",
    "El dataset aporta evidencia.",
    "R procesa y resume.",
    "R Markdown comunica y documenta.",
    "GitHub Pages / Netlify publica el reporte.",
    "Shiny Apps permite explorar los resultados.",
    "```",
    "",
    "# Referencias",
    "",
    "C5 de la Ciudad de México. (s. f.). *Versión pública de la base de datos del número de atención a emergencias 9-1-1: Guía del usuario*. Gobierno de la Ciudad de México.<br>",
    "<https://datos.cdmx.gob.mx/dataset/llamadas-numero-de-atencion-a-emergencias-911>",
    "",
    "Xie, Y., Allaire, J. J., & Grolemund, G. (2018). *R Markdown: The definitive guide*. Chapman & Hall/CRC."
  )
}

#' Construir contenido completo del archivo index.Rmd
#'
#' @return character. Líneas del documento R Markdown.
construir_contenido_index_rmd_911 <- function() {
  tryCatch(
    {
      c(
        construir_yaml_y_setup_index_911(),
        construir_intro_index_911(),
        construir_tareas_index_911(),
        construir_cierre_index_911()
      )
    },
    error = function(e) {
      log_index_911("ERROR", paste("Error construyendo contenido index.Rmd:", e$message))
      stop(e$message)
    }
  )
}

#' Escribir archivo index.Rmd
#'
#' @param rutas list. Rutas principales.
#' @param overwrite logical. Si TRUE sobrescribe index.Rmd existente.
#' @return character. Ruta del archivo creado o existente.
escribir_index_rmd_911 <- function(rutas, overwrite = FALSE) {
  tryCatch(
    {
      ruta_index <- file.path(rutas$dashboard, "index.Rmd")

      if (file.exists(ruta_index) && !isTRUE(overwrite)) {
        log_index_911(
          "WARN",
          paste("Ya existe index.Rmd; no se sobrescribió:", ruta_index)
        )
        return(ruta_index)
      }

      contenido <- construir_contenido_index_rmd_911()
      writeLines(contenido, ruta_index, useBytes = TRUE)

      if (!file.exists(ruta_index)) {
        stop("No se pudo crear index.Rmd.")
      }

      log_index_911("OK", paste("Archivo creado/sobrescrito:", ruta_index))
      ruta_index
    },
    error = function(e) {
      log_index_911("ERROR", paste("Error escribiendo index.Rmd:", e$message))
      stop(e$message)
    }
  )
}

#' Validar estructura mínima de index.Rmd
#'
#' @param ruta_index character. Ruta del archivo index.Rmd.
#' @return NULL invisible.
validar_index_rmd_911 <- function(ruta_index) {
  tryCatch(
    {
      if (!file.exists(ruta_index)) {
        stop("No existe index.Rmd en la ruta esperada.")
      }

      lineas <- readLines(ruta_index, warn = FALSE, encoding = "UTF-8")

      requeridos <- c(
        "---",
        "# Introducción",
        "# Comunicación reproducible con R Markdown",
        "# Relevancia del dataset 911 CDMX",
        "# Descripción y calidad de los datos",
        "# Metodología de procesamiento en R",
        "# Tarea 1. Incidentes por mes, alcaldía y categoría",
        "## Datos utilizados",
        "## Objetivos específicos",
        "## Hallazgos e interpretación",
        "# Tarea 2. Día de la semana con más incidentes",
        "# Tarea 3. Distribución horaria para DELITO, EMERGENCIA y URGENCIAS MÉDICAS",
        "# Tarea 4. Tiempo entre creación y cierre del incidente",
        "# Tarea 5. Porcentaje de llamadas clasificadas como FALSA ALARMA",
        "# Cierre integrado y reflexión inferencial",
        "# Publicación del análisis: GitHub Pages y Shiny Apps",
        "# Enlaces del proyecto",
        "# Cierre técnico",
        "# Referencias"
      )

      for (patron in requeridos) {
        if (!any(grepl(patron, lineas, fixed = TRUE))) {
          stop("Falta sección requerida en index.Rmd: ", patron)
        }
      }

      log_index_911("OK", "Validación de estructura index.Rmd V2 correcta.")
      invisible(NULL)
    },
    error = function(e) {
      log_index_911("ERROR", paste("Validación fallida:", e$message))
      stop(e$message)
    }
  )
}

#' Crear index.Rmd del proyecto 911 de forma portable
#'
#' @param root character | NULL. Ruta raíz del proyecto. Si es NULL, se infiere.
#' @param overwrite logical. Si TRUE sobrescribe index.Rmd existente.
#' @return character. Ruta del archivo index.Rmd creado o existente.
crear_index_rmd_911 <- function(root = NULL, overwrite = FALSE) {
  tryCatch(
    {
      root <- resolver_root_index_911(root)
      log_index_911("INFO", paste("Raíz del proyecto:", root))

      rutas <- crear_directorios_index_911(root)
      insumos <- contar_insumos_index_911(rutas)

      if (insumos$n_csv == 0L) {
        log_index_911(
          "WARN",
          "No hay CSV todavía en dashboard_911/data. El index.Rmd se creará igual y mostrará avisos hasta que existan insumos."
        )
      }

      if (insumos$n_png == 0L) {
        log_index_911(
          "WARN",
          "No hay PNG todavía en dashboard_911/www. El index.Rmd se creará igual y mostrará avisos hasta que existan insumos."
        )
      }

      ruta_index <- escribir_index_rmd_911(rutas, overwrite = overwrite)
      validar_index_rmd_911(ruta_index)

      cat("\n============================================================\n")
      cat("PIPELINE crear_index_rmd_911_PORTABLE_V2 TERMINADO\n")
      cat("============================================================\n")
      cat("Archivo index.Rmd:\n")
      cat(ruta_index, "\n\n")
      cat("Siguiente paso recomendado:\n")
      cat("source('render_rmarkdown_github_pages_911.R')\n")
      cat("============================================================\n\n")

      ruta_index
    },
    error = function(e) {
      log_index_911("ERROR", e$message)
      stop(e$message)
    }
  )
}

#' Ejecutar pipeline en modo automático controlado
#'
#' @return character | NULL. Ruta del index.Rmd si se ejecuta.
ejecutar_auto_index_rmd_911 <- function() {
  tryCatch(
    {
      auto <- as_logical_index_911(
        Sys.getenv("CREAR_INDEX_RMD_911_AUTO", unset = "TRUE"),
        default = TRUE
      )

      if (!isTRUE(auto)) {
        log_index_911("INFO", "Ejecución automática desactivada.")
        return(invisible(NULL))
      }

      overwrite_auto <- as_logical_index_911(
        Sys.getenv("CREAR_INDEX_RMD_911_OVERWRITE", unset = "FALSE"),
        default = FALSE
      )

      crear_index_rmd_911(
        root = NULL,
        overwrite = overwrite_auto
      )
    },
    error = function(e) {
      log_index_911("ERROR", paste("Error en ejecución automática:", e$message))
      stop(e$message)
    }
  )
}

ejecutar_auto_index_rmd_911()
