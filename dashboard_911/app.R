# ============================================================
# Dashboard Shiny - EDA Llamadas 911 CDMX
# Versión ajustada para despliegue web
#
# Estructura esperada:
# dashboard_911_shiny/
# ├── app.R
# ├── data/
# │   ├── 01_diagnostico_calidad_general.csv
# │   ├── 01_diagnostico_por_archivo.csv
# │   ├── 01_diagnostico_rango_fechas.csv
# │   ├── 02_tarea1_conteo_mes_alcaldia_categoria.csv
# │   ├── 02_tarea1_promedio_mensual_por_alcaldia_categoria.csv
# │   ├── 03_tarea2_dia_semana_maximo.csv
# │   ├── 03_tarea2_incidentes_por_dia_semana.csv
# │   ├── 04_tarea3_distribucion_hora_categoria.csv
# │   ├── 04_tarea3_hora_pico_por_categoria.csv
# │   ├── 05_tarea4_duraciones_negativas.csv
# │   ├── 05_tarea4_resumen_tiempo_cierre.csv
# │   ├── 06_tarea5_distribucion_categorias.csv
# │   └── 06_tarea5_porcentaje_falsa_alarma.csv
# └── www/
#     ├── 02_tarea1_promedio_mensual_alcaldia_categoria.png
#     ├── 03_tarea2_incidentes_por_dia_semana.png
#     ├── 04_tarea3_distribucion_horaria_categorias.png
#     ├── 05_tarea4_histograma_duracion_minutos.png
#     └── 06_tarea5_porcentaje_por_clasificacion.png
#
# Nota:
# Esta app NO carga los datasets originales. Solo consume salidas agregadas
# y diagnósticos generados previamente por eda_911_cdmx_v4.R.
# ============================================================

# ------------------------------------------------------------
# 1. Bitácora y validación
# ------------------------------------------------------------

#' Registrar evento del dashboard
#'
#' Imprime mensajes de bitácora con marca temporal, nivel y descripción.
#'
#' @param nivel character. Nivel del evento: INFO, WARN o ERROR.
#' @param mensaje character. Descripción del evento.
#' @return NULL de forma invisible.
log_event <- function(nivel, mensaje) {
  cat(sprintf("[%s] [%s] %s\n", Sys.time(), nivel, mensaje))
  invisible(NULL)
}

#' Validar paquetes requeridos
#'
#' Revisa que los paquetes necesarios para ejecutar el dashboard estén instalados.
#'
#' @param paquetes character vector. Nombres de paquetes requeridos.
#' @return TRUE si todos los paquetes están disponibles; si falta alguno, lanza error.
validar_paquetes <- function(paquetes) {
  for (pkg in paquetes) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Falta instalar el paquete requerido: %s", pkg))
    }
  }

  log_event("INFO", "Paquetes validados correctamente.")
  TRUE
}

#' Cargar paquetes requeridos
#'
#' Carga las librerías necesarias para interfaz web, lectura, manipulación
#' y despliegue de tablas.
#'
#' @return TRUE si la carga termina correctamente.
cargar_paquetes <- function() {
  suppressPackageStartupMessages({
    library(shiny)
    library(readr)
    library(dplyr)
    library(DT)
  })

  log_event("INFO", "Paquetes cargados correctamente.")
  TRUE
}

#' Obtener especificación de archivos para el dashboard
#'
#' Define de forma centralizada los CSV y PNG que debe consumir Shiny.
#'
#' @return list con vectores csv y png.
obtener_archivos_requeridos <- function() {
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

#' Validar estructura de carpetas del dashboard
#'
#' Comprueba que existan las carpetas data y www dentro del directorio de la app.
#'
#' @return list con rutas data_dir y www_dir.
validar_directorios_dashboard <- function() {
  app_dir <- getwd()
  data_dir <- file.path(app_dir, "data")
  www_dir <- file.path(app_dir, "www")

  if (!dir.exists(data_dir)) {
    stop("No existe la carpeta data/. Ejecuta primero preparar_dashboard_shiny_911.R.")
  }

  if (!dir.exists(www_dir)) {
    stop("No existe la carpeta www/. Ejecuta primero preparar_dashboard_shiny_911.R.")
  }

  log_event("INFO", paste("Directorio app:", app_dir))
  log_event("INFO", paste("Directorio data:", data_dir))
  log_event("INFO", paste("Directorio www:", www_dir))

  list(data_dir = data_dir, www_dir = www_dir)
}

#' Validar archivos requeridos
#'
#' Comprueba que todos los CSV estén en data/ y todos los PNG en www/.
#'
#' @param data_dir character. Ruta de la carpeta data.
#' @param www_dir character. Ruta de la carpeta www.
#' @param archivos list. Lista de archivos requeridos.
#' @return TRUE si todos los archivos existen.
validar_archivos_requeridos <- function(data_dir, www_dir, archivos) {
  faltantes <- character(0)

  for (archivo in archivos$csv) {
    ruta <- file.path(data_dir, archivo)

    if (!file.exists(ruta)) {
      faltantes <- c(faltantes, file.path("data", archivo))
    }
  }

  for (archivo in archivos$png) {
    ruta <- file.path(www_dir, archivo)

    if (!file.exists(ruta)) {
      faltantes <- c(faltantes, file.path("www", archivo))
    }
  }

  if (length(faltantes) > 0) {
    stop(
      "Faltan archivos requeridos para el dashboard:\n",
      paste(faltantes, collapse = "\n")
    )
  }

  log_event("INFO", "Archivos requeridos validados correctamente.")
  TRUE
}

# ------------------------------------------------------------
# 2. Lectura y preparación de salidas agregadas
# ------------------------------------------------------------

#' Leer CSV del dashboard
#'
#' Lee un CSV agregado desde la carpeta data con manejo controlado de errores.
#'
#' @param data_dir character. Ruta de la carpeta data.
#' @param archivo character. Nombre del CSV.
#' @return tibble con los datos leídos.
leer_csv_dashboard <- function(data_dir, archivo) {
  ruta <- file.path(data_dir, archivo)

  tryCatch(
    {
      datos <- readr::read_csv(ruta, show_col_types = FALSE, progress = FALSE)
      log_event("INFO", sprintf("CSV cargado: %s | filas: %s", archivo, nrow(datos)))
      datos
    },
    error = function(e) {
      log_event("ERROR", paste("No se pudo leer", archivo, ":", e$message))
      stop(e)
    }
  )
}

#' Cargar todas las salidas agregadas
#'
#' Lee únicamente las tablas requeridas por las cinco tareas del EDA.
#'
#' @param data_dir character. Ruta de la carpeta data.
#' @return list con tibbles por tarea.
cargar_salidas_dashboard <- function(data_dir) {
  list(
    diagnostico_calidad = leer_csv_dashboard(data_dir, "01_diagnostico_calidad_general.csv"),
    diagnostico_archivo = leer_csv_dashboard(data_dir, "01_diagnostico_por_archivo.csv"),
    diagnostico_fechas = leer_csv_dashboard(data_dir, "01_diagnostico_rango_fechas.csv"),
    tarea1_conteo = leer_csv_dashboard(data_dir, "02_tarea1_conteo_mes_alcaldia_categoria.csv"),
    tarea1_promedio = leer_csv_dashboard(data_dir, "02_tarea1_promedio_mensual_por_alcaldia_categoria.csv"),
    tarea2_maximo = leer_csv_dashboard(data_dir, "03_tarea2_dia_semana_maximo.csv"),
    tarea2_dias = leer_csv_dashboard(data_dir, "03_tarea2_incidentes_por_dia_semana.csv"),
    tarea3_horas = leer_csv_dashboard(data_dir, "04_tarea3_distribucion_hora_categoria.csv"),
    tarea3_picos = leer_csv_dashboard(data_dir, "04_tarea3_hora_pico_por_categoria.csv"),
    tarea4_negativas = leer_csv_dashboard(data_dir, "05_tarea4_duraciones_negativas.csv"),
    tarea4_resumen = leer_csv_dashboard(data_dir, "05_tarea4_resumen_tiempo_cierre.csv"),
    tarea5_categorias = leer_csv_dashboard(data_dir, "06_tarea5_distribucion_categorias.csv"),
    tarea5_falsa = leer_csv_dashboard(data_dir, "06_tarea5_porcentaje_falsa_alarma.csv")
  )
}

#' Formatear número entero
#'
#' Devuelve un número con separador de miles.
#'
#' @param x numeric. Valor numérico.
#' @return character formateado.
formatear_entero <- function(x) {
  if (length(x) == 0 || is.na(x)) {
    return("N/D")
  }

  format(round(as.numeric(x), 0), big.mark = ",", scientific = FALSE, trim = TRUE)
}

#' Formatear decimal
#'
#' Devuelve un número decimal con separador de miles.
#'
#' @param x numeric. Valor numérico.
#' @param digitos integer. Número de decimales.
#' @return character formateado.
formatear_decimal <- function(x, digitos = 2) {
  if (length(x) == 0 || is.na(x)) {
    return("N/D")
  }

  formatC(as.numeric(x), format = "f", digits = digitos, big.mark = ",")
}

#' Formatear porcentaje
#'
#' Devuelve un porcentaje con símbolo.
#'
#' @param x numeric. Valor porcentual en escala 0-100.
#' @param digitos integer. Número de decimales.
#' @return character formateado.
formatear_porcentaje_dashboard <- function(x, digitos = 2) {
  if (length(x) == 0 || is.na(x)) {
    return("N/D")
  }

  paste0(formatC(as.numeric(x), format = "f", digits = digitos, big.mark = ","), "%")
}

#' Obtener métrica desde tabla vertical
#'
#' Busca una métrica en una tabla con columnas metrica y valor.
#'
#' @param datos data.frame. Tabla de resumen.
#' @param nombre_metrica character. Métrica a buscar.
#' @return numeric o NA_real_.
obtener_metrica <- function(datos, nombre_metrica) {
  if (!is.data.frame(datos) || nrow(datos) == 0) {
    return(NA_real_)
  }

  if (!all(c("metrica", "valor") %in% names(datos))) {
    return(NA_real_)
  }

  valor <- datos |>
    dplyr::filter(.data$metrica == nombre_metrica) |>
    dplyr::pull(.data$valor)

  if (length(valor) == 0) {
    return(NA_real_)
  }

  as.numeric(valor[1])
}

#' Tomar valor seguro de una tabla
#'
#' Extrae un valor por columna y posición, controlando tablas vacías.
#'
#' @param datos data.frame. Tabla fuente.
#' @param columna character. Columna a extraer.
#' @param fila integer. Fila a extraer.
#' @param default character. Valor por defecto.
#' @return character con el valor encontrado.
tomar_valor <- function(datos, columna, fila = 1, default = "N/D") {
  if (!is.data.frame(datos) || nrow(datos) < fila || !(columna %in% names(datos))) {
    return(default)
  }

  valor <- datos[[columna]][fila]

  if (is.na(valor)) {
    return(default)
  }

  as.character(valor)
}

#' Obtener indicador de diagnóstico general
#'
#' Extrae un valor desde la tabla 01_diagnostico_calidad_general.csv,
#' cuya estructura esperada es indicador-valor.
#'
#' @param datos data.frame. Tabla de diagnóstico general.
#' @param indicador_buscado character. Indicador requerido.
#' @return numeric o NA_real_.
obtener_indicador_diagnostico <- function(datos, indicador_buscado) {
  if (!is.data.frame(datos) || nrow(datos) == 0) {
    return(NA_real_)
  }

  if (!all(c("indicador", "valor") %in% names(datos))) {
    return(NA_real_)
  }

  valor <- datos |>
    dplyr::filter(.data$indicador == indicador_buscado) |>
    dplyr::pull(.data$valor)

  if (length(valor) == 0) {
    return(NA_real_)
  }

  as.numeric(valor[1])
}

#' Obtener rango de fecha desde diagnóstico
#'
#' Extrae mínimo y máximo para una variable temporal desde
#' 01_diagnostico_rango_fechas.csv.
#'
#' @param datos data.frame. Tabla de rangos de fechas.
#' @param variable_buscada character. Variable temporal requerida.
#' @return character con formato mínimo a máximo.
obtener_rango_fecha_diagnostico <- function(datos, variable_buscada) {
  if (!is.data.frame(datos) || nrow(datos) == 0) {
    return("N/D")
  }

  columnas_requeridas <- c("variable", "minimo", "maximo")

  if (!all(columnas_requeridas %in% names(datos))) {
    return("N/D")
  }

  fila <- datos |>
    dplyr::filter(.data$variable == variable_buscada) |>
    utils::head(1)

  if (nrow(fila) == 0) {
    return("N/D")
  }

  paste(fila$minimo[1], "a", fila$maximo[1])
}

# ------------------------------------------------------------
# 3. Componentes visuales reutilizables
# ------------------------------------------------------------

#' Crear tarjeta KPI
#'
#' Construye una tarjeta visual para indicadores clave.
#'
#' @param titulo character. Título de la tarjeta.
#' @param valor character. Valor principal.
#' @param detalle character. Texto explicativo.
#' @return UI tag de Shiny.
crear_kpi <- function(titulo, valor, detalle) {
  div(
    class = "kpi-card",
    h4(titulo),
    div(class = "kpi-value", valor),
    div(class = "kpi-detail", detalle)
  )
}

#' Crear tabla interactiva
#'
#' Construye un objeto DT con filtros, paginación y desplazamiento horizontal.
#'
#' @param datos data.frame. Datos a mostrar.
#' @return objeto DT.
crear_tabla_dt <- function(datos) {
  if (!is.data.frame(datos) || nrow(datos) == 0) {
    datos <- data.frame(mensaje = "No hay datos disponibles para esta sección.")
  }

  DT::datatable(
    datos,
    rownames = FALSE,
    filter = "top",
    extensions = c("Buttons"),
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel")
    )
  )
}

#' Insertar imagen estática del EDA
#'
#' Inserta un PNG localizado en www/.
#'
#' @param archivo_png character. Nombre del archivo PNG.
#' @param texto_alt character. Texto alternativo.
#' @return UI tag de Shiny.
insertar_png <- function(archivo_png, texto_alt) {
  if (!file.exists(file.path(getwd(), "www", archivo_png))) {
    return(div(class = "alert alert-warning", paste("Imagen no disponible:", archivo_png)))
  }

  div(
    class = "plot-container",
    tags$img(
      src = archivo_png,
      alt = texto_alt,
      class = "plot-img"
    )
  )
}

#' Crear bloque de tabla
#'
#' Construye una sección visual con título y tabla DT.
#'
#' @param titulo character. Título de la tabla.
#' @param output_id character. ID de salida DT.
#' @return UI tag de Shiny.
crear_bloque_tabla <- function(titulo, output_id) {
  div(
    class = "table-card",
    h3(titulo),
    DT::DTOutput(output_id)
  )
}


#' Crear bloque de objetivos y hallazgos
#'
#' Construye una tarjeta doble para cada tarea con objetivos específicos
#' y hallazgos principales. Está diseñada para enriquecer la etapa de
#' visualización sin modificar la preparación analítica del pipeline.
#'
#' @param objetivo character vector. Objetivos específicos del análisis.
#' @param hallazgos character vector. Hallazgos principales e interpretación.
#' @return UI tag de Shiny.
crear_bloque_objetivo_hallazgos <- function(objetivo, hallazgos) {
  div(
    class = "analysis-grid",
    div(
      class = "analysis-card",
      h3("Objetivos específicos del análisis"),
      tags$ul(lapply(objetivo, tags$li))
    ),
    div(
      class = "analysis-card",
      h3("Principales hallazgos e interpretación"),
      tags$ul(lapply(hallazgos, tags$li))
    )
  )
}

#' Crear bloque académico de tarea
#'
#' Construye una sección de tres tarjetas para cada tarea: características
#' de datos utilizados, objetivos específicos y hallazgos principales.
#'
#' @param datos_utilizados character vector. Características de los datos usados.
#' @param objetivo character vector. Objetivos específicos del análisis.
#' @param hallazgos character vector. Hallazgos principales e interpretación.
#' @return UI tag de Shiny.
crear_bloque_academico_tarea <- function(datos_utilizados, objetivo, hallazgos) {
  div(
    class = "analysis-grid three-cols",
    div(
      class = "analysis-card",
      h3("Datos utilizados"),
      tags$ul(lapply(datos_utilizados, tags$li))
    ),
    div(
      class = "analysis-card",
      h3("Objetivos específicos"),
      tags$ul(lapply(objetivo, tags$li))
    ),
    div(
      class = "analysis-card",
      h3("Hallazgos e interpretación"),
      tags$ul(lapply(hallazgos, tags$li))
    )
  )
}

#' Crear bloque de introducción académica
#'
#' Presenta el problema, la relevancia y el alcance metodológico del dashboard.
#'
#' @return UI tag de Shiny.
crear_bloque_introduccion <- function() {
  div(
    class = "table-card academic-card",
    h3("Introducción"),
    p(
      "Este dashboard analiza la versión pública de llamadas cerradas al 911 de la Ciudad de México, ",
      "registros administrativos generados por el servicio de atención a emergencias coordinado por el C5. ",
      "El problema abordado consiste en transformar datos operativos de gran volumen en evidencia descriptiva ",
      "sobre concentración territorial, comportamiento temporal, tiempos de cierre y clasificación de llamadas."
    ),
    p(
      "La relevancia del análisis radica en que estos registros permiten identificar necesidades ciudadanas, ",
      "horarios de mayor presión operativa y zonas con mayor volumen absoluto de reportes. ",
      "La aplicación no busca establecer causalidad ni medir riesgo ajustado; su finalidad es exploratoria, ",
      "descriptiva y orientada a apoyar la lectura operativa de los resultados."
    ),
    p(
      strong("Alcance técnico: "),
      "Shiny consume únicamente CSV agregados y gráficos PNG generados previamente por el pipeline EDA. ",
      "No recalcula los datasets originales, lo que mantiene separadas la preparación de información y la visualización."
    )
  )
}

#' Crear bloque de características generales de los datos
#'
#' Describe origen, estructura y variables analíticas relevantes.
#'
#' @return UI tag de Shiny.
crear_bloque_caracteristicas_datos <- function() {
  div(
    class = "table-card academic-card",
    h3("Características principales de los datos"),
    tags$ul(
      tags$li("Origen: versión pública de la base de datos del número de atención a emergencias 9-1-1 de la Ciudad de México."),
      tags$li("Unidad de análisis: folios cerrados de llamadas al 911, clasificados por variables administrativas, territoriales y temporales."),
      tags$li("Estructura procesada: tres periodos agregados —2021_s1, 2021_s2 y 2022_s1— integrados en salidas CSV para el dashboard."),
      tags$li("Variables analíticas principales: fecha_creacion, fecha_cierre, hora_creacion, alcaldia, categoria_etiqueta, clasificación general, duración entre creación y cierre."),
      tags$li("Insumos de visualización: tablas agregadas en data/ y gráficos estáticos en www/, generados por el pipeline EDA.")
    )
  )
}

#' Crear bloque de enlaces del proyecto
#'
#' Presenta enlaces externos del dataset, repositorio y portal de datos CDMX.
#'
#' @return UI tag de Shiny.

#' Crear bloque de preparación de insumos para GitHub Pages
#'
#' Explica la etapa documental previa al renderizado del reporte estático.
#'
#' @return UI tag de Shiny.
crear_bloque_preparacion_github_pages <- function() {
  div(
    class = "table-card academic-card section-note",
    h3("Preparación de insumos para despliegue en GitHub Pages"),
    p(
      "Antes de renderizar el documento R Markdown, debe ejecutarse la preparación de insumos del proyecto. ",
      "Esta etapa genera los CSV agregados y los gráficos PNG consumidos por el reporte estático y por la app Shiny."
    ),
    p(
      "La salida para GitHub Pages se replica en ",
      tags$code("docs/"),
      ", manteniendo ",
      tags$code("index.html"),
      ", ",
      tags$code("data/"),
      " y ",
      tags$code("www/"),
      "."
    ),
    tags$pre(
      class = "pipeline-code",
      "source(\"render_rmarkdown_github_pages_911.R\")\nsource(\"crear_app_shiny_911.R\")\nsource(\"probar_shiny_local_911.R\")"
    )
  )
}

crear_bloque_enlaces_proyecto <- function() {
  div(
    class = "table-card academic-card reference-card",
    h3("Enlaces del proyecto"),
    tags$ul(
      tags$li(
        tags$strong("Datasets 911: "),
        tags$br(),
        tags$a(
          href = "https://datos.cdmx.gob.mx/dataset/llamadas-numero-de-atencion-a-emergencias-911",
          target = "_blank",
          "https://datos.cdmx.gob.mx/dataset/llamadas-numero-de-atencion-a-emergencias-911"
        )
      ),
      tags$li(
        tags$strong("Repositorio: "),
        tags$br(),
        tags$a(
          href = "https://github.com/antoniot73/App_R_tidyr_lubridate_dplyr_markdown",
          target = "_blank",
          "https://github.com/antoniot73/App_R_tidyr_lubridate_dplyr_markdown"
        )
      ),
      tags$li(
        tags$strong("Datasets CDMX: "),
        tags$br(),
        tags$a(
          href = "https://datos.cdmx.gob.mx/",
          target = "_blank",
          "https://datos.cdmx.gob.mx/"
        )
      )
    )
  )
}


#' Crear cierre integrado con reflexión inferencial
#'
#' Presenta una lectura final de los resultados y una reflexión general.
#'
#' @return UI tag de Shiny.
crear_bloque_cierre_integrado <- function() {
  div(
    class = "table-card academic-card",
    h3("Cierre integrado y reflexión inferencial"),
    p(
      "Los hallazgos muestran que la demanda registrada por el 911 presenta concentraciones claras en tres dimensiones: ",
      "territorial, semanal y horaria. Iztapalapa, Gustavo A. Madero y Cuauhtémoc concentran los mayores volúmenes absolutos; ",
      "domingo y sábado agrupan una parte importante de las llamadas; y las categorías DELITO, EMERGENCIA y URGENCIAS MÉDICAS ",
      "no comparten el mismo patrón horario."
    ),
    p(
      "De manera inferencial, aunque el análisis no permite afirmar causas, sí sugiere que la operación del servicio podría beneficiarse ",
      "de estrategias diferenciadas por territorio, día de la semana, categoría y franja horaria. La evidencia apunta a que una planeación ",
      "uniforme tendría menor capacidad explicativa que una asignación sensible a patrones temporales y territoriales."
    ),
    p(
      "El tiempo de cierre también muestra asimetría: la mediana describe mejor el caso típico que el promedio, debido a valores extremos. ",
      "Por ello, la toma de decisiones debería considerar medidas robustas y documentar inconsistencias temporales antes de interpretar ",
      "los indicadores como tiempos reales de atención."
    )
  )
}

#' Crear bloque de referencia del dataset
#'
#' Muestra la referencia formal del conjunto de datos utilizado.
#'
#' @return UI tag de Shiny.
crear_bloque_referencia_dataset <- function() {
  div(
    class = "table-card academic-card reference-card",
    h3("Referencia del dataset"),
    tags$ul(
      tags$li(
        "C5 de la Ciudad de México. (s. f.). ",
        em("Versión pública de la base de datos del número de atención a emergencias 9-1-1: Guía del usuario. "),
        "Gobierno de la Ciudad de México.",
        tags$br(),
        tags$a(
          href = "https://datos.cdmx.gob.mx/dataset/llamadas-numero-de-atencion-a-emergencias-911",
          target = "_blank",
          "https://datos.cdmx.gob.mx/dataset/llamadas-numero-de-atencion-a-emergencias-911"
        )
      )
    )
  )
}

#' Crear bloque de referencias bibliográficas en Inicio
#'
#' Presenta las referencias bibliográficas principales usadas para documentar
#' el dataset 911 CDMX y el enfoque de comunicación reproducible con R Markdown.
#'
#' @return UI tag de Shiny.
crear_bloque_referencias_bibliograficas_inicio <- function() {
  div(
    class = "table-card academic-card reference-card",
    h3("Referencias bibliográficas"),
    tags$ul(
      tags$li(
        "C5 de la Ciudad de México. (s. f.). ",
        em("Versión pública de la base de datos del número de atención a emergencias 9-1-1: Guía del usuario. "),
        "Gobierno de la Ciudad de México. ",
        tags$br(),
        tags$a(
          href = "https://datos.cdmx.gob.mx/dataset/llamadas-numero-de-atencion-a-emergencias-911",
          target = "_blank",
          "https://datos.cdmx.gob.mx/dataset/llamadas-numero-de-atencion-a-emergencias-911"
        )
      ),
      tags$li(
        "Xie, Y., Allaire, J. J., & Grolemund, G. (2018). ",
        em("R Markdown: The definitive guide. "),
        "Chapman & Hall/CRC."
      )
    )
  )
}

#' Definir estilos CSS del dashboard
#'
#' Establece estilos internos para tarjetas, imágenes y distribución visual.
#'
#' @return UI tag style.
definir_estilos_css <- function() {
  tags$style(HTML("
    body {
      background-color: #f7f9fb;
    }

    .navbar {
      margin-bottom: 0;
    }

    .dashboard-header {
      background: linear-gradient(135deg, #0b5d4d, #16845f);
      color: white;
      padding: 28px 32px;
      margin-bottom: 24px;
      border-radius: 0 0 14px 14px;
    }

    .dashboard-header h2 {
      margin-top: 0;
      font-weight: 700;
    }

    .dashboard-header p {
      font-size: 16px;
      margin-bottom: 0;
    }

    .kpi-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(240px, 1fr));
      gap: 16px;
      margin-bottom: 24px;
    }

    .kpi-card {
      background: white;
      padding: 18px 20px;
      border-radius: 12px;
      box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
      border-left: 5px solid #16845f;
      min-height: 130px;
    }

    .kpi-card h4 {
      margin-top: 0;
      font-size: 15px;
      color: #34495e;
      font-weight: 700;
    }

    .kpi-value {
      font-size: 28px;
      font-weight: 700;
      color: #0b5d4d;
      margin: 8px 0;
    }

    .kpi-detail {
      color: #6c757d;
      font-size: 13px;
    }

    .plot-container {
      background: white;
      padding: 16px;
      margin-bottom: 24px;
      border-radius: 12px;
      box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
    }

    .plot-img {
      max-width: 100%;
      height: auto;
      display: block;
      margin: 0 auto;
      border-radius: 8px;
    }

    .table-card {
      background: white;
      padding: 16px;
      margin-bottom: 24px;
      border-radius: 12px;
      box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
    }

    .table-card h3 {
      margin-top: 0;
      color: #0b5d4d;
      font-size: 20px;
      font-weight: 700;
    }

    .section-note {
      background: #eef8f3;
      border-left: 5px solid #16845f;
      padding: 12px 16px;
      margin-bottom: 18px;
      border-radius: 8px;
      color: #305143;
    }


    .analysis-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
      gap: 16px;
      margin-bottom: 22px;
    }

    .analysis-card {
      background: white;
      padding: 16px 18px;
      border-radius: 12px;
      box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
      border-top: 4px solid #16845f;
    }

    .analysis-card h3 {
      margin-top: 0;
      color: #0b5d4d;
      font-size: 18px;
      font-weight: 700;
    }

    .analysis-card ul {
      padding-left: 20px;
      margin-bottom: 0;
    }

    .analysis-card li {
      margin-bottom: 6px;
      line-height: 1.45;
    }

    .analysis-card .finding-emphasis {
      color: #0b5d4d;
      font-weight: 700;
    }

    .analysis-grid.three-cols {
      grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
    }

    .academic-card {
      line-height: 1.58;
      border-left: 5px solid #0b5d4d;
    }

    .academic-card p {
      margin-bottom: 12px;
    }

    .reference-card a {
      overflow-wrap: anywhere;
      font-weight: 700;
    }

    .dashboard-footer {
      border-top: 1px solid #d9e2dd;
      margin-top: 28px;
      padding: 14px 8px 10px 8px;
      text-align: center;
      font-size: 0.92rem;
      color: #5f6f68;
      font-weight: 600;
    }
  "))
}

# ------------------------------------------------------------
# 4. Interfaz
# ------------------------------------------------------------


#' Crear footer del dashboard
#'
#' Genera el pie de página institucional que debe aparecer al final
#' de cada pestaña del dashboard.
#'
#' @return Componente UI con el texto de autoría del análisis.
crear_footer_dashboard <- function() {
  tags$footer(
    class = "dashboard-footer",
    'Análisis de datasets "llamadas 911 cerradas CDMX" realizado por Antonio Toro'
  )
}

#' Crear interfaz del dashboard
#'
#' Define la estructura visible de la aplicación Shiny usando únicamente
#' los resultados agregados de las cinco tareas.
#'
#' @param datos list. Tablas cargadas del EDA.
#' @return UI de Shiny.
crear_ui <- function(datos) {
  top_t1 <- datos$tarea1_promedio |>
    dplyr::arrange(dplyr::desc(.data$promedio_mensual)) |>
    utils::head(1)

  promedio_top <- tomar_valor(top_t1, "promedio_mensual")
  alcaldia_top <- tomar_valor(top_t1, "alcaldia")
  categoria_top <- tomar_valor(top_t1, "categoria_etiqueta")

  dia_maximo <- tomar_valor(datos$tarea2_maximo, "dia_semana")
  llamadas_dia_maximo <- tomar_valor(datos$tarea2_maximo, "total_llamadas")

  promedio_cierre <- obtener_metrica(datos$tarea4_resumen, "promedio_minutos")
  porcentaje_falsa <- tomar_valor(datos$tarea5_falsa, "porcentaje_falsa_alarma")

  registros_total <- obtener_indicador_diagnostico(datos$diagnostico_calidad, "registros")
  columnas_total <- obtener_indicador_diagnostico(datos$diagnostico_calidad, "columnas")
  duplicados_fila <- obtener_indicador_diagnostico(datos$diagnostico_calidad, "duplicados_fila_completa")
  folios_duplicados <- obtener_indicador_diagnostico(datos$diagnostico_calidad, "folios_duplicados")
  duraciones_negativas <- obtener_indicador_diagnostico(datos$diagnostico_calidad, "duraciones_negativas")
  total_archivos <- if (is.data.frame(datos$diagnostico_archivo)) nrow(datos$diagnostico_archivo) else NA_integer_
  rango_creacion <- obtener_rango_fecha_diagnostico(datos$diagnostico_fechas, "fecha_creacion")
  rango_cierre <- obtener_rango_fecha_diagnostico(datos$diagnostico_fechas, "fecha_cierre")

  shiny::navbarPage(
    title = "EDA 911 CDMX",
    header = tagList(
      definir_estilos_css(),
      div(
        class = "dashboard-header",
        h2("Análisis exploratorio de llamadas 911 CDMX"),
        p("Dashboard Shiny basado únicamente en tablas agregadas y gráficos PNG generados por el pipeline EDA.")
      )
    ),

    shiny::tabPanel(
      "Inicio",
      br(),
      crear_bloque_introduccion(),
      crear_bloque_caracteristicas_datos(),
      crear_bloque_preparacion_github_pages(),
      crear_bloque_enlaces_proyecto(),
      div(
        class = "kpi-grid",
        crear_kpi(
          "Mayor promedio mensual",
          formatear_decimal(promedio_top, 2),
          paste("Alcaldía:", alcaldia_top, "| Categoría:", categoria_top)
        ),
        crear_kpi(
          "Día con más incidentes",
          dia_maximo,
          paste("Total de llamadas:", formatear_entero(llamadas_dia_maximo))
        ),
        crear_kpi(
          "Tiempo promedio de cierre",
          paste(formatear_decimal(promedio_cierre, 2), "min"),
          "Calculado entre creación y cierre del incidente."
        ),
        crear_kpi(
          "Falsa alarma",
          formatear_porcentaje_dashboard(porcentaje_falsa, 2),
          "Porcentaje calculado desde la clasificación general."
        )
      ),
      div(
        class = "section-note",
        strong("Estructura ajustada para Shiny: "),
        "esta aplicación no consume datasets originales; solo lee diagnósticos, CSV agregados de las tareas 1 a 5 y sus PNG correspondientes."
      ),
      crear_bloque_referencias_bibliograficas_inicio(),
      crear_footer_dashboard()
    ),

    shiny::tabPanel(
      "Resumen datasets",
      br(),
      div(
        class = "section-note",
        "Resumen técnico de los tres archivos de diagnóstico generados por el pipeline EDA: calidad general, registros por archivo y rangos de fechas."
      ),
      div(
        class = "kpi-grid",
        crear_kpi(
          "Registros integrados",
          formatear_entero(registros_total),
          paste("Columnas analíticas:", formatear_entero(columnas_total))
        ),
        crear_kpi(
          "Archivos analizados",
          formatear_entero(total_archivos),
          "Corresponden a 2021_s1, 2021_s2 y 2022_s1."
        ),
        crear_kpi(
          "Folios duplicados",
          formatear_entero(folios_duplicados),
          paste("Filas duplicadas completas:", formatear_entero(duplicados_fila))
        ),
        crear_kpi(
          "Duraciones negativas",
          formatear_entero(duraciones_negativas),
          "Casos marcados para revisión antes del resumen de tiempo."
        )
      ),
      div(
        class = "section-note",
        strong("Rangos temporales detectados: "),
        paste("fecha_creacion:", rango_creacion, "| fecha_cierre:", rango_cierre)
      ),
      crear_bloque_tabla("Diagnóstico general de calidad", "tabla_diag_calidad"),
      crear_bloque_tabla("Registros por archivo", "tabla_diag_archivo"),
      crear_bloque_tabla("Rango de fechas", "tabla_diag_fechas"),
      crear_footer_dashboard()
    ),

    shiny::tabPanel(
      "Tarea 1",
      br(),
      div(
        class = "section-note",
        "Agrupación de incidentes por mes, alcaldía y categoría; cálculo de promedio mensual por categoría."
      ),
      crear_bloque_academico_tarea(
        datos_utilizados = c(
          "Insumos: 02_tarea1_conteo_mes_alcaldia_categoria.csv y 02_tarea1_promedio_mensual_por_alcaldia_categoria.csv.",
          "Estructura: combinaciones de mes_cierre, alcaldía y categoria_etiqueta con conteos y promedios mensuales.",
          "Origen: folios cerrados del 911 CDMX previamente integrados y agregados por el pipeline EDA."
        ),
        objetivo = c(
          "Identificar patrones territoriales y mensuales de llamadas cerradas al 911.",
          "Comparar el volumen de incidentes por alcaldía y categoría para ubicar concentraciones relevantes.",
          "Distinguir tendencias de volumen absoluto sin asumir causalidad ni riesgo ajustado por población."
        ),
        hallazgos = c(
          "Iztapalapa concentra los mayores promedios mensuales, principalmente en DELITO, FALTA CÍVICA y SERVICIO.",
          "Gustavo A. Madero y Cuauhtémoc también aparecen entre las alcaldías con mayor volumen absoluto.",
          "El resultado debe interpretarse como concentración de llamadas, no como mayor riesgo territorial sin ajustes demográficos o urbanos."
        )
      ),
      insertar_png(
        "02_tarea1_promedio_mensual_alcaldia_categoria.png",
        "Promedio mensual por alcaldía y categoría"
      ),
      crear_bloque_tabla("Conteo por mes, alcaldía y categoría", "tabla_tarea1_conteo"),
      crear_bloque_tabla("Promedio mensual por alcaldía y categoría", "tabla_tarea1_promedio"),
      crear_footer_dashboard()
    ),

    shiny::tabPanel(
      "Tarea 2",
      br(),
      div(
        class = "section-note",
        "Identificación del día de la semana con más incidentes y total de llamadas asociado."
      ),
      crear_bloque_academico_tarea(
        datos_utilizados = c(
          "Insumos: 03_tarea2_dia_semana_maximo.csv y 03_tarea2_incidentes_por_dia_semana.csv.",
          "Estructura: conteo de llamadas por dia_semana calculado desde fecha_creacion.",
          "Origen: folios cerrados del 911 CDMX agregados por comportamiento semanal."
        ),
        objetivo = c(
          "Determinar qué día de la semana concentra más llamadas al 911.",
          "Reconocer comportamientos semanales útiles para planeación operativa.",
          "Comparar la carga de llamadas entre fines de semana y días laborales."
        ),
        hallazgos = c(
          "Domingo es el día con mayor número de llamadas, seguido por sábado.",
          "Sábado y domingo concentran cerca de cuatro de cada diez llamadas del periodo analizado.",
          "El patrón sugiere mayor presión operativa en fines de semana, aunque no permite establecer causalidad."
        )
      ),
      insertar_png(
        "03_tarea2_incidentes_por_dia_semana.png",
        "Incidentes por día de la semana"
      ),
      crear_bloque_tabla("Día de la semana con mayor número de incidentes", "tabla_tarea2_maximo"),
      crear_bloque_tabla("Incidentes por día de la semana", "tabla_tarea2_dias"),
      crear_footer_dashboard()
    ),

    shiny::tabPanel(
      "Tarea 3",
      br(),
      div(
        class = "section-note",
        "Distribución horaria para las categorías DELITO, EMERGENCIA y URGENCIAS MÉDICAS."
      ),
      crear_bloque_academico_tarea(
        datos_utilizados = c(
          "Insumos: 04_tarea3_distribucion_hora_categoria.csv y 04_tarea3_hora_pico_por_categoria.csv.",
          "Estructura: conteos por hora de 0 a 23 para DELITO, EMERGENCIA y URGENCIAS MÉDICAS.",
          "Origen: registros 911 procesados con hora_reporte y categoría normalizada."
        ),
        objetivo = c(
          "Analizar la distribución horaria de DELITO, EMERGENCIA y URGENCIAS MÉDICAS.",
          "Identificar horas pico por categoría para reconocer comportamientos temporales diferenciados.",
          "Apoyar una lectura operativa por franjas horarias, no uniforme para todas las categorías."
        ),
        hallazgos = c(
          "DELITO presenta mayor concentración nocturna, con pico alrededor de las 21:00.",
          "EMERGENCIA concentra mayor carga entre tarde y noche, con pico cercano a las 19:00.",
          "URGENCIAS MÉDICAS alcanza su mayor volumen alrededor de las 14:00 y mantiene demanda relevante durante tarde y noche."
        )
      ),
      insertar_png(
        "04_tarea3_distribucion_horaria_categorias.png",
        "Distribución horaria por categoría"
      ),
      crear_bloque_tabla("Distribución por hora y categoría", "tabla_tarea3_horas"),
      crear_bloque_tabla("Hora pico por categoría", "tabla_tarea3_picos"),
      crear_footer_dashboard()
    ),

    shiny::tabPanel(
      "Tarea 4",
      br(),
      div(
        class = "section-note",
        "Tiempo entre creación y cierre del incidente. El histograma se limita al percentil 99 para mejorar legibilidad."
      ),
      crear_bloque_academico_tarea(
        datos_utilizados = c(
          "Insumos: 05_tarea4_resumen_tiempo_cierre.csv y 05_tarea4_duraciones_negativas.csv.",
          "Estructura: métricas de duración en minutos entre datetime_creacion y datetime_cierre.",
          "Origen: folios cerrados del 911 CDMX con fechas y horas validadas por el pipeline EDA."
        ),
        objetivo = c(
          "Medir el tiempo transcurrido entre creación y cierre de los incidentes.",
          "Detectar comportamientos atípicos, valores extremos e inconsistencias temporales.",
          "Diferenciar el comportamiento típico del promedio afectado por casos extremos."
        ),
        hallazgos = c(
          "La mediana representa mejor el cierre típico que el promedio, porque la distribución es asimétrica a la derecha.",
          "Existen pocos casos con duraciones negativas, por lo que deben documentarse como inconsistencias y no como tiempos reales.",
          "El histograma se limita al percentil 99 para mejorar la legibilidad visual y reducir distorsión por valores extremos."
        )
      ),
      insertar_png(
        "05_tarea4_histograma_duracion_minutos.png",
        "Histograma de duración entre creación y cierre"
      ),
      crear_bloque_tabla("Resumen del tiempo entre creación y cierre", "tabla_tarea4_resumen"),
      crear_bloque_tabla("Duraciones negativas detectadas", "tabla_tarea4_negativas"),
      crear_footer_dashboard()
    ),

    shiny::tabPanel(
      "Tarea 5",
      br(),
      div(
        class = "section-note",
        "Porcentaje de llamadas clasificadas como FALSA ALARMA y distribución general por clasificación."
      ),
      crear_bloque_academico_tarea(
        datos_utilizados = c(
          "Insumos: 06_tarea5_distribucion_categorias.csv y 06_tarea5_porcentaje_falsa_alarma.csv.",
          "Estructura: distribución porcentual por clasificación general y cálculo específico de FALSA ALARMA.",
          "Origen: clasificación general normalizada de llamadas cerradas al 911 CDMX."
        ),
        objetivo = c(
          "Calcular el porcentaje de llamadas clasificadas como FALSA ALARMA.",
          "Comparar su peso relativo frente a SERVICIO, FALTA CÍVICA, DELITO, EMERGENCIA y URGENCIAS MÉDICAS.",
          "Evitar confundir la clasificación general con el código de cierre del incidente."
        ),
        hallazgos = c(
          "FALSA ALARMA representa una fracción mínima del total de llamadas.",
          "La operación se concentra principalmente en SERVICIO, FALTA CÍVICA y DELITO.",
          "El análisis usa la clasificación general de falsa alarma; no se debe mezclar con codigo_cierre == 'F'."
        )
      ),
      insertar_png(
        "06_tarea5_porcentaje_por_clasificacion.png",
        "Porcentaje por clasificación"
      ),
      crear_bloque_tabla("Porcentaje de Falsa Alarma", "tabla_tarea5_falsa"),
      crear_bloque_tabla("Distribución de llamadas por clasificación", "tabla_tarea5_categorias"),
      crear_footer_dashboard()
    ),

    shiny::tabPanel(
      "Cierre integrado",
      br(),
      crear_bloque_cierre_integrado(),
      crear_bloque_enlaces_proyecto(),
      crear_bloque_referencia_dataset(),
      crear_footer_dashboard()
    )
  )
}

# ------------------------------------------------------------
# 5. Servidor
# ------------------------------------------------------------

#' Crear servidor del dashboard
#'
#' Conecta las tablas agregadas con los componentes DT de la interfaz.
#'
#' @param datos list. Tablas cargadas desde data/.
#' @return función server para shinyApp().
crear_server <- function(datos) {
  force(datos)

  function(input, output, session) {
    output$tabla_diag_calidad <- DT::renderDT({
      crear_tabla_dt(datos$diagnostico_calidad)
    })

    output$tabla_diag_archivo <- DT::renderDT({
      crear_tabla_dt(datos$diagnostico_archivo)
    })

    output$tabla_diag_fechas <- DT::renderDT({
      crear_tabla_dt(datos$diagnostico_fechas)
    })

    output$tabla_tarea1_conteo <- DT::renderDT({
      crear_tabla_dt(datos$tarea1_conteo)
    })

    output$tabla_tarea1_promedio <- DT::renderDT({
      crear_tabla_dt(datos$tarea1_promedio)
    })

    output$tabla_tarea2_maximo <- DT::renderDT({
      crear_tabla_dt(datos$tarea2_maximo)
    })

    output$tabla_tarea2_dias <- DT::renderDT({
      crear_tabla_dt(datos$tarea2_dias)
    })

    output$tabla_tarea3_horas <- DT::renderDT({
      crear_tabla_dt(datos$tarea3_horas)
    })

    output$tabla_tarea3_picos <- DT::renderDT({
      crear_tabla_dt(datos$tarea3_picos)
    })

    output$tabla_tarea4_resumen <- DT::renderDT({
      crear_tabla_dt(datos$tarea4_resumen)
    })

    output$tabla_tarea4_negativas <- DT::renderDT({
      crear_tabla_dt(datos$tarea4_negativas)
    })

    output$tabla_tarea5_categorias <- DT::renderDT({
      crear_tabla_dt(datos$tarea5_categorias)
    })

    output$tabla_tarea5_falsa <- DT::renderDT({
      crear_tabla_dt(datos$tarea5_falsa)
    })

    log_event("INFO", "Servidor Shiny inicializado.")
  }
}

# ------------------------------------------------------------
# 6. Ejecución principal
# ------------------------------------------------------------

#' Ejecutar dashboard Shiny
#'
#' Coordina validación de paquetes, validación de estructura, lectura de archivos,
#' construcción de interfaz y arranque de la aplicación.
#'
#' @return objeto shinyApp.
main <- function() {
  tryCatch(
    {
      validar_paquetes(c("shiny", "readr", "dplyr", "DT"))
      cargar_paquetes()

      archivos <- obtener_archivos_requeridos()
      directorios <- validar_directorios_dashboard()

      validar_archivos_requeridos(
        data_dir = directorios$data_dir,
        www_dir = directorios$www_dir,
        archivos = archivos
      )

      datos <- cargar_salidas_dashboard(directorios$data_dir)

      log_event("INFO", "Dashboard construido correctamente.")

      shiny::shinyApp(
        ui = crear_ui(datos),
        server = crear_server(datos)
      )
    },
    error = function(e) {
      log_event("ERROR", e$message)
      stop(e)
    }
  )
}

main()
