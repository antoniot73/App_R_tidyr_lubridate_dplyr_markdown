# Análisis 911 CDMX con R Markdown, GitHub Pages y Shiny Apps

## Credenciales académicas del proyecto

| Campo | Información |
|---|---|
| **Autor** | Antonio Nicolás Toro González |
| **Programa académico** | Maestría en Inteligencia Artificial para la Transformación Digital |
| **Institución** | Instituto Internacional de Aguascalientes |
| **Tutor académico** | Dr. Jonás Velasco Álvarez |
| **Tipo de proyecto** | Proyecto académico aplicado de ciencia de datos, R Markdown, GitHub Pages y Shiny Apps |
| **Dataset base** | Llamadas cerradas al 911 de la Ciudad de México |
| **Fecha** | Junio de 2026 |

## Licencia académica

Este repositorio se publica con fines **académicos, formativos, demostrativos y de revisión docente** en el marco de la Maestría en Inteligencia Artificial para la Transformación Digital.

Se permite consultar, ejecutar, adaptar y reutilizar el código con fines educativos, siempre que se mantenga la atribución al autor, al programa académico y a la institución.

No se permite presentar este trabajo como propio sin atribución, eliminar la información de autoría, usarlo con fines comerciales sin autorización expresa, ni redistribuir versiones modificadas sin indicar claramente los cambios realizados.

Los datos utilizados provienen de fuentes públicas del Gobierno de la Ciudad de México. La interpretación, procesamiento, visualización y comunicación de resultados son responsabilidad del autor del proyecto.

**Cita sugerida:**

Toro González, A. N. (2026). *Análisis 911 CDMX con R Markdown, GitHub Pages y Shiny Apps*. Proyecto académico de la Maestría en Inteligencia Artificial para la Transformación Digital, Instituto Internacional de Aguascalientes.


---

Repositorio para procesar, documentar y publicar un análisis exploratorio de llamadas cerradas al **911 de la Ciudad de México**.

El proyecto integra dos salidas:

1. **GitHub Pages**: publicación estática del reporte HTML generado con R Markdown.
2. **Shiny Apps**: publicación interactiva del dashboard construido con Shiny.

## 1. Objetivo del proyecto

El objetivo es transformar registros públicos del 911 CDMX en productos analíticos reproducibles:

- archivos CSV agregados;
- gráficos PNG;
- reporte HTML con R Markdown;
- aplicación interactiva Shiny;
- carpeta `docs/` lista para GitHub Pages;
- carpeta `dashboard_911/` lista para Shiny Apps.

El alcance del análisis es **exploratorio y descriptivo**. Los resultados permiten identificar patrones territoriales, temporales y operativos, pero no establecen relaciones causales.

---

## 2. Estructura general del repositorio

```text
.
├── README.md
├── eda_911_cdmx_v4.R
├── crear_app_shiny_911.R
├── preparar_dashboard_shiny_911.R
├── crear_index_rmd_911.R
├── render_rmarkdown_github_pages_911.R
├── probar_shiny_local_911.R
├── orquestar_proyecto_911.R
├── dashboard_911/
│   ├── app.R
│   ├── index.Rmd
│   ├── index.html
│   ├── data/
│   │   └── *.csv
│   └── www/
│       └── *.png
└── docs/
    ├── index.html
    ├── data/
    │   └── *.csv
    └── www/
        └── *.png
```

---

## 3. Arquitectura lógica del proyecto

```text
Datasets originales 911 CDMX
        ↓
eda_911_cdmx_v4.R
        ↓
salidas_911_eda_*/
        ↓
crear_app_shiny_911.R
        ↓
dashboard_911/app.R
        ↓
preparar_dashboard_shiny_911.R
        ↓
dashboard_911/data/
dashboard_911/www/
        ↓
crear_index_rmd_911.R
        ↓
dashboard_911/index.Rmd
        ↓
render_rmarkdown_github_pages_911.R
        ↓
dashboard_911/index.html
docs/index.html
docs/data/
docs/www/
        ↓
GitHub Pages

dashboard_911/app.R
dashboard_911/data/
dashboard_911/www/
        ↓
probar_shiny_local_911.R
        ↓
Shiny local / shinyapps.io
```

---

## 4. Pipelines oficiales

| Pipeline | Función |
|---|---|
| `eda_911_cdmx_v4.R` | Procesa los datos originales del 911 CDMX y genera CSV, PNG y salidas analíticas. |
| `crear_app_shiny_911.R` | Genera `dashboard_911/app.R`. |
| `preparar_dashboard_shiny_911.R` | Copia los CSV y PNG requeridos hacia `dashboard_911/data/` y `dashboard_911/www/`. |
| `crear_index_rmd_911.R` | Genera el documento fuente `dashboard_911/index.Rmd`. |
| `render_rmarkdown_github_pages_911.R` | Renderiza el reporte HTML y actualiza `docs/` para GitHub Pages. |
| `probar_shiny_local_911.R` | Valida o levanta la app Shiny localmente. Solo debe ejecutarse desde el menú del orquestador. |
| `orquestar_proyecto_911.R` | Controla la ejecución secuencial del proyecto y muestra el menú local de despliegues. |

---

## 5. Archivo principal de ejecución

El archivo principal de operación es:

```r
source("orquestar_proyecto_911.R")
```

Los demás pipelines deben considerarse componentes internos del flujo. Solo conviene ejecutarlos manualmente para mantenimiento puntual o depuración.

---

## 6. Secuencia automática del orquestador

El orquestador ejecuta automáticamente esta secuencia:

```text
1. eda_911_cdmx_v4.R
2. crear_app_shiny_911.R
3. preparar_dashboard_shiny_911.R
4. crear_index_rmd_911.R
5. render_rmarkdown_github_pages_911.R
```

Después de esa secuencia muestra un menú local.

`probar_shiny_local_911.R` **no se ejecuta automáticamente**. Se invoca únicamente desde el menú local.

---

## 7. Menú local de despliegues

Al final de la orquestación aparece:

```text
MENÚ LOCAL DE DESPLIEGUES 911
============================================================
1. Abrir reporte R Markdown local: docs/index.html
2. Validar app Shiny local sin levantar servidor
3. Levantar app Shiny local en segundo plano: dashboard_911/app.R
4. Detener servidor Shiny local iniciado por el orquestador
5. Consultar estado del servidor Shiny local
0. Salir sin abrir despliegues
============================================================
```

### Uso de las opciones

| Opción | Acción |
|---|---|
| `1` | Abre localmente `docs/index.html`. |
| `2` | Ejecuta validaciones de Shiny sin abrir servidor. |
| `3` | Levanta Shiny local en segundo plano. |
| `4` | Detiene el servidor Shiny iniciado por el orquestador. |
| `5` | Consulta si el servidor Shiny local sigue activo. |
| `0` | Sale del menú. |

---

## 8. Ejecución recomendada desde cero

Desde la raíz del proyecto:

```r
source("orquestar_proyecto_911.R")
```

El orquestador valida si ya existen los archivos requeridos y evita reprocesar innecesariamente.

---

## 9. Salidas esperadas

Al terminar correctamente deben existir:

```text
dashboard_911/app.R
dashboard_911/index.Rmd
dashboard_911/index.html
dashboard_911/data/*.csv
dashboard_911/www/*.png
docs/index.html
docs/data/*.csv
docs/www/*.png
```

---

## 10. GitHub Pages

La carpeta pública para GitHub Pages es:

```text
docs/
```

Configurar GitHub Pages así:

```text
Settings
  → Pages
    → Source: Deploy from a branch
    → Branch: main
    → Folder: /docs
```

GitHub Pages publica contenido estático:

- HTML;
- CSS;
- JavaScript;
- PNG;
- CSV.

GitHub Pages no ejecuta R ni Shiny.

---

## 11. Shiny Apps

La app Shiny se encuentra en:

```text
dashboard_911/app.R
```

y consume:

```text
dashboard_911/data/*.csv
dashboard_911/www/*.png
```

Antes de desplegar en shinyapps.io, validar desde el menú del orquestador:

```text
2. Validar app Shiny local sin levantar servidor
```

Luego levantar localmente:

```text
3. Levantar app Shiny local en segundo plano
```

---

## 12. Despliegue sugerido en shinyapps.io

Una vez validada localmente:

```r
rsconnect::deployApp(
  appDir = "dashboard_911",
  appName = "eda-911-cdmx-updated",
  account = "antoniot73",
  forceUpdate = TRUE
)
```

Ajustar `appName` y `account` si cambian.

---

## 13. Flujo recomendado antes de publicar en GitHub

Validar que existan las salidas principales:

```r
file.exists("dashboard_911/app.R")
file.exists("dashboard_911/index.Rmd")
file.exists("dashboard_911/index.html")
file.exists("docs/index.html")
length(list.files("dashboard_911/data", pattern = "\\.csv$")) > 0
length(list.files("dashboard_911/www", pattern = "\\.png$")) > 0
```

Revisar estado del repositorio:

```bash
git status
```

Agregar archivos:

```bash
git add README.md
git add eda_911_cdmx_v4.R
git add crear_app_shiny_911.R
git add preparar_dashboard_shiny_911.R
git add crear_index_rmd_911.R
git add render_rmarkdown_github_pages_911.R
git add probar_shiny_local_911.R
git add orquestar_proyecto_911.R
git add dashboard_911/app.R
git add dashboard_911/index.Rmd
git add dashboard_911/index.html
git add dashboard_911/data
git add dashboard_911/www
git add docs
```

Confirmar y subir:

```bash
git commit -m "Publica analisis 911 con R Markdown, GitHub Pages y Shiny Apps"
git push origin main
```

---

## 14. Archivos que deben versionarse

Sí deben versionarse:

```text
README.md
scripts .R oficiales
orquestar_proyecto_911.R
dashboard_911/app.R
dashboard_911/index.Rmd
dashboard_911/index.html
dashboard_911/data/*.csv
dashboard_911/www/*.png
docs/index.html
docs/data/*.csv
docs/www/*.png
```

No se recomienda versionar:

```text
datasets originales masivos
archivos temporales
.Rhistory
.RData
.Rproj.user/
backups .bak_*
logs temporales
```

---

## 15. Requisitos principales de R

El proyecto puede requerir paquetes como:

```r
install.packages(c(
  "dplyr",
  "tidyr",
  "lubridate",
  "ggplot2",
  "readr",
  "rmarkdown",
  "knitr",
  "shiny",
  "DT",
  "callr",
  "rsconnect"
))
```

---

## 16. Referencias

C5 de la Ciudad de México. (s. f.). *Versión pública de la base de datos del número de atención a emergencias 9-1-1: Guía del usuario*. Gobierno de la Ciudad de México.  
<https://datos.cdmx.gob.mx/dataset/llamadas-numero-de-atencion-a-emergencias-911>

Xie, Y., Allaire, J. J., & Grolemund, G. (2018). *R Markdown: The definitive guide*. Chapman & Hall/CRC.

---

## 17. Enlaces del proyecto

- **Datasets 911:**  
  <https://datos.cdmx.gob.mx/dataset/llamadas-numero-de-atencion-a-emergencias-911>

- **Repositorio:**  
  <https://github.com/antoniot73/App_R_tidyr_lubridate_dplyr_markdown>

- **Portal de datos abiertos CDMX:**  
  <https://datos.cdmx.gob.mx/>
