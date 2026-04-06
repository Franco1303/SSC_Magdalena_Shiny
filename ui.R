# ─────────────────────────────────────────
# ui.R — Interfaz de usuario
# CSS Magdalena EDA — Shiny
# ─────────────────────────────────────────

source("global.R")

ui <- fluidPage(
  # ── Google Fonts + CSS global ──
  tags$head(
    tags$link(rel="stylesheet",
      href="https://fonts.googleapis.com/css2?family=Merriweather:wght@700&family=Lato:wght@300;400;600;700&display=swap"),
    tags$style(HTML(paste0("
      body { background-color:", COLOR_BG, "; font-family:'Lato',sans-serif;
             color:", COLOR_TEXT, "; margin:0; }
      .navbar-header { display:none; }

      /* Tabs principales */
      .nav-tabs { border-bottom:1px solid ", COLOR_BORDER, "; background:", COLOR_BG, ";
                  padding:0 48px; margin-bottom:0; }
      .nav-tabs > li > a {
        font-family:'Lato',sans-serif; font-size:13.5px; font-weight:600;
        color:", COLOR_MUTED, "; background:", COLOR_BG, ";
        border:none; border-bottom:2px solid ", COLOR_BORDER, ";
        padding:12px 22px; letter-spacing:0.03em; border-radius:0;
      }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        color:", COLOR_ACCENT, "; background:", COLOR_CARD, ";
        border:none; border-bottom:2px solid ", COLOR_ACCENT, "; border-radius:0;
      }
      .tab-content { padding:36px 48px 60px 48px; }

      /* Sub-tabs hidro */
      .hydro-tabs .nav-tabs { padding:0; margin-bottom:16px; }

      /* Selectize / dropdowns */
      .selectize-input { border-color:", COLOR_BORDER, "; border-radius:6px; font-size:13px; }
      .selectize-dropdown { font-size:13px; }

      /* Labels */
      label { font-size:13px; font-weight:600; color:", COLOR_MUTED, "; }

      /* Radio buttons */
      .radio-inline label { font-weight:400; color:", COLOR_TEXT, "; }

      /* Checkbox */
      .checkbox-inline label { font-weight:600; }

      /* Slider */
      .irs--shiny .irs-bar { background:", COLOR_ACCENT, "; border-top:1px solid ", COLOR_ACCENT, ";
                              border-bottom:1px solid ", COLOR_ACCENT, "; }
      .irs--shiny .irs-handle { border:2px solid ", COLOR_ACCENT, "; }
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
        background:", COLOR_ACCENT, "; }

      /* Plotly */
      .plotly .modebar { display:none !important; }

      /* DT table */
      .dataTables_wrapper { font-size:13px; }
      table.dataTable thead th { color:", COLOR_MUTED, "; font-size:12px; font-weight:600; }
      table.dataTable tbody td { color:", COLOR_TEXT, "; }

      /* Objetivo bullets */
      .obj-bullet {
        display:flex; gap:16px; align-items:flex-start; margin-bottom:12px;
      }
      .obj-number {
        min-width:32px; height:32px; border-radius:50%;
        background:", COLOR_ACCENT, "; color:white;
        display:flex; align-items:center; justify-content:center;
        font-weight:700; font-size:14px; flex-shrink:0;
        margin-top:2px;
      }
      /* Conclusion items */
      .concl-num { font-family:'Merriweather',serif; font-size:22px;
                   color:", COLOR_ACCENT, "33; font-weight:700; min-width:40px; }
    ")))
  ),

  # ── HEADER ──
  div(
    style = paste0(
      "background-color:", COLOR_CARD, ";border-bottom:1px solid ", COLOR_BORDER, ";",
      "padding:0 48px;display:flex;align-items:center;gap:16px;",
      "height:64px;box-shadow:0 1px 3px rgba(0,0,0,0.06);"
    ),
    span("🌊", style="font-size:24px;"),
    div(
      tags$span("CSS Magdalena", style=paste0(
        "font-family:'Merriweather',serif;font-size:17px;color:",COLOR_TEXT,";font-weight:700;")),
      tags$span(" — Análisis Exploratorio de Datos", style=paste0(
        "font-family:'Lato',sans-serif;font-size:14px;color:",COLOR_MUTED,";"))
    ),
    div("Universidad del Norte · 2025–2026",
        style=paste0("margin-left:auto;font-size:12px;color:",COLOR_MUTED,";letter-spacing:0.04em;"))
  ),

  # ── TABS PRINCIPALES ──
  tabsetPanel(id="main_tabs", type="tabs",

    # ══════════════════════════════════════
    # INTRODUCCIÓN
    # ══════════════════════════════════════
    tabPanel("Introducción",
      card(
        style_extra = paste0(
          "background:linear-gradient(135deg,", COLOR_ACCENT, "18 0%,", COLOR_CARD, " 60%);",
          "border-left:4px solid ", COLOR_ACCENT, ";padding:36px 40px;"
        ),
        tags$h1(
          "Estimación de Concentración de Sedimentos en Suspensión en el Río Magdalena mediante Imágenes Satelitales Sentinel-2",
          style=paste0("font-family:'Merriweather',serif;font-size:26px;color:",COLOR_TEXT,
                       ";line-height:1.4;margin-bottom:20px;")
        ),
        tags$p("Este Dash presenta un resumen general de mi tesis de pregrado de Geología en la Universidad del Norte,
          que se enfoca en el desarrollo de un modelo empírico para estimar la concentración de sedimentos en suspensión (SSC)
          en el tramo final del río Magdalena a partir de reflectancia superficial del agua obtenida de imágenes satelitales
          Sentinel-2 del programa Copernicus de la Agencia Espacial Europea (ESA). El dataset final está compuesto por
          aquellos puntos de las campañas de campo para los cuales fue posible obtener reflectancia de Sentinel-2
          aplicando criterios de control de calidad rigurosos.",
          style=paste0("font-size:15px;color:",COLOR_TEXT,";line-height:1.8;max-width:820px;margin-bottom:28px;")),
        tags$p("Los datos de campo fueron tomados con un perfilador LISST (laser in situ scattering and transmissometer),
          que permite obtener perfiles verticales de SSC, cada dos semanas entre Junio 2025 y Marzo 2026.
          A continuación se presentan los puntos finales unidos con reflectancia de Sentinel-2 con tolerancia de 1 día.",
          style=paste0("font-size:15px;color:",COLOR_TEXT,";line-height:1.8;max-width:820px;margin-bottom:28px;")),
        div(style="display:flex;gap:16px;flex-wrap:wrap;",
          stat_card("Observaciones", as.character(nrow(df)), "puntos"),
          stat_card("Período", "Jun 2025 – Mar 2026", ""),
          stat_card("Rango CSS", paste0(floor(min(df$SSC,na.rm=T)),"–",ceiling(max(df$SSC,na.rm=T))), "mg/L"),
          stat_card("Estaciones", as.character(length(unique(df$km))), "km")
        )
      ),
      card(
        section_title("Estructura del dashboard"),
        div(style="display:grid;grid-template-columns:repeat(3,1fr);gap:16px;",
          lapply(list(
            list("Introducción","Presentación general del proyecto y resumen estadístico del dataset."),
            list("Contexto","Descripción del área de estudio y relevancia del río Magdalena."),
            list("Problema","Planteamiento del problema de investigación."),
            list("Objetivo","Objetivo general y específicos del estudio."),
            list("Marco Teórico","Fundamentos de teledetección de sedimentos y Sentinel-2."),
            list("EDA","Análisis exploratorio interactivo con filtro global por estación.")
          ), function(x) div(
            style=paste0("background:",COLOR_CARD,";border-radius:10px;padding:16px 20px;",
                         "box-shadow:0 1px 4px rgba(0,0,0,0.07);border:1px solid ",COLOR_BORDER,";"),
            div(x[[1]], style=paste0("font-weight:700;color:",COLOR_ACCENT,";font-size:13px;margin-bottom:4px;")),
            div(x[[2]], style=paste0("font-size:13px;color:",COLOR_MUTED,";line-height:1.6;"))
          ))
        )
      )
    ),

    # ══════════════════════════════════════
    # CONTEXTO
    # ══════════════════════════════════════
    tabPanel("Contexto",
      card(
        section_title("Área de estudio","Tramo estuarino del río Magdalena, Barranquilla, Colombia"),
        tags$p("El río Magdalena es el ecosistema fluvial con la mayor área y extensión en el país,
          cubriendo un área de 257,438 km² que representa el 24% del territorio nacional. Tiene como
          principales tributarios el río Cauca, Sogamoso, San Jorge y Cesar (Restrepo et al., 2006).
          Representa el mayor contribuyente de sedimentos en el Caribe con una descarga de 144 × 10⁶ t yr⁻¹
          (Higgins et al., 2016) y es uno de los principales influyentes en los cambios morfodinámicos del
          Caribe Colombiano (Restrepo et al., 2006).",
          style=paste0("font-size:14.5px;color:",COLOR_TEXT,";line-height:1.8;max-width:820px;margin-bottom:20px;")),
        tags$p("A continuación se presentan las estaciones de medición nombradas por su distancia a la desembocadura.",
          style=paste0("font-size:14.5px;color:",COLOR_TEXT,";line-height:1.8;max-width:820px;margin-bottom:20px;")),
        plotlyOutput("map_estaciones", height="500px")
      ),
      card(
        section_title("Estaciones de muestreo"),
        tags$p("Las estaciones del kilómetro 5 y 7 fueron descartadas del análisis final por estar fuertemente
          afectadas por actividades de dragado que introducen mayor incertidumbre. Las estaciones 0, 1 y 3
          también introducen este tipo de ruido en menor medida.",
          style=paste0("font-size:14.5px;color:",COLOR_TEXT,";line-height:1.8;max-width:820px;margin-bottom:20px;")),
        uiOutput("km_badges")
      ),
      card(
        section_title("Calamar","Estación de monitoreo del IDEAM"),
        tags$p("La estación hidrológica del IDEAM ubicada a unos 100 km de Barranquilla en Calamar es una fuente
          de datos adicionales. En esta no se mide directamente la SSC pero sí el caudal (Q) y la carga sólida
          total (TSS), de las cuales puede derivarse SSC = TSS / Q para comparar con las mediciones de campo.",
          style=paste0("font-size:14.5px;color:",COLOR_TEXT,";line-height:1.8;max-width:820px;margin-bottom:20px;")),
        plotlyOutput("map_calamar", height="400px")
      )
    ),

    # ══════════════════════════════════════
    # PROBLEMA
    # ══════════════════════════════════════
    tabPanel("Problema",
      card(
        style_extra = paste0("border-left:4px solid #e07b2a;"),
        section_title("Planteamiento del problema"),
        tags$p("El monitoreo de la CSS en ríos de gran caudal como el Magdalena representa un desafío
          logístico y económico considerable. Los métodos tradicionales requieren campañas de campo
          intensivas con equipos especializados como el perfilador LISST.",
          style=paste0("font-size:14.5px;color:",COLOR_TEXT,";line-height:1.8;margin-bottom:16px;")),
        tags$p("La teledetección satelital con Sentinel-2 ofrece una alternativa de bajo costo con cobertura
          sistemática. Sin embargo, en entornos estuarinos la estimación de CSS es compleja por la interferencia
          de otros constituyentes ópticos y los efectos de marea.",
          style=paste0("font-size:14.5px;color:",COLOR_TEXT,";line-height:1.8;"))
      ),
      div(style="display:grid;grid-template-columns:1fr 1fr;gap:20px;",
        card(
          tags$h4("Limitaciones del monitoreo tradicional",
            style=paste0("font-family:'Merriweather',serif;font-size:15px;color:",COLOR_TEXT,";margin-bottom:14px;")),
          tags$ul(
            tags$li("Alta demanda de recursos para campañas de campo"),
            tags$li("Cobertura temporal limitada a fechas de muestreo"),
            tags$li("Variabilidad espacial difícil de capturar puntualmente"),
            tags$li("Influencia de dragados en zonas del canal navegable"),
            style="font-size:14px;padding-left:18px;"
          )
        ),
        card(
          tags$h4("Potencial de la teledetección",
            style=paste0("font-family:'Merriweather',serif;font-size:15px;color:",COLOR_TEXT,";margin-bottom:14px;")),
          tags$ul(
            tags$li("Revisita cada 5 días con Sentinel-2"),
            tags$li("Cobertura espacial continua del tramo fluvial"),
            tags$li("Datos gratuitos accesibles mediante Google Earth Engine"),
            tags$li("Posibilidad de reconstrucción histórica de series de CSS"),
            style="font-size:14px;padding-left:18px;"
          )
        )
      )
    ),

    # ══════════════════════════════════════
    # OBJETIVO
    # ══════════════════════════════════════
    tabPanel("Objetivo",
      card(
        style_extra = paste0("border-left:4px solid ", COLOR_ACCENT, ";"),
        section_title("Objetivo general"),
        tags$p("Estimar la concentración superficial de sedimento en suspensión (SSC) en el sector fluvial
          entre Calamar y Bocas de Ceniza (bajo río Magdalena) mediante un modelo empírico derivado de
          variables espectrales satelitales e información hidrosedimentológica in situ, orientado a
          caracterizar su variabilidad espaciotemporal.",
          style=paste0("font-size:15px;color:",COLOR_TEXT,";line-height:1.8;max-width:800px;font-weight:600;"))
      ),
      card(
        section_title("Objetivos específicos"),
        div(
          lapply(list(
            "Caracterizar la respuesta espectral del agua asociada a diferentes concentraciones de sedimento
            en suspensión, utilizando bandas del visible, NIR y SWIR (e índices espectrales derivados),
            en el sector fluvial entre Calamar y Bocas de Ceniza.",
            "Calibrar y validar un modelo empírico de estimación de SSC a partir de variables espectrales
            satelitales, empleando información hidrosedimentológica in situ para el sector fluvial entre
            Calamar y Bocas de Ceniza.",
            "Cuantificar la variabilidad espaciotemporal de la SSC superficial en el sector fluvial entre
            Calamar y Bocas de Ceniza, a partir de la serie satelital estimada, incluyendo estacionalidad,
            eventos extremos y tendencias."
          ), function(t) {
            i <- which(list(
              "Caracterizar", "Calibrar", "Cuantificar"
            ) == substr(t, 1, 9))
            div(class="obj-bullet",
              div(class="obj-number", which(c("Caracterizar","Calibrar","Cuantificar") ==
                    strsplit(trimws(t)," ")[[1]][1])),
              tags$p(t, style=paste0("font-size:14.5px;color:",COLOR_TEXT,";line-height:1.7;margin:0;"))
            )
          })
        )
      )
    ),

    # ══════════════════════════════════════
    # MARCO TEÓRICO
    # ══════════════════════════════════════
    tabPanel("Marco Teórico",
      card(
        section_title("Teledetección de sedimentos en suspensión","Fundamentos físicos y estado del arte"),
        tags$p("La estimación de CSS mediante teledetección se basa en la relación entre la reflectancia
          espectral del agua y la concentración de partículas en suspensión. Los sedimentos aumentan la
          reflectancia en las bandas roja y NIR al incrementar la retrodispersión de la señal.",
          style=paste0("font-size:14.5px;color:",COLOR_TEXT,";line-height:1.8;"))
      ),
      div(style="display:grid;grid-template-columns:1fr 1fr;gap:20px;",
        card(
          tags$h4("Sentinel-2 MSI",
            style=paste0("font-family:'Merriweather',serif;font-size:15px;color:",COLOR_TEXT,";margin-bottom:14px;")),
          DT::dataTableOutput("sentinel_table")
        ),
        card(
          tags$h4("Índices espectrales evaluados",
            style=paste0("font-family:'Merriweather',serif;font-size:15px;color:",COLOR_TEXT,";margin-bottom:14px;")),
          lapply(list(
            list("RANS","(Red+NIR)/(Red+NIR+Blue+Green+SWIR1+SWIR2)","Índice normalizado para sedimentos"),
            list("VNES","(Red+RE1+NIR)/(Blue+Green+Red+NIR+SWIR1+SWIR2)","Variante extendida con red edge"),
            list("NDTI","(Red−Green)/(Red+Green)","Índice de turbidez normalizado"),
            list("NIR/RED","NIR/Red","Relación simple entre NIR y rojo")
          ), function(x) div(
            style=paste0("margin-bottom:16px;padding-bottom:16px;border-bottom:1px solid ",COLOR_BORDER,";"),
            div(x[[1]], style=paste0("font-weight:700;color:",COLOR_ACCENT,";font-size:14px;margin-bottom:4px;")),
            div(x[[2]], style="font-family:monospace;font-size:12px;color:#6b7280;margin-bottom:4px;"),
            div(x[[3]], style=paste0("font-size:13px;color:",COLOR_TEXT,";line-height:1.5;"))
          ))
        )
      ),
      card(
        section_title("Transporte de sedimentos en suspensión (TSS)"),
        div("TSS [ton/día] = CSS [mg/L] × Q [m³/s] × 0.0864",
          style=paste0("font-family:monospace;font-size:15px;background-color:",COLOR_ACCENT,"18;",
                       "border:1px solid ",COLOR_ACCENT,"44;border-radius:6px;",
                       "padding:14px 20px;color:",COLOR_TEXT,";margin-bottom:12px;"))
      ),
      card(
        section_title("SSC derivado de TSS y Caudal"),
        div("SSC [mg/L] = TSS / Q",
          style=paste0("font-family:monospace;font-size:15px;background-color:",COLOR_ACCENT,"18;",
                       "border:1px solid ",COLOR_ACCENT,"44;border-radius:6px;",
                       "padding:14px 20px;color:",COLOR_TEXT,";margin-bottom:12px;"))
      )
    ),

    # ══════════════════════════════════════
    # EDA
    # ══════════════════════════════════════
    tabPanel("EDA",

      # ── Filtro global ──
      card(
        style_extra = paste0("border-left:4px solid ",COLOR_ACCENT,";padding:20px 32px;"),
        section_title("Filtro global por estación",
          "Selecciona los kilómetros a incluir en todo el análisis. Los km 1–11 pueden introducir ruido por dragados."),
        div(style="display:flex;align-items:center;gap:16px;flex-wrap:wrap;",
          checkboxGroupInput("km_filter", label=NULL,
            choices  = setNames(KMS_ALL, paste0("Km ", KMS_ALL)),
            selected = KMS_ALL,
            inline   = TRUE
          ),
          textOutput("km_filter_count")
        )
      ),

      # ── Estadísticas ──
      card(
        section_title("Estadísticas descriptivas","Resumen del subconjunto seleccionado"),
        DT::dataTableOutput("stats_table")
      ),

      # ── Perfiles de campo ──
      card(
        section_title("Perfiles de concentración de campo",
          "Perfiles verticales de SSC medidos con LISST — selecciona fecha, Km y transecto"),
        div(style="display:flex;gap:16px;margin-bottom:16px;align-items:center;flex-wrap:wrap;",
          div(style="width:180px;",
            selectInput("profile_fecha","Fecha:", choices=NULL)),
          div(style="width:110px;",
            selectInput("profile_km","Km:", choices=NULL)),
          div(style="width:110px;",
            selectInput("profile_pm","+m:", choices=NULL))
        ),
        uiOutput("profile_stats_ui"),
        plotlyOutput("profile_plot", height="480px")
      ),

      # ── Distribución ──
      card(
        section_title("Distribución de CSS por estación",
          "Histograma y boxplot de concentración de sedimentos en suspensión"),
        div(style="width:200px;margin-bottom:16px;",
          selectInput("dist_variable","Variable:",
            choices = c("SSC", BANDAS, INDICES), selected="SSC")),
        plotlyOutput("dist_plot", height="380px")
      ),

      # ── Series de tiempo ──
      card(
        section_title("Series de tiempo","Evolución temporal de CSS y reflectancia por estación"),
        div(style="width:200px;margin-bottom:16px;",
          selectInput("ts_variable","Variable:",
            choices = c("SSC", BANDAS, INDICES), selected="SSC")),
        plotlyOutput("ts_plot", height="360px")
      ),

      # ── Scatter ──
      card(
        section_title("Relación reflectancia / CSS","Scatter con ajuste de regresión y estadísticos"),
        div(style="display:flex;gap:16px;margin-bottom:16px;align-items:flex-end;flex-wrap:wrap;",
          div(style="width:160px;",
            selectInput("scatter_x","Variable X:", choices=c(BANDAS,INDICES), selected="red")),
          div(style="width:160px;",
            selectInput("scatter_y","Variable Y (CSS):", choices=SSCS, selected="SSC")),
          div(radioButtons("scatter_transform","Transformación Y:",
              choices=c("CSS"="linear","ln(CSS)"="log"), selected="log", inline=TRUE)),
          div(radioButtons("scatter_color","Color por:",
              choices=c("Km"="km","Ninguno"="none"), selected="km", inline=TRUE))
        ),
        plotlyOutput("scatter_plot", height="400px"),
        uiOutput("scatter_stats_ui")
      ),

      # ── Firmas espectrales ──
      card(
        section_title("Firmas espectrales","Reflectancia por banda para cada observación, coloreada por CSS"),
        div(style="width:160px;margin-bottom:16px;",
          selectInput("spec_km","Estación (km):",
            choices=c("Todas"="all"), selected="all")),
        plotlyOutput("spec_plot", height="560px")
      ),

      # ── Hidrología ──
      card(
        section_title("Hidrología — Calamar y Barranquilla",
          "Series de tiempo, estacionalidad y relaciones entre caudal y transporte de sedimentos"),
        div(class="hydro-tabs",
          tabsetPanel(id="hydro_tabs",
            tabPanel("Series de tiempo",  value="hydro_ts",   plotlyOutput("hydro_ts_plot",  height="520px"),
                     uiOutput("hydro_ts_stats")),
            tabPanel("Estacionalidad",    value="hydro_seas", plotlyOutput("hydro_seas_plot", height="440px")),
            tabPanel("Q vs TSS Calamar",  value="hydro_qtss", plotlyOutput("hydro_qtss_plot", height="440px"),
                     uiOutput("hydro_qtss_stats")),
            tabPanel("Q Calamar vs Q Baq",value="hydro_qq",  plotlyOutput("hydro_qq_plot",   height="460px")),
            tabPanel("TSS Barranquilla",  value="hydro_tss",  plotlyOutput("hydro_tss_plot",  height="460px"),
                     uiOutput("hydro_tss_note"))
          )
        ),
        tags$br(),
        div(
          tags$label("Intervalo de años:",
            style=paste0("font-size:13px;color:",COLOR_MUTED,";font-weight:600;display:block;margin-bottom:8px;")),
          sliderInput("hydro_years", label=NULL,
            min=HYDRO_YEAR_MIN, max=HYDRO_YEAR_MAX,
            value=c(2010, HYDRO_YEAR_MAX), step=1, sep="",
            width="100%")
        )
      ),

      # ── Correlación ──
      card(
        section_title("Matriz de correlación",
          "Correlación de Pearson entre bandas espectrales, índices y CSS"),
        div(
          radioButtons("corr_transform","Transformación CSS:",
            choices=c("CSS"="linear","ln(CSS)"="log"), selected="log", inline=TRUE)),
        plotlyOutput("corr_plot", height="480px")
      ),

      # ── Ranking correlaciones ──
      card(
        section_title("Ranking de correlaciones con CSS",
          "Correlación de Pearson entre cada banda/índice y CSS, ordenado por valor absoluto"),
        div(
          radioButtons("corrbar_transform","Transformación CSS:",
            choices=c("CSS"="linear","ln(CSS)"="log"), selected="log", inline=TRUE)),
        plotlyOutput("corrbar_plot", height="420px")
      ),

      # ── Mapa de calor ──
      card(
        section_title("Mapa de calor espacio-temporal",
          "CSS promedio por estación (km) y fecha de imagen — dataset matcheado"),
        div(style="width:180px;margin-bottom:16px;",
          selectInput("heatmap_var","Variable:",
            choices=c("SSC",BANDAS,INDICES), selected="SSC")),
        plotlyOutput("heatmap_plot", height="380px")
      ),

      # ── Climatograma ──
      card(
        section_title("Climatograma de CSS",
          "Distribución mensual de CSS en el período de estudio — dataset matcheado"),
        plotlyOutput("climo_plot", height="420px")
      )

    ), # fin EDA

    # ══════════════════════════════════════
    # CONCLUSIONES
    # ══════════════════════════════════════
    tabPanel("Conclusiones",
      card(
        style_extra = paste0("border-left:4px solid ",COLOR_ACCENT,";"),
        section_title("Conclusiones del análisis exploratorio"),
        lapply(list(
          list("01","Dataset final de calibración",
            "El proceso de control de calidad consolidó entre 27 y 39 observaciones con R² 0.58–0.73 en km 0, 1, 3, 14, 17, 18 y 19. Aunque podrian ser mas si finalmente los puntos de Calamar ofrecen resultados favorables.."),
          list("02","Bandas más informativas",
            "Las bandas Red y NIR mostraron las correlaciones más altas con CSS, consistente con la literatura."),
          list("03","Perspectivas de modelado",
            "Con 30 puntos el dataset es apto para regresión potencial log-log y regresión múltiple, validadas con LOOCV. De ampliarse podria aplicarse algoritmos de machine learning como Random Forest."),
          list("04","Limitaciones",
            "Un modelo de este tipo busca ofrecer una alternativa ante la falta importante de datos in situ, pero su desarrollo esta tambien limitado por esta problematica")
        ), function(x) div(
          style="display:flex;gap:16px;margin-bottom:20px;",
          div(x[[1]], class="concl-num"),
          div(
            tags$strong(paste0(x[[2]],": "),
              style=paste0("color:",COLOR_TEXT,";font-size:14.5px;")),
            tags$span(x[[3]],
              style=paste0("font-size:14.5px;color:",COLOR_TEXT,";line-height:1.7;"))
          )
        ))
      ),
      card(
        section_title("Referencias"),
        tags$ul(
          tags$li("Qiu, Z., Liu, D., Duan, M., Chen, P., Yang, C., Li, K., & Duan, H. (2024). Four-decades of sediment transport variations in the Yellow River on the Loess Plateau using Landsat imagery. Remote Sensing of Environment, 306. https://doi.org/10.1016/j.rse.2024.114147"),
          tags$li("Qiu, Z., Liu, D., Yan, N., Yang, C., Chen, P., Zhang, C., & Duan, H. (2024). Improving the observations of suspended sediment concentrations in rivers from Landsat to Sentinel-2 imagery. International Journal of Applied Earth Observation and Geoinformation, 134. https://doi.org/10.1016/j.jag.2024.104209"),
          tags$li("Restrepo, J. D., Zapata, P., Díaz, J. M., Garzón-Ferreira, J., & García, C. B. (2006). Fluvial fluxes into the Caribbean Sea and their impact on coastal ecosystems: The Magdalena River, Colombia. Global and Planetary Change, 50(1–2), 33–49. https://doi.org/10.1016/j.gloplacha.2005.09.002"),
          tags$li("Yepez, S., Laraque, A., Martinez, J. M., De Sa, J., Carrera, J. M., Castellanos, B., Gallay, M., & Lopez, J. L. (2018). Retrieval of suspended sediment concentrations using Landsat-8 OLI satellite images in the Orinoco River (Venezuela). Comptes Rendus - Geoscience, 350(1–2), 20–30. https://doi.org/10.1016/j.crte.2017.08.004"),
        )
      )
    )

  ) # fin tabsetPanel
)
