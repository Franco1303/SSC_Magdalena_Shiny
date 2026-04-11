# ─────────────────────────────────────────
# global.R — Datos, constantes y funciones compartidas
# CSS Magdalena EDA — Shiny (v2)
# ─────────────────────────────────────────

library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(lubridate)
library(DT)

# ─────────────────────────────────────────
# CONSTANTES DE ESTILO
# ─────────────────────────────────────────
COLOR_BG     <- "#f7f8fa"
COLOR_CARD   <- "#ffffff"
COLOR_ACCENT <- "#1a6b9a"
COLOR_TEXT   <- "#1c2331"
COLOR_MUTED  <- "#6b7280"
COLOR_BORDER <- "#e2e6ea"

KM_COLORS <- c(
  "0"  = "#1a6b9a",
  "1"  = "#c5e71c",
  "3"  = "#1aff00",
  "11" = "#2eaa6b",
  "14" = "#6a00ed",
  "17" = "#e07b2a",
  "18" = "#c0392b",
  "19" = "#7b52ab"
)

BANDAS  <- c("aerosol","blue","green","red","rojo 1","rojo 2",
             "rojo 3","NIR","rojo 4","SWIR1","SWIR2")
INDICES <- c("RANS","VNES","NDTI","NIR/RED")
SSCS    <- c("SSC","SSC2","SSC4")
MESES   <- c("Ene","Feb","Mar","Abr","May","Jun",
             "Jul","Ago","Sep","Oct","Nov","Dic")

WL_NAMES <- c("aerosol","blue","green","red","rojo 1","rojo 2",
              "rojo 3","NIR","rojo 4","SWIR1","SWIR2")
WL_REAL  <- c(443.9, 496.6, 560, 664.5, 703.9, 740.2,
              782.5, 835.1, 864.8, 1613.7, 2202.4)

# ─────────────────────────────────────────
# DATASET PRINCIPAL
# ─────────────────────────────────────────
df <- read_csv("puntos_alternativos.csv", show_col_types = FALSE) %>%
  mutate(
    reflectance_date = as_datetime(reflectance_date),
    scc_date         = as_datetime(scc_date),
    km_label         = paste0("Km ", km),
    km_chr           = as.character(km)
  )

KMS_ALL <- sort(unique(df$km))

# ─────────────────────────────────────────
# PERFILES DE CAMPO
# ─────────────────────────────────────────
load_all_profiles <- function(base_path = "DATOS_FRANCISCO") {
  records <- list()
  if (!dir.exists(base_path)) return(
    data.frame(km=integer(), pm=integer(), depth=numeric(),
               ssc=numeric(), fecha=as.Date(character()))
  )
  month_folders <- list.dirs(base_path, recursive = FALSE, full.names = TRUE)
  for (mf in month_folders) {
    csv_files <- list.files(mf, pattern="\\.csv$", full.names=TRUE, ignore.case=TRUE)
    for (cf in csv_files) {
      tryCatch({
        raw <- tryCatch(
          read_delim(cf, delim=";", col_names=FALSE,
                     locale=locale(encoding="UTF-8"), show_col_types=FALSE),
          error = function(e)
            read_delim(cf, delim=";", col_names=FALSE,
                       locale=locale(encoding="latin1"), show_col_types=FALSE)
        )
        colnames(raw) <- c("km","pm","depth","ssc")
        fecha <- as.Date(tools::file_path_sans_ext(basename(cf)), format="%d%m%Y")
        raw$fecha <- fecha
        records[[length(records)+1]] <- raw
      }, error=function(e) NULL)
    }
  }
  if (length(records)==0) return(
    data.frame(km=integer(), pm=integer(), depth=numeric(),
               ssc=numeric(), fecha=as.Date(character()))
  )
  bind_rows(records)
}

df_profiles <- load_all_profiles()

# ─────────────────────────────────────────
# DATOS HIDROLÓGICOS
# ─────────────────────────────────────────
load_hydro <- function() {
  Q_cal <- tryCatch({
    d <- read_delim("Q_MEDIA_D@29037020.data", delim="|", show_col_types=FALSE)
    colnames(d) <- c("Fecha","Q_calamar")
    d$Fecha <- as_datetime(d$Fecha)
    d
  }, error=function(e) data.frame(Fecha=as.POSIXct(character()), Q_calamar=numeric()))

  TSS_cal <- tryCatch({
    d <- read_delim("TR_KT_D_QS_D@29037020.data", delim="|", show_col_types=FALSE)
    colnames(d) <- c("Fecha","TSS_calamar")
    d$Fecha <- as_datetime(d$Fecha)
    d
  }, error=function(e) data.frame(Fecha=as.POSIXct(character()), TSS_calamar=numeric()))

  Q_baq <- tryCatch({
    d <- read_excel("caudal_ganara.xlsx")
    colnames(d) <- c("Fecha","Q_barranquilla")
    d$Fecha <- as_datetime(d$Fecha)
    d
  }, error=function(e) data.frame(Fecha=as.POSIXct(character()), Q_barranquilla=numeric()))

  merged <- full_join(Q_cal, TSS_cal, by="Fecha") %>%
    full_join(Q_baq, by="Fecha") %>%
    arrange(Fecha) %>%
    mutate(
      ssc_derived  = ifelse(!is.na(TSS_calamar) & !is.na(Q_calamar),
                            ((TSS_calamar * 1e6/86400) / Q_calamar) * 1e6/1e3, NA),
      Q_sinincora  = ifelse(!is.na(Q_calamar),
                            Q_calamar - (0.080649*Q_calamar - 126.19), NA)
    )

  km19_ssc <- df %>% filter(km==19) %>%
    select(Fecha=scc_date, SSC) %>%
    mutate(Fecha=as_datetime(Fecha))

  tss_baq <- if (nrow(km19_ssc)>0 && nrow(Q_baq)>0) {
    inner_join(km19_ssc, Q_baq, by="Fecha") %>%
      mutate(TSS_barranquilla = SSC * Q_barranquilla * 0.0864 / 1000)
  } else {
    data.frame(Fecha=as.POSIXct(character()), SSC=numeric(),
               Q_barranquilla=numeric(), TSS_barranquilla=numeric())
  }

  list(Q_cal=Q_cal, TSS_cal=TSS_cal, Q_baq=Q_baq, merged=merged, tss_baq=tss_baq)
}

hydro      <- load_hydro()
Q_cal      <- hydro$Q_cal
TSS_cal    <- hydro$TSS_cal
Q_baq      <- hydro$Q_baq
df_hydro   <- hydro$merged
df_tss_baq <- hydro$tss_baq

HYDRO_YEAR_MIN <- if (nrow(df_hydro)>0) min(year(df_hydro$Fecha), na.rm=TRUE) else 1972
HYDRO_YEAR_MAX <- if (nrow(df_hydro)>0) max(year(df_hydro$Fecha), na.rm=TRUE) else 2026

# ─────────────────────────────────────────
# BANDAS SENTINEL-2 (para marco teórico)
# ─────────────────────────────────────────
BANDS_S2 <- list(
  list(name="Aerosol (B1)",     lambda=443,  res="60 m", color="#7F77DD",
       desc="Detección de aerosoles costeros. Útil para corrección atmosférica."),
  list(name="Blue (B2)",        lambda=492,  res="10 m", color="#378ADD",
       desc="Alta reflectancia en agua clara. Sensible a sedimentos finos."),
  list(name="Green (B3)",       lambda=560,  res="10 m", color="#639922",
       desc="Pico de reflectancia del agua. Correlaciona bien con CSS."),
  list(name="Red (B4)",         lambda=665,  res="10 m", color="#E24B4A",
       desc="Fuerte absorción en agua con sedimentos. Clave para NDTI."),
  list(name="Red Edge 1 (B5)", lambda=704,  res="20 m", color="#D85A30",
       desc="Transición rojo-NIR. Sensible a fitoplancton y material orgánico."),
  list(name="Red Edge 2 (B6)", lambda=740,  res="20 m", color="#BA7517",
       desc="Complementa B5 para análisis de vegetación acuática."),
  list(name="Red Edge 3 (B7)", lambda=783,  res="20 m", color="#854F0B",
       desc="Permite separar sedimentos de clorofila."),
  list(name="NIR (B8)",         lambda=833,  res="10 m", color="#3C3489",
       desc="Muy alta reflectancia en agua turbia con sedimentos gruesos."),
  list(name="Red Edge 4 (B8A)",lambda=865,  res="20 m", color="#534AB7",
       desc="Alta sensibilidad a SSC en aguas turbias."),
  list(name="SWIR1 (B11)",      lambda=1614, res="20 m", color="#0F6E56",
       desc="Penetra neblina leve. Discrimina humedad en sedimentos."),
  list(name="SWIR2 (B12)",      lambda=2202, res="20 m", color="#085041",
       desc="Alta absorción en agua. Útil para mapeo de sedimentos costeros.")
)

FORMULAS <- list(
  list(tag="Sedimentos", title="Transporte de sedimentos en suspensión (TSS)",
       sub="Conversión de concentración y caudal a flujo másico diario",
       formula="TSS [ton/día] = SSC [mg/L] × Q [m³/s] × 0.0864",
       desc="El factor 0.0864 convierte mg/L × m³/s a toneladas por día."),
  list(tag="Derivado", title="SSC derivado de TSS y caudal",
       sub="Estimación inversa de concentración a partir de datos hidrológicos",
       formula="SSC [mg/L] = ( TSS [Kt/día] × 10⁶ / 86400 ) / Q [m³/s] × 10⁶ / 10³",
       desc="Permite estimar la CSS cuando se dispone de TSS en Calamar y caudal simultáneo."),
  list(tag="Hidrología", title="Caudal aguas abajo del Canal del Dique",
       sub="Corrección por pérdida de caudal hacia el Canal del Dique",
       formula="Q_sinincora [m³/s] = Q_calamar − ( 0.080649 × Q_calamar − 126.19 )",
       desc="La estación Calamar se ubica aguas arriba del Canal del Dique. Esta fórmula empírica descuenta ese caudal derivado.")
)

# ─────────────────────────────────────────
# FUNCIONES AUXILIARES
# ─────────────────────────────────────────
km_color <- function(km) {
  col <- unname(KM_COLORS[as.character(km)])
  if (length(col)==0 || is.na(col)) return(COLOR_ACCENT)
  col
}

to_fict <- function(wl) {
  s1_real <- c(1550,1700); s2_real <- c(2140,2290)
  s1_fict <- c(0,150);     s2_fict <- c(170,320)
  if (wl>=s1_real[1] && wl<=s1_real[2]) return(s1_fict[1]+(wl-s1_real[1]))
  if (wl>=s2_real[1] && wl<=s2_real[2]) return(s2_fict[1]+(wl-s2_real[1]))
  return(NA_real_)
}

card <- function(..., style_extra="") {
  div(style=paste0(
    "background-color:",COLOR_CARD,";border-radius:10px;padding:28px 32px;",
    "margin-bottom:24px;box-shadow:0 1px 4px rgba(0,0,0,0.07);",
    "border:1px solid ",COLOR_BORDER,";", style_extra), ...)
}

section_title <- function(text, subtitle=NULL) {
  tagList(
    tags$h3(text, style=paste0(
      "font-family:'Merriweather',serif;font-size:20px;color:",COLOR_TEXT,
      ";margin-bottom:6px;font-weight:700;")),
    if (!is.null(subtitle))
      tags$p(subtitle, style=paste0(
        "font-family:'Lato',sans-serif;color:",COLOR_MUTED,
        ";font-size:14px;margin-top:0;margin-bottom:18px;"))
  )
}

stat_card <- function(label, value, unit="") {
  div(style=paste0(
    "background-color:",COLOR_CARD,";border-radius:10px;padding:20px 24px;",
    "text-align:center;box-shadow:0 1px 4px rgba(0,0,0,0.07);border:1px solid ",COLOR_BORDER,";"),
    tags$p(label, style=paste0(
      "font-size:12px;color:",COLOR_MUTED,
      ";margin:0 0 4px 0;text-transform:uppercase;letter-spacing:0.06em;")),
    div(
      tags$span(value, style=paste0(
        "font-family:'Merriweather',serif;font-size:28px;font-weight:700;color:",COLOR_ACCENT,";")),
      tags$span(paste0(" ",unit), style=paste0("font-size:13px;color:",COLOR_MUTED,";"))
    )
  )
}
