\# CSS Magdalena — Análisis Exploratorio de Datos



Versión en R Shiny de mi Dashboard interactivo desarrollado como parte de la tesis de pregrado en Geología

(Universidad del Norte) para el análisis exploratorio de datos de concentración de

sedimentos en suspensión (CSS) en el tramo estuarino del río Magdalena, Barranquilla.


Desde los años 80 se ha reportado el uso de reflectancia de superficie como un predictor confiable de concentración de sedimentos de cuerpos de aguas naturales.
El exploratorio aqui contenido busca verificar la relación entre la reflectancia extraida de imagenes satelitales Sentinel-2 y SSC medido in situ en el río Magdalena.



\## Requisitos



\- R, Rstudio versión mas reciente

\- Git



\## Instalación y ejecución local



1\. Clona el repositorio:

```

&#x20;  git clone https://github.com/Franco1303/SSC_Magdalena_Shiny.git

&#x20;  cd css-magdalena-eda

```


2\. Instala las dependencias:

```

Ejecuta el Script "dependencias.R" en RStudio

```



4\. Corre la app:

```

&#x20;  Ejecutar ui.R

```



5\. Rstudio abrira una ventana con el Dashborad interactivo



\## Estructura del proyecto

\- `ui.R` — aplicación principal, contiene todos los elementos de la interfas

\- `server.R` — Contiene la logica de las graficas interactivas.

\- `global.R` — Carga datos y dependencias. 

\- `puntos\_finales2.csv` — dataset final de calibración (reflectancia + CSS)

\- `Q\_MEDIA\_D@29037020.data` — serie de caudal diario estación Calamar

\- `TR\_KT\_D\_QS\_D@29037020.data` — serie de TSS diario estación Calamar

\- `caudal\_ganara.xlsx` — caudal medido en Barranquilla (Jun 2025 – Mar 2026)

\- `DATOS\_FRANCISCO/` — perfiles LISST organizados por mes y fecha



\## Datos



Los datos de campo fueron recolectados entre junio 2025 y marzo 2026 en el tramo

estuarino del río Magdalena utilizando un perfilador LISST. Las imágenes Sentinel-2

fueron procesadas mediante Google Earth Engine.

```


