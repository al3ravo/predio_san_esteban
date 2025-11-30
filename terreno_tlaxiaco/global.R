# global

# ============================
#   GLOBAL.R OPTIMIZADO
# ============================

library(shiny)
library(shinydashboard)
library(sf)
library(concaveman)
library(tidyverse)
library(janitor)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(plotly)
library(terra)
library(RColorBrewer)

# Desactivar S2 — importante para evitar errores innecesarios
sf_use_s2(FALSE)

# ============================
# Perímetro del terreno
# ============================

pts <- st_read("data/perimetro/perimetro_tlaxiaco.geojson", quiet = TRUE)

# Eliminar Z/M si existen
pts <- st_zm(pts, drop = TRUE)

# Asegurar que son puntos
pts <- st_cast(pts, "POINT")

# Transformar a UTM dinámico
coords <- st_coordinates(pts)
lon_c <- mean(coords[,1])

utm_zone <- floor((lon_c + 180) / 6) + 1
crs_utm  <- paste0(
  "+proj=utm +zone=", utm_zone,
  " +datum=WGS84 +units=m +no_defs"
)

pts_utm <- st_transform(pts, crs_utm)

# Concave Hull para reconstruir el polígono
perimetro_utm <- concaveman(
  pts_utm,
  concavity = 2,
  length_threshold = 0
)

# Asegurar geometría correcta
perimetro_utm <- st_make_valid(perimetro_utm)
perimetro_utm <- st_union(perimetro_utm)
perimetro_utm <- st_collection_extract(perimetro_utm, "POLYGON")

# Transformar a WGS84 para Leaflet
perimetro <- st_transform(perimetro_utm, 4326)
perimetro <- st_make_valid(perimetro)
perimetro <- st_collection_extract(perimetro, "POLYGON")

# ============================
# Métricas del polígono
# ============================

area_m2 <- st_area(perimetro_utm)
area_ha <- as.numeric(area_m2) / 10000

perim_m  <- st_length(st_cast(perimetro_utm, "MULTILINESTRING"))
perim_km <- as.numeric(perim_m) / 1000

# Centroide correcto (en UTM → 4326)
centro_utm <- st_centroid(perimetro_utm)
centro <- st_transform(centro_utm, 4326)
coords_cent <- st_coordinates(centro)




# ============================
# SHP recortados
# ============================

mg_mpios <- st_read("data/shp_recortados/mg_mpios.shp", quiet = TRUE)
mg_localidades <- st_read("data/shp_recortados/mg_localidades.shp", quiet = TRUE)
hidro_lineas <- st_read("data/shp_recortados/hidro_lineas.shp", quiet = TRUE)
hidro_arroyos <- st_read("data/shp_recortados/hidro_arroyos.shp", quiet = TRUE)
rnc <- st_read("data/shp_recortados/rnc.shp", quiet = TRUE)
usv <- st_read("data/shp_recortados/usv.shp", quiet = TRUE)
rtp <- st_read("data/shp_recortados/rtp.shp", quiet = TRUE)
suelos <- st_read("data/shp_recortados/edafologia.shp", quiet = TRUE)
puntos <- st_read("data/puntos/puntos_predio.geojson")
# clima: 
regiones_clima <- st_read("data/shp_recortados/regiones_clima.shp", quiet = TRUE)
promedio_anual_regional <- st_read("data/shp_recortados/promedio_anual_regional.shp", quiet = TRUE) %>%
  select(any_of(
    c(
      # Temperatura
      "TMINMEANP","TMAXMEANP","TNN_P","TNX_P","TXN_P","TXX_P","DTR_P",
      # Precipitación
      "PRCPT_P","R10MM_P","R20MM_P","R25MM_P","R95P_P","R99P_P",
      "SDII_P","Rx1DAYP","Rx5DAYP",
      # Fenología
      "GSL_P","CDD_P","CWD_P",
      # Otros extremos
      "FDO_P","SU25_P","TR20_P"
    )
  )) %>%
  mutate(
    etiqueta_html = paste0(
      "<div style='font-size:12px;'>", # Opcional: ajustar tamaño de fuente
      "<strong>DATOS CLIMÁTICOS REGIONALES</strong><br/>",
      "<hr>", # Una línea separadora
      
      #"<strong>--- TEMPERATURA (°C) ---</strong><br/>",
      #"<b>Temp. Mínima Promedio (TMINMEANP):</b> ", round(TMINMEANP, 1), " °C<br/>",
      #"<b>Temp. Máxima Promedio (TMAXMEANP):</b> ", round(TMAXMEANP, 1), " °C<br/>",
      #"<b>Rango Diurno (DTR_P):</b> ", round(DTR_P, 1), " °C<br/>",
      #"<b>Mínima Extrema (TNN_P):</b> ", round(TNN_P, 1), " °C<br/>",
      #"<b>Mínima más Alta (TNX_P):</b> ", round(TNX_P, 1), " °C<br/>",
      #"<b>Máxima más Baja (TXN_P):</b> ", round(TXN_P, 1), " °C<br/>",
      #"<b>Máxima Extrema (TXX_P):</b> ", round(TXX_P, 1), " °C<br/>",
      
      #"<br/><strong>--- PRECIPITACIÓN (mm) ---</strong><br/>",
      #"<b>Precipitación Total Anual (PRCPT_P):</b> ", round(PRCPT_P, 0), " mm<br/>",
      #"<b>Intensidad Diaria (SDII_P):</b> ", round(SDII_P, 1), " mm/día<br/>",
      #"<b>Máx. en 1 día (Rx1DAYP):</b> ", round(Rx1DAYP, 1), " mm<br/>",
      #"<b>Máx. en 5 días (Rx5DAYP):</b> ", round(Rx5DAYP, 1), " mm<br/>",
      
      "<br/><strong>--- DÍAS CON LLUVIA (Días/año) ---</strong><br/>",
      "<b>Lluvia > 10mm (R10MM_P):</b> ", round(R10MM_P, 0), " días<br/>",
      "<b>Lluvia > 20mm (R20MM_P):</b> ", round(R20MM_P, 0), " días<br/>",
      "<b>Lluvia > 25mm (R25MM_P):</b> ", round(R25MM_P, 0), " días<br/>",
      "<b>Días muy húmedos (>95%) (R95P_P):</b> ", round(R95P_P, 0), " mm<br/>",
      "<b>Días ext. húmedos (>99%) (R99P_P):</b> ", round(R99P_P, 0), " mm<br/>",
      
      "<br/><strong>--- FENOLOGÍA Y EXTREMOS ---</strong><br/>",
      "<b>Estación de Cultivo (GSL_P):</b> ", round(GSL_P, 0), " días<br/>",
      "<b>Días Secos Consecutivos (CDD_P):</b> ", round(CDD_P, 0), " días<br/>",
      "<b>Días Húmedos Consecutivos (CWD_P):</b> ", round(CWD_P, 0), " días<br/>",
      "<b>Días de Heladas (<0°C) (FDO_P):</b> ", round(FDO_P, 0), " días<br/>",
      "<b>Días de Verano (>25°C) (SU25_P):</b> ", round(SU25_P, 0), " días<br/>",
      "<b>Noches Tropicales (>20°C) (TR20_P):</b> ", round(TR20_P, 0), " días<br/>",
      "</div>"
    )
  )

climatologia <- st_read("data/shp_recortados/climatologia_final.shp", quiet = TRUE) %>% 
  mutate(
    # Lógica de Colores (Semáforo de Riesgo)
    c_frio    = ifelse(TMIN_EN < 12, "#D32F2F", "#388E3C"), # Rojo si < 12°C
    c_lluvia  = ifelse(P5_ANUA > 250, "#F57C00", "#333333"), # Naranja si > 250mm
    c_helada  = ifelse(DIAS_HE > 1, "#1976D2", "#9E9E9E"), # Azul si hay heladas
    c_oscil   = ifelse(OSCILAC > 15, "#C2185B", "#333333"), # Rosa fuerte si oscila mucho
    
    etiqueta_html = paste0(
      "<div style='font-family: Roboto, Arial, sans-serif; font-size: 11px; width: 230px; color: #333;'>",
      
      # ENCABEZADO
      "<div style='background-color: #ECEFF1; padding: 4px; border-bottom: 2px solid #607D8B; margin-bottom: 5px;'>",
      "<strong>📍 REGIÓN: ", NR_CLIM, "</strong>",
      "</div>",
      
      # SECCIÓN 1: AGUA (Icono Gota)
      "<div style='margin-bottom: 4px;'>",
      "<strong>💧 HIDROLOGÍA (Riesgo)</strong><br/>",
      "<table style='width:100%; border-collapse:collapse;'>",
      "<tr><td>Lluvia Torrencial (5d):</td><td style='text-align:right; color:", c_lluvia, ";'><b>", round(P5_ANUA, 0), " mm</b></td></tr>",
      "<tr><td>Pico Máximo (Sep):</td><td style='text-align:right;'>", round(P5_SEP, 0), " mm</td></tr>",
      "<tr><td>Periodo Seco:</td><td style='text-align:right;'><b>", round(DIAS_SE, 0), " días</b></td></tr>",
      "</table>",
      "</div>",
      
      # SECCIÓN 2: TEMPERATURA (Icono Termómetro)
      "<div style='margin-bottom: 4px; border-top: 1px dotted #ccc; padding-top: 4px;'>",
      "<strong>🌡️ CONFORT TÉRMICO</strong><br/>",
      "<table style='width:100%; border-collapse:collapse;'>",
      "<tr><td>Mínima Enero:</td><td style='text-align:right; color:", c_frio, ";'><b>", round(TMIN_EN, 1), "°C</b></td></tr>",
      "<tr><td>Máxima Mayo:</td><td style='text-align:right;'>", round(TMAX_MA , 1), "°C</td></tr>",
      "<tr><td>Oscilación (Mar):</td><td style='text-align:right; color:", c_oscil, ";'>", round(OSCILAC, 1), "°C</td></tr>",
      "<tr><td>Heladas/Año:</td><td style='text-align:right; color:", c_helada, ";'>", round(DIAS_HE, 1), " días</td></tr>",
      "</table>",
      "</div>",
      
      # CONCLUSIÓN
      "<div style='background-color: #FFF3E0; padding: 3px; border-radius: 3px; margin-top: 5px;'>",
      "<strong>⚠️ ALERTAS PROYECTO:</strong><br/>",
      ifelse(TMIN_EN < 12, "• <span style='color:#D32F2F;'><b>INVERNADERO OBLIGATORIO</b></span> (Frío)<br/>", ""),
      ifelse(OSCILAC > 15, "• <span style='color:#C2185B;'>Shock Térmico (Alevines)</span><br/>", ""),
      ifelse(P5_ANUA > 250, "• <span style='color:#F57C00;'>Reforzar Taludes (Deslave)</span>", ""),
      "</div>",
      
      "</div>"
    )
  )
curvas_nivel <- st_read("data/shp_recortados/curvas_nivel.shp", quiet = TRUE)
puntos_referencia <- st_read("data/shp_recortados/puntos_referencia_topografica.shp", quiet = TRUE)

# DEM
dem_oaxaca <- rast("data/DEM_San_Esteban.tif")
dem_proyectado <- project(dem_oaxaca, "EPSG:3857")
valores_elevacion <- minmax(dem_proyectado)
min_elevacion <- valores_elevacion[1]
max_elevacion <- valores_elevacion[2]

# Usamos colorNumeric para asignar un color a cada rango de elevación
paleta_elevacion <- colorNumeric(
  # Usamos la paleta de colores 'Spectral' de RColorBrewer, invertida
  palette = brewer.pal(11, "Spectral") %>% rev(), 
  domain = c(min_elevacion, max_elevacion),
  na.color = "transparent" # Mantiene transparente los valores sin datos
)


# ============================
# Bounding box
# ============================

bb <- st_bbox(perimetro)
bb <- unname(as.numeric(bb))
names(bb) <- NULL

# ======================
# Validaciones
# ======================

# Validar bbox (soluciona errores silenciosos)
if (any(is.na(bb)) || any(!is.finite(bb))) {
  stop("BBox inválido: revisa el objeto 'perimetro'")
}

if (st_is_empty(perimetro)) {
  stop("El perímetro está vacío o no se generó correctamente.")
}

# ==============================
# Calcular intersección espacial
# ==============================

usv_predio <- st_intersection(usv, perimetro)


usv_utm <- st_transform(usv, crs_utm)
perimetro_utm  # ya lo tienes

usv_predio_utm <- st_intersection(usv_utm, perimetro_utm)

usv_predio_utm$area_m2 <- st_area(usv_predio_utm)
usv_predio_utm$area_ha <- as.numeric(usv_predio_utm$area_m2) / 10000

usv_summary <- usv_predio_utm %>%
  st_drop_geometry() %>%
  group_by(DESCRIPCIO) %>%
  summarise(area_ha = sum(area_ha)) %>%
  arrange(desc(area_ha)) %>% 
  rename(`Uso de suelo` = DESCRIPCIO, `Área (ha)` = area_ha)


# ============================
# Agregar emojis a puntos
# ============================

puntos <- puntos %>% 
  mutate(
    icono = case_when(
      REMARKS %in% c("casa", "casa c piso") ~ "casa.png",
      REMARKS == "pozo" ~ "pozo.png",
      TRUE ~ "punto.png"))

# ============================
# Cargar info TerraClimate consolidada
# ============================

terraclimate_consolidado_largo <- read_csv("data/terraclimate/terraclimate_consolidado_largo.csv", show_col_types = FALSE)
terraclimate_consolidado_ancho <- read_csv("data/terraclimate/terraclimate_consolidado_ancho.csv", show_col_types = FALSE)


