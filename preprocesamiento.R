# Preprocesamiento de shp
setwd("terreno_tlaxiaco")

library(sf)
library(tidyverse)

#
# Recortaré solo San Esteban Atatlahuca y municipios colindantes. 
mg_mpios <- st_read("data/inegi/mg_areas_geoestadisticas_municipales.shp") %>%  
  st_make_valid() %>% 
  st_transform(4326)

mun_target <- mg_mpios %>% 
  filter(CVE_MUN == "133" | NOMGEO == "San Esteban Atatlahuca")

mun_vecinos <- mg_mpios %>% 
  filter(st_touches(., mun_target, sparse = FALSE))

mun_buffer <- rbind(mun_target, mun_vecinos)

#
# Crear un bounding box
bb <- st_bbox(mun_buffer)

#
# Recortar todos los shp y guardar

# Marco Geoestadístico (municipios y localidades)
mg_mpios <- st_read("data/inegi/mg_areas_geoestadisticas_municipales.shp") %>%  
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.))
st_write(mg_mpios, "data/shp_recortados/mg_mpios.shp")

mg_localidades <- st_read("data/inegi/mg_localidades_puntuales_rurales.shp") %>%  
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.))
st_write(mg_localidades, "data/shp_recortados/mg_localidades.shp")

# Hidrografía (ríos, arroyos)
hidro_lineas <- st_read("data/hidrologia/RH20Ad_poligono_subcuenca.shp") %>%  
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.))
st_write(hidro_lineas, "data/shp_recortados/hidro_lineas.shp")

hidro_arroyos <- st_read("data/hidrologia/RH20Ad_lineas_flujo.shp") %>%  
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.))
st_write(hidro_arroyos, "data/shp_recortados/hidro_arroyos.shp")

# Red Nacional de Caminos
rnc <- st_read("data/caminos/red_vial.shp") %>%  
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.))
st_write(rnc, "data/shp_recortados/rnc.shp")

# Uso de suelo y vegetación Serie VI (clase de vegetación/uso)
usv <- st_read("data/uso_suelo_vegetacion/usv250s6_union.shp") %>%  
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.))
st_write(usv, "data/shp_recortados/usv.shp")

# Regiones Terrestres Prioritarias (CONABIO)
rtp <- st_read("data/biodiversidad/rtp1mgw.shp") %>%  
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.))
st_write(rtp, "data/shp_recortados/rtp.shp")

# Edafología
edafologia <- st_read("data/edafologia/E1409e.shp") %>%  
  st_set_crs(32614) %>% 
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.)) %>% 
  select(GRUPO1, CLASE_TEXT, FRUDICA, HECTARES, geometry) %>%
  mutate(tipo_suelo = recode(GRUPO1,
                             "AC" = "Acrisol",
                             "CM" = "Cambisol",
                             "LP" = "Leptosol",
                             "LV" = "Luvisol",
                             "PZ" = "Phaeozem",
                             "FL" = "Fluvisol",
                             "GL" = "Gleysol",
                             "RG" = "Regosol",
                             "UM" = "Umbrisol",
                             "VR" = "Vertisol",
                             "PH" = "Phaeozem",
                             .default = GRUPO1),
         textura = CLASE_TEXT,
         profundidad = FRUDICA)
st_write(edafologia, "data/shp_recortados/edafologia.shp")

# Climatología
regiones_clima <- st_read("data/climatologia/regiones_clima_itrf08.shp") %>%  
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.)) 
st_write(regiones_clima, "data/shp_recortados/regiones_clima.shp")

promedio_anual_regional <- st_read("data/climatologia/promedio_anual_regional_extremos_climaticos.shp") %>%  
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.))
st_write(promedio_anual_regional, "data/shp_recortados/promedio_anual_regional.shp")

precipitacion_max_1_dia <- st_read("data/climatologia/promedios_regionales_mensuales_y_estacionales_rx1day.shp") %>%  
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.))
st_write(precipitacion_max_1_dia, "data/shp_recortados/precipitacion_max_1_dia.shp")

precipitacion_max_5_dia <- st_read("data/climatologia/promedios_regionales_mensuales_y_estacionales_rx5day.shp") %>%  
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.))
st_write(precipitacion_max_5_dia, "data/shp_recortados/precipitacion_max_5_dia.shp")

rango_temp_diaria <- st_read("data/climatologia/promedios_regionales_mensuales_y_estacionales_dtr.shp") %>%  
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.))
st_write(rango_temp_diaria, "data/shp_recortados/rango_temp_diaria.shp")

temp_max_promedio <- st_read("data/climatologia/promedios_regionales_mensuales_y_estacionales_tmaxmean.shp") %>%  
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.))
st_write(temp_max_promedio, "data/shp_recortados/temp_max_promedio.shp")

temp_min_promedio <- st_read("data/climatologia/promedios_regionales_mensuales_y_estacionales_tminmean.shp") %>%  
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.))
st_write(temp_min_promedio, "data/shp_recortados/temp_min_promedio.shp")

duracion_temporada_crecimiento <- st_read("data/climatologia/tendencia_regional_gsl.shp") %>%  
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.))
st_write(duracion_temporada_crecimiento, "data/shp_recortados/duracion_temporada_crecimiento.shp")

dias_secas_consecutivos <- st_read("data/climatologia/tendencia_regional_cdd.shp") %>%  
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.))
st_write(dias_secas_consecutivos, "data/shp_recortados/dias_secas_consecutivos.shp")

dias_lluvias_consecutivos <- st_read("data/climatologia/tendencia_regional_cwd.shp") %>%  
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.))
st_write(dias_lluvias_consecutivos, "data/shp_recortados/dias_lluvias_consecutivos.shp")

dias_heladas <- st_read("data/climatologia/tendencia_regional_fdo.shp") %>%  
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.))
st_write(dias_heladas, "data/shp_recortados/dias_heladas.shp")

dias_calidos_extremos <- st_read("data/climatologia/tendencia_regional_tx90p.shp") %>%  
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.))
st_write(dias_calidos_extremos, "data/shp_recortados/dias_calidos_extremos.shp")

noche_anormalmente_frias <- st_read("data/climatologia/tendencia_regional_tn10p.shp") %>%  
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.))
st_write(noche_anormalmente_frias, "data/shp_recortados/noche_anormalmente_frias.shp")




capa_maestra <- regiones_clima %>% 
  select(R_CLIM = R_clim, NR_CLIM = NR_clim) # Nos quedamos solo con los IDs básicos

# 2. SELECCIÓN DE VARIABLES CRÍTICAS (Drop geometry para evitar duplicados)
# Extraemos solo lo que realmente afecta la viabilidad productiva

df_precip_1 <- st_drop_geometry(precipitacion_max_1_dia) %>% 
  select(R_CLIM, P1_ANUAL = RX1DAYP_13, P1_SEP = RX1DAYP_9)

df_precip_5 <- st_drop_geometry(precipitacion_max_5_dia) %>% 
  select(R_CLIM, P5_ANUAL = RX5DAYP_13, P5_SEP = RX5DAYP_9)

df_tmin <- st_drop_geometry(temp_min_promedio) %>% 
  select(R_CLIM, TMIN_ENE = TMINMEAP_1, TMIN_ANUAL = TMINMEAP13)

df_tmax <- st_drop_geometry(temp_max_promedio) %>% 
  select(R_CLIM, TMAX_MAY = TMAXMEAP_5)

df_rango <- st_drop_geometry(rango_temp_diaria) %>%
  select(R_CLIM, OSCILACION_MAR = DTR_P_3)

df_heladas <- st_drop_geometry(dias_heladas) %>%
  select(R_CLIM, DIAS_HELADA = FDO_P)

df_dias_secos <- st_drop_geometry(dias_secas_consecutivos) %>% 
  select(R_CLIM, DIAS_SECOS = CDD_P)

# 3. UNIÓN DE DATOS (LEFT JOIN)
# Usamos R_CLIM como llave para pegar todo a la capa maestra

climatologia_final <- capa_maestra %>%
  left_join(df_precip_1, by = "R_CLIM") %>%
  left_join(df_precip_5, by = "R_CLIM") %>%
  left_join(df_tmin, by = "R_CLIM") %>%
  left_join(df_tmax, by = "R_CLIM") %>%
  left_join(df_rango, by = "R_CLIM") %>%
  left_join(df_heladas, by = "R_CLIM") %>% 
  left_join(df_dias_secos, by = "R_CLIM")

# 4. CREACIÓN DE LA ETIQUETA INTELIGENTE (Compacta)
# Usamos estilos CSS para hacerla pequeña pero legible

climatologia_final <- climatologia_final %>%
  mutate(
    # Lógica de Colores (Semáforo de Riesgo)
    c_frio    = ifelse(TMIN_ENE < 12, "#D32F2F", "#388E3C"), # Rojo si < 12°C
    c_lluvia  = ifelse(P5_ANUAL > 250, "#F57C00", "#333333"), # Naranja si > 250mm
    c_helada  = ifelse(DIAS_HELADA > 1, "#1976D2", "#9E9E9E"), # Azul si hay heladas
    c_oscil   = ifelse(OSCILACION_MAR > 15, "#C2185B", "#333333"), # Rosa fuerte si oscila mucho
    
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
      "<tr><td>Lluvia Torrencial (5d):</td><td style='text-align:right; color:", c_lluvia, ";'><b>", round(P5_ANUAL, 0), " mm</b></td></tr>",
      "<tr><td>Pico Máximo (Sep):</td><td style='text-align:right;'>", round(P5_SEP, 0), " mm</td></tr>",
      "<tr><td>Periodo Seco:</td><td style='text-align:right;'><b>", round(DIAS_SECOS, 0), " días</b></td></tr>",
      "</table>",
      "</div>",
      
      # SECCIÓN 2: TEMPERATURA (Icono Termómetro)
      "<div style='margin-bottom: 4px; border-top: 1px dotted #ccc; padding-top: 4px;'>",
      "<strong>🌡️ CONFORT TÉRMICO</strong><br/>",
      "<table style='width:100%; border-collapse:collapse;'>",
      "<tr><td>Mínima Enero:</td><td style='text-align:right; color:", c_frio, ";'><b>", round(TMIN_ENE, 1), "°C</b></td></tr>",
      "<tr><td>Máxima Mayo:</td><td style='text-align:right;'>", round(TMAX_MAY, 1), "°C</td></tr>",
      "<tr><td>Oscilación (Mar):</td><td style='text-align:right; color:", c_oscil, ";'>", round(OSCILACION_MAR, 1), "°C</td></tr>",
      "<tr><td>Heladas/Año:</td><td style='text-align:right; color:", c_helada, ";'>", round(DIAS_HELADA, 1), " días</td></tr>",
      "</table>",
      "</div>",
      
      # CONCLUSIÓN
      "<div style='background-color: #FFF3E0; padding: 3px; border-radius: 3px; margin-top: 5px;'>",
      "<strong>⚠️ ALERTAS PROYECTO:</strong><br/>",
      ifelse(TMIN_ENE < 12, "• <span style='color:#D32F2F;'><b>INVERNADERO OBLIGATORIO</b></span> (Frío)<br/>", ""),
      ifelse(OSCILACION_MAR > 15, "• <span style='color:#C2185B;'>Shock Térmico (Alevines)</span><br/>", ""),
      ifelse(P5_ANUAL > 250, "• <span style='color:#F57C00;'>Reforzar Taludes (Deslave)</span>", ""),
      "</div>",
      
      "</div>"
    )
  )

st_write(climatologia_final, "data/shp_recortados/climatologia_final.shp", delete_layer = TRUE)

# Topografía
curvas <- st_read("data/topografia/curva_nivel250_l.shp") %>% 
  st_make_valid() %>% 
  st_transform(4326) %>%
  select(elevacion, tipo, geometry) %>%
  mutate(
    tipo_curva = case_when(
      tipo == "REGULAR"   ~ "Regular",
      tipo == "DEPRESIÓN" ~ "Depresión",
      TRUE ~ tipo
    ),
    elev = elevacion) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.))
st_write(curvas, "data/shp_recortados/curvas_nivel.shp")

suje <- st_read("data/topografia/terreno_suje250_a.shp") %>% 
  st_make_valid() %>% 
  st_transform(4326) %>%
  select(geografico, geometry) %>%
  mutate(unidad = str_to_title(geografico)) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.))
#st_write(suje, "data/shp_recortados/sujecion.shp") # DENTRO DEL BB NO HAY TÉRRENOS SUJETOS A INUNDACIÓN

ref <- st_read("data/topografia/referencia_g250_p.shp") %>% 
  st_make_valid() %>% 
  st_transform(4326) %>%
  select(nombre, geometry, term_gen) %>%
  mutate(nombre_ref = str_c(nombre, " (", term_gen, ")")) %>% 
  st_crop(bb) %>% 
  filter(!st_is_empty(.))
st_write(ref, "data/shp_recortados/puntos_referencia_topografica.shp")

# DEM (descarga)
library(FedData)
library(sf)
library(terra)
library(elevatr)

# Coordenadas proporcionadas (del bb):
xmin <- -97.85322
ymin <- 16.94409
xmax <- -97.56469
ymax <- 17.34750

# Crear la matriz de coordenadas del polígono
# Nota: La lista de coordenadas debe cerrarse repitiendo el punto inicial
coordenadas_bb <- matrix(c(xmin, ymin, 
                           xmax, ymin, 
                           xmax, ymax, 
                           xmin, ymax, 
                           xmin, ymin), 
                         ncol=2, byrow=TRUE)

# Crear el polígono sf con el sistema de referencia geográfico (WGS 84)
AOI <- st_as_sf(st_sfc(st_polygon(list(coordenadas_bb)), crs = 4326))

# Registrar en OpenTopography y bucar llave (https://portal.opentopography.org/)
# elevatr::set_opentopo_key("a88baf18a62409714cc5fb672ac89bfd")

dem_oaxaca <- get_elev_raster(locations = AOI, z = 12, src = "aws")  # usa AWS Terrain Tiles

dem <- rast(dem_oaxaca)
writeRaster(dem, "data/DEM_aws_z12.tif", overwrite = TRUE) # guardar raster completo

## versión ligera para dashboard: 

# Cargar DEM completo
dem_full <- rast("data/DEM_aws_z12.tif")

# Reducir resolución a 90m (3x si era 30m)
dem_utm <- project(dem_full, "EPSG:32614")   # UTM zone 14N (metros)
dem_shiny <- aggregate(dem_full, fact = 3, fun = mean)

writeRaster(dem_shiny,
            "data/DEM_San_Esteban.tif",
            overwrite = TRUE)

# Visualizar el DEM descargado
plot(dem_full, main = "DEM de la Región Mixteca (SRTM - 30m)")
plot(dem_shiny, main = "DEM de la Región Mixteca (SRTM - 90m)")





## tibbles: 
vars_clima_utiles <- c(
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

clima_predio_filtrado <- promedio_anual_regional %>%
  as_tibble() %>%  
  select(any_of(vars_clima_utiles))


#
# SHP de puntos dentro del predio
pts <- st_read("data/perimetro/perimetro_tlaxiaco.geojson", quiet = TRUE)
st_write(pts, "data/puntos/puntos_predio.geojson")



# Datos TerraClimate
# https://www.climatologylab.org/uploads/2/2/1/3/22133936/terraclimate_downloadv2.r
library(ncdf4)
library(tidyverse)
library(reshape2)
library(fs) 

# Variables a descargar
vars <- c("tmin", "tmax", "ppt", "pet", "aet", "def", "q", "soil", "swe")

# Tus coordenadas (Bounding Box)

xmin <- -97.75253
xmax <- -97.63320
ymin <- 17.01874 
ymax <- 17.12996

# Crear el directorio si no existe
dir_path <- "data/terraclimate"
if (!dir_exists(dir_path)) {
  dir_create(dir_path, recurse = TRUE)
  message("Directorio creado: ", dir_path)
}

# ==============================================================================
# 2. OBTENER GEOMETRÍA DE LA REJILLA (Solo se hace una vez)
# ==============================================================================
message("--- Calculando geometría de la rejilla (usando ppt como referencia) ---")

# Usamos 'ppt' como referencia para obtener índices de lat/lon y fechas
ref_var <- "ppt"
baseurl_ref <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_", ref_var, "_1958_CurrentYear_GLOBE.nc")

nc_ref <- nc_open(baseurl_ref)
lons_all <- ncvar_get(nc_ref, "lon")
lats_all <- ncvar_get(nc_ref, "lat")

# Índices
lon_idxs <- which(lons_all >= xmin & lons_all <= xmax)
lat_idxs <- which(lats_all >= ymin & lats_all <= ymax)

# Parámetros de descarga (Start & Count)
start <- c(min(lon_idxs), min(lat_idxs), 1)
count <- c(length(lon_idxs), length(lat_idxs), -1)

# Coordenadas recortadas y Fechas para usar después
my_lons <- lons_all[lon_idxs]
my_lats <- lats_all[lat_idxs]
# Asumimos que el tiempo es igual para todas las vars (1958-Presente)
# Nota: Obtenemos el tamaño del tiempo leyendo solo el header de la variable
time_dim <- nc_ref$var[[ref_var]]$varsize[3] 
date_seq <- seq(as.Date("1958-01-01"), by = "month", length.out = time_dim)

nc_close(nc_ref) # Cerramos la referencia
message("Geometría calculada. Iniciando descarga de ", length(vars), " variables...")

# ==============================================================================
# 3. FUNCIÓN DE DESCARGA
# ==============================================================================

download_and_save <- function(variable) {
  
  message(paste0("Procesando: ", variable, "..."))
  
  tryCatch({
    # 1. Construir URL
    url <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_", variable, "_1958_CurrentYear_GLOBE.nc")
    
    # 2. Descargar datos usando los parámetros pre-calculados (start/count)
    nc <- nc_open(url)
    data_array <- ncvar_get(nc, varid = variable, start = start, count = count)
    nc_close(nc) # Cerrar inmediatamente
    
    # 3. Estructurar dimensiones
    dimnames(data_array) <- list(
      longitude = my_lons,
      latitude = my_lats,
      time = as.character(date_seq)
    )
    
    # 4. Convertir a Tibble
    df <- melt(data_array) %>% 
      as_tibble() %>% 
      rename(
        date = time,
        val = value
      ) %>% 
      mutate(
        date = as.Date(date),
        var_name = variable # Agregamos columna con el nombre de la variable
      )
    
    # 5. Guardar
    file_name <- file.path(dir_path, paste0("terraclimate_", variable, ".csv"))
    write_csv(df, file_name)
    message(paste0("   -> Guardado en: ", file_name))
    
  }, error = function(e) {
    message(paste0("   ERROR en variable ", variable, ": ", e$message))
  })
}

# ==============================================================================
# 4. EJECUCIÓN (MAP/WALK)
# ==============================================================================

# Usamos walk porque nos interesa el efecto secundario (guardar archivo) 
# y no necesitamos que devuelva una lista enorme a la memoria de R ahora mismo.
walk(vars, download_and_save)

message("--- ¡Proceso finalizado! ---")


# ==============================
# 5. Integrar en un solo tibble
# ==============================

files <- dir_ls("data/terraclimate", glob = "*.csv")

message(paste("Se encontraron", length(files), "archivos para procesar."))

# 2. LEER Y UNIR (STACKING)
# map_dfr lee cada archivo y los "apila" uno debajo del otro automáticamente
full_data_long <- map_dfr(files, show_col_types = FALSE, read_csv)

# --- OPCIÓN A: FORMATO LARGO (LONG) ---
# Este ya lo tienes en 'full_data_long'.
# Estructura: lon, lat, date, val, var_name
# Es ideal para ggplot (facets).
print("Vista previa formato LARGO:")
print(head(full_data_long))

# --- OPCIÓN B: FORMATO ANCHO (WIDE) - RECOMENDADO ---
# Generalmente querrás cada variable climática en su propia columna
# para poder ver qué temperatura hizo cuando llovió X cantidad.

full_data_wide <- full_data_long %>%
  pivot_wider(
    names_from = var_name, # Los nombres de las nuevas columnas vienen de aquí (ppt, tmax, etc)
    values_from = val      # Los valores vienen de aquí
  ) %>%
  arrange(date, latitude, longitude) # Ordenamos cronológicamente

print("Vista previa formato ANCHO (Tabla final):")
print(full_data_wide)

# Calcular tmean
full_data_wide <- full_data_wide %>% 
  mutate(tmean = (tmin + tmax)/2)

# 3. GUARDAR RESULTADO FINAL (Opcional)
# Guardamos el consolidado limpio para no tener que volver a leer los 10 archivos
write_csv(full_data_wide, "data/terraclimate_consolidado_ancho.csv")
message("Datos integrados guardados en 'data/terraclimate_consolidado.csv'")

# Formato largo con tmean
full_data_long <- full_data_wide %>%
  pivot_longer(
    cols = aet:tmean, 
    names_to = "var_name",  
    values_to = "val"
  )

write_csv(full_data_long, "data/terraclimate_consolidado_largo.csv")










