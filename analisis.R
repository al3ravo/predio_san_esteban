# Análisis
setwd("terreno_tlaxiaco")

library(sf)
library(terra)
library(tidyverse)
library(tidyterra)
library(concaveman)
library(scales)

# =============================
# CREAR ZONA BUFFER (150 m)
# =============================

# 1. Definir el Polígono del Terreno (Simulación basada en tus puntos)
pts <- st_read("data/perimetro/perimetro_tlaxiaco.geojson", quiet = TRUE)

# Asegurar que son puntos
pts <- st_cast(pts, "POINT")

# Concave Hull para reconstruir el polígono
terreno_sf <- concaveman(
  pts,
  concavity = 2,
  length_threshold = 0
)

# Transformar a UTM para poder hacer buffers en metros
terreno_utm <- st_transform(terreno_sf, crs = 32614)

# 2. Crear Área de Influencia (AOI) - Buffer de 150 metros
aoi_buffer <- st_buffer(terreno_utm, dist = 150)
aoi_wgs84 <- st_transform(aoi_buffer, crs = 4326) # Para cruzar con capas lat/lon



# =============================
# ANÁLISIS TOPOGRÁFICO
# =============================

# Cargar tu DEM (OpenTopography)
dem <- rast("data/DEM_aws_z12.tif")
dem_utm <- project(dem, crs(aoi_buffer), method = "bilinear")

# Recortar el DEM al AOI
dem_sitio <- crop(dem_utm, vect(aoi_buffer))
dem_sitio <- mask(dem_sitio, vect(aoi_buffer))

# Calcular Pendiente (Slope) y Orientación (Aspect)
topografia <- terra::terrain(dem_sitio, v = c("slope", "aspect"), unit = "degrees")

# Extraer estadísticas zonales SOLO del terreno (sin el buffer) para ser precisos
terreno_vect <- vect(terreno_utm)
# Media de la pendiente
stats_pendiente <- terra::extract(
  x   = topografia[["slope"]],
  y   = terreno_vect,
  fun = mean,
  na.rm = TRUE
)

# Máximo de la pendiente
stats_max_pendiente <- terra::extract(
  x   = topografia[["slope"]],
  y   = terreno_vect,
  fun = max,
  na.rm = TRUE
)

# Visualización rápida para ti
plot(topografia$slope, main = "Pendiente (Grados)")
plot(terreno_vect, add = TRUE, border = "red", lwd = 2)

print(paste("Pendiente Promedio:", round(stats_pendiente$slope, 2), "grados"))


# =============================
# HIDROLOGÍA DE PRECISIÓN
# =============================

hidro_arroyos <- st_read("data/shp_recortados/hidro_arroyos.shp", quiet = TRUE)

# Asegurar que los arroyos están en el mismo CRS (UTM)
rios_utm <- st_transform(hidro_arroyos, crs = st_crs(terreno_utm))

# Filtrar ríos que intersectan el buffer (no solo el terreno)
rios_cercanos <- st_intersection(rios_utm, aoi_buffer)

# Calcular distancia mínima del terreno al cuerpo de agua más cercano
distancia_agua <- st_distance(terreno_utm, rios_cercanos) %>% min()

print(paste("Distancia al agua:", round(as.numeric(distancia_agua), 2), "metros"))


# =============================
# ANÁLISIS CLIMÁTICO HISTÓRICO
# =============================

terraclimate_consolidado_ancho <- read_csv("data/terraclimate/terraclimate_consolidado_ancho.csv", show_col_types = FALSE)

clima_resumen <- terraclimate_consolidado_ancho %>%
  mutate(mes = month(date), anio = year(date)) %>%
  group_by(mes) %>%
  summarise(
    tmin_prom = mean(tmin, na.rm = TRUE),
    tmax_prom = mean(tmax, na.rm = TRUE),
    ppt_acum = mean(ppt, na.rm = TRUE),
    # Días probables de helada (si tmin promedio baja de 4, riesgo alto de eventos <0)
    riesgo_helada = mean(tmin, na.rm = TRUE) < 5 
  )

# Identificar sequía (meses con ppt < 40mm por ejemplo)
meses_secos <- clima_resumen %>% filter(ppt_acum < 40) %>% pull(mes)

# Visualizar régimen térmico
ggplot(clima_resumen, aes(x = mes)) +
  geom_ribbon(aes(ymin = tmin_prom, ymax = tmax_prom), fill = "orange", alpha = 0.3) +
  geom_line(aes(y = tmin_prom), color = "blue") +
  geom_line(aes(y = tmax_prom), color = "red") +
  geom_bar(aes(y = ppt_acum / 10), stat = "identity", alpha = 0.2) + # Escala dual simulada
  scale_x_continuous(breaks = 1:12) +
  theme_minimal() +
  labs(title = "Climatograma Sintético (San Esteban)", y = "Temp (°C) / PPT (x10 mm)")



# ==========================================
# MAPA DE APTITUD TERRITORIAL (SEMÁFORO)
# ==========================================

# 1. Clasificación de la Pendiente (El factor limitante)
# Definimos matriz de reclasificación:
# 0-10°  (Clase 1: Verde - Apto Agricultura/Construcción fácil)
# 10-20° (Clase 2: Amarillo - Apto Frutales con terrazas/Maguey)
# 20-35° (Clase 3: Naranja - Forestal/Maguey/Cabañas pilotadas)
# >35°   (Clase 4: Rojo - Conservación estricta/Peligro)

slope_deg <- topografia[["slope"]]   # ya lo tienes calculado con terrain()

# Clasificar por rangos de grados
pendiente_class <- classify(
  slope_deg,
  rcl = matrix(c(
    0, 10, 1,   # Plano
    10, 20, 2,  # Medio
    20, 35, 3,  # Ladera
    35, 90, 4   # Escarpado
  ), ncol = 3, byrow = TRUE)
)
# Convertir a factor/categórico
pendiente_class <- as.factor(pendiente_class)

# 2. Mapa de Distancia al Agua (Río + Pozo)
# Primero, necesitamos rasterizar el río y el pozo
# Vamos a crear un punto aproximado para el pozo (basado en el centroide como ejemplo, ajusta si tienes coord exacta)
pozo_sf <- pts %>% filter(REMARKS == "pozo")
pozo_vect <- vect(pozo_sf)

# Crear raster de distancias
# Unimos río y pozo como "fuentes de agua"
# 1. Convertir rios_cercanos (sf) a SpatVector
rios_vect <- vect(rios_cercanos)    # geometry = lines
# 2. Convertir las líneas en puntos (por vértices de la línea)
rios_puntos <- as.points(rios_vect) # geometry = points

fuentes_agua <- rbind(rios_puntos, pozo_vect)
distancia_agua_rast <- distance(dem_sitio, fuentes_agua)

# Enmascarar con el terreno
distancia_agua_rast <- mask(distancia_agua_rast, terreno_vect)
pendiente_class <- mask(pendiente_class, terreno_vect)



# 3. Visualización con ggplot2 y tidyterra
# Mapa de Pendientes (La Restricción Física)
terreno_utm_sf <- st_transform(terreno_sf, crs = 32614)
hidro_utm_sf <- st_transform(hidro_arroyos, crs = 32614)
pozo_utm_sf <- st_transform(pozo_sf, crs = 32614)

mapa_pendiente <- 
  ggplot() +
  geom_spatraster(data = pendiente_class) +
  scale_fill_manual(
    values = c(
      "1" = "#4CAF50",  # Verde
      "2" = "#FFEB3B",  # Amarillo
      "3" = "#FF9800",  # Naranja
      "4" = "#F44336"   # Rojo
    ),
    labels = c(
      "1" = "Plano (0–10°)",
      "2" = "Medio (10–20°)",
      "3" = "Ladera (20–35°)",
      "4" = "Escarpado (>35°)"
    ),
    name = "Aptitud por Pendiente",
    na.value = "transparent"
  ) +
  geom_sf(data = terreno_utm_sf, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = rios_cercanos, color = "blue", size = 1) +
  geom_sf(data = pozo_utm_sf, color = "blue", shape = 18, size = 4) +
  labs(
    title    = "Mapa de Aptitud del Terreno",
    subtitle = "San Esteban Atatlahuca (Basado en Pendiente)",
    caption  = "Verde: Cultivos anuales | Amarillo: Frutales/Agave | Naranja: Forestal/Cabañas"
  ) +
  theme_minimal()

# Mapa de Costo Hídrico (Distancia al agua)
mapa_agua <- ggplot() +
  geom_spatraster(data = distancia_agua_rast) +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Dist. (m)") +
  geom_sf(data = terreno_sf, fill = NA, color = "white") +
  geom_sf(data = rios_cercanos, color = "cyan", size = 1) +
  geom_sf(data = pozo_utm_sf, color = "cyan", shape = 18, size = 4) +
  labs(title = "Distancia a Fuentes de Agua",
       subtitle = "Costo energético de distribución") +
  theme_minimal()

# Mostrar
print(mapa_pendiente)
print(mapa_agua)

# Calcular cuántas hectáreas hay de cada clase
freq(pendiente_class) %>% 
  mutate(has = count * res(pendiente_class)[1] * res(pendiente_class)[2] / 10000) %>%
  select(value, has)





# ==========================================
# SIMULACIÓN FINANCIERA: SANTUARIO + TRUCHA
# ==========================================

# Función para generar flujo de caja a 5 años
simular_escenario <- function(nombre, ocupacion_meta, precio_noche, inversion_inicial) {
  
  # Años de proyección
  anios <- 1:5
  
  # --- INGRESOS ---
  # Curva de aprendizaje de ocupación: Año 1 bajo, Año 3 llega a meta
  tasa_ocupacion <- c(ocupacion_meta * 0.4, # Año 1 (40% del potencial)
                      ocupacion_meta * 0.7, # Año 2
                      ocupacion_meta,       # Año 3 (Estable)
                      ocupacion_meta,       # Año 4
                      ocupacion_meta)       # Año 5
  
  dias_operativos <- 365
  num_cabanas <- 3
  
  ingreso_hospedaje <- num_cabanas * dias_operativos * tasa_ocupacion * precio_noche
  
  # Ingreso por Restaurante y Trucha (Estimado como % del hospedaje + venta externa)
  # Asumimos que cada huésped gasta un extra y llegan algunos externos
  ingreso_alimentos_trucha <- ingreso_hospedaje * 0.30 
  
  total_ingresos <- ingreso_hospedaje + ingreso_alimentos_trucha
  
  # --- EGRESOS (OPEX) ---
  # Costos Fijos: Nómina (3 personas), Mantenimiento camino/auto, Marketing
  costos_fijos <- c(350000, 365000, 380000, 400000, 420000) # Inflación considerada
  
  # Costos Variables: Insumos (comida, alimento trucha, gasolina, lavandería)
  # Aprox 25% del ingreso total
  costos_variables <- total_ingresos * 0.25
  
  total_egresos <- costos_fijos + costos_variables
  
  # --- FLUJO DE CAJA ---
  ebitda <- total_ingresos - total_egresos
  
  # Crear Dataframe
  tibble(
    Escenario = nombre,
    Ano = anios,
    Ingresos = total_ingresos,
    Egresos = total_egresos,
    EBITDA = ebitda,
    Flujo_Acumulado = cumsum(ebitda) - inversion_inicial
  )
}

# DEFINICIÓN DE ESCENARIOS
# Inversión Inicial estimada: $1,300,000 MXN (Construcción + Auto + Tanques)
inversion <- 1300000 

# 1. Pesimista: Ocupación baja (20%), precio bajo (competencia local)
esc_pesimista <- simular_escenario("Pesimista", ocupacion_meta = 0.20, precio_noche = 2200, inversion_inicial = inversion)

# 2. Realista: Ocupación media (35%), precio mercado (Glamping)
esc_realista <- simular_escenario("Realista", ocupacion_meta = 0.35, precio_noche = 2800, inversion_inicial = inversion)

# 3. Optimista: Ocupación alta (50%), precio premium (Experiencia UMA/Transporte VIP)
esc_optimista <- simular_escenario("Optimista", ocupacion_meta = 0.50, precio_noche = 3500, inversion_inicial = inversion)

# Unir datos
datos_financieros <- bind_rows(esc_pesimista, esc_realista, esc_optimista)

# ==========================================
# VISUALIZACIÓN DE ROI (Retorno de Inversión)
# ==========================================

# Gráfico de Punto de Equilibrio
g1 <- ggplot(datos_financieros, aes(x = Ano, y = Flujo_Acumulado, color = Escenario)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) + # Línea de recuperación
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = label_dollar(prefix = "$", scale = 1e-3, suffix = "k")) +
  scale_color_manual(values = c("Pesimista" = "#F44336", "Realista" = "#FF9800", "Optimista" = "#4CAF50")) +
  labs(
    title = "Proyección de Retorno de Inversión (5 Años)",
    subtitle = "Escenario A: Glamping + UMA + Trucha (Inversión: $1.3M)",
    y = "Flujo de Caja Acumulado (Miles MXN)",
    x = "Año de Operación",
    caption = "Línea punteada = Punto de Recuperación de Inversión"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Gráfico de Flujo Anual (EBITDA)
g2 <- ggplot(datos_financieros, aes(x = Ano, y = EBITDA, fill = Escenario)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = label_dollar(prefix = "$", scale = 1e-3, suffix = "k")) +
  scale_fill_manual(values = c("Pesimista" = "#F44336", "Realista" = "#FF9800", "Optimista" = "#4CAF50")) +
  labs(
    title = "Utilidad Operativa Anual (EBITDA)",
    subtitle = "Dinero disponible antes de impuestos y depreciación",
    y = "Utilidad Anual (Miles MXN)"
  ) +
  theme_minimal()

print(g1)
print(g2)

# Tabla Resumen al Año 5
datos_financieros %>% 
  filter(Ano == 5) %>% 
  select(Escenario, Ingresos, EBITDA, Flujo_Acumulado) %>% 
  mutate(Recuperacion = ifelse(Flujo_Acumulado > 0, "SI", "NO"))






# ==========================================
# PLAN DE PASOS CRÍTICOS (GANTT) - 6 MESES
# ==========================================

# Definir las tareas y sus tiempos
actividades <- tribble(
  ~Fase, ~Tarea, ~Inicio, ~Fin, ~Critico,
  
  # FASE 1: PREPARACIÓN Y LEGAL (Mes 1-2)
  "1. Legal & Social", "Permiso Comunal/Municipal (San Esteban)", 0, 1.0, TRUE,
  "1. Legal & Social", "Registro UMA (SEMARNAT - Plan Manejo)", 0.5, 3.5, TRUE,
  "1. Legal & Social", "Levantamiento Topográfico Preciso (Estacas)", 0.5, 1.0, FALSE,
  
  # FASE 2: AGUA Y ACCESO (Mes 1-3)
  "2. Infraestructura", "Limpieza selectiva (Aclareo) + Camino interno", 1.0, 2.0, FALSE,
  "2. Infraestructura", "Captación Agua (Bocatoma arroyo + Tubería)", 1.0, 2.5, TRUE,
  "2. Infraestructura", "Cisterna y Fosa Séptica (Biodigestor)", 2.0, 3.0, TRUE,
  
  # FASE 3: CONSTRUCCIÓN (Mes 2-5)
  "3. Construcción", "Cimentación (Pilotes) Cabañas + Palapa", 2.0, 3.0, TRUE,
  "3. Construcción", "Estructura y Techos (A-Frame/Domo)", 3.0, 4.5, TRUE,
  "3. Construcción", "Instalación Tanques Trucha (Geomembrana)", 3.5, 4.5, FALSE,
  "3. Construcción", "Acabados e Interiores (Baños/Cocina)", 4.5, 5.5, FALSE,
  
  # FASE 4: OPERACIÓN Y LANZAMIENTO (Mes 5-6)
  "4. Operación", "Compra y Adecuación Camioneta 4x4", 4.5, 5.0, FALSE,
  "4. Operación", "Siembra Alevines (Trucha) + Inoculación Hongos", 5.0, 5.5, TRUE,
  "4. Operación", "Marketing (Fotos) + Soft Launch (Amigos)", 5.5, 6.0, TRUE
)

# Ordenar fases para el gráfico
actividades$Fase <- factor(actividades$Fase, levels = c("4. Operación", "3. Construcción", "2. Infraestructura", "1. Legal & Social"))

# Visualización
gantt_chart <- ggplot(actividades, aes(y = Tarea, x = Inicio, xend = Fin, color = Fase)) +
  geom_segment(size = 8, alpha = 0.8) +
  geom_text(aes(label = paste("Mes", Inicio, "-", Fin), x = (Inicio + Fin)/2), 
            color = "white", size = 3, fontface = "bold") +
  scale_color_manual(values = c("1. Legal & Social" = "#607D8B", 
                                "2. Infraestructura" = "#0288D1", 
                                "3. Construcción" = "#F57C00", 
                                "4. Operación" = "#43A047")) +
  labs(title = "Plan Maestro: Santuario Micológico & Trucha",
       subtitle = "Ruta Crítica para Lanzamiento en 6 Meses",
       x = "Meses del Proyecto", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_line(linetype = "dotted"),
        axis.text.y = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = 0:6, labels = paste("Mes", 0:6))

print(gantt_chart)























