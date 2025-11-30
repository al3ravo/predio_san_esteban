# Mapa

source("global.R")


mod_mapa_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, uiOutput("areaBox")),
      column(4, uiOutput("perimBox")),
      box(width=4, title="Superficie por uso de suelo",
          tableOutput("tablaUsoPredio"))
      
    ),
    
    fluidRow(
      box(width = 12,
          title = tagList(icon("globe-americas"), "Mapa interactivo"),
          status = "primary",
          solidHeader = TRUE,
          leafletOutput("mapa", height = "80vh")
      )
    )
  )
}


mod_mapa_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. VALUEBOX: Área
    output$areaBox <- renderUI({
      div(class="info-card",
          h3("Área aproximada del predio"),
          div(class="value", paste0(round(area_ha, 2), " ha"))
      )
    })
    
    # 2. VALUEBOX: Perímetro
    output$perimBox <- renderUI({
      div(class="info-card",
          h3("Perímetro aproximado"),
          div(class="value", paste0(round(perim_km, 3), " km"))
      )
    })
    
    # 3. TABLA: Superficie por uso de suelo
    output$tablaUsoPredio <- renderTable({
      usv_summary
    })
    
    # 4. MAPA: Leaflet
    output$mapa <- renderLeaflet({
      
      # Mapa base
      map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
        addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
        addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satélite (2014–2023)") %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Topográfico") %>%
        
        # Predio (lo queremos visible por defecto)
        addPolygons(
          data = perimetro,
          group = "Predio",
          color = "#ff7f00", weight = 3,
          fillColor = "#ffeda0", fillOpacity = 0.4,
          label = "Predio Tlaxiaco"
        ) %>%
        
        addRasterImage(
          dem_proyectado, 
          colors = paleta_elevacion, 
          opacity = 0.8,
          group = "Modelo Digital de Elevación (DEM)"
        ) %>%
        
        # --- 2.4 Añadir la Leyenda de Elevación ---
        addLegend(
          pal = paleta_elevacion,
          values = c(min_elevacion, max_elevacion),
          title = "Elevación (m)",
          position = "bottomright"
        ) %>% 
        
        # Municipios
        addPolygons(
          data = mg_mpios,
          group = "Municipios (INEGI MG)",
          color = "#636363", weight = 4,
          fill = FALSE,
          label = ~paste0(NOMGEO)
        ) %>%
        
        # Localidades
        addCircleMarkers(
          data = mg_localidades,
          group = "Localidades (INEGI MG)",
          radius = 10, stroke = FALSE,
          fillOpacity = 0.7,
          label = ~paste0(NOMGEO)
        ) %>%
        
        # Hidrografía
        #addPolylines(
        #  data = hidro_lineas,
        #  group = "Polígono subcuenca",
        #  weight = 4,
        #  color = "#3182bd",
        #  label = ~paste0(SUBCUENCA)
        #) %>%
        
        addPolylines(
          data = hidro_arroyos,
          group = "Red hidrográfica",
          weight = 3,
          color = "#1c9099", 
          label = ~paste(ENTIDAD, CONDICION, sep = ", ")
        ) %>%
        
        # Edafología
        addPolygons(
          data = suelos,
          fillColor = ~colorFactor(
            palette = c(
              "Acrisol" = "#C7EFD4",
              "Cambisol" = "#8CC9AF",
              "Leptosol" = "#4C8F77",
              "Luvisol" = "#1F4F3E",
              "Phaeozem" = "#F0F8F6"),
            domain = suelos$tipo_sl)(tipo_sl),
          fillOpacity = 0.6,
          color = "#1F4F3E",
          weight = 1,
          group = "Suelos",
          popup = ~paste0(
            "<b>Tipo de suelo:</b> ", tipo_sl, "<br>",
            "<b>Textura:</b> ", textura, "<br>",
            "<b>Profundidad:</b> ", prfnddd, "<br>",
            "<b>Área:</b> ", round(HECTARE, 1), " ha"
          )
        ) %>%      
        
        
        # Caminos
        addPolylines(
          data = rnc,
          group = "Red Nacional de Caminos",
          weight = 4,
          color = "#cb181d",
          label = ~paste(TIPO_VIAL, NOMBRE, sep = " ")
        ) %>%
        
        # Uso de suelo y vegetación
        addPolygons(
          data = usv,
          group = "Uso de suelo y vegetación",
          color = "#31a354",
          weight = 4,
          fillOpacity = 0.3,
          label = ~paste0(DESCRIPCIO)
        ) %>%
        
        # Regiones Terrestres Prioritarias
        addPolygons(
          data = rtp,
          group = "Regiones Terrestres Prioritarias",
          color = "#e31a1c",
          weight = 4,
          fillOpacity = 0.15,
          label = ~paste0(NOMBRE)
        ) %>% 
        
        ## Climatología
        addPolygons(
          data = climatologia,
          group = "Climatología",
          color = "#555",
          weight = 1,
          opacity = 1,
          fillOpacity = 0.05,
          # Coloreamos el mapa según la Aptitud Térmica (Mínima Enero) 
          # Azul = Frío, Rojo = Cálido. Útil para ver dónde poner invernaderos.
          fillColor = ~colorNumeric("RdYlBu", TMIN_EN, reverse = TRUE)(TMIN_EN),
          highlightOptions = highlightOptions(
            color = "white", weight = 2, bringToFront = TRUE
          ),
          label = lapply(climatologia$etiqueta_html, HTML),
          labelOptions = labelOptions(
            style = list("padding" = "0px"),
            direction = "auto"
          )
        ) %>%
        addLegend("bottomright", 
                  pal = colorNumeric("RdYlBu", climatologia$TMIN_EN, reverse = TRUE), 
                  values = climatologia$TMIN_EN,
                  title = "Temp. Mínima Enero (°C)") %>% 
        
        addPolygons(
          data = promedio_anual_regional,
          group = "Promedios anuales regionales",
          color = "#e31a1c",
          weight = 4,
          fillOpacity = 0.05,
          label = lapply(promedio_anual_regional$etiqueta_html, htmltools::HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px",
            direction = "auto"
          )
        ) %>% 
        
        # Curvas de nivel
        addPolylines(
          data = curvas_nivel,
          color = "#4C8F77",
          weight = ~ifelse(tipo_curva == "Regular", 2, 1),
          group = "Curvas de nivel",
          label = ~paste0("Elevación: ", elev, " m")
        ) %>%
        
        # Puntos de referencia
        addCircleMarkers(
          data = puntos_referencia,
          radius = 4,
          color = "red",
          fillOpacity = 0.9,
          group = "Puntos de referencia topográfica",
          label = ~nombre_ref
        ) 
      
      # Puntos internos del predio (si existen)
      if (exists("puntos") && nrow(puntos) > 0) {
        
        for(i in 1:nrow(puntos)) {
          icon_file <- paste0("icons/", puntos$icono[i])
          
          ic <- makeIcon(
            iconUrl = icon_file,
            iconWidth = 30,
            iconHeight = 30,
            iconAnchorX = 20,
            iconAnchorY = 20
          )
          
          map <- map %>% addMarkers(
            lng = st_coordinates(puntos)[i,1],
            lat = st_coordinates(puntos)[i,2],
            icon = ic,
            popup = puntos$REMARKS[i],
            group = "Puntos"
          )
        }
        
        map <- map %>% addCircleMarkers(
          data = puntos,
          radius = 12,
          opacity = 0,
          fillOpacity = 0,
          label = ~REMARKS,
          labelOptions = labelOptions(direction="top"),
          group = "Puntos predio"
        )
      }
      
      
      # Controles y ajustes finales
      map %>%
        addLayersControl(
          baseGroups = c("CartoDB", "OSM", "Satélite (2014–2023)", "Topográfico"),
          overlayGroups = c(
            "Predio",
            #"Puntos predio",
            "Municipios (INEGI MG)",
            "Localidades (INEGI MG)",
            #"Polígono subcuenca",
            "Red hidrográfica",
            "Red Nacional de Caminos",
            "Uso de suelo y vegetación",
            "Regiones Terrestres Prioritarias",
            "Climatología",
            "Promedios anuales regionales",
            "Suelos",
            "Curvas de nivel",
            "Puntos de referencia topográfica",
            "Modelo Digital de Elevación (DEM)"
          ),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        
        # Centroide
        addMarkers(
          lng = coords_cent[1],
          lat = coords_cent[2],
          popup = paste0("Centroide<br>", 
                         "Lat: ", round(coords_cent[2], 6),
                         "<br>Lon: ", round(coords_cent[1], 6)),
          group = "Predio"
        ) %>% 
        
        # Ocultar por defecto todas las capas excepto el predio
        hideGroup(c(
          "Municipios (INEGI MG)",
          "Localidades (INEGI MG)",
          #"Polígono subcuenca",
          "Red hidrográfica",
          "Red Nacional de Caminos",
          "Uso de suelo y vegetación",
          "Regiones Terrestres Prioritarias",
          "Climatología",
          "Promedios anuales regionales",
          "Suelos",
          "Curvas de nivel",
          "Puntos de referencia topográfica",
          "Modelo Digital de Elevación (DEM)"
          #"Puntos predio"
        )) %>%
        
        addScaleBar(position = "bottomleft") %>%
        addMeasure(primaryLengthUnit = "meters",
                   primaryAreaUnit = "hectares") %>%
        fitBounds(bb[1], bb[2], bb[3], bb[4])
    })
    
  })
}


