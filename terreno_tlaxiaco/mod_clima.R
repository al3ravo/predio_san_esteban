# Clima

source("global.R")


mod_clima_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    fluidRow(
      box(
        title = "Controles climáticos",
        width = 3,
        status = "success",
        solidHeader = TRUE,
        
        selectInput(
          "var_clima",
          "Variable:",
          choices = c(
            "Precipitación (ppt)" = "ppt",
            "Evapotranspiración real (aet)" = "aet",
            "Evapotranspiración potencial (pet)" = "pet",
            "Déficit hídrico (def)" = "def",
            "Escurrimiento (q)" = "q",
            "Humedad del suelo (soil)" = "soil",
            "Nieve (swe)" = "swe",
            "Temperatura máxima (tmax)" = "tmax",
            "Temperatura mínima (tmin)" = "tmin",
            "Temperatura media (tmean)" = "tmean"
          )
        ),
        
        dateRangeInput(
          "rango_clima",
          "Rango de fechas",
          start = "1958-01-01",
          end = "2024-12-31"
        )
      ),
      
      box(
        title = "Serie temporal",
        width = 9,
        status = "success",
        solidHeader = TRUE,
        plotlyOutput("clima_ts", height = "350px")
      )
    ),
    
    fluidRow(
      box(
        title = "Climatología mensual",
        width = 6,
        status = "success",
        solidHeader = TRUE,
        plotlyOutput("clima_mensual", height = "350px")
      ),
      
      box(
        title = "Tendencia histórica",
        width = 6,
        status = "success",
        solidHeader = TRUE,
        plotlyOutput("clima_tendencia", height = "350px")
      )
    )

  )
}


mod_clima_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Filtrar TerraClimate
    clima_filtrado <- reactive({
      req(input$var_clima)
      
      terraclimate_consolidado_largo %>%
        filter(var_name == input$var_clima) %>%
        filter(date >= input$rango_clima[1],
               date <= input$rango_clima[2])
    })
    
    # 2. GRÁFICO: Serie temporal
    output$clima_ts <- renderPlotly({
      df <- clima_filtrado()
      
      plot_ly(df, x = ~date, y = ~val, type = "scatter", mode = "lines", line = list(color = "#1F4F3E")) %>%
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = ""),
          title = paste("Serie temporal:", input$var_clima)
        )
    })
    
    # 3. GRÁFICO: Promedio mensual
    output$clima_mensual <- renderPlotly({
      df <- clima_filtrado() %>%
        mutate(mes = lubridate::month(date, label = TRUE)) %>%
        group_by(mes) %>%
        summarise(media = mean(val, na.rm = TRUE))
      
      plot_ly(df, x = ~mes, y = ~media, type = "bar", marker = list(color = "#1F4F3E")) %>%
        layout(title = "Climatología mensual")
    })
    
    # 4. GRÁFICO: Promedio anual histórico
    output$clima_tendencia <- renderPlotly({
      df <- clima_filtrado() %>%
        mutate(año = lubridate::year(date)) %>%
        group_by(año) %>%
        summarise(media = mean(val, na.rm = TRUE))
      
      plot_ly(df, x = ~año, y = ~media, type = "scatter", mode = "lines", name = "Promedio", line = list(color = "#1F4F3E")) %>%
        add_lines(y = fitted(lm(media ~ año, df)), name = "Tendencia", line = list(dash = "dash", color = "orange")) %>%
        layout(title = "Tendencia histórica anual")
    })

  })
}



