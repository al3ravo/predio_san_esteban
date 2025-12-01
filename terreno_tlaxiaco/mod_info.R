# Información

source("global.R")


mod_info_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    fluidRow(
      box(
        width = 4,
        title = tagList(icon("file"), "Documentos"),
        solidHeader = TRUE,
        tableOutput(ns("tablaPDFs"))
      ),
      
      box(
        width = 4,
        title = tagList(icon("table"), "Datos geométricos básicos"),
        solidHeader = TRUE,
        tableOutput(ns("tablaResumen"))
      ),
      
      box(
        width = 4,
        title = tagList(icon("info-circle"), "Capas usadas"),
        solidHeader = TRUE,
        htmlOutput(ns("linksFuentes"))
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Visor de documentos",
        solidHeader = TRUE,
        uiOutput(ns("visorPDF"))
      )
    )
    

  )
}


mod_info_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. TABLA: Resumen
    output$tablaResumen <- renderTable({
      data.frame(
        Indicador = c("Área (ha)", "Perímetro (km)", "Centroide lat", "Centroide lon"),
        Valor = c(
          round(area_ha, 2),
          round(perim_km, 3),
          round(coords_cent[2], 6),
          round(coords_cent[1], 6)
        ),
        check.names = FALSE
      )
    })
    
    # 2. ENLACES: Fuentes
    output$linksFuentes <- renderUI({
      HTML("
      <ul>
        <li><a href='https://www.inegi.org.mx/programas/mg/#descargas' target='_blank'>Marco Geoestadístico (2024)</a></li>
        <li><a href='https://www.inegi.org.mx/temas/hidrografia/#descargas' target='_blank'>Hidrografía (2010)</a></li>
        <li><a href='https://www.inegi.org.mx/programas/rnc/#descargas' target='_blank'>Red Nacional de Caminos (2024)</a></li>
        <li><a href='https://www.inegi.org.mx/temas/usosuelo/#descargas' target='_blank'>Uso de Suelo y Vegetación (2025)</a></li>
        <li><a href='http://geoportal.conabio.gob.mx/metadatos/doc/html/rtp1mgw.html' target='_blank'>Regiones Terrestres Prioritarias (2004)</a></li>
        <li><a href='https://www.inegi.org.mx/temas/climatologia/#descargas' target='_blank'>Climatología (2022)</a></li>
        <li><a href='https://www.climatologylab.org/uploads/2/2/1/3/22133936/terraclimate_downloadv2.r' target='_blank'>TerraClimate (datos climáticos históricos)</a></li>
        <li><a href='https://www.inegi.org.mx/temas/edafologia/#descargas' target='_blank'>Edafología</a></li>
        <li><a href='https://www.inegi.org.mx/app/biblioteca/ficha.html?upc=889463489672' target='_blank'>Topografía</a></li>
        <li><a href='https://portal.opentopography.org/' target='_blank'>DEM 30 m (OpenTopography, no usado)</a></li>
        <li><a href='https://www.inegi.org.mx/app/geo2/elevacionesmex/' target='_blank'>DEM 15 (INEGI)</a></li>
      </ul>
      ")
    })
    
    # 3. VISOR: Docs en PDF
    pdfs <- data.frame(
      Titulo = c("Compendio de información geográfica municipal 2010"),
      Archivo = c("compendio_informacion_geografica_municipal_2010.pdf"),
      stringsAsFactors = FALSE
    )
    
    output$tablaPDFs <- renderTable({
      btns <- sprintf(
        "<button class='btn btn-success' onclick=\"Shiny.setInputValue('%s', '%s', {priority: 'event'})\">Ver</button>",
        ns("pdf_sel"),
        pdfs$Archivo
      )
      
      data.frame(
        Documento = pdfs$Titulo,
        Accion = btns,
        stringsAsFactors = FALSE
      )
    }, sanitize.text.function = function(x) x)
    
    output$visorPDF <- renderUI({
      req(input$pdf_sel)
      
      tags$iframe(
        style="height:85vh; width:100%; border:none;",
        src=input$pdf_sel
      )
    })
    
  })
}


