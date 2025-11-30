# UI

source("mod_mapa.R")
source("mod_clima.R")
source("mod_info.R")

ui <- dashboardPage(
  
  dashboardHeader(
    title = tags$span("Predio | San Esteban Atatlahuca")
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Mapa", tabName = "mapa", icon = icon("map")),
      menuItem("Clima", tabName = "clima", icon = icon("cloud")),
      menuItem("Información", tabName = "info", icon = icon("info-circle"))
      
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "estilos.css")
    ),
    
    tabItems(
      tabItem(tabName = "mapa", mod_mapa_ui("mapa") ),
      tabItem(tabName = "clima", mod_clima_ui("clima") ),
      tabItem(tabName = "info", mod_info_ui("info") )
      
    )
  )
)
