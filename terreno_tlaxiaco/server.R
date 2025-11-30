# Server

source("global.R")
source("mod_mapa.R")
source("mod_clima.R")
source("mod_info.R")



function(input, output, session) {

  mod_mapa_server("mapa")
  mod_clima_server("clima")
  mod_info_server("info")

  
}



