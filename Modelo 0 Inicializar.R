# Lista de paquetes que deseas comprobar e instalar
paquetes_a_instalar <- c("readr", "readxl", "dplyr","tictoc","lubridate",
                         "ggplot2","scales","gridExtra","purrrlyr","patchwork","magick")


# Función para verificar e instalar los paquetes
instalar_paquetes <- function(paquetes) {
  for (paquete in paquetes) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      install.packages(paquete, dependencies = TRUE)
      library(paquete, character.only = TRUE)
      cat(paste("Paquete", paquete, "instalado.\n"))
    } else {
      cat(paste("Paquete", paquete, "ya está instalado.\n"))
    }
  }
}


Inicializar <- function() {
  
  # Llama a la función para instalar los paquetes
  instalar_paquetes(paquetes_a_instalar)
  
  # Cargar paquetes
  library("readr")  #Para CSV
  library("readxl") #Para EXCEL
  library("dplyr")
  library("tictoc")
  
  library("lubridate") #Para manejar Fechas
  library("ggplot2")
  library("scales")
  library("gridExtra")
  
  library("purrrlyr")
  library("patchwork")
  
  library(magick)

  windowsFonts("Arial" = windowsFont("Arial"))
  
  source(paste0(texto1,"Modelo 2-price_based.R"))
  source(paste0(texto1,"Modelo 2-all_as_one.R"))
  source(paste0(texto1,"Modelo 2-surplus_based_ownership.R"))
  source(paste0(texto1,"Modelo 2-surplus_based_ownership_v2.R"))
  source(paste0(texto1,"Modelo 2-bill_sharing.R"))

  getwd()# Carpeta donde estan los datos de partida --> DATOS INICIO
  file0 <- paste0(texto1,"Datos_Base_R.xlsx")
  DATOS_INICIO <- read_excel(file0)

  return(DATOS_INICIO)
  
}
