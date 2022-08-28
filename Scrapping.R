#Problem Set 1#
## Daniel Lasso, Matteo Rozo y Gabriela Mejía ##

# Punto 1 -------------------------------------------------
library(pacman)
library(rvest, tidyverse)
library (dplyr)
# llamar la librería pacman: contiene la función p_load()
require(pacman)
# p_load llama/instala-llama las librerías que se enlistan: # contiene las librerías ggplot, dplyr...
p_load(tidyverse,rvest) # web-scraping
vignette("rvest")

#Creo una url base sobre la cual pueda iterar el loop del siguiente paso y obtener los datos de cada chunk
url_base <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", 
                   1:10, ".html")

#Creo un data frame vacío
datos_geih<- data.frame()

#Loop para obtener cada chunk de datos y crear una base de datos con todo 
for (url in url_base){
  #Imprime cada url para poder ver en qué estado de iteración se encuentra el loop
  print(url)
  #creo una base temporal con la tabla contenida en el html de cada chunk
  temporal <- read_html(url) %>%
    html_table()
  #tomo la tabla de uno de todas las tablas presentes
  temporal <- as.data.frame(temporal[[1]])
  #hago un merge de las tablas por filas
  datos_geih<- rbind(datos_geih, temporal)
}

#Me quedo con las observaciones de +18
datos_geih<- subset(datos_geih, age >= 18)

#Punto 3-----------------------------
#Como se encuentra en la base, la dirección de la dummy está "al revés" se crea la variable female
datos_geih$female <- ifelse(datos_geih$sex==0, 1,0)
#Con la medida de ingreo total imputado ingtotes
mod = lm(logingtotes ~ female, data = datos_geih, x = TRUE) #No me corre sale un error
lm_summary = summary(mod)$coefficients


