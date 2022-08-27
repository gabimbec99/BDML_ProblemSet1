## llamar la librería pacman: contiene la función p_load()
require(pacman)
## p_load llama/instala-llama las librerías que se enlistan: # contiene las librerías ggplot, dplyr...
p_load(tidyverse,rvest) # web-scraping
vignette("rvest")


url_base <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/page",1:10,".html0")

df<- data.frame()

for (x in url_base){
  # entro a la pagina
  load<- read_html(url)
  # guardo todas las tablas html de las paginas
  html_table(load)
  # tomo la tabla 1 de todas las tablas presentes
  tabla <- as.data.frame(html_table[[1]])
  # hago un merge de las tablas por las filas. 
  df <- rbind(df, tabla)
}