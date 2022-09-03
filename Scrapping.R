#Problem Set 1#
## Daniel Lasso, Matteo Rozo y Gabriela Mejía ##

# Punto 1 -------------------------------------------------
library(pacman)
library(rvest, tidyverse)
library (dplyr)
# llamar la librería pacman: contiene la función p_load()
require(pacman)
# p_load llama/instala-llama las librerías que se enlistan: # contiene las librerías ggplot, dplyr...
p_load(knitr,kableExtra,here,jtools,ggstance,broom,broom.mixed,skimr)
p_load(tidyverse,rvest, skimr) # web-scraping
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

# revisar si tiene alguna restricción. 
browseURL("https://en.wikipedia.org/robots.txt")


# B data cleaning


#Me quedo con las observaciones de +18
datos_geih<- subset(datos_geih, age >= 18)


# ver los missing values 
is.na(df)
colSums(is.na(df))


unique(df$cod_ase_)
df$cod_ase_ <- NULL

df$estrato %>% table(useNA="ifany") %>% prop.table() %>% round(3)*100


# imputar por 1 el estrato 
df$estrato[is.na(df$estrato)] <-1

voy a arreglar la edad en dias. 

df$edad_[df$uni_med_==1] == df$edad_[df$uni_med_==1]*365
df$edad_[df$uni_med_==2] == df$edad_[df$uni_med_==2]*30


#Punto 2-----------------------------

X=datos_geih[, c('age')]
y= datos_geih[, "y_salary_m"]
data_punto2 =cbind(y,X)
data_punto2= data.frame(data_punto2)


# agregar 
ggplot(data=data_punto2 ,aes(X,y)) + geom_point() + 
  # arreglando el eje x
  scale_x_continuous(n.breaks=10, limits=c(18,87)) + 
  # Arreglar el eje y
  scale_y_continuous(n.breaks=8,labels=scales::dollar) + 
  #anadir el titulo, subtitulo y caption
  labs(title= "Relacion entre salario y edad", subtitle = "Evidence for Colombia", caption="Source: GEIH 2008")+  
  # anadir los labels de los ejes
  xlab("Edad") + ylab("Salario") + theme_bw()


# tiene que tener los pesos 
modelo <- lm("y ~ X + I(X^2)", data=data_punto2, weights= data_punto2$fweight, x=TRUE )
summary(modelo)


# este codigo arroja todas las medidas para la eleccion del modelo. 
broom::glance(modelo)

#------- grafica ultimo punto del 2 ---------------
##### nota #####
# hay que revisar en este punto si se puede personalizar la linea de ajuste con la regresion, especialmente por los pesos. 


# Grafica de relacion de salarios con Funcion de ajuste cuadratica 
ggplot(data=data_punto2 ,aes(X,y)) + geom_point()+ stat_smooth(method = "lm", formula = "y ~ x + I(x^2)", size = 1, color = "red") + 
  # arreglando el eje x
  scale_x_continuous(n.breaks=10, limits=c(18,87)) + 
  # Arreglar el eje y
  scale_y_continuous(n.breaks=8,labels=scales::dollar) + 
  #anadir el titulo, subtitulo y caption
  labs(title= "Relacion entre salario y edad", subtitle = "Evidence for Colombia", caption="Source: GEIH 2008")+  
  # anadir los labels de los ejes
  xlab("Edad") + ylab("Salario") + theme_bw()

## graficar solo la linea de ajuste. 

ggplot(data=data_punto2 ,aes(X,y)) + stat_smooth(method = "lm", formula = "y ~ x + I(x^2)", size = 1, color = "red") + 
# arreglando el eje x
scale_x_continuous(n.breaks=10, limits=c(18,87)) + 
  # Arreglar el eje y
  scale_y_continuous(n.breaks=8,labels=scales::dollar) + 
  #anadir el titulo, subtitulo y caption
  labs(title= "Relacion entre salario y edad", subtitle = "Evidence for Colombia", caption="Source: GEIH 2008")+  
  # anadir los labels de los ejes
  xlab("Edad") + ylab("Salario") + theme_bw()

#Punto 3-----------------------------
#Como se encuentra en la base, la dirección de la dummy está "al revés" se crea la variable female
datos_geih$female <- ifelse(datos_geih$sex==0, 1,0)

X=datos_geih[, c('female')]
y= datos_geih[, "ingtotes"]
data_punto3 =cbind(y,X)
data_punto3= data.frame(data_punto3)


#Con la medida de ingreo total imputado ingtotes

modelo2 = lm("y ~ X" , data = data_punto3) #No me corre sale un error
