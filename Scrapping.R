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

install.packages('rlang')
library('rlang')
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
browseURL("https://ignaciomsarmiento.github.io/GEIH2018%20sample/robots.txt")

# B data cleaning


#Me quedo con las observaciones de +18
datos_geih<- subset(datos_geih, age >= 18)

# elegimos las variables que vamos a usar para el problem set: 
X1=datos_geih[, c('age','oficio','relab','college','cotPension','fweight', 'formal', 'hoursWorkUsual')]
y1= datos_geih[,"y_total_m_ha"]
data_punto1 =cbind(y1,X1)
data_punto1= data.frame(data_punto1)

# limpieza de la base, cambiar las etiquetas y volver las variables factores.

# obtener la tabla de nombres de las variables: 

url2 <- 'https://ignaciomsarmiento.github.io/GEIH2018_sample/labels.html'
labels <- read_html(url2) %>%
  html_table()
#tomo la tabla de uno de todas las tablas presentes
labels <- as.data.frame(labels)

# extraer los nombres de las variables de oficios: 


labelsoficio=labels[labels$Variable == 'oficio',]
# quitar las comillas de la variable levels
labelsoficio$level <- gsub('"','',as.character(labelsoficio$level))
oficios <- labelsoficio[["level"]]
numoficio <- labelsoficio[["values"]]

#relab
labelsrelab=labels[labels$Variable == 'relab',]
labelsrelab$level <- gsub('"','',as.character(labelsrelab$level))
relab <- labelsrelab[["level"]]
numrelab <- labelsrelab[["values"]]

# relabel de los oficios
data_punto1 <- data_punto1 %>% 
  mutate(oficio = recode(oficio, !!!(set_names(oficios, numoficio)), .default = NA_character_))
# relabel de los relab
data_punto1 <- data_punto1 %>% 
  mutate(relab = recode(relab, !!!(set_names(relab,numrelab)), .default = NA_character_))


# otros relab
data_punto1 <- data_punto1 %>% 
  mutate(formal = recode(formal, !!!(set_names(c("informal","formal"), 0:1)), .default = NA_character_))

data_punto1 <- data_punto1 %>% 
  mutate(college = recode(college, !!!(set_names(c("No universidad","Universidad"), 0:1)), .default = NA_character_))

data_punto1 <- data_punto1 %>% 
  mutate(cotPension = recode(cotPension, !!!(set_names(c("Cotiza","No cotiza","Pensionado"), 1:3)), .default = NA_character_))



# convertir las variables en factores. 
variables_chr <- names(select_if(data_punto1, is.character))

for (v in variables_chr) {
  data_punto1[, v] <- as.factor(data_punto1[, v, drop = T])
}

glimpse(data_punto1)

datos_geih$formal %>% table(useNA="ifany") %>% prop.table() %>% round(3)*100
datos_geih$oficio %>% table(useNA="ifany") %>% prop.table() %>% round(3)*100

# imputacion de las variables omitidas. 
# en realidad al haber una correlacion entre las variables omitidas no se puede imputar o solamente explotariamos la varianza al simplemente expandir observaciones.



# ver los missing values 
colSums(is.na(data_punto1))



datos_geih$ingtot %>% table(useNA="ifany") %>% prop.table() %>% round(3)*100

df$cod_ase_ <- NULL


# imputar por 1 el estrato 
df$estrato[is.na(df$estrato)] <-1

# voy a arreglar la edad en dias. 

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
