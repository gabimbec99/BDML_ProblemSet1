# BDML_ProblemSet1
 En este repositorio se encuentra la solución del Problem Set 1 de Big Data y Machine Learning for Applied Economics.

 Grupo: [Daniel Lasso](https://github.com/daniell419), [Matteo Rozo](https://github.com/MatteoRozo) y [Gabriela Mejía](https://github.com/gabimbec99).
 
 Profesor: [Ignacio Sarmiento](https://github.com/ignaciomsarmiento)

 Dentro de este repositorio se encuentra:
 
 El documento pdf describiendo las respuestas completas al Taller 1 del curso de BDML Fall 2022, el cual fue trabajado en el siguiente enlace de overleaf:(https://www.overleaf.com/8567919536rcvkfqcvpyvg)
 ## Data Files

 ## Code Files 
#Problem Set 1#
## Daniel Lasso, Matteo Rozo y Gabriela MejÃ­a ##

# Punto 1 -------------------------------------------------
library(ggplot2)
library(pacman)
library(rvest, tidyverse)
library (dplyr)
# llamar la librerÃ­a pacman: contiene la funciÃ³n p_load()
require(pacman)
# p_load llama/instala-llama las librerÃ­as que se enlistan: # contiene las librerÃ­as ggplot, dplyr...
p_load(knitr,kableExtra,here,jtools,ggstance,broom,broom.mixed,skimr)
p_load(tidyverse,rvest, skimr) # web-scraping
vignette("rvest")

#Creo una url base sobre la cual pueda iterar el loop del siguiente paso y obtener los datos de cada chunk
url_base <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", 
                   1:10, ".html")

#Creo un data frame vacÃ­o
datos_geih<- data.frame()

#Loop para obtener cada chunk de datos y crear una base de datos con todo 
for (url in url_base){
  #Imprime cada url para poder ver en quÃ© estado de iteraciÃ³n se encuentra el loop
  print(url)
  #creo una base temporal con la tabla contenida en el html de cada chunk
  temporal <- read_html(url) %>%
    html_table()
  #tomo la tabla de uno de todas las tablas presentes
  temporal <- as.data.frame(temporal[[1]])
  #hago un merge de las tablas por filas
  datos_geih<- rbind(datos_geih, temporal)
}

# B data cleaning
summary(datos_geih)

#Me quedo con las observaciones de +18
datos_geih<- subset(datos_geih, age >= 18)
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

data_punto2$fitted <- coef(modelo)["(Intercept)"][[1]] + coef(modelo)["X"][[1]]*data_punto2$x +coef(modelo)["I(X^2)"][[1]]*(data_punto2$x)^2


# Grafica de relacion de salarios predichos y edad

ggplot(data=modelo ,aes(modelo$X,modelo$fitted.values)) + geom_point()+
  # arreglando el eje x
  scale_x_continuous(n.breaks=10, limits=c(18,87)) + 
  # Arreglar el eje y
  scale_y_continuous(n.breaks=8,labels=scales::dollar) + 
  #anadir el titulo, subtitulo y caption
  labs(title= "Relacion entre salario predicho y edad", subtitle = "Evidence for Colombia", caption="Source: GEIH 2008")+  
  # anadir los labels de los ejes
  xlab("Edad") + ylab("Salario") + theme_bw()


## discusion sobre el peak-age
set.seed(123)
nb=10000
coefmat=matrix(0,nb,3)
resid<-modelo$residuals
pred<-modelo$fitted.values

df2<-data_punto2[complete.cases(data_punto2),]
modelo <- lm("y ~ X + I(X^2)", data=df2, weights= data_punto2$fweight, x=TRUE )

for (i in 1:nb)
{

  yboot <- pred + sample(resid, replace=TRUE)
  bmod <- update(modelo,"yboot ~ X + I(X^2)")
  coefmat[i,] = coef(bmod)
}

#Construir intervalos
colnames(coefmat) = c("Intercept","age","age^2")
coefmat <- data.frame(coefmat)
cis=apply(coefmat, 2, function(x) quantile(x, c(.025,.975)))
cis

# Predicir valor de la edad peak x=50:
peakage= coef(modelo)["(Intercept)"] + coef(modelo)["X"]*50 +coef(modelo)["I(X^2)"]*50
peakage

#Punto 3-----------------------------
#Como se encuentra en la base, la direcciÃ³n de la dummy estÃ¡ "al revÃ©s" se crea la variable female
datos_geih$female <- ifelse(datos_geih$sex==0, 1,0)

X=datos_geih[, c('female')]
y=log(datos_geih[, "ingtotes"])
data_punto3 =cbind(y,X)
data_punto3= data.frame(data_punto3)


#Con la medida de ingreo total imputado ingtotes

modelo2 <- lm("y ~ X" , data = data_punto3, weights= data_punto2$fweight, x=TRUE ) 

#Replicando la metodología del punto 2

