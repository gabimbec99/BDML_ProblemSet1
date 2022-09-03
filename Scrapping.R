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
p_load(tidyverse,rvest, skimr, stargazer, fabricatr) # web-scraping
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
w=datos_geih[, c("fweight")]
data_punto2 =cbind(y,X,w)
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
modelo1 <- lm("y ~ X + I(X^2)", data=data_punto2, weights= w, x=TRUE )
summary(modelo1)
stargazer(modelo1, type = "text")

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

##Discusion Peak Ages: Boostrap sobre peak ages
peakage=50

var(modelo$fitted.values)

data_punto2$modelo<-predict(modelo)
with(data_punto2,mean((y-modelo)^2))


#Sin bootstrap
predict(modelo, list(X = peakage), interval = "c")

#Con bootstrap
n<-nrow(modelo$model)
B <- 10000
pred <- numeric(B)
for (i in 1:B) {
  boot <- sample(n, n, replace = TRUE)
  fit.b <- lm("y ~ X + I(X^2)", data = data_punto2[boot,])
  pred[i] <- predict(fit.b, list(X = 50)) + sample(resid(fit.b), size = 1)
}
quantile(pred, c(0.025, 0.975))
var(fit.b$fitted.values)




#Punto 3-----------------------------
#Como se encuentra en la base, la direcciÃ³n de la dummy estÃ¡ "al revÃ©s" se crea la variable female
datos_geih$female <- ifelse(datos_geih$sex==0, 1,0)

female=datos_geih[,c('female')]
age=datos_geih[,c('age')]
w=datos_geih[,c("fweight")]
y=log(datos_geih[, "ingtotes"])
data_punto3 =cbind(y,age,female,w)
data_punto3= data.frame(data_punto3)

#Punto A
modelo2 <- lm("y ~female" , data = data_punto3, weights= w, x=TRUE ) 
summary(modelo2)
stargazer(modelo2, type = "text")
#Este código arroja todas las medidas para la eleccion del modelo. 
broom::glance(modelo2)


#Punto B

#Con la medida de ingreso total imputado ingtotes

data_punto3$age <- na.omit(data_punto3$age)

modelo3 <- lm("y ~age+I(age^2)+female+(age+I(age^2)):female" , data = data_punto3, weights= w, x=TRUE ) 
summary(modelo3)
stargazer(modelo3, type = "text")


#Este código arroja todas las medidas para la eleccion del modelo. 
broom::glance(modelo3)

## graficar solo la linea de ajuste. 

ggplot(data=data_punto3 ,aes(age,y)) +
  geom_smooth(method = "lm", formula ="y ~age+I(age^2)+female+(age+I(age^2)):female",aes(colour=female)) + 
  # arreglando el eje x
  scale_x_continuous(n.breaks=10, limits=c(18,87)) + 
  # Arreglar el eje y
  scale_y_continuous(n.breaks=8,labels=scales::dollar) + 
  #anadir el titulo, subtitulo y caption
  labs(title= "Relacion entre salario y edad", subtitle = "Evidence for Colombia", caption="Source: GEIH 2008")+  
  # anadir los labels de los ejes
  xlab("Edad") + ylab("Salario") + theme_bw()





#Punto 4-----------------------------
#A

set.seed(10101)
datos_geih <- datos_geih %>%
         mutate(holdout= as.logical(1:nrow(datos_geih) %% 
                                      sample(nrow(datos_geih), nrow(datos_geih)*.3))
                )

  
test<-datos_geih[datos_geih$holdout==T,]
train<-datos_geih[datos_geih$holdout==F,]

#B-Formato general

modelo1<-train(y~z,
                      # specification to fit
                      data = datos_heih,
                      trControl = trainControl(method = "cv", number = 5),
                      method = "null")


modelo2<-train(y~z,
               # specification to fit
               data = datos_heih,
               trControl = trainControl(method = "cv", number = 5),
               method = "null")


modelo3<-train(y~z,
               # specification to fit
               data = datos_heih,
               trControl = trainControl(method = "cv", number = 5),
               method = "null")


modelo4<-train(y~z,
               # specification to fit
               data = datos_heih,
               trControl = trainControl(method = "cv", number = 5),
               method = "null")


modelo5<-train(y~z,
               # specification to fit
               data = datos_heih,
               trControl = trainControl(method = "cv", number = 5),
               method = "null")


#C

library(caret)

#specify the cross-validation method
ctrl <- trainControl(method = "LOOCV")

modelo1<-train(y~z,
               # specification to fit
               data = datos_heih,
               trControl = ctrl,
               method = "null")


modelo1<-train(y~z,
               # specification to fit
               data = datos_heih,
               trControl = ctrl,
               method = "null")

