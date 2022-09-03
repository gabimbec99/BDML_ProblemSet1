#Problem Set 1#
## Daniel Lasso, Matteo Rozo y Gabriela Mejía ##

# Punto 1 -------------------------------------------------

#Antes de comenzar, es necesario installar y cargar aquellos paquetes y librerías útiles para nuestro desarrollo.De tal forma:

install.packages("sandwich")
library(pacman)
library(rvest, tidyverse)
library (dplyr)

#Posteriormente, se requiere pacman para usar el comando p_loadm con el se cargan las librerias necesarias
require(pacman)
# p_load llama/instala-llama las librerías que se enlistan: # contiene las librerías ggplot, dplyr...
p_load(knitr,kableExtra,here,jtools,ggstance,broom,broom.mixed,skimr)
p_load(tidyverse,rvest, skimr) # web-scraping
vignette("rvest")

#Se crea una url base sobre la cual pueda iterar el loop del siguiente paso y obtener los datos de cada chunk
url_base <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", 
                   1:10, ".html")

#Se crea un data frame vacío
datos_geih<- data.frame()

#Se hace loop para obtener cada chunk de datos y crear una base de datos con todo 
for (url in url_base){
  #Se imprime cada url para poder ver en qué estado de iteración se encuentra el loop
  print(url)
  #Se crea una base temporal con la tabla contenida en el html de cada chunk
  temporal <- read_html(url) %>%
    html_table()
  #Se toma la tabla de uno de todas las tablas presentes
  temporal <- as.data.frame(temporal[[1]])
  #Se realiza un merge de las tablas por filas
  datos_geih<- rbind(datos_geih, temporal)
}

#Anteriormente, se revisar si tiene alguna restricción: 
browseURL("https://ignaciomsarmiento.github.io/robots.txt")


# B data cleaning
#Se guardan las observaciones de +18
datosgeih<- subset(datos_geih, age >= 18)
save(datosgeih, file = "datageih.RData")
load("datageih.RData")

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



# ver los missing values 
colSums(is.na(data_punto1))



datos_geih$ingtot %>% table(useNA="ifany") %>% prop.table() %>% round(3)*100





#Punto 2-----------------------------

#Para comernzar, vamos a seleccionar aquellas variables más relevantes para el análisis
x=datosgeih[, c('age')]
y= datosgeih[, "y_total_m_ha"]
w=datosgeih[, c("fweight")]
datap2 =cbind(y,X,w)
datap2= data.frame(datap2)



#Punto a
#Dado el enfoque de nuestro trabajo, se tomará la variable de ___, por las siguientes razones;
#Se observa como esta´n distribuídos los ingresos a lo largo de la muestra (para los valores de edad) 
ggplot(data=datosgeih ,aes(x,y)) + geom_point() + 
  #Arreglar el eje x
  scale_x_continuous(n.breaks=10, limits=c(18,87)) + 
  #Arreglar el eje y
  scale_y_continuous(n.breaks=8,labels=scales::dollar) + 
  #Añadir el titulo, subtitulo y caption
  labs(title= "Relacion entre salario y edad", subtitle = "Evidence for Colombia", caption="Source: GEIH 2008")+  
  #Añadir los labels de los ejes
  xlab("Edad") + ylab("Salario") + theme_bw()


#Punto b
# A continuación, se estima el modelo base con los pesos ponderados de cada observación sea presentativa de la población
modelo1 <- lm("y ~ X + I(X^2)", data=datap2, weights= w, x=TRUE )
stargazer(modelo1) #Para tex
stargazer(modelo1, type ="text") #Para text


#Punto c
# Se encuentran todas las medidas de ajuste del modelo. 
ajuste1 <- broom::glance(modelo1)
#Se encuetran los errores cuadráticos medios 
mse1= mean(modelo1$residuals^2)
rtmse1= sqrt(mean(modelo1$residuals^2))
#Se encuetran los errores cuadráticos medios (manual)
mse1m= mean((modelo1$model$y - modelo1$fitted.values)^2)
rtmse1m= sqrt(mean((modelo1$model$y - modelo1$fitted.values)^2))

#Punto d
## Se gráfican los valores predichos con los ajustes de pesos ponderados
ggplot(data=datap2 ,aes(X,y)) + stat_smooth(method = "lm", formula = "y ~ x + I(x^2)", size = 1, color = "red", aes(weight = w)) + 
  # Arreglando el eje x
  scale_x_continuous(n.breaks=10, limits=c(18,87)) + 
  # Arreglando el eje y
  scale_y_continuous(n.breaks=8,labels=scales::dollar) + 
  #Se añade el titulo, subtitulo y caption
  labs(title= "Relacion entre salario y edad", subtitle = "Evidence for Colombia", caption="Source: GEIH 2008")+  
  #Se añaden los labels de los ejes
  xlab("Age") + ylab("Income salaried + Independents Total - Nominal hourly") + theme_bw()

##Discusion Peak Ages: Boostrap sobre peak ages
peakage=50

#Sin bootstrap
predict(modelo1, list(X = peakage), interval = "c")

#Con bootstrap
n<-nrow(modelo1$model)
B <- 10000
pred <- numeric(B)
for (i in 1:B) {
  boot <- sample(n, n, replace = TRUE)
  fit.b <- lm("y ~ X + I(X^2)", data = datap2[boot,])
  pred[i] <- predict(fit.b, list(X = 50)) + sample(resid(fit.b), size = 1)
}
quantile(pred, c(0.025, 0.975))
var(fit.b$fitted.values)


#Punto 3-----------------------------
#Buscamos una medida del ingreso salarial que logre capturar dinámicas del mercado laboral, y tenga pocas distorsiones frente
#a por ejemplo, la cantidad de horas que se trabajan
#Como se encuentra en la base, la dirección de la dummy está "al revés" se crea la variable female
datosgeih$female <- ifelse(datosgeih$sex==0, 1,0)
datosgeih$sqage <- datosgeih$age^2
#Teniendo en cuenta el modelo del inciso anterior se deben crear las interacciones como nuevas variables
datosgeih$agefemale <- datosgeih$female * datosgeih$age
datosgeih$sqagefemale <- datosgeih$female * datosgeih$age^2

#Modelo y visualización de resultados
x=datos_geih[, c("female","age","sqage","agefemale", "sqagefemale")]
data_punto3 =cbind(y,X)
data_punto3= data.frame(data_punto3)
skim(data_punto3)
modelo2 = lm("y ~ female+age+sqage+agefemale+sqagefemale" , data = data_punto3, weights= datos_geih$fweight, x=TRUE) 
summary(modelo2)
#Es mejor usar stargazer
m2_summary=summary(modelo2)$coefficients
m2_summary_print = m2_summary
m2_summary_print[,'t value'] = abs(m2_summary_print[,'t value'])
kable(m2_summary_print[,c('Estimate', 'Std. Error', 't value','Pr(>|t|)')], digits = 1, col.names = c('Weight', 'SE', "|t|","P-value"), booktabs = TRUE, center = TRUE) %>% kable_styling(position = "center")
#Sin embargo, es razonable pensar que a la hora de hablar de ingreso o salario, el error será heterocedástico
summ(modelo2, robust = "HC1")

#Punto 3 (VersiónMatteo para sacar gráfico)
datosgeih$female <- ifelse(datosgeih$sex==0, "Mujer","Hombre")

x=datosgeih[,c('female', 'fweight','age')]
y=datosgeih[,c('ingtotes')]
datap3 =cbind(y,x)
datap3= data.frame(datap3)
datap3$female <- as.character(datap3$female)

#Punto A
modelo2 <- lm("y ~female" , data = datap3, weights= datap3$fweights, x=TRUE ) 
summary(modelo2)
stargazer(modelo2)#Para subir a Latex
stargazer(modelo2, type = "text")#En text

# Se encuentran todas las medidas de ajuste del modelo. 
ajuste2 <- broom::glance(modelo2)
#Se encuetran los errores cuadráticos medios 
mse2= mean(modelo2$residuals^2)
rtmse2= sqrt(mean(modelo2$residuals^2))
#Se encuetran los errores cuadráticos medios (manual)
mse2m= mean((modelo2$model$y - modelo2$fitted.values)^2)
rtmse2m= sqrt(mean((modelo2$model$y - modelo2$fitted.values)^2))


#Punto B

#Con la medida de ingreso total imputado ingtotes
modelo3 <- lm("y ~age+I(age^2)+female+(age+I(age^2)):female" , data = datap3, weights= datap3$fweights, x=TRUE ) 

#Se encuentran los coefficientes y la tabla en latex
summary(modelo3)
stargazer(modelo3)#Para subir a Latex
stargazer(modelo3, type = "text")#En text

# Se encuentran todas las medidas de ajuste del modelo. 
ajuste3 <- broom::glance(modelo3)
#Se encuetran los errores cuadráticos medios 
mse3= mean(modelo3$residuals^2)
rtmse3= sqrt(mean(modelo3$residuals^2))
#Se encuetran los errores cuadráticos medios (manual)
mse3m= mean((modelo3$model$y - modelo3$fitted.values)^2)
rtmse3m= sqrt(mean((modelo3$model$y - modelo3$fitted.values)^2))


## graficar solo la linea de ajuste para cada sexo (corregir pesos)

ggplot(data=datap3 ,aes(x=age,y=y,col=female)) +
  stat_smooth(method = "lm", formula="y ~x+I(x^2)", aes(weight=datap3$fweight)) + 
  # arreglando el eje x
  scale_x_continuous(n.breaks=10, limits=c(18,87)) + 
  # Arreglar el eje y
  scale_y_continuous(n.breaks=8,labels=scales::dollar) + 
  #anadir el titulo, subtitulo y caption
  labs(title= "Relacion entre salario y edad", subtitle = "Evidence for Colombia", caption="Source: GEIH 2008")+  
  # anadir los labels de los ejes
  xlab("Edad") + ylab("Salario") + theme_bw()

#Punto 4-----------------------------
#Punto a

set.seed(10101)
datos_geih <- datosgeih %>%
  mutate(holdout= as.logical(1:nrow(datosgeih) %% 
                               sample(nrow(datosgeih), nrow(datosgeih)*.3))
  )


test<-datos_geih[datosgeih$holdout==T,]
train<-datos_geih[datosgeih$holdout==F,]

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


#Punto b

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





