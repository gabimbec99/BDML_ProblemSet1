#Problem Set 1#
## Daniel Lasso, Matteo Rozo y Gabriela Mejía ##

# Punto 1 -------------------------------------------------

#Antes de comenzar, es necesario installar y cargar aquellos paquetes y librerías útiles para nuestro desarrollo.De tal forma:
#install.packages("stargazer")
#install.packages("sandwich")
#install.packages("estimatr")
library(pacman)
library(rvest, tidyverse)
library (dplyr)
library(stargazer)
library(estimatr)


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


# B data cleaning
#Se guardan las observaciones de +18
datosgeih<- subset(datos_geih, age >= 18)

#write.csv(datosgeih, "D:/noveno semestre/big data/Problem_set1/BDML_ProblemSet1/GEIH_BIG_DATA.csv", row.names=FALSE)
#datosgeih <- read.csv("D:/noveno semestre/big data/Problem_set1/BDML_ProblemSet1/GEIH_BIG_DATA.csv", row.names=FALSE)

# elegimos las variables que vamos a usar para el problem set: 
X1=datosgeih[, c('age','oficio','relab','college','cotPension','fweight', 'formal', 'hoursWorkUsual', 'sex', "clase", "maxEducLevel", "dsi","p6426","sizeFirm","wap")]
y1= datosgeih[,"y_total_m_ha"]
y2= datosgeih[,"ingtot"]
datap1 =cbind(y1,y2,X1)
datap1= data.frame(datap1)


# vamos a cambiar el orden de las variables sexo
datap1$female <- ifelse(datap1$sex==0, 1,0)

#Teniendo en cuenta los modelos que se deben realizar, en los incisos a continucación  inciso anterior se deben crear las interacciones como nuevas variables
datap1$sqage <- datap1$age^2
datap1$agefemale <- datap1$female * datap1$age
datap1$sqagefemale <- datap1$female * datap1$age^2


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

# sizefirm 
labelsfirma=labels[labels$Variable == 'sizeFirm',]
firma <- labelsfirma[["level"]]
numfirma <- labelsfirma[["values"]]

# max educ level 
labelseduc=labels[labels$Variable == 'maxEducLevel',]
educ <- labelseduc[["level"]]
numeduc <- labelseduc[["values"]]


# relabel de los oficios
datap1 <- datap1 %>% 
  mutate(oficio = recode(oficio, !!!(set_names(oficios, numoficio)), .default = NA_character_))
# relabel de los relab
datap1 <- datap1 %>% 
  mutate(relab = recode(relab, !!!(set_names(relab,numrelab)), .default = NA_character_))
# relab del tamano de la firma
datap1 <- datap1 %>% 
  mutate(sizeFirm = recode(sizeFirm, !!!(set_names(firma, numfirma)), .default = NA_character_))
# relab maxeduc
datap1 <- datap1 %>% 
  mutate(maxEducLevel = recode(maxEducLevel, !!!(set_names(c("Ninguno","Prescolar","Primaria Completa","Primaria Incompleta", "Secundaria Completa","secundaria Incompleta","Terciaria", "NAN"), numeduc)), .default = NA_character_))


# otros relab
datap1 <- datap1 %>% 
  mutate(formal = recode(formal, !!!(set_names(c("informal","formal"), 0:1)), .default = NA_character_))

datap1 <- datap1 %>% 
  mutate(college = recode(college, !!!(set_names(c("No universidad","Universidad"), 0:1)), .default = NA_character_))

datap1 <- datap1 %>% 
  mutate(cotPension = recode(cotPension, !!!(set_names(c("Cotiza","No cotiza","Pensionado"), 1:3)), .default = NA_character_))

datap1 <- datap1 %>% 
  mutate(female = recode(female, !!!(set_names(c("Hombre","Mujer"), 0:1)), .default = NA_character_))

datap1 <- datap1 %>% 
  mutate(clase = recode(clase, !!!(set_names(c("Rural","Urban"), 0:1)), .default = NA_character_))

datap1 <- datap1 %>% 
  mutate(wap = recode(wap, !!!(set_names(c("Otra cosa","Poblacion en edad de trabajar"), 0:1)), .default = NA_character_))

datap1 <- datap1 %>% 
  mutate(dsi = recode(dsi, !!!(set_names(c("Otra cosa","Desempleado"), 0:1)), .default = NA_character_))

# convertir las variables en factores. 
variables_chr <- names(select_if(datap1, is.character))

for (v in variables_chr) {
  datap1[, v] <- as.factor(datap1[, v, drop = T])
}

glimpse(datap1)
estadisticas <- skim(datap1)
stargazer(datap1, type="text")
# imputacion de las variables omitidas. 

# no se puede imputar las variables omitidas porque estan autocorrelacionadas. 
# la variable y la literatura economica enuncia que no es bueno imputarla .

# ver los missing values y omitirlos 
colSums(is.na(datap1))
datap1 <- na.omit(datap1)
colSums(is.na(datap1))

#Ya no tiene missing values. 


# estadisticas descriptivas. 

# variables numericas
variables_numericas <- names(select_if(datap1, is.numeric))
df_numeric=datap1[,variables_numericas]

df1_summary<-as.data.frame(apply(df_numeric,2,summary))
df1_summary

write_excel_csv(estadisticas_numericas, "airbnb_summary.csv")

# variables factores. 
#install.packages("ggplot2")
#install.packages("waffle")

library(ggplot2)
library(waffle)

int<-table(datap1$maxEducLevel)
int2<-table(datap1$female)
int3<-table(datap1$sizeFirm)

# Gráfico de waffle

waffle(int/100, rows=7, size=0.8, title="Nivel educativo de los individuos de la muestra", 
       xlab="1 cuadrado = 100 personas")

waffle(int2/100, rows=5, size=0.8, title="Balance de sexo en los individuos de la muestra", 
       xlab="1 cuadrado = 100 personas")
waffle(int3/100, rows=7, size=0.8, title="Tamaño de la firma donde trabajan los empleados", 
       xlab="1 cuadrado = 100 personas")



# Algunas gráficas relevantes
##### boxplot

ggplot(datap1, aes(x=relab,y=y1, color=female))+ geom_boxplot() +
  #Arreglar el eje y
  scale_y_continuous(n.breaks=8,labels=scales::dollar) + 
  #
  scale_x_discrete(labels=c("Empleado Doméstico","Jornalero u Obrero", "Empleado privado", "Empleado del Gobierno", "Otro", "Patrón o Empleador", "Independiente")) + 
  #Añadir el titulo, subtitulo y caption
  labs(title= "Salario por sexo en Bogota", subtitle = "Evidence for Colombia", caption="Source: GEIH 2008")+  
  #Añadir los labels de los ejes
  xlab("Tipo de empleado") + ylab("Ingresos por hora") + theme_bw() 

datap1$oficio <- as.character(datap1$oficio)

# grafica 2 

ggplot(datap1, aes(x=oficio,y=y1))+ geom_boxplot() +
  #Arreglar el eje y
  scale_y_continuous(n.breaks=8,labels=scales::dollar) + 
  #+ 
  #Añadir el titulo, subtitulo y caption
  labs(title= "Salario por oficio", subtitle = "Evidence for Colombia", caption="Source: GEIH 2008")+  
  #Añadir los labels de los ejes
  xlab("Cateogria de oficio") + ylab("Ingresos por hora") + theme_bw() 


datap1$formal %>% table(useNA="ifany") %>% prop.table() %>% round(3)*100
datap1$oficio %>% table(useNA="ifany") %>% prop.table() %>% round(3)*100
datap1$ingtot %>% table(useNA="ifany") %>% prop.table() %>% round(3)*100

#Histograma simple
ggplot(datap1, aes(x=y1)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.8,fill="#00C0AF")+
  scale_x_continuous(n.breaks=8,labels=scales::dollar)+ geom_vline(xintercept = 2403, linetype="dashed", color = "blue", size=0.5)+
  labs(title= "Histograma de ingreso laboral", subtitle = "Evidencia para Colombia", caption="Fuente: GEIH 2008")+  
  #Añadir los labels de los ejes
  xlab("Ingreso laboral") + ylab("Densidad") + theme_bw() 



#Punto 2-----------------------------


#Para comernzar, vamos a seleccionar aquellas variables más relevantes para el análisis
x=datap1[, c('age')]
y= datap1[, "y1"]
w=datap1[, c("fweight")]
datap2 =cbind(y,x,w)
datap2= data.frame(datap2)



#Punto a
#Dado el enfoque de nuestro trabajo, se tomará la variable de ___, por las siguientes razones;
#Se observa como están distribuídos los ingresos a lo largo de la muestra (para los valores de edad) 
ggplot(data=datap2 ,aes(x,y)) + geom_point(color="#00C0AF") + 
  #Arreglar el eje x
  scale_x_continuous(n.breaks=10, limits=c(18,87)) + 
  #Arreglar el eje y
  scale_y_continuous(n.breaks=8,labels=scales::dollar) + 
  #Añadir el titulo, subtitulo y caption
  labs(title= "Relacion entre ingreso laboral y edad", subtitle = "Evidencia para Colombia", caption="Fuente: GEIH 2008")+  
  #Añadir los labels de los ejes
  xlab("Edad") + ylab("Ingreso laboral por hora") + theme_bw()


#Punto b
# A continuación, se estima el modelo base con los pesos ponderados de cada observación sea presentativa de la población
modelo1 <- lm("y ~ x + I(x^2)", data=datap2, weights= w, x=TRUE )
stargazer(modelo1) #Para tex
stargazer(modelo1, type ="text") #Para text


#Punto c
# Se encuentran todas las medidas de ajuste del modelo. 
ajuste1 <- broom::glance(modelo1)
ajuste1<-data.frame(t(ajuste1))
stargazer(ajuste1, summary=FALSE,rownames=TRUE)
#Se encuetran los errores cuadráticos medios 
mse1= mean(modelo1$residuals^2)
rtmse1= sqrt(mean(modelo1$residuals^2))

#Punto d
##Discusion Peak Ages: Boostrap sobre peak ages
peakage=50

#Sin bootstrap
predict(modelo1, list(X = peakage), interval = "c")

#Con bootstrap v1
set.seed(428)
library(boot)

#define function to calculate R-squared
coef_function <- function(formula, data, indices) {
  d <- data[indices,] #allows boot to select sample
  fit <-coef(lm(y ~ x + I(x^2), data = d,  weights = w))
  #fit regression model
  return(fit) 
}
#perform bootstrapping with 2000 replications
reps <- boot(data=datap2, statistic=coef_function, R=2000, formula=y ~ x + I(x^2), weights = w)
#Usar errores estandares para estimar los intervalos de confianza
x=datap1[, c('age')]
boots <- data.frame(x)
boots$x2 <-boots$x^2

seI=apply(reps$t,2,sd)[1]
seX=apply(reps$t,2,sd)[2]
seX2=apply(reps$t,2,sd)[3]

I=apply(reps$t,2,mean)[1]
x=apply(reps$t,2,mean)[2]
x2=apply(reps$t,2,mean)[3]

I=reps$t0[1]
x=reps$t0[2]
x2=reps$t0[3]

bIl=(I-seI*1.96)
bxl=(x-seX*1.96)
bx2l=(x2-seX2*1.96)

bIu=(I+seI*1.96)
bxu=(x+seX*1.96)
bx2u=(x2+seX2*1.96)

yhatl3=bIl+bxl*boots$x+bx2l*boots$x2
yhatu3=bIu+bxu*boots$x+bx2u*boots$x2

bootsic <- cbind(boots,yhatl3,yhatu3)

#Con bootstrap v2

lboundsi= boot.ci(reps,type="basic",index=1)$basic[4]
uboundsi=boot.ci(reps,type="basic",index=1)$basic[5]

lboundsx= boot.ci(reps,type="basic",index=2)$basic[4]
uboundsx=boot.ci(reps,type="basic",index=2)$basic[5]

lboundsx2= boot.ci(reps,type="basic",index=3)$basic[4]
uboundsx2=boot.ci(reps,type="basic",index=3)$basic[5]

yhatl2=lboundsi+lboundsx*boots$x+lboundsx2*boots$x2
yhatu2=uboundsi+uboundsx*boots$x+uboundsx2*boots$x2

bootsic <- cbind(bootsic,yhatl2,yhatu2)

#Con bootstrap v3
n<-nrow(modelo1$model)
B <- 10000
pred <- numeric(B)
upper <- numeric(B)
lower <- numeric(B)

set.seed(42)
for (i in 1:B) {
  boot <- sample(n, n, replace = TRUE)
  fit.b <- lm("y ~ x + I(x^2)", data = datap2[boot,], weights= w, x=TRUE )
  pred[i] <- predict(fit.b, data.frame(x=50)) 
  lower[i] <- predict(fit.b, data.frame(x=50),interval = "c")[2]
  upper[i] <- predict(fit.b, data.frame(x=50),interval = "c")[3]
}
quantile(pred, c(0.025, 0.975))

yhatbt=mean(pred)
upperbt=mean(upper)
lowerbt=mean(lower)

boots <- data.frame(yhatbt, lowerbt,upperbt)
names(boots) <- c("fit", "lwr", "upr")

icbt <- rbind(matriz,boots)

## Se gráfican los valores predichos con los ajustes de pesos ponderados
ggplot(data=datap2 ,aes(x,y)) + stat_smooth(method = "lm", formula = "y ~ x + I(x^2)", size = 1, color = "red", aes(weight = w)) + 
  geom_ribbon(data=bootsic,aes(ymin=yhatl3,ymax=yhatu3), alpha=0.1, fill = "steelblue2")+
  # Arreglando el eje x
  scale_x_continuous(n.breaks=10, limits=c(18,87)) + 
  # Arreglando el eje y
  scale_y_continuous(n.breaks=8,labels=scales::dollar) + 
  #Se añade el titulo, subtitulo y caption
  labs(title= "Relacion entre salario y edad", subtitle = "Evidence for Colombia", caption="Source: GEIH 2008")+  
  #Se añaden los labels de los ejes
  xlab("Age") + ylab("Income salaried + Independents Total - Nominal hourly") + theme_bw()



######### probar el paquete a ver si da lo mismo ################

set.seed(0)
library(boot)

#define function to calculate R-squared
coef_function <- function(formula, data, indices) {
  d <- data[indices,] #allows boot to select sample
  fit <- lm(formula, data=d) #fit regression model
  return(coef(fit)) #return R-squared of model
}
#perform bootstrapping with 2000 replications
reps <- boot(data=datap2, statistic=coef_function, R=1000, formula=y ~ x + I(x^2))
reps
boot.ci(reps, type="basic", index=1) #disp predictor variable`
boot.ci(reps, type="basic", index=2) #disp predictor variable`
boot.ci(reps, type="basic", index=3) #disp predictor variable`


#Punto 3-----------------------------
#Modelo y visualización de resultados
w=datap1[, c("fweight")]
y= datap1[, "y1"]
x=datap1[, c("female","age","sqage","agefemale", "sqagefemale")]
datap3 =cbind(y,x,w)
datap3= data.frame(datap3)
skim(datap3)


#Punto A
modelo2 <- lm("y ~female" , data = datap3, weights= datap3$fweights, x=TRUE ) 
summary(modelo2)
modelo2robust <- commarobust(modelo2)
stargazer(modelo2, se=starprep(modelo2robust))#Para subir a Latex
stargazer(modelo2, se=starprep(modelo2robust), type="text") #En text

# Se encuentran todas las medidas de ajuste del modelo. 
ajuste2 <- broom::glance(modelo2)
ajuste2<-data.frame(t(ajuste2))
stargazer(ajuste1,ajuste2, summary=FALSE,rownames=TRUE,single.row=TRUE,
          align=TRUE, dep.var.labels=c("Modelo 1","Modelo 2")) #Para ver en R
#Se encuetran los errores cuadráticos medios 
mse2= mean(modelo2$residuals^2)
rtmse2= sqrt(mean(modelo2$residuals^2))


#Punto B

modelo3= lm(y ~ female+age+sqage+agefemale+sqagefemale , data = datap3, weights= w, x=TRUE)
modelo3robust <- commarobust(modelo3)
stargazer(modelo3, se =starprep(modelo3robust)) #Para tex
stargazer(modelo3, se =starprep(modelo3robust), type ="text") #Para text


# Se encuentran todas las medidas de ajuste del modelo. 
ajuste3 <- broom::glance(modelo3)
ajuste3<-data.frame(t(ajuste3))
ajustetotal <- cbind(ajuste1,ajuste2,ajuste3)
stargazer(ajustetotal, summary=FALSE,rownames=TRUE,single.row=TRUE,
          align=TRUE, dep.var.labels=c("Modelo 1","Modelo 2","Modelo 3"), type="text") #Para ver en R
stargazer(ajustetotal, summary=FALSE,rownames=TRUE,single.row=TRUE,
          align=TRUE, dep.var.labels=c("Modelo 1","Modelo 2","Modelo 3")) #Para Latex

#Se encuetran los errores cuadráticos medios 
mse3= mean(modelo3$residuals^2)
rtmse3= sqrt(mean(modelo3$residuals^2))


#Tengo 3 modelos (Solo edad, género y el que tiene sus interacciones)
stargazer(modelo1, modelo2, modelo3, title="Regression Results",
          align=TRUE, dep.var.labels=c("Ingreso-Edad","Ingreso-Género", "Ingreso- Género y Edad"),no.space=TRUE)
stargazer(modelo1, modelo2, modelo3, title="Regression Results",
          align=TRUE, dep.var.labels=c("Ingreso-Edad","Ingreso-Género", "Ingreso- Género y Edad"),no.space=TRUE, type="te")

#Punto 3.B(VersiónMatteo para sacar gráfico) -----------------------------
datosgeih$female <- ifelse(datosgeih$sex==0, "Mujer","Hombre")

x=datap1[,c('female', 'fweight','age')]
y=datap1[,c("y1")]
datap3 =cbind(y,x)
datap3= data.frame(datap3)
datap3$female <- as.character(datap3$female)


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

# Punto C
#Las variables que permiten capturar alguna noción de la ocupación de los colombianos, son oficio, relab y la formalidad
#que establecen respectivamente el oficio (dentro de categorías amplias) y  la relación laboral que ocupan (Empleado del gobierno, de empresas privadas) junto con si es de caracter formal o informal.
#Adicionalmente, la naturaleza del trabajo estará condicionado al tamaño de la empresa, el nivel de educación de los individuos y su experiencia o antiguedad en su trabajo.
names(select_if(datap1, is.factor))
#Dumificamos para poder condicionar las variables que se encuentran como factores con 2 o mas categorías
#dummiesfijos <- model.matrix(~ oficio+relab+formal+maxEducLevel+sizeFirm, datap1) %>%
#  as.data.frame()
dummiesfijos <- model.matrix(~ oficio+relab+formal, datap1) %>%
  as.data.frame()
datap3c=cbind(dummiesfijos,datap3)
tenure=datap1[,c('p6426')]
datap3c=cbind(datap3c,tenure)

sapply(lapply(dummiesfijos, unique), length)
sapply(lapply(datap1$oficio, unique), length)
modelo3c<-lm("y~ -1+.-w", data=datap3c, weights=w)

stargazer(modelo3c,type="text")
stargazer(modelo3c)
summary(modelo3c)

skim(datap1$clase)
skim(datosgeih$clase)

datacontroles=cbind(dummiesfijos,tenure,x,w)

modelo4<-lm("y~ -1+.-tenure-w",data=datap3c, weights=w)
modelo4b <-lm("tenure~.-w",data=datacontroles, weights=w)

stargazer(modelo4,type="text")

res_y_a=modelo4$residuals
res_s_a=modelo4b$residuals
w_res=datap3c$w
db<-data.frame(res_y_a,res_s_a,w)
modelo4c<-lm(res_y_a~res_s_a-1,db, weights=w)
stargazer(modelo3c,modelo4c,type="text")
stargazer(modelo3c,modelo4c)

#Con bootstrap v1
set.seed(428)
library(boot)

#define function to calculate R-squared
res_function <- function(formula, data, indices) {
  d <- data[indices,] #allows boot to select sample
  fit <-coef(lm("res_y_a~res_s_a-1", data = d,  weights = w))
  return(fit) 
}

#perform bootstrapping with 2000 replications
reps2 <- boot(data=datap3c, statistic=res_function, R=2000, formula="res_y_a~res_s_a-1", weights = w)
seRBs=apply(reps2$t,2,sd)[1]
seR=sqrt(diag(vcov(modelo4c)))

stargazer(modelo3c,modelo4c,type="text")
stargazer(modelo3c,modelo4c)



#Punto 4-----------------------------
#Punto a

#Punto a
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(datap3c), replace=TRUE, prob=c(0.7,0.3))
train  <- datap3c[sample, ]
test   <- datap3c[!sample, ]

#Punto b-Formato general

#Se crea una tabla vacía para almacenar los resultados
N=10
modelos <- numeric(N)
pred <- numeric(N)
rmsemodelos <- data.frame(modelos, pred)
names(rmsemodelos) <- c("modelo", "rmse")

modelo1 <- lm("y ~ age+I(age^2)", data=train, weights= train$w)
test$modelo1<-predict(modelo1,newdata = test)
rmse<-with(test,sqrt(mean((y-modelo1)^2)))
rmsemodelos[1,] <- c(1,rmse)

modelo2 <- lm("y ~ female", data=train, weights= train$w)
test$modelo2<-predict(modelo2,newdata = test)
rmse<-with(test,sqrt(mean((y-modelo2)^2)))
rmsemodelos[2,] <- c(2,rmse)

modelo3 <- lm("y ~age+I(age^2)+female+(age+I(age^2)):female", data=train, weights= train$w)
test$modelo3<-predict(modelo3,newdata = test)
rmse<-with(test,sqrt(mean((y-modelo3)^2)))
rmsemodelos[3,] <- c(3,rmse)

modelo4 <- lm("y ~ -1+.-w", data=train, weights= train$w)
test$modelo4<-predict(modelo4,newdata = test)
rmse<-with(test,sqrt(mean((y-modelo4)^2)))
rmsemodelos[4,] <- c(4,rmse)

#Versión FWL
modelo4b<-lm("y~ -1+.-tenure-w",data=train, weights=w)
modelo4c <-lm("tenure~.-w-y",data=train, weights=w)

res_y_a=modelo4b$residuals
res_s_a=modelo4c$residuals

train<-cbind(train,res_y_a,res_s_a)

modelo4b<-lm("y~ -1+.-tenure-w",data=test, weights=w)
modelo4c <-lm("tenure~.-w-y",data=test, weights=w)

res_y_a=modelo4b$residuals
res_s_a=modelo4c$residuals

test<-cbind(test,res_y_a,res_s_a)

modelo5 <- lm("res_y_a~res_s_a-1", data=train, weights= w)
test$modelo5<-predict(modelo5,newdata = test)
rmse<-with(test,sqrt(mean((res_y_a-modelo5)^2)))
rmsemodelos[5,] <- c(5,rmse)

modelo6 <- lm("y1~ -1+.-tenure-w", data=train, weights= train$fweight)
test$modelo4<-predict(modelo4,newdata = test)
rmse<-with(test,sqrt(mean((y1-modelo4)^2)))
rmsemodelos[4,] <- c(4,rmse)

modelo7 <- lm("y1~ -1+.-tenure-w", data=train, weights= train$fweight)
test$modelo4<-predict(modelo4,newdata = test)
rmse<-with(test,sqrt(mean((y1-modelo4)^2)))
rmsemodelos[4,] <- c(4,rmse)

modelo8 <- lm("y1~ -1+.-tenure-w", data=train, weights= train$fweight)
test$modelo4<-predict(modelo4,newdata = test)
rmse<-with(test,sqrt(mean((y1-modelo4)^2)))
rmsemodelos[4,] <- c(4,rmse)


modelo9 <- lm("y1~ -1+.-tenure-w", data=train, weights= train$fweight)
test$modelo4<-predict(modelo4,newdata = test)
rmse<-with(test,sqrt(mean((y1-modelo4)^2)))
rmsemodelos[4,] <- c(4,rmse)

modelo10 <- lm("y1~ -1+.-tenure-w", data=train, weights= train$fweight)
test$modelo4<-predict(modelo4,newdata = test)
rmse<-with(test,sqrt(mean((y1-modelo4)^2)))
rmsemodelos[4,] <- c(4,rmse)


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

