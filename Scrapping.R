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

#Punto A) Data Scrapping

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


#Punto B) Data Cleaning
#Se guardan las observaciones de +18
datosgeih<- subset(datos_geih, age >= 18)

#write.csv(datosgeih, "D:/noveno semestre/big data/Problem_set1/BDML_ProblemSet1/GEIH_BIG_DATA.csv", row.names=FALSE)
#datosgeih <- read.csv("D:/noveno semestre/big data/Problem_set1/BDML_ProblemSet1/GEIH_BIG_DATA.csv", row.names=FALSE)

# Se elegen las variables que vamos a usar para el problem set: 
X1=datosgeih[, c('age','oficio','relab','college','cotPension','fweight', 'formal', 'hoursWorkUsual', 'sex', "clase", "maxEducLevel", "dsi","p6426","sizeFirm","wap")]
y1= datosgeih[,"y_total_m_ha"]
y2= datosgeih[,"ingtot"]
datap1 =cbind(y1,y2,X1)
datap1= data.frame(datap1)


#Se cambia el orden de las variables sexo
datap1$female <- ifelse(datap1$sex==0, 1,0)

#Teniendo en cuenta los modelos que se deben realizar, en los incisos a continucación  inciso anterior se deben crear las interacciones como nuevas variables
datap1$sqage <- datap1$age^2
datap1$agefemale <- datap1$female * datap1$age
datap1$sqagefemale <- datap1$female * datap1$age^2


# En la siguiente sección, se hará limpieza de la base, cambiar las etiquetas y volver las variables factores.

# Se obtiene la tabla de nombres de las variables: 

url2 <- 'https://ignaciomsarmiento.github.io/GEIH2018_sample/labels.html'
labels <- read_html(url2) %>%
  html_table()
#Se toma la tabla de uno de todas las tablas presentes
labels <- as.data.frame(labels)

#Se extraem los nombres de las variables de oficios: 


labelsoficio=labels[labels$Variable == 'oficio',]
#Se quitan las comillas de la variable levels
labelsoficio$level <- gsub('"','',as.character(labelsoficio$level))
oficios <- labelsoficio[["level"]]
numoficio <- labelsoficio[["values"]]

#Se repite el proceso para relab
labelsrelab=labels[labels$Variable == 'relab',]
labelsrelab$level <- gsub('"','',as.character(labelsrelab$level))
relab <- labelsrelab[["level"]]
numrelab <- labelsrelab[["values"]]

#Se repite el proceso para sizefirm 
labelsfirma=labels[labels$Variable == 'sizeFirm',]
firma <- labelsfirma[["level"]]
numfirma <- labelsfirma[["values"]]

#Se repite el proceso para max educ level 
labelseduc=labels[labels$Variable == 'maxEducLevel',]
educ <- labelseduc[["level"]]
numeduc <- labelseduc[["values"]]


#Se hace relabel de los oficios
datap1 <- datap1 %>% 
  mutate(oficio = recode(oficio, !!!(set_names(oficios, numoficio)), .default = NA_character_))
#Se hace relabel de los relab
datap1 <- datap1 %>% 
  mutate(relab = recode(relab, !!!(set_names(relab,numrelab)), .default = NA_character_))
#Se hace relab del tamano de la firma
datap1 <- datap1 %>% 
  mutate(sizeFirm = recode(sizeFirm, !!!(set_names(firma, numfirma)), .default = NA_character_))
#Se hace relab maxeduc
datap1 <- datap1 %>% 
  mutate(maxEducLevel = recode(maxEducLevel, !!!(set_names(c("Ninguno","Prescolar","Primaria Completa","Primaria Incompleta", "Secundaria Completa","secundaria Incompleta","Terciaria", "NAN"), numeduc)), .default = NA_character_))


#Se hacen otros relabs
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

#Se convierten las variables en factores. 
variables_chr <- names(select_if(datap1, is.character))

for (v in variables_chr) {
  datap1[, v] <- as.factor(datap1[, v, drop = T])
}

glimpse(datap1)
estadisticas <- skim(datap1)
stargazer(datap1, type="text")

#Con respecto, a la imputacion de las variables omitidas. 
# Idealmente, no se debe imputar las variables omitidas porque estan autocorrelacionadas. 
# la variable y la literatura economica enuncia que no es bueno imputarla .

#Se ven los missing values y los omitimos 
colSums(is.na(datap1))
datap1 <- na.omit(datap1)
colSums(is.na(datap1))
#Ya no tiene missing values. 


#Estadisticas Descriptivas. 

#Variables numericas
variables_numericas <- names(select_if(datap1, is.numeric))
df_numeric=datap1[,variables_numericas]

df1_summary<-as.data.frame(apply(df_numeric,2,summary))
df1_summary


#Se llaman los siguientes paquetes y librerias para los gráficos:
install.packages("ggplot2")
install.packages("waffle")
library(ggplot2)
library(waffle)

#Variables factores. 
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

#Boxplot (1)

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

#Boxplot (2)

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



# Punto 2 -------------------------------------------------

#Para comernzar, vamos a seleccionar aquellas variables más relevantes para el análisis
x=datap1[, c('age')]
y= datap1[, "y1"]
w=datap1[, c("fweight")]
datap2 =cbind(y,x,w)
datap2= data.frame(datap2)



#Punto A)
#Dado el enfoque de nuestro trabajo, se tomará la variable de ___, por las siguientes razones;
#Se observa como están distribuídos los ingresos a lo largo de la muestra (para los valores de edad) 
ggplot(data=datap2 ,aes(x,y)) + geom_point(color="#00C0AF") + 
  #Se arregla el eje x
  scale_x_continuous(n.breaks=10, limits=c(18,87)) + 
  #Se arregla el eje y
  scale_y_continuous(n.breaks=8,labels=scales::dollar) + 
  #Se añade el titulo, subtitulo y caption
  labs(title= "Relacion entre ingreso laboral y edad", subtitle = "Evidencia para Colombia", caption="Fuente: GEIH 2008")+  
  #Se añade los labels de los ejes
  xlab("Edad") + ylab("Ingreso laboral por hora") + theme_bw()


#Punto B)
# A continuación, se estima el modelo base con los pesos ponderados de cada observación sea presentativa de la población
modelo1 <- lm("y ~ age + sqage", data=datap1, weights= fweight, x=TRUE )
modelo1_r <- commarobust(modelo1)
stargazer(modelo1, se=starprep(modelo1_r)) #Para tex
stargazer(modelo1,se=starprep(modelo1_r), type ="text") #Para text

#Punto C)
# Se encuentran todas las medidas de ajuste del modelo. 
ajuste1 <- broom::glance(modelo1)
#Se encuetran los errores cuadráticos medios 
mse1= mean(modelo1$residuals^2)
rtmse1= sqrt(mean(modelo1$residuals^2))
#Se encuetran los errores cuadráticos medios (manual)
mse1m= mean((modelo1$model$y - modelo1$fitted.values)^2)
rtmse1m= sqrt(mean((modelo1$model$y - modelo1$fitted.values)^2))

#Punto D)
##Discusion Peak Ages: Boostrap sobre peak ages

#Sin bootstrap
predict(modelo1, list(X = peakage), interval = "c")

#Con bootstrap v1(paquete boots)
set.seed(428)
library(boot)

#Se dfine función para guardar coeficientes del bootstrap
coef_function <- function(formula, data, indices) {
  d <- data[indices,] 
  fit <-coef(lm(y ~ x + I(x^2), data = d,  weights = w))
  return(fit) 
}

#Se utiliza el paquete boots
reps <- boot(data=datap2, statistic=coef_function, R=2000, formula=y ~ x + I(x^2), weights = w)
#Usar errores estandares para estimar los intervalos de confianza y construir manuealmente los intervalos
x=datap1[, c('age')]
boots <- data.frame(x)
boots$x2 <-boots$x^2

seI=apply(reps$t,2,sd)[1]
seX=apply(reps$t,2,sd)[2]
seX2=apply(reps$t,2,sd)[3]

#I=apply(reps$t,2,mean)[1]
#x=apply(reps$t,2,mean)[2]
#x2=apply(reps$t,2,mean)[3]

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

#Con bootstrap v2(con boot.ci)

lboundsi= boot.ci(reps,type="basic",index=1)$basic[4]
uboundsi=boot.ci(reps,type="basic",index=1)$basic[5]

lboundsx= boot.ci(reps,type="basic",index=2)$basic[4]
uboundsx=boot.ci(reps,type="basic",index=2)$basic[5]

lboundsx2= boot.ci(reps,type="basic",index=3)$basic[4]
uboundsx2=boot.ci(reps,type="basic",index=3)$basic[5]

yhatl2=lboundsi+lboundsx*boots$x+lboundsx2*boots$x2
yhatu2=uboundsi+uboundsx*boots$x+uboundsx2*boots$x2

bootsic <- cbind(bootsic,yhatl2,yhatu2)

#Con bootstrap v3 (de forma manual para x=50)
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

## Se gráfican los valores predichos con los ajustes de pesos ponderados (en base a la versión 1 del bootstrap)
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


# Punto 2 -------------------------------------------------
#Se busca una medida del ingreso salarial que logre capturar dinámicas del mercado laboral, y tenga pocas distorsiones frente
#a por ejemplo, la cantidad de horas que se trabajan

#Modelo y visualización de resultados
w=datap1[, c("fweight")]
y= datap1[, "y1"]
x=datap1[, c("female","age","sqage","agefemale", "sqagefemale")]
datap3 =cbind(y,x,w)
datap3= data.frame(datap3)
skim(datap3)


#Punto A)
modelo2 <- lm("y ~female" , data = datap3, weights= datap3$fweights, x=TRUE ) 
modelo2robust <- commarobust(modelo2)
stargazer(modelo2, se=starprep(modelo2robust))#Para subir a Latex
stargazer(modelo2, se=starprep(modelo2robust), type="text") #En text

# Se encuentran todas las medidas de ajuste del modelo. 
ajuste2 <- broom::glance(modelo2)
ajuste2<-data.frame(t(ajuste2))
stargazer(ajuste1,ajuste2, summary=FALSE,rownames=TRUE,single.row=TRUE,
          align=TRUE) #Para ver en R
#Se encuetran los errores cuadráticos medios 
mse2= mean(modelo2$residuals^2)
rtmse2= sqrt(mean(modelo2$residuals^2))


#Punto B)

#Se adiciona a la especificación del modelo no condicionado las variables de edad
modelo3= lm("y ~ female+age+sqage+agefemale+sqagefemale" , data = datap3, weights= w, x=TRUE)

#Se encuentran los coefficientes y la tabla en latex
modelo3robust <- commarobust(modelo3)
stargazer(modelo3, se =starprep(modelo3robust)) #Para tex
stargazer(modelo3, se =starprep(modelo3robust), type ="text") #Para text

# Se encuentran todas las medidas de ajuste del modelo. 
ajuste3 <- broom::glance(modelo3)
ajuste3<-data.frame(t(ajuste3))
ajustetotal <- cbind(ajuste1,ajuste2,ajuste3)
stargazer(ajustetotal, summary=FALSE,rownames=TRUE,single.row=TRUE,
          align=TRUE, type="text") #Para ver en R
stargazer(ajustetotal, summary=FALSE,rownames=TRUE,single.row=TRUE,
          align=TRUE) #Para Latex
#Se encuetran los errores cuadráticos medios 
mse3= mean(modelo3$residuals^2)
rtmse3= sqrt(mean(modelo3$residuals^2))



##Se grafica solo la linea de ajuste para cada sexo
ggplot(data=datap3 ,aes(x=age,y=y,col=female)) +
  stat_smooth(method = "lm", formula="y ~x+I(x^2)", aes(weight=w)) + 
  # arreglando el eje x
  scale_x_continuous(n.breaks=10, limits=c(18,87)) + 
  # Arreglar el eje y
  scale_y_continuous(n.breaks=8,labels=scales::dollar) + 
  #anadir el titulo, subtitulo y caption
  labs(title= "Relacion entre salario y edad", subtitle = "Evidence for Colombia", caption="Source: GEIH 2008")+  
  # anadir los labels de los ejes
  xlab("Edad") + ylab("Salario") + theme_bw()

#Tengo 3 modelos (Solo edad, género y el que tiene sus interacciones)
#Reporte de coeficientes
stargazer(modelo1, modelo2, modelo3, title="Regression Results", align=TRUE,no.space=TRUE, se=starprep(modelo1_r,modelo2robust,modelo3robust))
#Medidas de ajuste 
ajustetotal <- cbind(ajuste1,ajuste2,ajuste3)
stargazer(ajustetotal, summary=FALSE,rownames=TRUE,single.row=TRUE,
          align=TRUE, type="text") #Para ver en R
stargazer(ajustetotal, summary=FALSE,rownames=TRUE,single.row=TRUE,
          align=TRUE) #Para Latex
# Punto C)
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

#Modelo completo
modelo3c<-lm("y~ -1+.-w", data=datap3c, weights=w)

#Se encuentran los coefficientes y la tabla en latex
modelo3crobust <- commarobust(modelo3c)
stargazer(modelo3c,se=starprep(modelo3crobust),omit=c("oficio*", "relab*","formal"),type="text")
stargazer(modelo3c, omit=c("oficio*", "relab*","formal"))
stargazer(modelo3c,se=starprep(modelo3c))
stargazer(modelo1, modelo2, modelo3, modelo3c, title="Regression Results", align=TRUE,no.space=TRUE,omit=c("oficio*", "relab*","formal"))
summary(modelo3c)

skim(datap1$clase)
skim(datosgeih$clase)


#Modelos parcializados
x=datap1[, c("female","age","sqage","agefemale", "sqagefemale")]
datacontroles=cbind(dummiesfijos,tenure,x,w)

mmodelo4<-lm("y~ -1+.-tenure-w",data=datap3c, weights=w)
mmodelo4b <-lm("tenure~.-w",data=datacontroles, weights=w)


mres_y_a=mmodelo4$residuals
mres_s_a=mmodelo4b$residuals
w_res=datap3c$w
mdb<-data.frame(mres_y_a,mres_s_a,w_res)
modelo4c<-lm(mres_y_a~mres_s_a-1,mdb, weights=w_res)

#Se encuentran los coefficientes y la tabla en latex(se comprueban que ambos tienen los mismos coeficientes)
stargazer(modelo3c,modelo4c,omit=c("oficio*", "relab*","formal"),type="text")
stargazer(modelo3c,modelo4c, omit=c("oficio*", "relab*","formal"))

#Con bootstrap v1 (para errores estandares)
set.seed(428)
library(boot)

#define function to calculate R-squared
res_function <- function(formula, data, indices) {
  d <- data[indices,] #allows boot to select sample
  fit <-coef(lm("mres_y_a~mres_s_a-1", data = d,  weights = w_res))
  return(fit) 
}

reps2 <- boot(data=datap3c, statistic=res_function, R=2000, formula="mres_y_a~mres_s_a-1", weights = w_res)
seRBs=apply(reps2$t,2,sd)[1]
seR=sqrt(diag(vcov(modelo4c)))

stargazer(modelo3c,modelo4c,type="text")
stargazer(modelo3c,modelo4c)



#Punto 4-----------------------------


#Punto A)
set.seed(1)

#Se splitea la muestra
sample <- sample(c(TRUE, FALSE), nrow(datap3c), replace=TRUE, prob=c(0.7,0.3))
train  <- datap3c[sample, ]
test   <- datap3c[!sample, ]

#Punto B)
#Comparación de medidas de ajuste
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



modelo6 <- lm("log(y)~ -1.-res_y_a-res_y_a-w", data=train, weights= w)
test$modelo6<-predict(modelo6,newdata = test)
rmse<-with(test,sqrt(mean((y-modelo6)^2)))
rmsemodelos[6,] <- c(6,rmse)

modelo7 <- lm("y~age+I(age^2)+I(age^3)+female+formalinformal+(age+I(age^2)+I(age^3)):female:formalinformal ", data=train, weights= w)
test$modelo7<-predict(modelo7,newdata = test)
rmse<-with(test,sqrt(mean((y-modelo7)^2)))
rmsemodelos[7,] <- c(7,rmse)


modelo8 <- lm("log(y)~age+I(age^2)+I(age^3)+female+(age+I(age^2)+I(age^3)):female ", data=train, weights= w)
test$modelo8<-predict(modelo8,newdata = test)
rmse<-with(test,sqrt(mean((y-modelo8)^2)))
rmsemodelos[8,] <- c(8,rmse)


modelo9 <- lm("y~age+I(age^2)+I(age^3)+female+formalinformal+(age+I(age^2)+I(age^3)):female:formalinformal:tenure ", data=train, weights= w)
test$modelo9<-predict(modelo9,newdata = test)
rmse<-with(test,sqrt(mean((y-modelo9)^2)))
rmsemodelos[9,] <- c(9,rmse)


relabprivado=unlist(data.frame(train[81]))
train=cbind(train,relabprivado)

modelo10 <- lm("y~tenure+I(tenure^2)+I(tenure^3)+age+I(age^2)+I(age^3)+female+formalinformal+(age+I(age^2)+I(age^3)+tenure+I(tenure^2)+I(tenure^3)):female:formalinformal", data=train, weights= w)
test$modelo10<-predict(modelo10,newdata = test)
rmse<-with(test,sqrt(mean((y-modelo10)^2)))
rmsemodelos[10,] <- c(10,rmse)


#Punto C)
#Se hace la metodología de LOOCV

N=2
modelos <- numeric(N)
pred <- numeric(N)
rmsemodelos2 <- data.frame(modelos, pred)
names(rmsemodelos2) <- c("modelo", "rmse")


loocvrmse1 <- NULL
for(i in 1:nrow(datap3c)){
  #you did this part right
  testcv<-datap3c[i,]
  traincv<-datap3c[-i,]
  modelo4<-lm("y ~ -1+.-w", data=traincv, weights= w)
  testcv$modelo4<-predict(modelo4,newdata = testcv)
  rmse<-with(testcv,sqrt((y-modelo4)^2))
  loocvrmse1[i]<-rmse}
  
rmsemodelos2[1,] <- c(1,mean(loocvrmse1))

modelo4b<-lm("y~ -1+.-tenure-w",data=datap3c, weights=w)
modelo4c <-lm("tenure~.-w-y",data=datap3c, weights=w)

res_y_a=modelo4b$residuals
res_s_a=modelo4c$residuals

datap3c<-cbind(datap3c,res_y_a,res_s_a)

loocvrmse2 <- NULL
for(i in 1:nrow(datap3c)){
  #you did this part right
  testcv<-datap3c[i,]
  traincv<-datap3c[-i,]
  modelo5<-lm("res_y_a~res_s_a-1", data=traincv, weights= w)
  testcv$modelo5<-predict(modelo5,newdata = testcv)
  rmse<-with(testcv,sqrt((y-modelo5)^2))
  loocvrmse2[i]<-rmse}
rmsemodelos2[2,] <- c(2,mean(loocvrmse2))



