# BDML_ProblemSet1
 En este repositorio se encuentra la solución del Problem Set 1 de Big Data y Machine Learning for Applied Economics.

 Grupo: [Daniel Lasso](https://github.com/daniell419), [Matteo Rozo](https://github.com/MatteoRozo) y [Gabriela Mejía](https://github.com/gabimbec99).
 
 Profesor: [Ignacio Sarmiento](https://github.com/ignaciomsarmiento)

 Dentro de este repositorio se encuentra:
 
 El documento pdf describiendo las respuestas completas al Taller 1 del curso de BDML Fall 2022, el cual fue trabajado en el siguiente enlace de overleaf:(https://www.overleaf.com/8567919536rcvkfqcvpyvg)
 ## Data Files

En el archivo datosgeih.RData, se podrá encontrar la base scrappeado de la página web:https://ignaciomsarmiento.github.io/GEIH2018_sample/, por el cual se extrajó la información GEIH en su versión del año 2018, como también los labels asocidados con cada variable, que se pueden encontrar en el mismo.

A partir de este primer paso, se dio continuación al ejercicio de limpieza de la base datos, el cual consistio en la elección de nuestras variables de intéres, el anejo de sus valores missings o faltantes acompañado de un análisis de la distribución de los mismos. Para ello, se trabajó en una misma hoja de trabajo bajo el nombre de Scrapping.R.

 ## Code Files 
 
 En el archivo Scrapping.R, se pueden encontrar 4 grandes secciones, correspondientes a cada punto del Problem Set 1 Big Data y Machine Learning for Applied Economics, los cuales son:
 - Data Scrapping and Data Cleaning
 Consiste en los procesos de limpieza y selección de las variables de la base scrappeado del GEIH del año 2018. Así mismo, se incluyen una serie de gráficos y tablas descriptivas de las mismas.
 
 - Age-earnings profiles
 Tiene como objetivo ser una primera aproximación hacia los modelos de predicción de ingresos a través de la estimación de un modelo de regresión linear a partir del polinomio de segundo grado de la edad. También, se exploran las implicaciones de los resultados encontrados en términos de sus medidas de ajuste y se discute el peak age, siendo aquella edad que se alcanza un pico de ganancias según la literatura económica.
 
 - The gender earnings GAP
  
  Estudia las desigualdades en ingresos que pueden existir entre hombres y mujeres como una variable explicativa de la predicción de los ingresos en base a ciertas características de los individuos y en términos generales. En razón de ello, se implementan varias especificaciones de modelos lineales que buscan estudiar este fenómeno y cómo entenderlo aporta a la predicción de los ingresos. Como en la sección anterior, todavía se hace una discusión sobre el peak ages.
 
 - Predicting earnings
  Prueba el poder predictivo de cada uno de los modelos usando como función de pérdida la raíz de los errores medios al cuadrado aplicando diversas técnicaas de validación, con lo cual se buscará encontrar y analizar aquellos con mayor poder predictivo.
 

