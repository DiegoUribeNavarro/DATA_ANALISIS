### INICIO DEL PROYECTO ###

Setwd("~/COVID-19.Rdata")

## Importar librerias ##

install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(readxl)

# Ver los nombres de las variables #
names(COVID_19_Colombia)
# Ver los valores unicos de las variables #
unique(COVID_19_Colombia$Edad)

############## ORGANIZAR FECHAS CON LIBRERIA DE LUBRIDATE ##############
# cambiar la clase del formato a la variable fecha #
COVID_19_Colombia$`fecha reporte web`<- as.Date(COVID_19_Colombia$`fecha reporte web`, format = "%d/%m/%Y")
COVID_19_Colombia$`Fecha de inicio de síntomas` <- as.Date(COVID_19_Colombia$`Fecha de inicio de síntomas`, format = "%d/%m/%Y")
COVID_19_Colombia$`Fecha de notificación` <- as.Date(COVID_19_Colombia$`Fecha de notificación`, format = "%d/%m/%Y")
COVID_19_Colombia$`Fecha de diagnóstico` <- as.Date(COVID_19_Colombia$`Fecha de diagnóstico`, format = "%d/%m/%Y")
COVID_19_Colombia$`Fecha de muerte` <- as.Date(COVID_19_Colombia$`Fecha de muerte`, format = "%d/%m/%Y")
COVID_19_Colombia$`Fecha de recuperación` <- as.Date(COVID_19_Colombia$`Fecha de recuperación`, format = "%d/%m/%Y")


# Linea para ver la clase #
class(COVID_19_Colombia$`Fecha de notificación`)
# organizar las fechas en año, mes y dia #
ymd(COVID_19_Colombia$`Fecha de notificación`)
# Linea para ver fechas min y max #
inicio <- min(COVID_19_Colombia$`Fecha de notificación`)
final <- max(COVID_19_Colombia$`Fecha de notificación`)
# Linea para ver duración en tiempo de las fechas #
duracion <- interval(inicio, final)
as.period(duracion, unit = "years")


############## LIMPIAR LA BASE DE DATOS ############

COVID_19_Colombia <- COVID_19_Colombia %>%
  mutate(Sexo = str_to_upper(Sexo),
         `Nombre departamento`= str_to_upper(`Nombre departamento`), `Ubicación del caso` = str_to_upper(`Ubicación del caso`),
         `Tipo de contagio` = str_to_upper(`Tipo de contagio`),
         `Tipo de recuperación` = str_to_upper(`Tipo de recuperación`),
         `Estado` = str_to_upper(`Estado`),
         `Recuperado` = str_to_upper(Recuperado))
         
COVID_19_Colombia <- COVID_19_Colombia %>%
  mutate(`Código DIVIPOLA departamento` = str_pad(`Código DIVIPOLA departamento`, width = 2, side = "left", pad = "0"), `Código DIVIPOLA municipio` = str_pad(`Código DIVIPOLA municipio`, "5", side = "left", pad = "0"))
   
unique(COVID_19_Colombia$`Código DIVIPOLA municipio`)

# Resume las variables del dataframe #
summary(COVID_19_Colombia)

########### TOTAL COLOMBIA ############

### Se crea el objeto llamado confirmados y en ella se agrupan las fechas y el numero de caso por día ### Importante en summarise casos es el objeto en el cual se va a almacenar el dato o la columna.
Confirmados<- COVID_19_Colombia %>%
  group_by(`Fecha de notificación`) %>%
  summarise(casos = n ())

### Crear un grafico de linea ### en este la primera entrada corresponde a las x y el otro al eje y

plot(Confirmados$`Fecha de notificación`, Confirmados$casos, type = "l")

hist(Confirmados$casos)

### importar un archivo de excel ###

Poblacion <- read_excel("~/Downloads/poblacion.xlsx")
View(Poblacion)

### unir información entre bases de datos ###

municipio <- COVID_19_Colombia %>%
  group_by(`Nombre municipio`, `Código DIVIPOLA municipio`) %>%
  count() %>%
  filter(n> 50000) %>%
  arrange(desc(n)) %>%
  left_join(., Poblacion, by= c("Código DIVIPOLA municipio" = "CÓDIGO MUNICIPIO"))%>%
  mutate(peso = round(n/`POBLACIÓN TOTAL`,2))
  
plot(municipio$n, municipio$`POBLACIÓN TOTAL`)
cor(municipio$n, municipio$`POBLACIÓN TOTAL`)

#################### TOTAL BUCARAMANGA #######################

## estas lineas me hacen una tabla con los casos para Bucaramanga por fecha ##

Bucaramangacovid <- COVID_19_Colombia %>%
  filter(str_detect(`Nombre municipio`, "BUCARA")) %>%
  group_by(`Fecha de notificación`) %>%
  summarise(casos = n())

## Cuando se requiere filtrar por varios patrones se emplea | ## área metropolitana

AMcovid <- COVID_19_Colombia %>%
  filter(str_detect(`Nombre municipio`, "BUCARA|FLORIDABL|GIRON|PIEDECUESTA")) %>%
  group_by(`Fecha de notificación`) %>%
  summarise(casos = n())

## Cuando se requiere filtrar por varios patrones se emplea | ## Santander

Santandercovid <- COVID_19_Colombia %>%
  filter(str_detect(`Código DIVIPOLA departamento`, "68")) %>%
  group_by(`Fecha de notificación`) %>%
  summarise(casos = n())

# rm(x,y) se usa para borrar del enviroment los datasets creados



  
