install.packages("readr")

library(readr)

Datos <- read_csv("https://www.datos.gov.co/api/views/gt2j-8ykr/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
View(Datos)

#Setwd("~/COVID-19.Rdata")

## Importar librerias ##

install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(readxl)

# Ver los nombres de las variables #
names(Datos)
# Ver los valores unicos de las variables #
unique(Datos$Edad)

######### ORGANIZAR FECHAS CON LIBRERIA DE LUBRIDATE #########
# cambiar la clase del formato a la variable fecha #
Datos$`fecha reporte web`<- as.Date(Datos$`fecha reporte web`, format = "%d/%m/%Y")
Datos$`Fecha de inicio de síntomas` <- as.Date(Datos$`Fecha de inicio de síntomas`, format = "%d/%m/%Y")
Datos$`Fecha de notificación` <- as.Date(Datos$`Fecha de notificación`, format = "%d/%m/%Y")
Datos$`Fecha de diagnóstico` <- as.Date(Datos$`Fecha de diagnóstico`, format = "%d/%m/%Y")
Datos$`Fecha de muerte` <- as.Date(Datos$`Fecha de muerte`, format = "%d/%m/%Y")
Datos$`Fecha de recuperación` <- as.Date(Datos$`Fecha de recuperación`, format = "%d/%m/%Y")

# Linea para ver la clase #
class(Datos$`Fecha de notificación`)
# organizar las fechas en año, mes y dia #
ymd(Datos$`Fecha de notificación`)
# Linea para ver fechas min y max #
inicio <- min(Datos$`Fecha de notificación`)
final <- max(Datos$`Fecha de notificación`)
# Linea para ver duración en tiempo de las fechas #
duracion <- interval(inicio, final)
as.period(duracion, unit = "years")

############## LIMPIAR LA BASE DE DATOS ############

Datos <- Datos %>%
  mutate(Sexo = str_to_upper(Sexo),
         `Nombre departamento`= str_to_upper(`Nombre departamento`), `Ubicación del caso` = str_to_upper(`Ubicación del caso`),
         `Tipo de contagio` = str_to_upper(`Tipo de contagio`),
         `Tipo de recuperación` = str_to_upper(`Tipo de recuperación`),
         `Estado` = str_to_upper(`Estado`),
         `Recuperado` = str_to_upper(Recuperado)) %>%
  mutate(`Código DIVIPOLA departamento` = str_pad(`Código DIVIPOLA departamento`, width = 2, side = "left", pad = "0"), `Código DIVIPOLA municipio` = str_pad(`Código DIVIPOLA municipio`, "5", side = "left", pad = "0"))

unique(Datos$`Código DIVIPOLA municipio`)

# Resume las variables del dataframe #
summary(Datos)

########### TOTAL COLOMBIA ############

### Se crea el objeto llamado confirmados y en ella se agrupan las fechas y el numero de caso por día ### Importante en summarise casos es el objeto en el cual se va a almacenar el dato o la columna.
Confirmados<- Datos %>%
  group_by(`Fecha de notificación`) %>%
  summarise(casos = n ())

### Crear un grafico de linea ### en este la primera entrada corresponde a las x y el otro al eje y

plot(Confirmados$`Fecha de notificación`, Confirmados$casos, type = "l")

hist(Confirmados$casos)

### importar un archivo de excel ###

Poblacion <- read_excel("~/Downloads/poblacion.xlsx")
View(Poblacion)

### unir información entre bases de datos ###

municipio <- Datos %>%
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

Bucaramangacovid <- Datos %>%
  filter(str_detect(`Nombre municipio`, "BUCARA")) %>%
  group_by(`Fecha de notificación`) %>%
  summarise(casos = n())

## Cuando se requiere filtrar por varios patrones se emplea | ## área metropolitana

AMcovid <- Datos %>%
  filter(str_detect(`Nombre municipio`, "BUCARA|FLORIDABL|GIRON|PIEDECUESTA")) %>%
  group_by(`Fecha de notificación`) %>%
  summarise(casos = n())

## Cuando se requiere filtrar por varios patrones se emplea | ## Santander

Santandercovid <- Datos %>%
  filter(str_detect(`Código DIVIPOLA departamento`, "68")) %>%
  group_by(`Fecha de notificación`) %>%
  summarise(casos = n())

# rm(x,y) se usa para borrar del enviroment los datasets creados

#rm(Confirmados, AMcovid, Bucaramangacovid, COVID_19_Colombia)

# ejemplo de un filtro usando & #
etareo <- Datos %>%
  filter(Edad == 40 & Sexo == "M")

########### GRAFICOS ############

#install.packages("ggrepel")
library(ggrepel)

Confirmados1 <- Datos %>%
  group_by(Sexo) %>%
  summarise(casos = n()) %>%
  arrange(desc(casos)) %>%
  mutate(lab.ypos= cumsum(casos)- 0.5*casos)

mycols <- c("magenta", "gray")

install.packages("ggplot2")
library(ggplot2)

ggplot(Confirmados1, aes(x = "Sexo", y = casos, fill=Sexo)) + 
  geom_bar(stat = "identity", width = 10, color = "Black") +
  coord_polar("y", start = 0) + 
  geom_label_repel(aes(y = lab.ypos, label = casos, label.padding = 0.05)) +
  title("Grafico por sexo") + 
  scale_fill_manual(values = mycols) +
  theme_void() + 
  theme(legend.position = "bottom")

# Instalar y cargar el paquete ggrepel
install.packages("ggrepel")
library(ggrepel)

# Convertir la variable 'Sexo' a factor
Confirmados1$Sexo <- factor(Confirmados1$Sexo)

# Crear el gráfico
ggplot(Confirmados1, aes(x = Sexo, y = casos, fill = Sexo)) + 
  geom_bar(stat = "identity", width = 1, color = "yellow") +
  coord_polar(theta = "y", start = 0) + 
  geom_label_repel(aes(y = lab.ypos, label = casos, label.padding = 0.05 )) +
  labs(title = "Gráfico por sexo") + 
  scale_fill_manual(values = mycols) +
  theme_void() + 
  theme(legend.position = "bottom")

### boxplot ###

Sexo <- Datos %>%
  filter(Estado == "FALLECIDO" & Estado != "N/A") %>%
  group_by (Sexo, Edad) %>%
  summarise(casos = n())

# == coincidencia exacta, != diferente, %in% filtrar lo que incluya un vector #

ggplot(Sexo, aes(Sexo, Edad, color = Sexo)) + 
         geom_boxplot() +
         geom_jitter (alpha = 1)

# la función aes es para la parte estetica, esta es su estructura aes(x = variable_x, y = variable_y, color = variable_color, size = variable_size, shape = variable_shape, ...)

# geom_jitter(width = valor, height = valor, alpha = valor) para los graficos de dispersion"

#width: Especifica el ancho del jitter en el eje x. Un valor mayor dispersará más los puntos horizontalmente.
#height: Especifica la altura del jitter en el eje y. Un valor mayor dispersará más los puntos verticalmente.
#alpha: Especifica la transparencia de los puntos. Un valor cercano a 1 los hace más opacos, mientras que un valor cercano a 0 los hace más transparentes.

## Grafico por sexo y grupo etareo ##

Edad <- Datos %>%
  group_by(Edad, Sexo) %>%
  count () %>%
  spread(., key = "Sexo", value = "n", fill = Sexo)

ggplot(Datos, aes(Edad, fill = Sexo)) +
  geom_bar() +
  labs(title = "Distribución por edad y Sexo")

### graficar por edad y sexo santander ###


library(dplyr)
library(ggplot2)
  
# Filtrar los datos para SANTANDER, FALLECIDO y no N/A
Sexo2 <- Datos %>%
    filter(str_detect(`Nombre departamento`, "SANTANDER"), Estado == "FALLECIDO" & Estado != "N/A") %>%
  group_by(Sexo, Edad) %>%
  summarise(casos = n())
  
# Contar la frecuencia de edad y sexo
Edad <- Datos %>%
  group_by(Edad, Sexo) %>%
  count() %>%
  spread(key = Sexo, value = n, fill = 0)
  
# Gráfico de barras
ggplot(Edad, aes(x = Edad, y = n, fill = Sexo)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribución por edad y Sexo - SANTANDER")