# Este script lee una base de datos en excel de los refugios en el estado
# de nayarit. La salida es un archivo png conteniendo un mapa con los refugios
# y la ubucación de una persona y los 3 centros mas ceercanos a ella.

# librerias utilizadas.
# install.packages("readxl")
# install.packages("sp")
# install.packages("geosphere")

setwd("datalab/Evaluacion/")
source("helpers.R") # Carga funciones utiles para la carga y procesamiento de 
                    # de la base.

# Establecemos la ubicación de la persona, favor de ingresar las coordenadas en 
# formato decimal
person <- c(-104.70, 22.30)
names(person) <- c("lon", "lat")

# Configura el ambiente para que se use el encoding utf-8, ya que el script
# lo corrí en MAC OS el capitan con idioma inglés.
Sys.setlocale("LC_ALL", 'en_US.UTF-8')

# Función para la carga de los datos desde el libro de excel
# usando el paquete readxl
library(readxl)

filename <- "refugios_nayarit.xlsx" # Se define el nombre del archivo de la base
db <- read_excel_allsheets(filename)

# Se hace un merge de las listas en la lista db.
db = Reduce(function(...) merge(..., all=T), db)

# Creación de un vector con el nombre de las columnas de la base
nombres <- c("no" , "refugio", "municipio", "direccion", "uso.del.inmueble", "servicios", 
             "capacidad.de.personas", "latitud.n", "longitud.w", "altitud.msnm", "responsable",
             "telefono")

names(db) <- nombres

### Limpiando la base
# Eliminamos los registros que se hayan importado pero que no tengan "No."
db <- db[!is.na(db$no),]

# Elimanos aquellos datos que no tengan latitud ni longitud
db <- db[!is.na(db$latitud.n),]
db <- db[!is.na(db$longitud.w),]

# Creamos nuevas variables con la latitud y longitud en formato ddºmm'ss\"
# limpiando los caracteres distintos al formato. Añadimos 
db$lon = clean_chars(db$longitud.w)
db$lat = clean_chars(db$latitud.n)

db$lon = paste(db$lon, "W")
db$lat = paste(db$lat, "N")

# Conversión de las variables anteriores a formato decimal
db$lat = dms2dec(db$lat)
db$lon = dms2dec(db$lon)

db <- db[!is.na(db$lat),]
db <- db[!is.na(db$lon),]


# Acotamos la región de estudio de acuerdo a: http://www.nayarit.gob.mx/estado/
# Las coordenadas extremas son:
# Al norte 23º 05' y al sur 20º 36' de latitud norte.
# Al este 103º 43', al oeste 105º46' de longitud oeste.
extremos <- c("23º05'00\" N", "20º36'00\" N", "103º43'00\" W", "105º46'00\" W")
extremos <- dms2dec(extremos)
names(extremos) <- c("norte", "sur", "este", "oeste")

db <- db[(db$lat >= extremos["sur"] & db$lat <= extremos["norte"]), ]
db <- db[(db$lon >= extremos["oeste"] & db$lon <= extremos["este"]), ]

# Calculo de la distancia de la persona a los diferentes centros.
# Se hace uso del packete geosphere y en especial de la función
library(geosphere)

# La función distm genera una matriz con la distancia entre puntos
# es por eso que unimos la locación de la persona y los puntos de 
# acopio 
puntos <- rbind(person, db[, c("lon", "lat")])

distancias <-  as.data.frame(distm(puntos, fun = distHaversine))
distancias <- distancias[,"V1"] # Distancia de la persona a todos los otros puntos
distancias <- distancias[-1] # Eliminamos la distancia de la persona con persona

distancias<- as.data.frame(cbind(distancia = distancias, 
                                 lon = db$lon, lat = db$lat,
                                 no = db$no))
# Ordenamos las distancias y seleccionamos las primeras 3
distancias <- distancias[order(distancias$distancia),]
distancias_cercanas <- distancias[1:3,]
puntos_cercanos <-  db[db$no %in% distancias_cercanas$no,]
puntos_cercanos <- merge(puntos_cercanos, distancias_cercanas)

# Creamos un dataframe con la información de los 3 refugios mas cercanos
centros_cercanos <- puntos_cercanos[,c("refugio", "municipio", "direccion", "distancia")]
centros_cercanos$distancia <- round(centros_cercanos$distancia/1000,2)
centros_cercanos <- centros_cercanos[with(centros_cercanos, order(distancia)),]
names(centros_cercanos) <- c("Refugio", "Municipio", "Direccion", "Distancia(km)")

# Visualización de los puntos obtenidos en el mapa
library(sp)
# Se obtuvo de la base de datos de áreas administrativas globales “GADM” en:
# http://www.gadm.org/
ub_mexico = "MEX_adm2.rds"

mexico <- readRDS(ub_mexico)

nayarit <- mexico[mexico$NAME_1=="Nayarit",]

# Se imprime un mapa en un archivo png, comentando las lineas
# 117 y 128 se obtiene el mapa directo en R
png("mapa.png")

plot(nayarit, main="Refugios más cercanos en el estado de Nayarit")
size_cex <- 1.0
points(db$lon, db$lat, col="blue", pch=18, cex = 0.7)
points(person["lon"], person["lat"], col="red", pch=18)
points(distancias_cercanas$lon, distancias_cercanas$lat, col="green", pch=18, cex = size_cex)

legend("topleft", c("Refugios","Ubicacion Persona","Refugios Cercanos"),
                                        pch=c(18, 18, 18),
                                        col=c("blue", "red","green"), cex=0.7)
dev.off()

# Se imprime la informaciòn del dataframe centros cercanos a un archivo txt
sink("refugios_cercanos.txt")
print(centros_cercanos)
sink()
