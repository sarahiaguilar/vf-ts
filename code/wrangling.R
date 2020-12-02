library(dt_incidencia.table)
library(lubridate)



### Cifras de Incidencia Delictiva Estatal, 2015 - octubre 2020
# Cargar datos
dt_incidencia <- fread("./data/IDEFC_NM_oct2020.csv")

# Reemplazar espacios en nombres de columnas
names(dt_incidencia) <- gsub(" ", "_", names(dt_incidencia))

# Filtrar tipo de crimen
dt_incidencia <- dt_incidencia[Tipo_de_delito == "Violencia familiar", ]

# Melt data.table
dt_incidencia <- melt.data.table(dt_incidencia, id.vars=c("Anio", "Entidad"), measure.vars=months)

# Renombrar columnas
setnames(dt_incidencia, old=names(dt_incidencia), new=c("anio", "entidad", "mes", "incidencia"))

# Calcular incidencia nacional
dt_incidencia_nacional <- dt_incidencia[, .(incidencia = sum(incidencia)), by=.(anio, mes)]

# Añadir incidencia nacional a data.table general de incidencia 
dt_incidencia_nacional[, entidad := "República Mexicana"]
dt_incidencia <- rbind(dt_incidencia, dt_incidencia_nacional)



### Proyecciones de la Población de México y de las Entidades Federativas, 2016-2050 
# Cargar datos
dt_poblacion <- fread("./data/pob_mit_proyecciones.csv")

# Reemplazar espacios en nombres de columnas
names(dt_poblacion) <- gsub(" ", "_", names(dt_poblacion))

# Filtrar para 2018, 2019 y 2020
dt_poblacion <- dt_poblacion[ANIO >= 2015 & ANIO <= 2020, ]

# Sumar cifras de todas las edades y dos sexos por año 
dt_poblacion <- dt_poblacion[, .(poblacion = sum(POBLACION)), by=.(ANIO, ENTIDAD)]

# Renombrar columnas
setnames(dt_poblacion, old=names(dt_poblacion), new=c("anio", "entidad", "poblacion"))



# Merge
dt <- merge(dt_incidencia, dt_poblacion, by=c("anio", "entidad"), all.x=TRUE)

# Calcular presuntos delitos de violencia familiar por cada 100 mil habitantes
dt[, tasa_incidencia := incidencia/(poblacion/1000)*100] 

# Añadir columna de fecha en formato correcto (último día de cada mes)
dt$fecha <- ymd(paste(dt$anio, dt$mes, 1, sep="-"))+months(1)-days(1)


