# Comparando porcentaje de votación por el PRI y MORENA entre 2017 y 2023
# y competitividad electoral del PRI versus MORENA en 2023 en la elección 
# para la Gubernatura en Coahuila

# Fijando directorio de trabajo
setwd("D:/rijim/Desktop/PEL 2023 COAH COMPUTOS FINALES")

# Preparando para leer archivos .xlsx y limpiar datos
library(readxl)
library(tidyverse)

# Leyendo los datos de 2017
Gob_per_muni_17 <- read_excel("Gob_per_muni_17.xlsx")

# Añadiendo un sufijo para denotar que datos son del '17
colnames(Gob_per_muni_17) <- paste(colnames(Gob_per_muni_17),"17",sep="_")

# Separando datos de Saltillo y Torreón pues estan distribuidos por dist local
# y los necesitamos por municipio

slw_17 <- Gob_per_muni_17 %>% filter(grepl("SALTILLO", AYUNTAMIENTO_17))
trc_17 <- Gob_per_muni_17 %>% filter(grepl("TORREÓN", AYUNTAMIENTO_17))

# Limpiando y sumando datos de Saltillo y Torreón y añadiendo al df de '17
slw_17$AYUNTAMIENTO_17 = "SALTILLO"
trc_17$AYUNTAMIENTO_17 = "TORREÓN"
SALTILLO <- aggregate(. ~ AYUNTAMIENTO_17, data = slw_17, FUN = sum)
TORREON <- aggregate(. ~ AYUNTAMIENTO_17, data = trc_17, FUN = sum)
Gub17 <- rbind(Gob_per_muni_17, SALTILLO, TORREON)
Gub17 <- Gub17[!grepl("DISTRITO", Gub17$AYUNTAMIENTO_17),]

# Leyendo los datos de 2023
library(readr)
Gub23 <- read_csv("05_COAHUILA2/05/per_muni.csv")

# Añadiendo sufijo para marcar que datos son de '23
colnames(Gub23) <- paste(colnames(Gub23),"23",sep="_")

# Uniendo ambos dfs
mergedData <- merge(Gub17, Gub23, by.x = c("AYUNTAMIENTO_17"), 
                    by.y = c("nom_mun_23"))

# Dejando una columna innecesaria (número de Municipio)
mergedData <- mergedData[ , !names(mergedData) %in% c("Num_mun_23")]

# El PRI fue en coaliciones en ambas elecciones, pero nos interesa más el voto
# duro, esto es, el voto sólo por el partido.

# Determinando porcentajes de la votación y diferencias en puntos porcentuales
mergedData$pct_PRI_17 <- (mergedData$PRI_17/mergedData$TOTAL_17)*100
mergedData$pct_PRI_23 <- (mergedData$PRI_23/mergedData$`TOTAL VOTOS_23`)*100
mergedData$pct_MORENA_17 <- (mergedData$MORENA_17/mergedData$TOTAL_17)*100
mergedData$pct_MORENA_23 <- (mergedData$MORENA_23/mergedData$`TOTAL VOTOS_23`)*100
mergedData$diff_PRI <- (mergedData$pct_PRI_23-mergedData$pct_PRI_17)
mergedData$diff_MORENA <- (mergedData$pct_MORENA_23-mergedData$pct_MORENA_17)
mergedData$compete23 <- (mergedData$pct_PRI_23-mergedData$pct_MORENA_23)

# Preparando para hacer mapas con shapefile
library(sf)

# Leyendo shapefile de municipios del INE-INEGI, ECEG 2021, descargado
# previamente
setwd("D:/rijim/Desktop/PEL 2023 COAH COMPUTOS FINALES/05_COAHUILA2/05")
muni_outline <- st_read("MUNICIPIO.shp")

# Uniendo shp como df y df electoral
# Añadiendo ID en df electoral para la unión
mergedData <- mergedData %>% mutate(id = row_number())
muni_merge <- muni_outline %>% left_join(mergedData, by = c("MUNICIPIO" = "id"))

# Haciendo breaks manuales para mapear competitividad en 2023
br <- c(0,5,10,15,20,100)
labs <- br
muni_mut <- mutate(muni_merge, compete_cat = cut(compete23, breaks = br, 
                                                 dig.lab=5))
# Mapa de competitividad electoral 2023
# con una paleta de colores para color blindness
cbPal <- c("#F0E442", "#009E73", "#56B4E9", "#E69F00", "#999999")
ggplot(muni_mut) + geom_sf(aes(fill=compete_cat)) + 
  labs(title = "Diferencia entre % votos PRI - MORENA", 
       subtitle = "Elección por la Gubernatura de Coahuila, 2023", 
       caption = "Por Israel Jimenez. Datos de los cómputos finales, 
       Instituto Electoral de Coahuila.") + 
  scale_fill_manual(values = cbPal, name = "Dif. en puntos porcentuales") + 
  theme_void() + theme(plot.caption.position = "plot")

# Mapa de comparación PRI 2017 a 2023
ggplot(muni_merge) + geom_sf(aes(fill=diff_PRI)) + 
  theme_void() + scale_fill_distiller(palette = "Spectral") + 
  labs(title = "Diferencia entre % votos por el PRI (2017-2023)", 
       subtitle = "Elecciones por la Gubernatura de Coahuila", 
       caption = "Por Israel Jiménez. Datos de los cómputos finales, 
       Instituto Electoral de Coahuila.") + 
  theme(plot.caption.position = "plot") + 
  labs(fill = "Dif. en puntos\nporcentuales")

# Mapa de comparación MORENA 2017 a 2023
ggplot(muni_merge) + geom_sf(aes(fill=diff_MORENA)) + 
  theme_void() + scale_fill_distiller(palette = "Spectral") + 
  labs(title = "Diferencia entre % votos por MORENA (2017-2023)", 
       subtitle = "Elecciones por la Gubernatura de Coahuila", 
       caption = "Por Israel Jiménez. Datos de los cómputos finales, 
       Instituto Electoral de Coahuila.") + 
  theme(plot.caption.position = "plot") + 
  labs(fill = "Dif. en puntos\nporcentuales")

# Exportando datos para continuar análisis si interesa
write.csv(mergedData, file = 'computos_gubernatura_1723.csv')
