
library(leaflet)
library(tidyverse)
library(rgdal)

rm(list = ls())

# Read this shape file with the rgdal library. 
world_spdf <- readOGR( 
  dsn = "data" , 
  layer = "TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

# Data
load("data/data.rda")

# Filtro data
data <- data %>% 
  filter(fecha == 2014) %>% 
  filter(nomindicador == "Tasa de fertilidad en adolescentes (nacimientos por cada 1.000 mujeres entre 15 y 19 años de edad), según WDI") 
  
# Left join al world_spdf
world_spdf@data <- world_spdf@data %>% 
  left_join(select(data, pais_eng, valor_original), by = c("NAME" = "pais_eng"))

# # Clean the data object
# world_spdf@data$POP2005[ which(world_spdf@data$POP2005 == 0)] = NA
# world_spdf@data$POP2005 <- as.numeric(as.character(world_spdf@data$POP2005)) / 1000000 %>% round(2)

# Library
range(world_spdf@data$valor_original)


# Create a color palette with handmade bins.
min_val <- plyr::round_any(min(world_spdf@data$valor_original, na.rm = T), 10, floor)
max_val <- plyr::round_any(max(world_spdf@data$valor_original, na.rm = T), 10, ceiling)
dif <- max_val - min_val
dif_4 <- dif / 4
val_1 <- min_val + dif_4
val_2 <- min_val + (dif_4 * 2)
val_3 <- min_val + (dif_4 * 3)

mybins <- c(min_val, val_1, val_2, val_3, max_val)
mypalette <- colorBin(palette = "YlOrBr", 
                      domain = world_spdf@data$valor_original, 
                      na.color = "transparent", 
                      bins = mybins)

# Prepare the text for tooltips:
mytext <- paste(
  "País: ", world_spdf@data$NAME,"<br/>", 
  "Valor: ", round(world_spdf@data$valor_original, 2), 
  sep="") %>%
  lapply(htmltools::HTML)

# Final Map
leaflet(world_spdf) %>% 
  addTiles()  %>% 
  setView( lat=30, lng=0 , zoom=1.2) %>%
  addPolygons( 
    fillColor = ~mypalette(valor_original), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, values=~valor_original, opacity=0.9,
             title = "Nombre del indicador", position = "bottomleft" )

