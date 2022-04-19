
## **************************************************************************
## Procesar datos para app
## **************************************************************************

library(tidyverse)
library(readxl)

rm(list = ls())


## Leer data cruda ----------------------------------------------------------

# Metadata Economía
metadata_eco <- read_excel(
  "data-raw/Comparado_ec_060921.xlsx",
  col_types = c("numeric", "text", "text", "text",
                "text", "text", "text", "text",
                "text", "text", "text", "text",
                "text", "date", "text")) %>% 
  janitor::clean_names() %>% 
  mutate(tipo = "eco") %>%
  mutate(key = paste(tipo, codind, sep = "_"))

glimpse(metadata_eco)

# Metadata Social
metadata_soc <- readxl::read_excel(
  "data-raw/Comparado_soc_070422.xlsx",
  col_types = c("numeric", "text", "text", "text",
                "text", "text", "text", "text",
                "text", "text", "text", "date",
                "text")) %>% 
  janitor::clean_names() %>% 
  mutate(tipo = "soc") %>%
  mutate(key = paste(tipo, codind, sep = "_"))
  
glimpse(metadata_soc)

# Pestañas de metadata social 
tabs_soc <- readxl::read_excel(
  "data-raw/Dimensiones_Economia-Social_842022.xlsx",
  sheet = 3,
  col_types = c("numeric", "text", "text", "text", "text")) %>% 
  janitor::clean_names() %>%
  rename(p1 = d1,
         p2 = d2) %>% 
  select(codind, p1, p2)

metadata_soc <- metadata_soc %>% 
  left_join(tabs_soc)

# Unir metadatas
metadata <- plyr::rbind.fill(metadata_eco, metadata_soc) %>% 
  select(-nomindicador, -codind, -tipo)

glimpse(metadata)


# Data Economía
data_eco <- readxl::read_excel(
  "data-raw/Comparado_ec_060921.xlsx",
  col_types = c("numeric", "text", "text", "numeric", "numeric", "text",
                "text", "text", "text", "text", "text", "text"),
  sheet = 2) %>% 
  mutate(tipo = "eco") %>% 
  janitor::clean_names() %>%
  mutate(key = paste(tipo, codind, sep = "_"))

glimpse(data_eco)

# Data Social
data_soc <- readxl::read_excel(
  "data-raw/Comparado_soc_070422.xlsx",
  col_types = c("numeric", "text", "text", "numeric", "numeric", "text",
                "text", "text", "text", "text", "text", "text",
                "text", "text", "text", "text", "text", "text"),
  sheet = 2) %>% 
  mutate(tipo = "soc") %>% 
  janitor::clean_names() %>%
  mutate(key = paste(tipo, codind, sep = "_"))

glimpse(data_soc)

# Unir data
data <- plyr::rbind.fill(data_eco, data_soc)

glimpse(data)


## Procesar  ---------------------------------------------------------------

# Recodificar valores
data <- data %>% 
  left_join(metadata, by = "key") %>% 
  filter(!is.na(valor)) %>% 
  mutate(valor_original = valor) %>% 
  mutate(valor = ifelse(valor  <= 1, round(valor, digits = 3),
                        ifelse(valor >= 1 & valor < 100, round(valor, digits = 2),
                               ifelse(valor >= 100  & valor < 1000, round(valor, digits = 1),
                                      ifelse(valor >= 1000, round(valor, digits = 0), NA))))) %>% 
  mutate(valor_round = ifelse(valor  <= 1, round(valor, digits = 2),
                              ifelse(valor >= 1 & valor < 100, round(valor, digits = 0),
                                     ifelse(valor >= 100  & valor < 1000, round(valor, digits = 0),
                                            ifelse(valor >= 1000, round(valor, digits = 0), NA))))) %>% 
  relocate(key, nomindicador, fecha, valor) 

regiones <- c("África Sub Sahariana",
              "Altos ingresos", 
              "América Latina y el Caribe",
              "Asia del Este y Pacífico",
              "Asia del Sur",
              "Bajos ingresos",
              "Etapa avanzada del dividendo demográfico",
              "Etapa inicial del dividendo demográfico",
              "Etapa previa al dividendo demográfico",
              "Etapa posterior al dividendo demográfico",
              "Ingresos medios", 
              "Medio Oriente y África del Norte",
              "Unión Europea",
              "Norteamérica")

data <- data %>% 
  mutate(region = case_when(
    pais %in% regiones ~ 1,
    grepl("Etapa", pais) ~ 1, 
    TRUE ~ 0
  )) %>% 
  mutate(pais_region = case_when(
    region == 1 ~ "Regiones",
    region == 0 ~ "Países"
  ))

# table(data$region, data$pais_region)
# table(data$pais, data$region)

# Agregar códigos
codigos <- readxl::read_excel("data/codigos_iso.xlsx")

data <- data %>% 
  left_join(codigos, by = "pais")

glimpse(data)

# Countries in english
# Recodificar países para matchear con world_spf
data <- data %>% 
  mutate(pais_eng = case_when(
    pais == "Brasil" ~ "Brazil",
    pais == "Corea del Sur" ~ "Korea, Republic of",
    pais == "Dinamarca" ~ "Denmark",
    pais == "Eslovaquia" ~ "Slovakia",
    pais == "Eslovenia" ~ "Slovenia",
    pais == "España" ~ "Spain",
    pais == "Estados Unidos" ~ "United States",
    pais == "Filipinas" ~ "Philippines",
    pais == "Finlandia" ~ "Finland",
    pais == "Grecia" ~ "Greece",
    pais == "Hungría" ~ "Hungary",
    pais == "Irlanda" ~ "Ireland",
    pais == "Italia" ~ "Italy",
    pais == "Letonia" ~ "Latvia",
    pais == "Lituania" ~ "Lithuania",
    pais == "Malasia" ~ "Malaysia",
    pais == "México" ~ "Mexico",
    pais == "Nueva Zelanda" ~ "New Zealand",
    pais == "Panamá" ~ "Panama",
    pais == "Perú" ~ "Peru",
    pais == "República Checa" ~ "Czech Republic",
    pais == "República Dominicana" ~ "Dominican Republic",
    pais == "Singapur" ~ "Singapore",
    pais == "Tailandia" ~ "Thailand",
    pais == "Vietnam" ~ "Viet Nam",
    TRUE ~ pais
  ))

# data <- data %>% 
#   mutate(fuente = "Fuente")

## Guardar  ----------------------------------------------------------------

rio::export(data, 'data.rda')
load('data.rda')
data <- tibble::as_tibble(x)
save(data, file = here::here("data", "data.rda"))
file.remove('data.rda')

