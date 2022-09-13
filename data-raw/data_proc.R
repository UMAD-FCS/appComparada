
## **************************************************************************
## Procesar datos para app
## **************************************************************************

library(tidyverse)
library(readxl)

rm(list = ls())


## Leer data cruda ----------------------------------------------------------

# Metadata Economía
metadata_eco <- read_excel(
  "data-raw/Comparado_ec_21072022.xlsx",
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
  "data-raw/Comparado_soc_31082022.xlsx",
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
  "data-raw/Comparado_ec_21072022.xlsx",
  col_types = c("numeric", "text", "text", "numeric", "numeric", "text",
                "text", "text", "text", "text", "text", "text"),
  sheet = 2) %>% 
  mutate(tipo = "eco") %>% 
  janitor::clean_names() %>%
  mutate(key = paste(tipo, codind, sep = "_"))

glimpse(data_eco)

# Data Social
data_soc <- readxl::read_excel(
  "data-raw/Comparado_soc_31082022.xlsx",
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

test <- count(data, pais)

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
  relocate(key, nomindicador, fecha, valor) %>% 
  mutate(pais = case_when(
    pais == "Early-demographic dividend" ~ "Etapa inicial del dividendo demográfico",
    pais == "East Asia & Pacific" ~ "Asia del Este y Pacífico",
    pais == "European Union" ~ "Unión Europea",
    pais == "High income" ~ "Altos ingresos",
    pais == "Late-demographic dividend" ~ "Etapa avanzada del dividendo demográfico",
    pais == "Latin America & Caribbean" ~ "América Latina y el Caribe",
    pais == "Middle East & North Africa" ~ "Medio Oriente y África del Norte",
    pais == "Middle income" ~ "Ingresos medios",
    pais == "North America" ~ "Norteamérica",
    pais == "Post-demographic dividend" ~ "Etapa posterior al dividendo demográfico",
    pais == "Pre-demographic dividend" ~ "Etapa previa al dividendo demográfico",
    pais == "South Asia" ~ "Asia del Sur",
    TRUE ~ pais
  ))

regiones <- c("Etapa inicial del dividendo demográfico",
              "Asia del Este y Pacífico",
              "Unión Europea",
              "Altos ingresos",
              "Etapa avanzada del dividendo demográfico",
              "América Latina y el Caribe",
              "Medio Oriente y África del Norte",
              "Ingresos medios",
              "Norteamérica",
              "Africa subsahariana",
              "Etapa posterior al dividendo demográfico",
              "Etapa previa al dividendo demográfico",
              "Asia del Sur")

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

codigos_eng <- read_csv("data-raw/wikipedia-iso-country-codes.csv") %>% 
  janitor::clean_names() %>% 
  rename(pais = english_short_name_lower_case,
         cod_pais = alpha_3_code) %>% 
  select(pais, cod_pais)

codigos_eng2 <- read_csv("data-raw/iso_codes.csv") %>% 
  janitor::clean_names() %>% 
  rename(pais = country_name,
         cod_pais = iso_alpha_3_code) %>% 
  select(pais, cod_pais)

codigos_final <- rbind(codigos, codigos_eng, codigos_eng2) %>% 
  distinct(.keep_all = T)

data <- data %>% 
  left_join(codigos_final, by = "pais")

glimpse(data)

# Agregar códigos faltantes
data <- data %>% 
  mutate(cod_pais = case_when(
    pais == "Early-demographic dividend" ~ "EID",
    pais =="East Asia & Pacific" ~ "AEP",
    pais == "European Union" ~ "UER",
    pais == "High income" ~ "AIG",
    pais == "Hong Kong SAR, China" ~ "HKG",
    pais == "Korea, Rep." ~ "KOR",
    pais == "Late-demographic dividend" ~ "EAD",
    pais == "Latin America & Caribbean" ~ "LCN",
    pais == "Middle East & North Africa" ~ "MEA",
    pais == "Middle income" ~ "MIG",
    pais == "North America" ~ "NRA",
    pais == "Post-demographic dividend" ~ "EPD",
    pais == "Pre-demographic dividend" ~ "ERD",
    pais == "Slovak Republic" ~ "SVK",
    pais == "South Asia" ~ "ADS",
    pais == "Sub-Saharan Africa"  ~ "ASS",
    pais == "United States" ~ "USA",
    pais == "Venezuela, RB" ~ "VEN",
    TRUE ~ cod_pais
  ))

# test <- count(data, pais, cod_pais)

# Countries in english
# Recodificar países para matchear con world_spf
data <- data %>% 
  mutate(pais_eng = case_when(
    pais == "África Sub Sahariana" ~ "Sub-Saharan Africa",
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
    pais == "Venezuela, RB" ~ "Venezuela",
    TRUE ~ pais
  ))

table(data$pais)

data <- data %>% 
  mutate(pais = fct_relevel(pais, levels = c("Etapa previa al dividendo demográfico",
                              "Etapa inicial del dividendo demográfico",
                              "Etapa avanzada del dividendo demográfico",
                              "Etapa posterior al dividendo demográfico",
                              "Bajos ingresos",
                              "Ingresos medios",
                              "Altos ingresos",
                              "América Latina y el Caribe",
                              "Asia del Este y Pacífico",
                              "Asia del Sur",
                              "Medio Oriente y África del Norte",
                              "Norteamérica",
                              "Unión Europea")))   
  
  table(data$pais)

# test <- count(data, pais_eng)

# data <- data %>% 
#   mutate(fuente = "Fuente")

## Guardar  ----------------------------------------------------------------

rio::export(data, 'data.rda')
load('data.rda')
data <- tibble::as_tibble(x)
save(data, file = here::here("data", "data.rda"))
file.remove('data.rda')

