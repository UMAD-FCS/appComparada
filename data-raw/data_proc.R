
## **************************************************************************
## Procesar datos para app
## **************************************************************************

library(tidyverse)
library(readxl)

rm(list = ls())


## Leer data cruda ----------------------------------------------------------

# Metadata Economía
metadata_eco <- read_excel(
  "data-raw/Base comparada_ Economía_270621.xlsx",
  col_types = c("numeric", "text", "text", "text",
                "text", "text", "text", "text")) %>% 
  janitor::clean_names() %>% 
  mutate(tipo = "eco") %>%
  mutate(key = paste(tipo, codind, sep = "_"))

glimpse(metadata_eco)

# Metadata Social
metadata_soc <- readxl::read_excel(
  "data-raw/Base comparada_Social_270621.xlsx",
  col_types = c("numeric", "text", "text", "text",
                "text", "text", "text", "text")) %>% 
  janitor::clean_names() %>% 
  mutate(tipo = "soc") %>%
  mutate(key = paste(tipo, codind, sep = "_"))
  

glimpse(metadata_soc)

metadata <- rbind(metadata_eco, metadata_soc) %>% 
  select(-nomindicador, -codind, -tipo)

glimpse(metadata)


# Data Economía
data_eco <- readxl::read_excel(
  "data-raw/Base comparada_ Economía_270621.xlsx",
  col_types = c("numeric", "text", "text", "numeric", "numeric", "text"),
  sheet = 2) %>% 
  mutate(tipo = "eco") %>% 
  janitor::clean_names() %>%
  mutate(key = paste(tipo, codind, sep = "_"))

glimpse(data_eco)

# Data Social
data_soc <- readxl::read_excel(
  "data-raw/Base comparada_Social_270621.xlsx",
  col_types = c("numeric", "text", "text", "numeric", "numeric", "text"),
  sheet = 2) %>% 
  mutate(tipo = "soc") %>% 
  janitor::clean_names() %>%
  mutate(key = paste(tipo, codind, sep = "_"))

glimpse(data_soc)

# Unir data
data <- rbind(data_eco, data_soc)

glimpse(data)


## Procesar  ---------------------------------------------------------------

data <- data %>% 
  left_join(metadata, by = "key") %>% 
  filter(!is.na(valor)) %>% 
  mutate(valor_original = valor) %>% 
  mutate(valor = ifelse(valor  < 1, round(valor, digits = 3),
                        ifelse(valor > 1 & valor < 100, round(valor, digits = 2),
                               ifelse(valor > 100  & valor < 1000, round(valor, digits = 1),
                                      ifelse(valor > 1000, round(valor, digits = 0), NA))))) %>% 
  relocate(key, nomindicador, fecha, valor)

glimpse(data)
        
        
## Guardar  ----------------------------------------------------------------

rio::export(data, 'data.rda')
load('data.rda')
data <- tibble::as_tibble(x)
save(data, file = here::here("data", "data.rda"))
file.remove('data.rda')






