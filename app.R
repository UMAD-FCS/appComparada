

## **************************************************************************
## Shiny APP Comparada (Economía & social) -
## Unidad de Métodos y Acceso a Datos
## **************************************************************************

##  0. DATA Y PAQUETES  =====================================================

library(DT)
library(shinythemes)
library(here)
library(plotly)
library(shinyWidgets)
library(stringr)
library(scales)
library(viridis)
library(ggrepel)
library(tidyverse)
library(shinycssloaders)
library(bslib)
library(rgdal)
library(maps)
library(leaflet)
library(RColorBrewer)


rm(list = ls())

source("utils.R")

theme_set(theme_bdd(base_size = 12))
update_geom_defaults("text", list(family = theme_get()$text$family))

dir.create('~/.fonts')
file.copy("www/Titillium Web.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')

# Spinner options
options(
  spinner.color = "#476481",
  spinner.color.background = "#ffffff",
  spinner.size = 2
)

# Shiny app customized theme
tema_umad <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#085792",
  primary = "#3E6C9A",
  sCP_compndary = "#3E6C9A",
  base_font = font_google("Roboto"),
  heading_font = font_google("Roboto"),
  code_font = font_google("Space Mono")
)

# bs_theme_preview(tema_umad)

##  1. PREPARAR DATA  =======================================================

load("data/data.rda")

data <- data %>% 
  relocate(sector_productivo, area_geografica, .after = everything()) %>% 
  mutate(p2 = case_when(
    p2 == "Crecimento" ~ "Crecimiento",
    TRUE ~ p2
  )) %>% 
  mutate(nomindicador = case_when(is.na(nomindicador) ~ "-",
                                  TRUE ~ nomindicador),
         definicion = case_when(is.na(definicion) ~ "-",
                                  TRUE ~ definicion),
         metodo_de_agregacion = case_when(is.na(metodo_de_agregacion) ~ "-",
                                  TRUE ~ metodo_de_agregacion),
         concepto_estadistico_y_metodologia = case_when(is.na(concepto_estadistico_y_metodologia) ~ "-",
                                  TRUE ~ concepto_estadistico_y_metodologia),
         limitaciones_y_excepciones = case_when(is.na(limitaciones_y_excepciones) ~ "-",
                                                        TRUE ~ limitaciones_y_excepciones),
         relevancia = case_when(is.na(relevancia) ~ "-",
                                  TRUE ~ relevancia))

library(RColorBrewer)
paleta_expandida <- c(brewer.pal(8, "Dark2"), "#B76A16", "#75A61A", "#D9318E",
                      "#986A74", "#C14D6A", "#C1632B", "#698446", "#7B6BB0",
                      "#A9A80F", "#DEAA03")

# Indicadores con variables de corte
vars_corte <- data %>% 
  filter(!is.na(sector_productivo) | !is.na(area_geografica)) %>% 
  distinct(nomindicador) %>% 
  pull(nomindicador)
  

##  2. USER INTERFACE  ======================================================

ui <- navbarPage(
  title = "Datos Comparados",
  theme = tema_umad,
  collapsible = TRUE,
  fluid = TRUE,
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  tabPanel(
    
    # * 2.1. Competitividad y precios  -------------------------------------
    
    title = "Competitividad y precios",
    icon = icon("dollar-sign"),
    
    tabsetPanel(
      type = "pills",
      id   = "CP",
      
      
      tabPanel(
        "Competitividad",
        br(),
        
        fluidRow(
          sidebarPanel(
            width = 3,
            # style = "position:fixed;width:22%;",
            
            selectInput(
              inputId = "visualizador_CP_comp",
              label = "Visualización",
              choices = c("Serie de tiempo",
                          "Anual gráfico",
                          "Anual mapa"),
              selected = "Serie de tiempo"
            ),
            
            selectInput(
              inputId = "indicador_CP_comp",
              label = "Indicador",
              choices = data %>%
                filter(str_detect(p2, "Competitividad")) %>%
                distinct(nomindicador) %>% 
                arrange(nomindicador) %>% 
                pull(nomindicador)
            ),
            
            uiOutput("corte_CP_comp"),
            
            uiOutput("fecha_dat_CP_comp"),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_CP_comp != 'Anual mapa'",
                             uiOutput("sel_CP_comp_pais")),
            
            br(),
            
            uiOutput("sel_CP_comp_region"),
            
            br(),
            
            tags$a(
              href = "https://umad.cienciassociales.edu.uy/",
              "Unidad de Métodos y Acceso a Datos",
              style = "font-size:12px; color:Navy;
                   text-decoration:underline;"
            ),
            br(),
            br(),
            img(
              src = "logo_umad.png",
              height = "70%",
              width = "70%",
              align = "left"
            ),
            style = "display:inline-block;",
            
          ),
          
          mainPanel(
            tags$h3(style = "display:inline-block",
                    uiOutput("title_dat_CP_comp")),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("calculator", lib = "font-awesome"),
                uiOutput("info_dat_CP_comp")
              )
            ),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("exclamation", lib = "font-awesome"),
                uiOutput("rel_dat_CP_comp")
              )
            ),
            
            tags$h5(uiOutput("subtitle_dat_CP_comp")),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_CP_comp != 'Anual mapa'",
                             withSpinner(
                               plotOutput("p_dat_CP_comp", height = "600px"),
                               type = 2
                             )),
            
            conditionalPanel(condition = "input.visualizador_CP_comp == 'Anual mapa'",
                             withSpinner(leafletOutput("map_CP_comp"),
                                         type = 2)),
            
            conditionalPanel(
              condition = "input.visualizador_CP_comp != 'Anual mapa'",
              downloadButton(outputId = "baja_plot_CP_comp",
                             label = "Descarga el gráfico")
            ),
            br(),
            br(),
            withSpinner(DTOutput("tab_dat_CP_comp"),
                        type = 2),
            br(),
            downloadButton("dl_tabla_dat_CP_comp", "Descarga la tabla"),
            br(),
            br()
          )
        )
      ),
      
      tabPanel(
        "Precios",
        br(),
        
        fluidRow(
          sidebarPanel(
            width = 3,
            
            selectInput(
              inputId = "visualizador_CP_precios",
              label = "Visualización",
              choices = c("Serie de tiempo",
                          "Anual gráfico",
                          "Anual mapa"),
              selected = "Serie de tiempo"
            ),
            
            selectInput(
              inputId = "indicador_CP_precios",
              label = "Indicador",
              choices = data %>%
                filter(str_detect(p2, "Precios")) %>%
                distinct(nomindicador) %>% 
                arrange(nomindicador) %>% 
                pull(nomindicador)
            ),
            
            uiOutput("corte_CP_precios"),
            
            uiOutput("fecha_dat_CP_precios"),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_CP_precios != 'Anual mapa'",
                             uiOutput("sel_CP_precios_pais")),
            
            br(),
            
            uiOutput("sel_CP_precios_region"),
            
            br(),
            
            tags$a(
              href = "https://umad.cienciassociales.edu.uy/",
              "Unidad de Métodos y Acceso a Datos",
              style = "font-size:12px; color:Navy;
                   text-decoration:underline;"
            ),
            br(),
            br(),
            img(
              src = "logo_umad.png",
              height = "70%",
              width = "70%",
              align = "left"
            ),
            style = "display:inline-block;",
            
          ),
          
          mainPanel(
            tags$h3(style = "display:inline-block",
                    uiOutput("title_dat_CP_precios")),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("calculator", lib = "font-awesome"),
                uiOutput("info_dat_CP_precios")
              )
            ),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("exclamation", lib = "font-awesome"),
                uiOutput("rel_dat_CP_precios")
              )
            ),
            
            tags$h5(uiOutput("subtitle_dat_CP_precios")),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_CP_precios != 'Anual mapa'",
                             withSpinner(
                               plotOutput("p_dat_CP_precios", height = "600px"),
                               type = 2
                             )),
            
            conditionalPanel(condition = "input.visualizador_CP_precios == 'Anual mapa'",
                             withSpinner(leafletOutput("map_CP_precios"),
                                         type = 2)),
            
            conditionalPanel(
              condition = "input.visualizador_CP_precios != 'Anual mapa'",
              downloadButton(outputId = "baja_plot_CP_precios",
                             label = "Descarga el gráfico")
            ),
            br(),
            br(),
            withSpinner(DTOutput("tab_dat_CP_precios"),
                        type = 2),
            br(),
            downloadButton("dl_tabla_dat_CP_precios", "Descarga la tabla"),
            br(),
            br()
          )
        )
      )
    ),
  ),
  
  tabPanel(
    
    # * 2.2. Desarrollo Económico  -----------------------------------------
    
    title = "Desarrollo Económico",
    icon = icon("signal"),
    
    tabsetPanel(
      type = "pills",
      id   = "CP",
      
      
      tabPanel(
        "Crecimento",

        br(),
        
        fluidRow(
          sidebarPanel(
            width = 3,
            
            selectInput(
              inputId = "visualizador_DE_crec",
              label = "Visualización",
              choices = c("Serie de tiempo",
                          "Anual gráfico",
                          "Anual mapa"),
              selected = "Serie de tiempo"
            ),
            
            selectInput(
              inputId = "indicador_DE_crec",
              label = "Indicador",
              choices = data %>%
                filter(str_detect(p2, "Crecimiento")) %>%
                distinct(nomindicador) %>% 
                arrange(nomindicador) %>% 
                pull(nomindicador)
            ),
            
            uiOutput("corte_DE_crec"),
            
            uiOutput("fecha_dat_DE_crec"),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_DE_crec != 'Anual mapa'",
                             uiOutput("sel_DE_crec_pais")),
            
            br(),
            
            uiOutput("sel_DE_crec_region"),
            
            br(),
            
            tags$a(
              href = "https://umad.cienciassociales.edu.uy/",
              "Unidad de Métodos y Acceso a Datos",
              style = "font-size:12px; color:Navy;
                   text-decoration:underline;"
            ),
            br(),
            br(),
            img(
              src = "logo_umad.png",
              height = "70%",
              width = "70%",
              align = "left"
            ),
            style = "display:inline-block;",

          ),

          mainPanel(
            tags$h3(style = "display:inline-block",
                    uiOutput("title_dat_DE_crec")),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("calculator", lib = "font-awesome"),
                uiOutput("info_dat_DE_crec")
              )
            ),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("exclamation", lib = "font-awesome"),
                uiOutput("rel_dat_DE_crec")
              )
            ),
            
            tags$h5(uiOutput("subtitle_dat_DE_crec")),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_DE_crec != 'Anual mapa'",
                             withSpinner(
                               plotOutput("p_dat_DE_crec", height = "600px"),
                               type = 2
                             )),
            
            conditionalPanel(condition = "input.visualizador_DE_crec == 'Anual mapa'",
                             withSpinner(leafletOutput("map_DE_crec"),
                                         type = 2)),
            
            conditionalPanel(
              condition = "input.visualizador_DE_crec != 'Anual mapa'",
              downloadButton(outputId = "baja_plot_DE_crec",
                             label = "Descarga el gráfico")
            ),
            br(),
            br(),
            withSpinner(DTOutput("tab_dat_DE_crec"),
                        type = 2),
            br(),
            downloadButton("dl_tabla_dat_DE_crec", "Descarga la tabla"),
            br(),
            br()
          )
        )
      ),
      
      
      tabPanel(
        "Productividad",

        br(),
        
        fluidRow(
          sidebarPanel(
            width = 3,
            
            selectInput(
              inputId = "visualizador_DE_prod",
              label = "Visualización",
              choices = c("Serie de tiempo",
                          "Anual gráfico",
                          "Anual mapa"),
              selected = "Serie de tiempo"
            ),
            
            selectInput(
              inputId = "indicador_DE_prod",
              label = "Indicador",
              choices = data %>%
                filter(str_detect(p2, "Productividad")) %>%
                distinct(nomindicador) %>% 
                arrange(nomindicador) %>% 
                pull(nomindicador)
            ),
            
            uiOutput("corte_DE_prod"),
            
            uiOutput("fecha_dat_DE_prod"),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_DE_prod != 'Anual mapa'",
                             uiOutput("sel_DE_prod_pais")),
            
            br(),
            
            uiOutput("sel_DE_prod_region"),
            
            br(),
            
            tags$a(
              href = "https://umad.cienciassociales.edu.uy/",
              "Unidad de Métodos y Acceso a Datos",
              style = "font-size:12px; color:Navy;
                   text-decoration:underline;"
            ),
            br(),
            br(),
            img(
              src = "logo_umad.png",
              height = "70%",
              width = "70%",
              align = "left"
            ),
            style = "display:inline-block;",
            
          ),
          
          mainPanel(
            tags$h3(style = "display:inline-block",
                    uiOutput("title_dat_DE_prod")),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("calculator", lib = "font-awesome"),
                uiOutput("info_dat_DE_prod")
              )
            ),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("exclamation", lib = "font-awesome"),
                uiOutput("rel_dat_DE_prod")
              )
            ),
            
            tags$h5(uiOutput("subtitle_dat_DE_prod")),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_DE_prod != 'Anual mapa'",
                             withSpinner(
                               plotOutput("p_dat_DE_prod", height = "600px"),
                               type = 2
                             )),
            
            conditionalPanel(condition = "input.visualizador_DE_prod == 'Anual mapa'",
                             withSpinner(leafletOutput("map_DE_prod"),
                                         type = 2)),
            
            conditionalPanel(
              condition = "input.visualizador_DE_prod != 'Anual mapa'",
              downloadButton(outputId = "baja_plot_DE_prod",
                             label = "Descarga el gráfico")
            ),
            br(),
            br(),
            withSpinner(DTOutput("tab_dat_DE_prod"),
                        type = 2),
            br(),
            downloadButton("dl_tabla_dat_DE_prod", "Descarga la tabla"),
            br(),
            br()
          )
        )
      ),
      
      
      
      tabPanel(
        "Estructura productiva",

        br(),
        
        fluidRow(
          sidebarPanel(
            width = 3,
            
            selectInput(
              inputId = "visualizador_DE_estr",
              label = "Visualización",
              choices = c("Serie de tiempo",
                          "Anual gráfico",
                          "Anual mapa"),
              selected = "Serie de tiempo"
            ),
            
            selectInput(
              inputId = "indicador_DE_estr",
              label = "Indicador",
              choices = data %>%
                filter(str_detect(p2, "Estructura productiva")) %>%
                distinct(nomindicador) %>% 
                arrange(nomindicador) %>% 
                pull(nomindicador)
            ),
            
            uiOutput("corte_DE_estr"),
            
            uiOutput("fecha_dat_DE_estr"),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_DE_estr != 'Anual mapa'",
                             uiOutput("sel_DE_estr_pais")),
            
            br(),
            
            uiOutput("sel_DE_estr_region"),
            
            br(),
            
            
            tags$a(
              href = "https://umad.cienciassociales.edu.uy/",
              "Unidad de Métodos y Acceso a Datos",
              style = "font-size:12px; color:Navy;
                   text-decoration:underline;"
            ),
            br(),
            br(),
            img(
              src = "logo_umad.png",
              height = "70%",
              width = "70%",
              align = "left"
            ),
            style = "display:inline-block;",
            
          ),
          
          mainPanel(
            tags$h3(style = "display:inline-block",
                    uiOutput("title_dat_DE_estr")),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("calculator", lib = "font-awesome"),
                uiOutput("info_dat_DE_estr")
              )
            ),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("exclamation", lib = "font-awesome"),
                uiOutput("rel_dat_DE_estr")
              )
            ),
            
            tags$h5(uiOutput("subtitle_dat_DE_estr")),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_DE_estr != 'Anual mapa'",
                             withSpinner(
                               plotOutput("p_dat_DE_estr", height = "600px"),
                               type = 2
                             )),
            
            conditionalPanel(condition = "input.visualizador_DE_estr == 'Anual mapa'",
                             withSpinner(leafletOutput("map_DE_estr"),
                                         type = 2)),
            
            conditionalPanel(
              condition = "input.visualizador_DE_estr != 'Anual mapa'",
              downloadButton(outputId = "baja_plot_DE_estr",
                             label = "Descarga el gráfico")
            ),
            br(),
            br(),
            withSpinner(DTOutput("tab_dat_DE_estr"),
                        type = 2),
            br(),
            downloadButton("dl_tabla_dat_DE_estr", "Descarga la tabla"),
            br(),
            br()
          )
        )
      ),
      
      
      
      tabPanel(
        "Inversión",

        br(),
        
        fluidRow(
          sidebarPanel(
            width = 3,
            
            selectInput(
              inputId = "visualizador_DE_inv",
              label = "Visualización",
              choices = c("Serie de tiempo",
                          "Anual gráfico",
                          "Anual mapa"),
              selected = "Serie de tiempo"
            ),
            
            selectInput(
              inputId = "indicador_DE_inv",
              label = "Indicador",
              choices = data %>%
                filter(str_detect(p2, "Inversión")) %>%
                distinct(nomindicador) %>% 
                arrange(nomindicador) %>% 
                pull(nomindicador)
            ),
            
            uiOutput("corte_DE_inv"),
            
            uiOutput("fecha_dat_DE_inv"),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_DE_inv != 'Anual mapa'",
                             uiOutput("sel_DE_inv_pais")),
            
            br(),
            
            uiOutput("sel_DE_inv_region"),
            
            br(),
            
            tags$a(
              href = "https://umad.cienciassociales.edu.uy/",
              "Unidad de Métodos y Acceso a Datos",
              style = "font-size:12px; color:Navy;
                   text-decoration:underline;"
            ),
            br(),
            br(),
            img(
              src = "logo_umad.png",
              height = "70%",
              width = "70%",
              align = "left"
            ),
            style = "display:inline-block;",
            
          ),
          
          mainPanel(
            tags$h3(style = "display:inline-block",
                    uiOutput("title_dat_DE_inv")),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("calculator", lib = "font-awesome"),
                uiOutput("info_dat_DE_inv")
              )
            ),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("exclamation", lib = "font-awesome"),
                uiOutput("rel_dat_DE_inv")
              )
            ),
            
            tags$h5(uiOutput("subtitle_dat_DE_inv")),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_DE_inv != 'Anual mapa'",
                             withSpinner(
                               plotOutput("p_dat_DE_inv", height = "600px"),
                               type = 2
                             )),
            
            conditionalPanel(condition = "input.visualizador_DE_inv == 'Anual mapa'",
                             withSpinner(leafletOutput("map_DE_inv"),
                                         type = 2)),
            
            conditionalPanel(
              condition = "input.visualizador_DE_inv != 'Anual mapa'",
              downloadButton(outputId = "baja_plot_DE_inv",
                             label = "Descarga el gráfico")
            ),
            br(),
            br(),
            withSpinner(DTOutput("tab_dat_DE_inv"),
                        type = 2),
            br(),
            downloadButton("dl_tabla_dat_DE_inv", "Descarga la tabla"),
            br(),
            br()
          )
        )
      ),
      
      tabPanel(
        "Capital humano",

        br(),
        
        fluidRow(
          sidebarPanel(
            width = 3,
            
            selectInput(
              inputId = "visualizador_DE_ch",
              label = "Visualización",
              choices = c("Serie de tiempo",
                          "Anual gráfico",
                          "Anual mapa"),
              selected = "Serie de tiempo"
            ),
            
            selectInput(
              inputId = "indicador_DE_ch",
              label = "Indicador",
              choices = data %>%
                filter(str_detect(p2, "Capital humano")) %>%
                distinct(nomindicador) %>% 
                arrange(nomindicador) %>% 
                pull(nomindicador)
            ),
            
            uiOutput("corte_DE_ch"),
            
            uiOutput("fecha_dat_DE_ch"),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_DE_ch != 'Anual mapa'",
                             uiOutput("sel_DE_ch_pais")),
            
            br(),
            
            uiOutput("sel_DE_ch_region"),
            
            br(),
            
            
            tags$a(
              href = "https://umad.cienciassociales.edu.uy/",
              "Unidad de Métodos y Acceso a Datos",
              style = "font-size:12px; color:Navy;
                   text-decoration:underline;"
            ),
            br(),
            br(),
            img(
              src = "logo_umad.png",
              height = "70%",
              width = "70%",
              align = "left"
            ),
            style = "display:inline-block;",
            
          ),
          
          mainPanel(
            tags$h3(style = "display:inline-block",
                    uiOutput("title_dat_DE_ch")),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("calculator", lib = "font-awesome"),
                uiOutput("info_dat_DE_ch")
              )
            ),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("exclamation", lib = "font-awesome"),
                uiOutput("rel_dat_DE_ch")
              )
            ),
            
            tags$h5(uiOutput("subtitle_dat_DE_ch")),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_DE_ch != 'Anual mapa'",
                             withSpinner(
                               plotOutput("p_dat_DE_ch", height = "600px"),
                               type = 2
                             )),
            
            conditionalPanel(condition = "input.visualizador_DE_ch == 'Anual mapa'",
                             withSpinner(leafletOutput("map_DE_ch"),
                                         type = 2)),
            
            conditionalPanel(
              condition = "input.visualizador_DE_ch != 'Anual mapa'",
              downloadButton(outputId = "baja_plot_DE_ch",
                             label = "Descarga el gráfico")
            ),
            br(),
            br(),
            withSpinner(DTOutput("tab_dat_DE_ch"),
                        type = 2),
            br(),
            downloadButton("dl_tabla_dat_DE_ch", "Descarga la tabla"),
            br(),
            br()
          )
        )
      ),
      
      tabPanel(
        "CTI",

        br(),
        
        fluidRow(
          sidebarPanel(
            width = 3,
            
            selectInput(
              inputId = "visualizador_DE_cti",
              label = "Visualización",
              choices = c("Serie de tiempo",
                          "Anual gráfico",
                          "Anual mapa"),
              selected = "Serie de tiempo"
            ),
            
            selectInput(
              inputId = "indicador_DE_cti",
              label = "Indicador",
              choices = data %>%
                filter(str_detect(p2, "CTI")) %>%
                distinct(nomindicador) %>% 
                arrange(nomindicador) %>% 
                pull(nomindicador)
            ),
            
            uiOutput("corte_DE_cti"),
            
            uiOutput("fecha_dat_DE_cti"),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_DE_cti != 'Anual mapa'",
                             uiOutput("sel_DE_cti_pais")),
            
            br(),
            
            uiOutput("sel_DE_cti_region"),
            
            br(),
            
            tags$a(
              href = "https://umad.cienciassociales.edu.uy/",
              "Unidad de Métodos y Acceso a Datos",
              style = "font-size:12px; color:Navy;
                   text-decoration:underline;"
            ),
            br(),
            br(),
            img(
              src = "logo_umad.png",
              height = "70%",
              width = "70%",
              align = "left"
            ),
            style = "display:inline-block;",
            
          ),
          
          mainPanel(
            tags$h3(style = "display:inline-block",
                    uiOutput("title_dat_DE_cti")),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("calculator", lib = "font-awesome"),
                uiOutput("info_dat_DE_cti")
              )
            ),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("exclamation", lib = "font-awesome"),
                uiOutput("rel_dat_DE_cti")
              )
            ),
            
            tags$h5(uiOutput("subtitle_dat_DE_cti")),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_DE_cti != 'Anual mapa'",
                             withSpinner(
                               plotOutput("p_dat_DE_cti", height = "600px"),
                               type = 2
                             )),
            
            conditionalPanel(condition = "input.visualizador_DE_cti == 'Anual mapa'",
                             withSpinner(leafletOutput("map_DE_cti"),
                                         type = 2)),
            
            conditionalPanel(
              condition = "input.visualizador_DE_cti != 'Anual mapa'",
              downloadButton(outputId = "baja_plot_DE_cti",
                             label = "Descarga el gráfico")
            ),
            br(),
            br(),
            withSpinner(DTOutput("tab_dat_DE_cti"),
                        type = 2),
            br(),
            downloadButton("dl_tabla_dat_DE_cti", "Descarga la tabla"),
            br(),
            br()
          )
        )
      ),
      
      tabPanel(
        "Infraestructura",

        br(),
        
        fluidRow(
          sidebarPanel(
            width = 3,
            
            selectInput(
              inputId = "visualizador_DE_infra",
              label = "Visualización",
              choices = c("Serie de tiempo",
                          "Anual gráfico",
                          "Anual mapa"),
              selected = "Serie de tiempo"
            ),
            
            selectInput(
              inputId = "indicador_DE_infra",
              label = "Indicador",
              choices = data %>%
                filter(str_detect(p2, "Infraestructura")) %>%
                distinct(nomindicador) %>% 
                arrange(nomindicador) %>% 
                pull(nomindicador)
            ),
            
            uiOutput("corte_DE_infra"),
            
            uiOutput("fecha_dat_DE_infra"),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_DE_infra != 'Anual mapa'",
                             uiOutput("sel_DE_infra_pais")),
            
            br(),
            
            uiOutput("sel_DE_infra_region"),
            
            br(),
            
            
            tags$a(
              href = "https://umad.cienciassociales.edu.uy/",
              "Unidad de Métodos y Acceso a Datos",
              style = "font-size:12px; color:Navy;
                   text-decoration:underline;"
            ),
            br(),
            br(),
            img(
              src = "logo_umad.png",
              height = "70%",
              width = "70%",
              align = "left"
            ),
            style = "display:inline-block;",
            
          ),
          
          mainPanel(
            tags$h3(style = "display:inline-block",
                    uiOutput("title_dat_DE_infra")),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("calculator", lib = "font-awesome"),
                uiOutput("info_dat_DE_infra")
              )
            ),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("exclamation", lib = "font-awesome"),
                uiOutput("rel_dat_DE_infra")
              )
            ),
            
            tags$h5(uiOutput("subtitle_dat_DE_infra")),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_DE_infra != 'Anual mapa'",
                             withSpinner(
                               plotOutput("p_dat_DE_infra", height = "600px"),
                               type = 2
                             )),
            
            conditionalPanel(condition = "input.visualizador_DE_infra == 'Anual mapa'",
                             withSpinner(leafletOutput("map_DE_infra"),
                                         type = 2)),
            
            conditionalPanel(
              condition = "input.visualizador_DE_infra != 'Anual mapa'",
              downloadButton(outputId = "baja_plot_DE_infra",
                             label = "Descarga el gráfico")
            ),
            br(),
            br(),
            withSpinner(DTOutput("tab_dat_DE_infra"),
                        type = 2),
            br(),
            downloadButton("dl_tabla_dat_DE_infra", "Descarga la tabla"),
            br(),
            br()
            )
        )
      )
    ),
  ),
  
  tabPanel(
    
    # * 2.3. Sistema financiero   -----------------------------------------
    
    title = "Sistema financiero",
    icon = icon("coins", lib = "font-awesome"),
    
    tabsetPanel(
      type = "pills",
      id   = "CP",
      
      
      tabPanel(
        "Ahorro",

        br(),
        
        fluidRow(
          sidebarPanel(
            width = 3,
            
            selectInput(
              inputId = "visualizador_SF_ahorro",
              label = "Visualización",
              choices = c("Serie de tiempo",
                          "Anual gráfico",
                          "Anual mapa"),
              selected = "Serie de tiempo"
            ),
            
            selectInput(
              inputId = "indicador_SF_ahorro",
              label = "Indicador",
              choices = data %>%
                filter(str_detect(p2, "Ahorro")) %>%
                distinct(nomindicador) %>% 
                arrange(nomindicador) %>% 
                pull(nomindicador)
            ),
            
            uiOutput("corte_SF_ahorro"),
            
            uiOutput("fecha_dat_SF_ahorro"),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_SF_ahorro != 'Anual mapa'",
                             uiOutput("sel_SF_ahorro_pais")),
            
            br(),
            
            uiOutput("sel_SF_ahorro_region"),
            
            br(),
            
            tags$a(
              href = "https://umad.cienciassociales.edu.uy/",
              "Unidad de Métodos y Acceso a Datos",
              style = "font-size:12px; color:Navy;
                   text-decoration:underline;"
            ),
            br(),
            br(),
            img(
              src = "logo_umad.png",
              height = "70%",
              width = "70%",
              align = "left"
            ),
            style = "display:inline-block;",
            
          ),
          
          mainPanel(
            tags$h3(style = "display:inline-block",
                    uiOutput("title_dat_SF_ahorro")),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("calculator", lib = "font-awesome"),
                uiOutput("info_dat_SF_ahorro")
              )
            ),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("exclamation", lib = "font-awesome"),
                uiOutput("rel_dat_SF_ahorro")
              )
            ),
            
            tags$h5(uiOutput("subtitle_dat_SF_ahorro")),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_SF_ahorro != 'Anual mapa'",
                             withSpinner(
                               plotOutput("p_dat_SF_ahorro", height = "600px"),
                               type = 2
                             )),
            
            conditionalPanel(condition = "input.visualizador_SF_ahorro == 'Anual mapa'",
                             withSpinner(leafletOutput("map_SF_ahorro"),
                                         type = 2)),
            
            conditionalPanel(
              condition = "input.visualizador_SF_ahorro != 'Anual mapa'",
              downloadButton(outputId = "baja_plot_SF_ahorro",
                             label = "Descarga el gráfico")
            ),
            br(),
            br(),
            withSpinner(DTOutput("tab_dat_SF_ahorro"),
                        type = 2),
            br(),
            downloadButton("dl_tabla_dat_SF_ahorro", "Descarga la tabla"),
            br(),
            br()
          )
        )
      ),
      
      
      tabPanel(
        "Deuda externa",

        br(),
        
        fluidRow(
          sidebarPanel(
            width = 3,
            
            selectInput(
              inputId = "visualizador_SF_externa",
              label = "Visualización",
              choices = c("Serie de tiempo",
                          "Anual gráfico",
                          "Anual mapa"),
              selected = "Serie de tiempo"
            ),
            
            selectInput(
              inputId = "indicador_SF_externa",
              label = "Indicador",
              choices = data %>%
                filter(str_detect(p2, "Deuda externa")) %>%
                distinct(nomindicador) %>% 
                arrange(nomindicador) %>% 
                pull(nomindicador)
            ),
            
            uiOutput("corte_SF_externa"),
            
            uiOutput("fecha_dat_SF_externa"),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_SF_externa != 'Anual mapa'",
                             uiOutput("sel_SF_externa_pais")),
            
            br(),
            
            uiOutput("sel_SF_externa_region"),
            
            br(),
            
            tags$a(
              href = "https://umad.cienciassociales.edu.uy/",
              "Unidad de Métodos y Acceso a Datos",
              style = "font-size:12px; color:Navy;
                   text-decoration:underline;"
            ),
            br(),
            br(),
            img(
              src = "logo_umad.png",
              height = "70%",
              width = "70%",
              align = "left"
            ),
            style = "display:inline-block;",
            
          ),
          
          mainPanel(
            tags$h3(style = "display:inline-block",
                    uiOutput("title_dat_SF_externa")),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("calculator", lib = "font-awesome"),
                uiOutput("info_dat_SF_externa")
              )
            ),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("exclamation", lib = "font-awesome"),
                uiOutput("rel_dat_SF_externa")
              )
            ),
            
            tags$h5(uiOutput("subtitle_dat_SF_externa")),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_SF_externa != 'Anual mapa'",
                             withSpinner(
                               plotOutput("p_dat_SF_externa", height = "600px"),
                               type = 2
                             )),
            
            conditionalPanel(condition = "input.visualizador_SF_externa == 'Anual mapa'",
                             withSpinner(leafletOutput("map_SF_externa"),
                                         type = 2)),
            
            conditionalPanel(
              condition = "input.visualizador_SF_externa != 'Anual mapa'",
              downloadButton(outputId = "baja_plot_SF_externa",
                             label = "Descarga el gráfico")
            ),
            br(),
            br(),
            withSpinner(DTOutput("tab_dat_SF_externa"),
                        type = 2),
            br(),
            downloadButton("dl_tabla_dat_SF_externa", "Descarga la tabla"),
            br(),
            br()
          )
        )
      ),
      
      
      tabPanel(
        "Inversión",

        br(),

        fluidRow(
          sidebarPanel(
            width = 3,

            selectInput(
              inputId = "visualizador_SF_inv",
              label = "Visualización",
              choices = c("Serie de tiempo",
                          "Anual gráfico",
                          "Anual mapa"),
              selected = "Serie de tiempo"
            ),

            selectInput(
              inputId = "indicador_SF_inv",
              label = "Indicador",
              choices = data %>%
                filter(str_detect(p2, "Inversión")) %>%
                distinct(nomindicador) %>%
                arrange(nomindicador) %>%
                pull(nomindicador)
            ),

            uiOutput("corte_SF_inv"),

            uiOutput("fecha_dat_SF_inv"),

            br(),

            conditionalPanel(condition = "input.visualizador_SF_inv != 'Anual mapa'",
                             uiOutput("sel_SF_inv_pais")),

            br(),

            uiOutput("sel_SF_inv_region"),

            br(),

            tags$a(
              href = "https://umad.cienciassociales.edu.uy/",
              "Unidad de Métodos y Acceso a Datos",
              style = "font-size:12px; color:Navy;
                   text-decoration:underline;"
            ),
            br(),
            br(),
            img(
              src = "logo_umad.png",
              height = "70%",
              width = "70%",
              align = "left"
            ),
            style = "display:inline-block;",

          ),

          mainPanel(
            tags$h3(style = "display:inline-block",
                    uiOutput("title_dat_SF_inv")),

            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("calculator", lib = "font-awesome"),
                uiOutput("info_dat_SF_inv")
              )
            ),

            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("exclamation", lib = "font-awesome"),
                uiOutput("rel_dat_SF_inv")
              )
            ),

            tags$h5(uiOutput("subtitle_dat_SF_inv")),

            br(),

            conditionalPanel(condition = "input.visualizador_SF_inv != 'Anual mapa'",
                             withSpinner(
                               plotOutput("p_dat_SF_inv", height = "600px"),
                               type = 2
                             )),

            conditionalPanel(condition = "input.visualizador_SF_inv == 'Anual mapa'",
                             withSpinner(leafletOutput("map_SF_inv"),
                                         type = 2)),

            conditionalPanel(
              condition = "input.visualizador_SF_inv != 'Anual mapa'",
              downloadButton(outputId = "baja_plot_SF_inv",
                             label = "Descarga el gráfico")
            ),
            br(),
            br(),
            withSpinner(DTOutput("tab_dat_SF_inv"),
                        type = 2),
            br(),
            downloadButton("dl_tabla_dat_SF_inv", "Descarga la tabla"),
            br(),
            br()
          )
        )
      ),
      
      
      tabPanel(
        "Indicadores monetarios",

        br(),
        
        fluidRow(
          sidebarPanel(
            width = 3,
            
            selectInput(
              inputId = "visualizador_SF_mon",
              label = "Visualización",
              choices = c("Serie de tiempo",
                          "Anual gráfico",
                          "Anual mapa"),
              selected = "Serie de tiempo"
            ),
            
            selectInput(
              inputId = "indicador_SF_mon",
              label = "Indicador",
              choices = data %>%
                filter(str_detect(p2, "Indicadores monetarios")) %>%
                distinct(nomindicador) %>% 
                arrange(nomindicador) %>% 
                pull(nomindicador)
            ),
            
            uiOutput("corte_SF_mon"),
            
            uiOutput("fecha_dat_SF_mon"),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_SF_mon != 'Anual mapa'",
                             uiOutput("sel_SF_mon_pais")),
            
            br(),
            
            uiOutput("sel_SF_mon_region"),
            
            br(),
            
            tags$a(
              href = "https://umad.cienciassociales.edu.uy/",
              "Unidad de Métodos y Acceso a Datos",
              style = "font-size:12px; color:Navy;
                   text-decoration:underline;"
            ),
            br(),
            br(),
            img(
              src = "logo_umad.png",
              height = "70%",
              width = "70%",
              align = "left"
            ),
            style = "display:inline-block;",
            
          ),
          
          mainPanel(
            tags$h3(style = "display:inline-block",
                    uiOutput("title_dat_SF_mon")),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("calculator", lib = "font-awesome"),
                uiOutput("info_dat_SF_mon")
              )
            ),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("exclamation", lib = "font-awesome"),
                uiOutput("rel_dat_SF_mon")
              )
            ),
            
            tags$h5(uiOutput("subtitle_dat_SF_mon")),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_SF_mon != 'Anual mapa'",
                             withSpinner(
                               plotOutput("p_dat_SF_mon", height = "600px"),
                               type = 2
                             )),
            
            conditionalPanel(condition = "input.visualizador_SF_mon == 'Anual mapa'",
                             withSpinner(leafletOutput("map_SF_mon"),
                                         type = 2)),
            
            conditionalPanel(
              condition = "input.visualizador_SF_mon != 'Anual mapa'",
              downloadButton(outputId = "baja_plot_SF_mon",
                             label = "Descarga el gráfico")
            ),
            br(),
            br(),
            withSpinner(DTOutput("tab_dat_SF_mon"),
                        type = 2),
            br(),
            downloadButton("dl_tabla_dat_SF_mon", "Descarga la tabla"),
            br(),
            br()
            
          )
        )
      )
    ),
  ),
  
  tabPanel(
    
    # * 2.4. Energía  ----------------------------------------------------
    
    title = "Energía",
    icon = icon("lightbulb", lib = "font-awesome"),
    
    tabsetPanel(
      type = "pills",
      id   = "CP",
      
      
      tabPanel(
        "Consumo",

        br(),
        
        fluidRow(
          sidebarPanel(
            width = 3,
            
            selectInput(
              inputId = "visualizador_EN_consumo",
              label = "Visualización",
              choices = c("Serie de tiempo",
                          "Anual gráfico",
                          "Anual mapa"),
              selected = "Serie de tiempo"
            ),
            
            selectInput(
              inputId = "indicador_EN_consumo",
              label = "Indicador",
              choices = data %>%
                filter(str_detect(p2, "Consumo")) %>%
                distinct(nomindicador) %>% 
                arrange(nomindicador) %>% 
                pull(nomindicador)
            ),
            
            uiOutput("corte_EN_consumo"),
            
            uiOutput("fecha_dat_EN_consumo"),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_EN_consumo != 'Anual mapa'",
                             uiOutput("sel_EN_consumo_pais")),
            
            br(),
            
            uiOutput("sel_EN_consumo_region"),
            
            br(),
            
            tags$a(
              href = "https://umad.cienciassociales.edu.uy/",
              "Unidad de Métodos y Acceso a Datos",
              style = "font-size:12px; color:Navy;
                   text-decoration:underline;"
            ),
            br(),
            br(),
            img(
              src = "logo_umad.png",
              height = "70%",
              width = "70%",
              align = "left"
            ),
            style = "display:inline-block;",
            
          ),
          
          mainPanel(
            tags$h3(style = "display:inline-block",
                    uiOutput("title_dat_EN_consumo")),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("calculator", lib = "font-awesome"),
                uiOutput("info_dat_EN_consumo")
              )
            ),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("exclamation", lib = "font-awesome"),
                uiOutput("rel_dat_EN_consumo")
              )
            ),
            
            tags$h5(uiOutput("subtitle_dat_EN_consumo")),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_EN_consumo != 'Anual mapa'",
                             withSpinner(
                               plotOutput("p_dat_EN_consumo", height = "600px"),
                               type = 2
                             )),
            
            conditionalPanel(condition = "input.visualizador_EN_consumo == 'Anual mapa'",
                             withSpinner(leafletOutput("map_EN_consumo"),
                                         type = 2)),
            
            conditionalPanel(
              condition = "input.visualizador_EN_consumo != 'Anual mapa'",
              downloadButton(outputId = "baja_plot_EN_consumo",
                             label = "Descarga el gráfico")
            ),
            br(),
            br(),
            withSpinner(DTOutput("tab_dat_EN_consumo"),
                        type = 2),
            br(),
            downloadButton("dl_tabla_dat_EN_consumo", "Descarga la tabla"),
            br(),
            br()
          )
        )
      ),
      
      
      tabPanel(
        "Emisiones",

        br(),
        
        fluidRow(
          sidebarPanel(
            width = 3,
            
            selectInput(
              inputId = "visualizador_EN_emisiones",
              label = "Visualización",
              choices = c("Serie de tiempo",
                          "Anual gráfico",
                          "Anual mapa"),
              selected = "Serie de tiempo"
            ),
            
            selectInput(
              inputId = "indicador_EN_emisiones",
              label = "Indicador",
              choices = data %>%
                filter(str_detect(p2, "Emisiones")) %>%
                distinct(nomindicador) %>% 
                arrange(nomindicador) %>% 
                pull(nomindicador)
            ),
            
            uiOutput("corte_EN_emisiones"),
            
            uiOutput("fecha_dat_EN_emisiones"),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_EN_emisiones != 'Anual mapa'",
                             uiOutput("sel_EN_emisiones_pais")),
            
            br(),
            
            uiOutput("sel_EN_emisiones_region"),
            
            br(),
            
            tags$a(
              href = "https://umad.cienciassociales.edu.uy/",
              "Unidad de Métodos y Acceso a Datos",
              style = "font-size:12px; color:Navy;
                   text-decoration:underline;"
            ),
            br(),
            br(),
            img(
              src = "logo_umad.png",
              height = "70%",
              width = "70%",
              align = "left"
            ),
            style = "display:inline-block;",
            
          ),
          
          mainPanel(
            tags$h3(style = "display:inline-block",
                    uiOutput("title_dat_EN_emisiones")),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("calculator", lib = "font-awesome"),
                uiOutput("info_dat_EN_emisiones")
              )
            ),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("exclamation", lib = "font-awesome"),
                uiOutput("rel_dat_EN_emisiones")
              )
            ),
            
            tags$h5(uiOutput("subtitle_dat_EN_emisiones")),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_EN_emisiones != 'Anual mapa'",
                             withSpinner(
                               plotOutput("p_dat_EN_emisiones", height = "600px"),
                               type = 2
                             )),
            
            conditionalPanel(condition = "input.visualizador_EN_emisiones == 'Anual mapa'",
                             withSpinner(leafletOutput("map_EN_emisiones"),
                                         type = 2)),
            
            conditionalPanel(
              condition = "input.visualizador_EN_emisiones != 'Anual mapa'",
              downloadButton(outputId = "baja_plot_EN_emisiones",
                             label = "Descarga el gráfico")
            ),
            br(),
            br(),
            withSpinner(DTOutput("tab_dat_EN_emisiones"),
                        type = 2),
            br(),
            downloadButton("dl_tabla_dat_EN_emisiones", "Descarga la tabla"),
            br(),
            br()
          )
        )
      ),
      
      tabPanel(
        "Producción",

        br(),
        
        fluidRow(
          sidebarPanel(
            width = 3,
            
            selectInput(
              inputId = "visualizador_EN_prod",
              label = "Visualización",
              choices = c("Serie de tiempo",
                          "Anual gráfico",
                          "Anual mapa"),
              selected = "Serie de tiempo"
            ),
            
            selectInput(
              inputId = "indicador_EN_prod",
              label = "Indicador",
              choices = data %>%
                filter(str_detect(p2, "Producción")) %>%
                distinct(nomindicador) %>% 
                arrange(nomindicador) %>% 
                pull(nomindicador)
            ),
            
            uiOutput("corte_EN_prod"),
            
            uiOutput("fecha_dat_EN_prod"),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_EN_prod != 'Anual mapa'",
                             uiOutput("sel_EN_prod_pais")),
            
            br(),
            
            uiOutput("sel_EN_prod_region"),
            
            br(),
            
            tags$a(
              href = "https://umad.cienciassociales.edu.uy/",
              "Unidad de Métodos y Acceso a Datos",
              style = "font-size:12px; color:Navy;
                   text-decoration:underline;"
            ),
            br(),
            br(),
            img(
              src = "logo_umad.png",
              height = "70%",
              width = "70%",
              align = "left"
            ),
            style = "display:inline-block;",
            
          ),
          
          mainPanel(
            tags$h3(style = "display:inline-block",
                    uiOutput("title_dat_EN_prod")),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("calculator", lib = "font-awesome"),
                uiOutput("info_dat_EN_prod")
              )
            ),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("exclamation", lib = "font-awesome"),
                uiOutput("rel_dat_EN_prod")
              )
            ),
            
            tags$h5(uiOutput("subtitle_dat_EN_prod")),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_EN_prod != 'Anual mapa'",
                             withSpinner(
                               plotOutput("p_dat_EN_prod", height = "600px"),
                               type = 2
                             )),
            
            conditionalPanel(condition = "input.visualizador_EN_prod == 'Anual mapa'",
                             withSpinner(leafletOutput("map_EN_prod"),
                                         type = 2)),
            
            conditionalPanel(
              condition = "input.visualizador_EN_prod != 'Anual mapa'",
              downloadButton(outputId = "baja_plot_EN_prod",
                             label = "Descarga el gráfico")
            ),
            br(),
            br(),
            withSpinner(DTOutput("tab_dat_EN_prod"),
                        type = 2),
            br(),
            downloadButton("dl_tabla_dat_EN_prod", "Descarga la tabla"),
            br(),
            br()
          )
        )
      )
    ),
  ),
  
  tabPanel(
    
    # * 2.5. Sostenibilidad ambiental  -------------------------------------
    
    title = "Sostenibilidad ambiental",
    icon = icon("seedling", lib = "font-awesome"),

    br(),
    
    fluidRow(
      
      sidebarPanel(
        width = 3,
        
        tags$a(
          href = "https://umad.cienciassociales.edu.uy/",
          "Unidad de Métodos y Acceso a Datos",
          style = "font-size:12px; color:Navy;text-decoration:underline;"
          ),
        
        br(),
        br(),
        img(src = "logo_umad.png",
            height = "70%",
            width = "70%",
            align = "left"
            ),
        style = "display:inline-block;",
        ),
      
      mainPanel(
        
      )
    ),
  ),
  
  tabPanel(
    
    # * 2.6. Demografía  -------------------------------------
    
    title = "Demografía",
    icon = icon("users", lib = "font-awesome"),
    
    br(),
    
    fluidRow(
      
      sidebarPanel(
        width = 3,
        
        tags$a(
          href = "https://umad.cienciassociales.edu.uy/",
          "Unidad de Métodos y Acceso a Datos",
          style = "font-size:12px; color:Navy;text-decoration:underline;"
        ),
        
        br(),
        br(),
        img(src = "logo_umad.png",
            height = "70%",
            width = "70%",
            align = "left"
        ),
        style = "display:inline-block;",
      ),
      
      mainPanel(
        
      )
    ),
  )
)


server <- function(session, input, output) {
  
  
  ##  3.  CP_comp (dat_CP_comp)   ============================================
  
  # Data CP_compn
  
  dat_CP_comp <- reactive({
    
    req(input$indicador_CP_comp)
    
    data %>%
      filter(nomindicador == input$indicador_CP_comp) %>% 
      janitor::remove_empty("cols")
  
    })
  
  
  # Titulo
  output$title_dat_CP_comp <- renderUI({
    helpText(HTML(unique(dat_CP_comp()$nomindicador)))
  })
  
  # Subtitulo
  output$subtitle_dat_CP_comp <- renderUI({
    helpText(HTML(unique(dat_CP_comp()$definicion)))
  })
  
  # Metodología
  output$info_dat_CP_comp <- renderUI({
    helpText(HTML(
      paste(
        "<b>Método de Agregación:</b>",
        unique(dat_CP_comp()$metodo_de_agregacion),
        "<b>Metodología:</b>",
        unique(dat_CP_comp()$concepto_estadistico_y_metodologia)
      )
    ))
  })
  
  # Relevancia:
  output$rel_dat_CP_comp <- renderUI({
    helpText(HTML(paste(
      "<b>Relvancia:</b>", unique(dat_CP_comp()$relevancia)
    )))
  })
  
  
  output$fecha_dat_CP_comp <- renderUI({
    if (input$visualizador_CP_comp == "Serie de tiempo") {
      tagList(
        tags$style(
          type = 'text/css',
          '#big_slider .irs-grid-text {font-size: 10px; transform: rotate(-90deg) translate(-15px);} ,.irs-grid-pol.large {height: 0px;}'
        ),
        div(
          id = 'big_slider',
          
          sliderTextInput(
            inputId = "fecha_dat_CP_comp",
            label = "Seleccione años",
            choices = sort(unique(dat_CP_comp()$fecha)),
            selected = c(min(dat_CP_comp()$fecha),
                         max(dat_CP_comp()$fecha)),
            hide_min_max = T,
            grid = TRUE,
            width = "100%"
          )
        )
      )
      
    } else {
      selectInput(
        inputId = "fecha_CP_comp",
        label = "Seleccione año:",
        choices = dat_CP_comp() %>%
          drop_na(valor) %>%
          select(fecha) %>%
          arrange(desc(fecha)) %>%
          unique() %>%
          pull(),
        selected = "2019"
      )
      
    }
    
  })
  
  
  output$corte_CP_comp <- renderUI({
    
    if (input$indicador_CP_comp %in% vars_corte) {
      
      selectInput(inputId = "corte_CP_comp",
                  label = "Seleccione categorías",
                  choices =  dat_CP_comp() %>%
                    distinct(get(names(dat_CP_comp()[,ncol(dat_CP_comp())]))) %>%
                    pull(),
                  selected = dat_CP_comp() %>%
                    filter(jerarquia == 1) %>% 
                    distinct(get(names(dat_CP_comp()[,ncol(dat_CP_comp())]))) %>%
                    pull()
                  )
    } else {
      
      return(NULL)
    }
    
    })
  
  
  # Checkbox por pais
  output$sel_CP_comp_pais <- renderUI({
    if (input$visualizador_CP_comp == "Serie de tiempo") {
      dropdown(
        label = "Seleccione países",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_pais_CP_comp",
          label = "Seleccione países",
          inline = TRUE,
          choices = dat_CP_comp() %>%
            filter(region == 0) %>%
            filter(nomindicador == input$indicador_CP_comp) %>%
            distinct(cod_pais) %>%
            pull(),
          selected = c("URY", "ARG", "BRA", "PRY")
        )
      )
      
    } else {
      checkboxGroupInput(
        inputId = "chbox_pais_reg_CP_comp",
        label = "Mostrar:",
        inline = FALSE,
        choices = c("Países", "Regiones"),
        selected = "Países"
      )
      
    }
    
  })
  
  # Checkbox por región
  output$sel_CP_comp_region <- renderUI({
    if (input$visualizador_CP_comp == "Serie de tiempo") {
      dropdown(
        label = "Seleccione regiones",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_reg_CP_comp",
          label = "Seleccione regiones",
          inline = TRUE,
          choices = dat_CP_comp() %>%
            filter(region == 1) %>%
            filter(nomindicador == input$indicador_CP_comp) %>%
            distinct(pais) %>%
            pull(),
          selected = NULL
        )
      )
      
    } else {
      return(NULL)
      
    }
  })
  

  dat_CP_comp_anual <- reactive({
    
    if (input$indicador_CP_comp %in% vars_corte) {
      
      dat_CP_comp() %>%
        filter(get(names(dat_CP_comp()[,ncol(dat_CP_comp())])) %in% input$corte_CP_comp) %>%
        filter(fecha == input$fecha_CP_comp)
    
    } else {
      
      dat_CP_comp() %>%
        filter(fecha == input$fecha_CP_comp)
      
      
    }
  })
  
  dat_CP_comp_simple <- reactive({
    
    if (input$indicador_CP_comp %in% vars_corte) {
      
    dat_CP_comp() %>%
      filter(get(names(dat_CP_comp()[,ncol(dat_CP_comp())])) %in% input$corte_CP_comp) %>% 
      filter(fecha >= input$fecha_dat_CP_comp[1] & fecha <= input$fecha_dat_CP_comp[2]) %>%
      filter(cod_pais %in% input$chbox_pais_CP_comp | pais %in% input$chbox_reg_CP_comp)
      
      } else {
        
        dat_CP_comp() %>%
          filter(fecha >= input$fecha_dat_CP_comp[1] & fecha <= input$fecha_dat_CP_comp[2]) %>%
          filter(cod_pais %in% input$chbox_pais_CP_comp | pais %in% input$chbox_reg_CP_comp)        
        }
    
    })
  
  
  # Gráficos CP_comp
  output$p_dat_CP_comp <- renderPlot({
    
    if (input$visualizador_CP_comp == "Serie de tiempo" ) {
      
      req(input$fecha_dat_CP_comp, input$indicador_CP_comp)
      
      plot_CP_comp <- ggplot(
        data = dat_CP_comp_simple(), 
        aes(x = fecha, y = valor)
      ) +
        geom_line(aes(color = pais), size = 1, alpha = 0.5) +
        geom_point(aes(color = pais), size = 3) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_CP_comp_simple()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        scale_colour_manual(name = "", values = paleta_expandida) +
        if(input$indicador_CP_comp %in% vars_corte){
          
          ggtitle(paste0(input$indicador_CP_comp, " (", input$corte_CP_comp, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_CP_comp))
          
        }
      
      print(plot_CP_comp)
      ggsave(
        "www/indicador CP_comp.png",
        width = 30,
        height = 20,
        units = "cm"
      )

    } else if (input$visualizador_CP_comp == "Anual gráfico") {
      
      base_plot_CP_comp <- dat_CP_comp_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_CP_comp)
      
      validate(need(
        nrow(base_plot_CP_comp) > 0,
        'No hay datos disponible para esta búsqueda'
      ))
      
      plot_CP_comp <- ggplot(base_plot_CP_comp,
                         aes(x = fct_reorder(pais, valor), y = valor)) +
        geom_segment(
          aes(
            x = fct_reorder(pais, valor),
            xend = fct_reorder(pais, valor),
            y = 0,
            yend = valor
          ),
          size = 1,
          color = "#2c3e50"
        ) +
        geom_point(color = "#2c3e50", size = 4) +
        theme_light() +
        coord_flip() +
        theme(
          axis.text.y = element_text(size = 12),
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_CP_comp_anual()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        if(input$indicador_CP_comp %in% vars_corte){
          
          ggtitle(paste0(input$indicador_CP_comp, " (", input$corte_CP_comp, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_CP_comp))
          
        }
      
      print(plot_CP_comp)
      ggsave(
        "www/indicador CP_comp.png",
        width = 25,
        height = 30,
        units = "cm"
      )
      
    }
    
  })
  
  output$map_CP_comp <- renderLeaflet({
    # Read this shape file with the rgdal library.
    world_spdf <- readOGR(dsn = "data" ,
                          layer = "TM_WORLD_BORDERS_SIMPL-0.3",
                          verbose = FALSE)
    
    # Pegar datos
    world_spdf@data <- world_spdf@data %>%
      left_join(select(dat_CP_comp_anual(), pais_eng, valor_original),
                by = c("NAME" = "pais_eng"))
    
    # Library
    
    # Create a color palette with handmade bins.
    
    
    if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= -1) {
      round_number <- .1
      
    }
    
    min_val <-
      plyr::round_any(min(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      floor)
    max_val <-
      plyr::round_any(max(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      ceiling)
    dif <- max_val - min_val
    dif_5 <- dif / 5
    val_1 <- min_val + dif_5
    val_2 <- min_val + (dif_5 * 2)
    val_3 <- min_val + (dif_5 * 3)
    val_4 <- min_val + (dif_5 * 4)
    
    mybins <- c(min_val, val_1, val_2, val_3, val_4, max_val)
    
    mypalette <- colorBin(
      palette = "YlOrBr",
      domain = world_spdf@data$valor_original,
      na.color = "transparent",
      bins = mybins
    )
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "País: ",
      world_spdf@data$NAME,
      "<br/>",
      "Valor: ",
      round(world_spdf@data$valor_original, 2),
      sep = ""
    ) %>%
      lapply(htmltools::HTML)
    
    # Final Map
    leaflet(world_spdf) %>%
      addTiles()  %>%
      setView(lat = 30,
              lng = 0 ,
              zoom = 1) %>%
      addPolygons(
        fillColor = ~ mypalette(valor_original),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = mypalette,
        values =  ~ valor_original,
        opacity = 0.9,
        title = wrapit(input$indicador_CP_comp),
        position = "bottomleft"
      )
    
  })
  
  # Botón descarga grafico
  output$baja_plot_CP_comp <- downloadHandler(filename <- function() {
    paste("indicador CP_comp", "png", sep = ".")
  },
  
  content <- function(file) {
    file.copy("www/indicador CP_comp.png", file)
  },
  contentType = "www/indicador CP_comp")
  
  
  ## Data series temporal
  
  # Data para tabla y exportar
  dat_CP_comp_st <- reactive({
    
    if (input$indicador_CP_comp %in% vars_corte) {
      
      dat_CP_comp() %>%
        filter(fecha >= input$fecha_dat_CP_comp[1] & fecha <= input$fecha_dat_CP_comp[2]) %>%
        filter(cod_pais %in% input$chbox_pais_CP_comp | pais %in% input$chbox_reg_CP_comp) %>%
        select(pais, fecha, names(dat_CP_comp()[,ncol(dat_CP_comp())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor)) 
      
    } else {
      
      dat_CP_comp() %>%
        filter(fecha >= input$fecha_dat_CP_comp[1] &
                 fecha <= input$fecha_dat_CP_comp[2]) %>%
        filter(cod_pais %in% input$chbox_pais_CP_comp |
                 pais %in% input$chbox_reg_CP_comp) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
  })
  
  # Metadata
  dat_CP_comp_m <- reactive({
    dat_CP_comp() %>%
      select(
        nomindicador,
        definicion,
        metodo_de_agregacion,
        concepto_estadistico_y_metodologia,
        relevancia,
        limitaciones_y_excepciones
      ) %>%
      mutate(`Unidad de Métodos y Acceso a Datos (FCS -UdelaR)` = " ") %>%
      distinct() %>%
      gather(key = "", value = " ")
    
  })
  
  # Data completa
  dat_CP_comp_c <- reactive({
    
    if (input$indicador_CP_comp %in% vars_corte) {
      
    dat_CP_comp() %>%
        select(pais, fecha, names(dat_CP_comp()[,ncol(dat_CP_comp())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {

      dat_CP_comp() %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
    
  })
  
  # Lista para descarga
  list_dat_CP_comp_st <- reactive({
    list_dat_CP_comp <- list(
      "Data" = dat_CP_comp_st(),
      "Metadata" = dat_CP_comp_m(),
      "Data Completa" = dat_CP_comp_c()
    )
  })
  
  
  ## Data por año
  # Data para tabla y exportar
  dat_CP_comp_a <- reactive({
    
    if (input$indicador_CP_comp %in% vars_corte) {
      
    dat_CP_comp_anual() %>%
      filter(pais_region %in% input$chbox_pais_reg_CP_comp) %>%
      select(pais, fecha, names(dat_CP_comp()[,ncol(dat_CP_comp())]), valor) %>%
      arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {

      dat_CP_comp_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_CP_comp) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    }
  })

  
  # Lista para descarga
  list_dat_CP_comp_a <- reactive({
    list_dat_CP_comp <- list("Data" = dat_CP_comp_a(),
                             "Metadata" = dat_CP_comp_m(),
                             "Data completa" = dat_CP_comp_c())
  })
  
  # Tablas en shiny
  output$tab_dat_CP_comp <- renderDT({
    
    if (input$indicador_CP_comp %in% vars_corte) {
      DT::datatable(
        dat_CP_comp_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Corte", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_CP_comp,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(4, '', mark = ",")
      
    } else if (input$visualizador_CP_comp == "Serie de tiempo") {
        DT::datatable(
          dat_CP_comp_st(),
          rownames = FALSE,
          colnames = c("País/Región", "Fecha", "Valor"),
          options = list(columnDefs = list(
            list(className = 'dt-center', targets = 1:2)
          )),
          caption = htmltools::tags$caption(input$indicador_CP_comp,
                                            style = "color:black; font-size:110%;")
        ) %>%
          formatCurrency(3, '', mark = ",")
      
        } else if (input$visualizador_CP_comp %in% c("Anual gráfico", "Anual mapa")) {
      DT::datatable(
        dat_CP_comp_a(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_CP_comp,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    }
  })
  
  # Descarga tabla
  output$dl_tabla_dat_CP_comp <- downloadHandler(
    filename = function() {
      paste("resultados-", input$indicador_CP_comp, ".xlsx", sep = "")
    },
    content = function(file) {
      if (input$visualizador_CP_comp == "Serie de tiempo") {
        openxlsx::write.xlsx(list_dat_CP_comp_st(), file)
        
      } else if (input$visualizador_CP_comp %in% c("Anual gráfico", "Anual mapa")) {
        openxlsx::write.xlsx(list_dat_CP_comp_a(), file)
        
      }
    }
  )

  
  ##  4.  CP_precios (dat_CP_precios)   ====================================
  
  # Data CP_precios
  
  dat_CP_precios <- reactive({
    
    req(input$indicador_CP_precios)
    
    data %>%
      filter(nomindicador == input$indicador_CP_precios) %>% 
      janitor::remove_empty("cols")
    
  })
  
  
  # Titulo
  output$title_dat_CP_precios <- renderUI({
    helpText(HTML(unique(dat_CP_precios()$nomindicador)))
  })
  
  # Subtitulo
  output$subtitle_dat_CP_precios <- renderUI({
    helpText(HTML(unique(dat_CP_precios()$definicion)))
  })
  
  # Metodología
  output$info_dat_CP_precios <- renderUI({
    helpText(HTML(
      paste(
        "<b>Método de Agregación:</b>",
        unique(dat_CP_precios()$metodo_de_agregacion),
        "<b>Metodología:</b>",
        unique(dat_CP_precios()$concepto_estadistico_y_metodologia)
      )
    ))
  })
  
  # Relevancia:
  output$rel_dat_CP_precios <- renderUI({
    helpText(HTML(paste(
      "<b>Relvancia:</b>", unique(dat_CP_precios()$relevancia)
    )))
  })
  
  
  output$fecha_dat_CP_precios <- renderUI({
    if (input$visualizador_CP_precios == "Serie de tiempo") {
      tagList(
        tags$style(
          type = 'text/css',
          '#big_slider .irs-grid-text {font-size: 10px; transform: rotate(-90deg) translate(-15px);} ,.irs-grid-pol.large {height: 0px;}'
        ),
        div(
          id = 'big_slider',
          
          sliderTextInput(
            inputId = "fecha_dat_CP_precios",
            label = "Seleccione años",
            choices = sort(unique(dat_CP_precios()$fecha)),
            selected = c(min(dat_CP_precios()$fecha),
                         max(dat_CP_precios()$fecha)),
            hide_min_max = T,
            grid = TRUE,
            width = "100%"
          )
        )
      )
      
    } else {
      selectInput(
        inputId = "fecha_CP_precios",
        label = "Seleccione año:",
        choices = dat_CP_precios() %>%
          drop_na(valor) %>%
          select(fecha) %>%
          arrange(desc(fecha)) %>%
          unique() %>%
          pull(),
        selected = "2019"
      )
      
    }
    
  })
  
  
  output$corte_CP_precios <- renderUI({
    
    if (input$indicador_CP_precios %in% vars_corte) {
      
      selectInput(inputId = "corte_CP_precios",
                  label = "Seleccione categorías",
                  choices =  dat_CP_precios() %>%
                    distinct(get(names(dat_CP_precios()[,ncol(dat_CP_precios())]))) %>%
                    pull(),
                  selected = dat_CP_precios() %>%
                    filter(jerarquia == 1) %>% 
                    distinct(get(names(dat_CP_precios()[,ncol(dat_CP_precios())]))) %>%
                    pull()
      )
    } else {
      
      return(NULL)
    }
    
  })
  
  
  # Checkbox por pais
  output$sel_CP_precios_pais <- renderUI({
    if (input$visualizador_CP_precios == "Serie de tiempo") {
      dropdown(
        label = "Seleccione países",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_pais_CP_precios",
          label = "Seleccione países",
          inline = TRUE,
          choices = dat_CP_precios() %>%
            filter(region == 0) %>%
            filter(nomindicador == input$indicador_CP_precios) %>%
            distinct(cod_pais) %>%
            pull(),
          selected = c("URY", "ARG", "BRA", "PRY")
        )
      )
      
    } else {
      checkboxGroupInput(
        inputId = "chbox_pais_reg_CP_precios",
        label = "Mostrar:",
        inline = FALSE,
        choices = c("Países", "Regiones"),
        selected = "Países"
      )
      
    }
    
  })
  
  # Checkbox por región
  output$sel_CP_precios_region <- renderUI({
    if (input$visualizador_CP_precios == "Serie de tiempo") {
      dropdown(
        label = "Seleccione regiones",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_reg_CP_precios",
          label = "Seleccione regiones",
          inline = TRUE,
          choices = dat_CP_precios() %>%
            filter(region == 1) %>%
            filter(nomindicador == input$indicador_CP_precios) %>%
            distinct(pais) %>%
            pull(),
          selected = NULL
        )
      )
      
    } else {
      return(NULL)
      
    }
  })
  
  
  dat_CP_precios_anual <- reactive({
    
    if (input$indicador_CP_precios %in% vars_corte) {
      
      dat_CP_precios() %>%
        filter(get(names(dat_CP_precios()[,ncol(dat_CP_precios())])) %in% input$corte_CP_precios) %>%
        filter(fecha == input$fecha_CP_precios)
      
    } else {
      
      dat_CP_precios() %>%
        filter(fecha == input$fecha_CP_precios)
      
      
    }
  })
  
  dat_CP_precios_simple <- reactive({
    
    if (input$indicador_CP_precios %in% vars_corte) {
      
      dat_CP_precios() %>%
        filter(get(names(dat_CP_precios()[,ncol(dat_CP_precios())])) %in% input$corte_CP_precios) %>% 
        filter(fecha >= input$fecha_dat_CP_precios[1] & fecha <= input$fecha_dat_CP_precios[2]) %>%
        filter(cod_pais %in% input$chbox_pais_CP_precios | pais %in% input$chbox_reg_CP_precios)
      
    } else {
      
      dat_CP_precios() %>%
        filter(fecha >= input$fecha_dat_CP_precios[1] & fecha <= input$fecha_dat_CP_precios[2]) %>%
        filter(cod_pais %in% input$chbox_pais_CP_precios | pais %in% input$chbox_reg_CP_precios)        
    }
    
  })
  
  
  # Gráficos CP_precios
  output$p_dat_CP_precios <- renderPlot({
    
    if (input$visualizador_CP_precios == "Serie de tiempo" ) {
      
      req(input$fecha_dat_CP_precios, input$indicador_CP_precios)
      
      plot_CP_precios <- ggplot(
        data = dat_CP_precios_simple(), 
        aes(x = fecha, y = valor)
      ) +
        geom_line(aes(color = pais), size = 1, alpha = 0.5) +
        geom_point(aes(color = pais), size = 3) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_CP_precios_simple()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        scale_colour_manual(name = "", values = paleta_expandida) +
        if(input$indicador_CP_precios %in% vars_corte){
          
          ggtitle(paste0(input$indicador_CP_precios, " (", input$corte_CP_precios, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_CP_precios))
          
        }
      
      print(plot_CP_precios)
      ggsave(
        "www/indicador CP_precios.png",
        width = 30,
        height = 20,
        units = "cm"
      )
      
    } else if (input$visualizador_CP_precios == "Anual gráfico") {
      
      base_plot_CP_precios <- dat_CP_precios_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_CP_precios)
      
      validate(need(
        nrow(base_plot_CP_precios) > 0,
        'No hay datos disponible para esta búsqueda'
      ))
      
      plot_CP_precios <- ggplot(base_plot_CP_precios,
                             aes(x = fct_reorder(pais, valor), y = valor)) +
        geom_segment(
          aes(
            x = fct_reorder(pais, valor),
            xend = fct_reorder(pais, valor),
            y = 0,
            yend = valor
          ),
          size = 1,
          color = "#2c3e50"
        ) +
        geom_point(color = "#2c3e50", size = 4) +
        theme_light() +
        coord_flip() +
        theme(
          axis.text.y = element_text(size = 12),
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_CP_precios_anual()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        if(input$indicador_CP_precios %in% vars_corte){
          
          ggtitle(paste0(input$indicador_CP_precios, " (", input$corte_CP_precios, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_CP_precios))
          
        }
      
      print(plot_CP_precios)
      ggsave(
        "www/indicador CP_precios.png",
        width = 25,
        height = 30,
        units = "cm"
      )
      
    }
    
  })
  
  output$map_CP_precios <- renderLeaflet({
    # Read this shape file with the rgdal library.
    world_spdf <- readOGR(dsn = "data" ,
                          layer = "TM_WORLD_BORDERS_SIMPL-0.3",
                          verbose = FALSE)
    
    # Pegar datos
    world_spdf@data <- world_spdf@data %>%
      left_join(select(dat_CP_precios_anual(), pais_eng, valor_original),
                by = c("NAME" = "pais_eng"))
    
    # Library
    
    # Create a color palette with handmade bins.
    
    
    if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= -1) {
      round_number <- .1
      
    }
    
    min_val <-
      plyr::round_any(min(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      floor)
    max_val <-
      plyr::round_any(max(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      ceiling)
    dif <- max_val - min_val
    dif_5 <- dif / 5
    val_1 <- min_val + dif_5
    val_2 <- min_val + (dif_5 * 2)
    val_3 <- min_val + (dif_5 * 3)
    val_4 <- min_val + (dif_5 * 4)
    
    mybins <- c(min_val, val_1, val_2, val_3, val_4, max_val)
    
    mypalette <- colorBin(
      palette = "YlOrBr",
      domain = world_spdf@data$valor_original,
      na.color = "transparent",
      bins = mybins
    )
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "País: ",
      world_spdf@data$NAME,
      "<br/>",
      "Valor: ",
      round(world_spdf@data$valor_original, 2),
      sep = ""
    ) %>%
      lapply(htmltools::HTML)
    
    # Final Map
    leaflet(world_spdf) %>%
      addTiles()  %>%
      setView(lat = 30,
              lng = 0 ,
              zoom = 1) %>%
      addPolygons(
        fillColor = ~ mypalette(valor_original),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = mypalette,
        values =  ~ valor_original,
        opacity = 0.9,
        title = wrapit(input$indicador_CP_precios),
        position = "bottomleft"
      )
    
  })
  
  # Botón descarga grafico
  output$baja_plot_CP_precios <- downloadHandler(filename <- function() {
    paste("indicador CP_precios", "png", sep = ".")
  },
  
  content <- function(file) {
    file.copy("www/indicador CP_precios.png", file)
  },
  contentType = "www/indicador CP_precios")
  
  
  ## Data series temporal
  
  # Data para tabla y exportar
  dat_CP_precios_st <- reactive({
    
    if (input$indicador_CP_precios %in% vars_corte) {
      
      dat_CP_precios() %>%
        filter(fecha >= input$fecha_dat_CP_precios[1] & fecha <= input$fecha_dat_CP_precios[2]) %>%
        filter(cod_pais %in% input$chbox_pais_CP_precios | pais %in% input$chbox_reg_CP_precios) %>%
        select(pais, fecha, names(dat_CP_precios()[,ncol(dat_CP_precios())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor)) 
      
    } else {
      
      dat_CP_precios() %>%
        filter(fecha >= input$fecha_dat_CP_precios[1] &
                 fecha <= input$fecha_dat_CP_precios[2]) %>%
        filter(cod_pais %in% input$chbox_pais_CP_precios |
                 pais %in% input$chbox_reg_CP_precios) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
  })
  
  # Metadata
  dat_CP_precios_m <- reactive({
    dat_CP_precios() %>%
      select(
        nomindicador,
        definicion,
        metodo_de_agregacion,
        concepto_estadistico_y_metodologia,
        relevancia,
        limitaciones_y_excepciones
      ) %>%
      mutate(`Unidad de Métodos y Acceso a Datos (FCS -UdelaR)` = " ") %>%
      distinct() %>%
      gather(key = "", value = " ")
    
  })
  
  # Data completa
  dat_CP_precios_c <- reactive({
    
    if (input$indicador_CP_precios %in% vars_corte) {
      
      dat_CP_precios() %>%
        select(pais, fecha, names(dat_CP_precios()[,ncol(dat_CP_precios())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_CP_precios() %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
    
  })
  
  # Lista para descarga
  list_dat_CP_precios_st <- reactive({
    list_dat_CP_precios <- list(
      "Data" = dat_CP_precios_st(),
      "Metadata" = dat_CP_precios_m(),
      "Data Completa" = dat_CP_precios_c()
    )
  })
  
  
  ## Data por año
  # Data para tabla y exportar
  dat_CP_precios_a <- reactive({
    
    if (input$indicador_CP_precios %in% vars_corte) {
      
      dat_CP_precios_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_CP_precios) %>%
        select(pais, fecha, names(dat_CP_precios()[,ncol(dat_CP_precios())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_CP_precios_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_CP_precios) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    }
  })
  
  # Lista para descarga
  list_dat_CP_precios_a <- reactive({
    list_dat_CP_precios <- list("Data" = dat_CP_precios_a(),
                             "Metadata" = dat_CP_precios_m(),
                             "Data Completa" = dat_CP_precios_c())
  })
  
  # Tablas en shiny
  output$tab_dat_CP_precios <- renderDT({
    
    if (input$indicador_CP_precios %in% vars_corte) {
      DT::datatable(
        dat_CP_precios_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Corte", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_CP_precios,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(4, '', mark = ",")
      
    } else if (input$visualizador_CP_precios == "Serie de tiempo") {
      DT::datatable(
        dat_CP_precios_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_CP_precios,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    } else if (input$visualizador_CP_precios %in% c("Anual gráfico", "Anual mapa")) {
      DT::datatable(
        dat_CP_precios_a(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_CP_precios,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    }
  })
  
  # Descarga tabla
  output$dl_tabla_dat_CP_precios <- downloadHandler(
    filename = function() {
      paste("resultados-", input$indicador_CP_precios, ".xlsx", sep = "")
    },
    content = function(file) {
      if (input$visualizador_CP_precios == "Serie de tiempo") {
        openxlsx::write.xlsx(list_dat_CP_precios_st(), file)
        
      } else if (input$visualizador_CP_precios %in% c("Anual gráfico", "Anual mapa")) {
        openxlsx::write.xlsx(list_dat_CP_precios_a(), file)
        
      }
    }
  )
  
  ##  5.  DE_crec (dat_DE_crec)   ====================================
  
  # Data DE_crec
  
  dat_DE_crec <- reactive({
    
    req(input$indicador_DE_crec)
    
    data %>%
      filter(nomindicador == input$indicador_DE_crec) %>% 
      janitor::remove_empty("cols")
    
  })
  
  
  # Titulo
  output$title_dat_DE_crec <- renderUI({
    helpText(HTML(unique(dat_DE_crec()$nomindicador)))
  })
  
  # Subtitulo
  output$subtitle_dat_DE_crec <- renderUI({
    helpText(HTML(unique(dat_DE_crec()$definicion)))
  })
  
  # Metodología
  output$info_dat_DE_crec <- renderUI({
    helpText(HTML(
      paste(
        "<b>Método de Agregación:</b>",
        unique(dat_DE_crec()$metodo_de_agregacion),
        "<b>Metodología:</b>",
        unique(dat_DE_crec()$concepto_estadistico_y_metodologia)
      )
    ))
  })
  
  # Relevancia:
  output$rel_dat_DE_crec <- renderUI({
    helpText(HTML(paste(
      "<b>Relvancia:</b>", unique(dat_DE_crec()$relevancia)
    )))
  })
  
  
  output$fecha_dat_DE_crec <- renderUI({
    if (input$visualizador_DE_crec == "Serie de tiempo") {
      tagList(
        tags$style(
          type = 'text/css',
          '#big_slider .irs-grid-text {font-size: 10px; transform: rotate(-90deg) translate(-15px);} ,.irs-grid-pol.large {height: 0px;}'
        ),
        div(
          id = 'big_slider',
          
          sliderTextInput(
            inputId = "fecha_dat_DE_crec",
            label = "Seleccione años",
            choices = sort(unique(dat_DE_crec()$fecha)),
            selected = c(min(dat_DE_crec()$fecha),
                         max(dat_DE_crec()$fecha)),
            hide_min_max = T,
            grid = TRUE,
            width = "100%"
          )
        )
      )
      
    } else {
      selectInput(
        inputId = "fecha_DE_crec",
        label = "Seleccione año:",
        choices = dat_DE_crec() %>%
          drop_na(valor) %>%
          select(fecha) %>%
          arrange(desc(fecha)) %>%
          unique() %>%
          pull(),
        selected = "2019"
      )
      
    }
    
  })
  
  
  output$corte_DE_crec <- renderUI({
    
    if (input$indicador_DE_crec %in% vars_corte) {
      
      selectInput(inputId = "corte_DE_crec",
                  label = "Seleccione categorías",
                  choices =  dat_DE_crec() %>%
                    distinct(get(names(dat_DE_crec()[,ncol(dat_DE_crec())]))) %>%
                    pull(),
                  selected = dat_DE_crec() %>%
                    filter(jerarquia == 1) %>% 
                    distinct(get(names(dat_DE_crec()[,ncol(dat_DE_crec())]))) %>%
                    pull()
      )
    } else {
      
      return(NULL)
    }
    
  })
  
  
  # Checkbox por pais
  output$sel_DE_crec_pais <- renderUI({
    if (input$visualizador_DE_crec == "Serie de tiempo") {
      dropdown(
        label = "Seleccione países",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_pais_DE_crec",
          label = "Seleccione países",
          inline = TRUE,
          choices = dat_DE_crec() %>%
            filter(region == 0) %>%
            filter(nomindicador == input$indicador_DE_crec) %>%
            distinct(cod_pais) %>%
            pull(),
          selected = c("URY", "ARG", "BRA", "PRY")
        )
      )
      
    } else {
      checkboxGroupInput(
        inputId = "chbox_pais_reg_DE_crec",
        label = "Mostrar:",
        inline = FALSE,
        choices = c("Países", "Regiones"),
        selected = "Países"
      )
      
    }
    
  })
  
  # Checkbox por región
  output$sel_DE_crec_region <- renderUI({
    if (input$visualizador_DE_crec == "Serie de tiempo") {
      dropdown(
        label = "Seleccione regiones",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_reg_DE_crec",
          label = "Seleccione regiones",
          inline = TRUE,
          choices = dat_DE_crec() %>%
            filter(region == 1) %>%
            filter(nomindicador == input$indicador_DE_crec) %>%
            distinct(pais) %>%
            pull(),
          selected = NULL
        )
      )
      
    } else {
      return(NULL)
      
    }
  })
  
  
  dat_DE_crec_anual <- reactive({
    
    if (input$indicador_DE_crec %in% vars_corte) {
      
      dat_DE_crec() %>%
        filter(get(names(dat_DE_crec()[,ncol(dat_DE_crec())])) %in% input$corte_DE_crec) %>%
        filter(fecha == input$fecha_DE_crec)
      
    } else {
      
      dat_DE_crec() %>%
        filter(fecha == input$fecha_DE_crec)
      
      
    }
  })
  
  dat_DE_crec_simple <- reactive({
    
    if (input$indicador_DE_crec %in% vars_corte) {
      
      dat_DE_crec() %>%
        filter(get(names(dat_DE_crec()[,ncol(dat_DE_crec())])) %in% input$corte_DE_crec) %>% 
        filter(fecha >= input$fecha_dat_DE_crec[1] & fecha <= input$fecha_dat_DE_crec[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_crec | pais %in% input$chbox_reg_DE_crec)
      
    } else {
      
      dat_DE_crec() %>%
        filter(fecha >= input$fecha_dat_DE_crec[1] & fecha <= input$fecha_dat_DE_crec[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_crec | pais %in% input$chbox_reg_DE_crec)        
    }
    
  })
  
  
  # Gráficos DE_crec
  output$p_dat_DE_crec <- renderPlot({
    
    if (input$visualizador_DE_crec == "Serie de tiempo" ) {
      
      req(input$fecha_dat_DE_crec, input$indicador_DE_crec)
      
      plot_DE_crec <- ggplot(
        data = dat_DE_crec_simple(), 
        aes(x = fecha, y = valor)
      ) +
        geom_line(aes(color = pais), size = 1, alpha = 0.5) +
        geom_point(aes(color = pais), size = 3) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_DE_crec_simple()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        scale_colour_manual(name = "", values = paleta_expandida) +
        if(input$indicador_DE_crec %in% vars_corte){
          
          ggtitle(paste0(input$indicador_DE_crec, " (", input$corte_DE_crec, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_DE_crec))
          
        }
      
      print(plot_DE_crec)
      ggsave(
        "www/indicador DE_crec.png",
        width = 30,
        height = 20,
        units = "cm"
      )
      
    } else if (input$visualizador_DE_crec == "Anual gráfico") {
      
      base_plot_DE_crec <- dat_DE_crec_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_DE_crec)
      
      validate(need(
        nrow(base_plot_DE_crec) > 0,
        'No hay datos disponible para esta búsqueda'
      ))
      
      plot_DE_crec <- ggplot(base_plot_DE_crec,
                                aes(x = fct_reorder(pais, valor), y = valor)) +
        geom_segment(
          aes(
            x = fct_reorder(pais, valor),
            xend = fct_reorder(pais, valor),
            y = 0,
            yend = valor
          ),
          size = 1,
          color = "#2c3e50"
        ) +
        geom_point(color = "#2c3e50", size = 4) +
        theme_light() +
        coord_flip() +
        theme(
          axis.text.y = element_text(size = 12),
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_DE_crec_anual()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        if(input$indicador_DE_crec %in% vars_corte){
          
          ggtitle(paste0(input$indicador_DE_crec, " (", input$corte_DE_crec, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_DE_crec))
          
        }
      
      print(plot_DE_crec)
      ggsave(
        "www/indicador DE_crec.png",
        width = 25,
        height = 30,
        units = "cm"
      )
      
    }
    
  })
  
  output$map_DE_crec <- renderLeaflet({
    # Read this shape file with the rgdal library.
    world_spdf <- readOGR(dsn = "data" ,
                          layer = "TM_WORLD_BORDERS_SIMPL-0.3",
                          verbose = FALSE)
    
    # Pegar datos
    world_spdf@data <- world_spdf@data %>%
      left_join(select(dat_DE_crec_anual(), pais_eng, valor_original),
                by = c("NAME" = "pais_eng"))
    
    # Library
    
    # Create a color palette with handmade bins.
    
    
    if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= -1) {
      round_number <- .1
      
    }
    
    min_val <-
      plyr::round_any(min(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      floor)
    max_val <-
      plyr::round_any(max(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      ceiling)
    dif <- max_val - min_val
    dif_5 <- dif / 5
    val_1 <- min_val + dif_5
    val_2 <- min_val + (dif_5 * 2)
    val_3 <- min_val + (dif_5 * 3)
    val_4 <- min_val + (dif_5 * 4)
    
    mybins <- c(min_val, val_1, val_2, val_3, val_4, max_val)
    
    mypalette <- colorBin(
      palette = "YlOrBr",
      domain = world_spdf@data$valor_original,
      na.color = "transparent",
      bins = mybins
    )
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "País: ",
      world_spdf@data$NAME,
      "<br/>",
      "Valor: ",
      round(world_spdf@data$valor_original, 2),
      sep = ""
    ) %>%
      lapply(htmltools::HTML)
    
    # Final Map
    leaflet(world_spdf) %>%
      addTiles()  %>%
      setView(lat = 30,
              lng = 0 ,
              zoom = 1) %>%
      addPolygons(
        fillColor = ~ mypalette(valor_original),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = mypalette,
        values =  ~ valor_original,
        opacity = 0.9,
        title = wrapit(input$indicador_DE_crec),
        position = "bottomleft"
      )
    
  })
  
  # Botón descarga grafico
  output$baja_plot_DE_crec <- downloadHandler(filename <- function() {
    paste("indicador DE_crec", "png", sep = ".")
  },
  
  content <- function(file) {
    file.copy("www/indicador DE_crec.png", file)
  },
  contentType = "www/indicador DE_crec")
  
  
  ## Data series temporal
  
  # Data para tabla y exportar
  dat_DE_crec_st <- reactive({
    
    if (input$indicador_DE_crec %in% vars_corte) {
      
      dat_DE_crec() %>%
        filter(fecha >= input$fecha_dat_DE_crec[1] & fecha <= input$fecha_dat_DE_crec[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_crec | pais %in% input$chbox_reg_DE_crec) %>%
        select(pais, fecha, names(dat_DE_crec()[,ncol(dat_DE_crec())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor)) 
      
    } else {
      
      dat_DE_crec() %>%
        filter(fecha >= input$fecha_dat_DE_crec[1] &
                 fecha <= input$fecha_dat_DE_crec[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_crec |
                 pais %in% input$chbox_reg_DE_crec) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
  })
  
  # Metadata
  dat_DE_crec_m <- reactive({
    dat_DE_crec() %>%
      select(
        nomindicador,
        definicion,
        metodo_de_agregacion,
        concepto_estadistico_y_metodologia,
        relevancia,
        limitaciones_y_excepciones
      ) %>%
      mutate(`Unidad de Métodos y Acceso a Datos (FCS -UdelaR)` = " ") %>%
      distinct() %>%
      gather(key = "", value = " ")
    
  })
  
  # Data completa
  dat_DE_crec_c <- reactive({
    
    if (input$indicador_DE_crec %in% vars_corte) {
      
      dat_DE_crec() %>%
        select(pais, fecha, names(dat_DE_crec()[,ncol(dat_DE_crec())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_DE_crec() %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
    
  })
  
  # Lista para descarga
  list_dat_DE_crec_st <- reactive({
    list_dat_DE_crec <- list(
      "Data" = dat_DE_crec_st(),
      "Metadata" = dat_DE_crec_m(),
      "Data Completa" = dat_DE_crec_c()
    )
  })
  
  
  ## Data por año
  # Data para tabla y exportar
  dat_DE_crec_a <- reactive({
    
    if (input$indicador_DE_crec %in% vars_corte) {
      
      dat_DE_crec_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_DE_crec) %>%
        select(pais, fecha, names(dat_DE_crec()[,ncol(dat_DE_crec())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_DE_crec_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_DE_crec) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    }
  })
  
  # Lista para descarga
  list_dat_DE_crec_a <- reactive({
    list_dat_DE_crec <- list("Data" = dat_DE_crec_a(),
                             "Metadata" = dat_DE_crec_m(),
                             "Data Completa" = dat_DE_crec_c())
  })
  
  # Tablas en shiny
  output$tab_dat_DE_crec <- renderDT({
    
    if (input$indicador_DE_crec %in% vars_corte) {
      DT::datatable(
        dat_DE_crec_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Corte", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_DE_crec,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(4, '', mark = ",")
      
    } else if (input$visualizador_DE_crec == "Serie de tiempo") {
      DT::datatable(
        dat_DE_crec_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_DE_crec,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    } else if (input$visualizador_DE_crec %in% c("Anual gráfico", "Anual mapa")) {
      DT::datatable(
        dat_DE_crec_a(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_DE_crec,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    }
  })
  
  # Descarga tabla
  output$dl_tabla_dat_DE_crec <- downloadHandler(
    filename = function() {
      paste("resultados-", input$indicador_DE_crec, ".xlsx", sep = "")
    },
    content = function(file) {
      if (input$visualizador_DE_crec == "Serie de tiempo") {
        openxlsx::write.xlsx(list_dat_DE_crec_st(), file)
        
      } else if (input$visualizador_DE_crec %in% c("Anual gráfico", "Anual mapa")) {
        openxlsx::write.xlsx(list_dat_DE_crec_a(), file)
        
      }
    }
  )
  
  ##  6.  DE_prod (dat_DE_prod)   ====================================
  
  # Data DE_prod
  
  dat_DE_prod <- reactive({
    
    req(input$indicador_DE_prod)
    
    data %>%
      filter(nomindicador == input$indicador_DE_prod) %>% 
      janitor::remove_empty("cols")
    
  })
  
  
  # Titulo
  output$title_dat_DE_prod <- renderUI({
    helpText(HTML(unique(dat_DE_prod()$nomindicador)))
  })
  
  # Subtitulo
  output$subtitle_dat_DE_prod <- renderUI({
    helpText(HTML(unique(dat_DE_prod()$definicion)))
  })
  
  # Metodología
  output$info_dat_DE_prod <- renderUI({
    helpText(HTML(
      paste(
        "<b>Método de Agregación:</b>",
        unique(dat_DE_prod()$metodo_de_agregacion),
        "<b>Metodología:</b>",
        unique(dat_DE_prod()$concepto_estadistico_y_metodologia)
      )
    ))
  })
  
  # Relevancia:
  output$rel_dat_DE_prod <- renderUI({
    helpText(HTML(paste(
      "<b>Relvancia:</b>", unique(dat_DE_prod()$relevancia)
    )))
  })
  
  
  output$fecha_dat_DE_prod <- renderUI({
    if (input$visualizador_DE_prod == "Serie de tiempo") {
      tagList(
        tags$style(
          type = 'text/css',
          '#big_slider .irs-grid-text {font-size: 10px; transform: rotate(-90deg) translate(-15px);} ,.irs-grid-pol.large {height: 0px;}'
        ),
        div(
          id = 'big_slider',
          
          sliderTextInput(
            inputId = "fecha_dat_DE_prod",
            label = "Seleccione años",
            choices = sort(unique(dat_DE_prod()$fecha)),
            selected = c(min(dat_DE_prod()$fecha),
                         max(dat_DE_prod()$fecha)),
            hide_min_max = T,
            grid = TRUE,
            width = "100%"
          )
        )
      )
      
    } else {
      selectInput(
        inputId = "fecha_DE_prod",
        label = "Seleccione año:",
        choices = dat_DE_prod() %>%
          drop_na(valor) %>%
          select(fecha) %>%
          arrange(desc(fecha)) %>%
          unique() %>%
          pull(),
        selected = "2019"
      )
      
    }
    
  })
  
  
  output$corte_DE_prod <- renderUI({
    
    if (input$indicador_DE_prod %in% vars_corte) {
      
      selectInput(inputId = "corte_DE_prod",
                  label = "Seleccione categorías",
                  choices =  dat_DE_prod() %>%
                    distinct(get(names(dat_DE_prod()[,ncol(dat_DE_prod())]))) %>%
                    pull(),
                  selected = dat_DE_prod() %>%
                    filter(jerarquia == 1) %>% 
                    distinct(get(names(dat_DE_prod()[,ncol(dat_DE_prod())]))) %>%
                    pull()
      )
    } else {
      
      return(NULL)
    }
    
  })
  
  
  # Checkbox por pais
  output$sel_DE_prod_pais <- renderUI({
    if (input$visualizador_DE_prod == "Serie de tiempo") {
      dropdown(
        label = "Seleccione países",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_pais_DE_prod",
          label = "Seleccione países",
          inline = TRUE,
          choices = dat_DE_prod() %>%
            filter(region == 0) %>%
            filter(nomindicador == input$indicador_DE_prod) %>%
            distinct(cod_pais) %>%
            pull(),
          selected = c("URY", "ARG", "BRA", "PRY")
        )
      )
      
    } else {
      checkboxGroupInput(
        inputId = "chbox_pais_reg_DE_prod",
        label = "Mostrar:",
        inline = FALSE,
        choices = c("Países", "Regiones"),
        selected = "Países"
      )
      
    }
    
  })
  
  # Checkbox por región
  output$sel_DE_prod_region <- renderUI({
    if (input$visualizador_DE_prod == "Serie de tiempo") {
      dropdown(
        label = "Seleccione regiones",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_reg_DE_prod",
          label = "Seleccione regiones",
          inline = TRUE,
          choices = dat_DE_prod() %>%
            filter(region == 1) %>%
            filter(nomindicador == input$indicador_DE_prod) %>%
            distinct(pais) %>%
            pull(),
          selected = NULL
        )
      )
      
    } else {
      return(NULL)
      
    }
  })
  
  
  dat_DE_prod_anual <- reactive({
    
    if (input$indicador_DE_prod %in% vars_corte) {
      
      dat_DE_prod() %>%
        filter(get(names(dat_DE_prod()[,ncol(dat_DE_prod())])) %in% input$corte_DE_prod) %>%
        filter(fecha == input$fecha_DE_prod)
      
    } else {
      
      dat_DE_prod() %>%
        filter(fecha == input$fecha_DE_prod)
      
      
    }
  })
  
  dat_DE_prod_simple <- reactive({
    
    if (input$indicador_DE_prod %in% vars_corte) {
      
      dat_DE_prod() %>%
        filter(get(names(dat_DE_prod()[,ncol(dat_DE_prod())])) %in% input$corte_DE_prod) %>% 
        filter(fecha >= input$fecha_dat_DE_prod[1] & fecha <= input$fecha_dat_DE_prod[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_prod | pais %in% input$chbox_reg_DE_prod)
      
    } else {
      
      dat_DE_prod() %>%
        filter(fecha >= input$fecha_dat_DE_prod[1] & fecha <= input$fecha_dat_DE_prod[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_prod | pais %in% input$chbox_reg_DE_prod)        
    }
    
  })
  
  
  # Gráficos DE_prod
  output$p_dat_DE_prod <- renderPlot({
    
    if (input$visualizador_DE_prod == "Serie de tiempo" ) {
      
      req(input$fecha_dat_DE_prod, input$indicador_DE_prod)
      
      plot_DE_prod <- ggplot(
        data = dat_DE_prod_simple(), 
        aes(x = fecha, y = valor)
      ) +
        geom_line(aes(color = pais), size = 1, alpha = 0.5) +
        geom_point(aes(color = pais), size = 3) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_DE_prod_simple()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        scale_colour_manual(name = "", values = paleta_expandida) +
        if(input$indicador_DE_prod %in% vars_corte){
          
          ggtitle(paste0(input$indicador_DE_prod, " (", input$corte_DE_prod, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_DE_prod))
          
        }
      
      print(plot_DE_prod)
      ggsave(
        "www/indicador DE_prod.png",
        width = 30,
        height = 20,
        units = "cm"
      )
      
    } else if (input$visualizador_DE_prod == "Anual gráfico") {
      
      base_plot_DE_prod <- dat_DE_prod_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_DE_prod)
      
      validate(need(
        nrow(base_plot_DE_prod) > 0,
        'No hay datos disponible para esta búsqueda'
      ))
      
      plot_DE_prod <- ggplot(base_plot_DE_prod,
                             aes(x = fct_reorder(pais, valor), y = valor)) +
        geom_segment(
          aes(
            x = fct_reorder(pais, valor),
            xend = fct_reorder(pais, valor),
            y = 0,
            yend = valor
          ),
          size = 1,
          color = "#2c3e50"
        ) +
        geom_point(color = "#2c3e50", size = 4) +
        theme_light() +
        coord_flip() +
        theme(
          axis.text.y = element_text(size = 12),
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_DE_prod_anual()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        if(input$indicador_DE_prod %in% vars_corte){
          
          ggtitle(paste0(input$indicador_DE_prod, " (", input$corte_DE_prod, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_DE_prod))
          
        }
      
      print(plot_DE_prod)
      ggsave(
        "www/indicador DE_prod.png",
        width = 25,
        height = 30,
        units = "cm"
      )
      
    }
    
  })
  
  output$map_DE_prod <- renderLeaflet({
    # Read this shape file with the rgdal library.
    world_spdf <- readOGR(dsn = "data" ,
                          layer = "TM_WORLD_BORDERS_SIMPL-0.3",
                          verbose = FALSE)
    
    # Pegar datos
    world_spdf@data <- world_spdf@data %>%
      left_join(select(dat_DE_prod_anual(), pais_eng, valor_original),
                by = c("NAME" = "pais_eng"))
    
    # Library
    
    # Create a color palette with handmade bins.
    
    
    if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= -1) {
      round_number <- .1
      
    }
    
    min_val <-
      plyr::round_any(min(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      floor)
    max_val <-
      plyr::round_any(max(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      ceiling)
    dif <- max_val - min_val
    dif_5 <- dif / 5
    val_1 <- min_val + dif_5
    val_2 <- min_val + (dif_5 * 2)
    val_3 <- min_val + (dif_5 * 3)
    val_4 <- min_val + (dif_5 * 4)
    
    mybins <- c(min_val, val_1, val_2, val_3, val_4, max_val)
    
    mypalette <- colorBin(
      palette = "YlOrBr",
      domain = world_spdf@data$valor_original,
      na.color = "transparent",
      bins = mybins
    )
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "País: ",
      world_spdf@data$NAME,
      "<br/>",
      "Valor: ",
      round(world_spdf@data$valor_original, 2),
      sep = ""
    ) %>%
      lapply(htmltools::HTML)
    
    # Final Map
    leaflet(world_spdf) %>%
      addTiles()  %>%
      setView(lat = 30,
              lng = 0 ,
              zoom = 1) %>%
      addPolygons(
        fillColor = ~ mypalette(valor_original),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = mypalette,
        values =  ~ valor_original,
        opacity = 0.9,
        title = wrapit(input$indicador_DE_prod),
        position = "bottomleft"
      )
    
  })
  
  # Botón descarga grafico
  output$baja_plot_DE_prod <- downloadHandler(filename <- function() {
    paste("indicador DE_prod", "png", sep = ".")
  },
  
  content <- function(file) {
    file.copy("www/indicador DE_prod.png", file)
  },
  contentType = "www/indicador DE_prod")
  
  
  ## Data series temporal
  
  # Data para tabla y exportar
  dat_DE_prod_st <- reactive({
    
    if (input$indicador_DE_prod %in% vars_corte) {
      
      dat_DE_prod() %>%
        filter(fecha >= input$fecha_dat_DE_prod[1] & fecha <= input$fecha_dat_DE_prod[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_prod | pais %in% input$chbox_reg_DE_prod) %>%
        select(pais, fecha, names(dat_DE_prod()[,ncol(dat_DE_prod())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor)) 
      
    } else {
      
      dat_DE_prod() %>%
        filter(fecha >= input$fecha_dat_DE_prod[1] &
                 fecha <= input$fecha_dat_DE_prod[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_prod |
                 pais %in% input$chbox_reg_DE_prod) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
  })
  
  # Metadata
  dat_DE_prod_m <- reactive({
    dat_DE_prod() %>%
      select(
        nomindicador,
        definicion,
        metodo_de_agregacion,
        concepto_estadistico_y_metodologia,
        relevancia,
        limitaciones_y_excepciones
      ) %>%
      mutate(`Unidad de Métodos y Acceso a Datos (FCS -UdelaR)` = " ") %>%
      distinct() %>%
      gather(key = "", value = " ")
    
  })
  
  # Data completa
  dat_DE_prod_c <- reactive({
    
    if (input$indicador_DE_prod %in% vars_corte) {
      
      dat_DE_prod() %>%
        select(pais, fecha, names(dat_DE_prod()[,ncol(dat_DE_prod())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_DE_prod() %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
    
  })
  
  # Lista para descarga
  list_dat_DE_prod_st <- reactive({
    list_dat_DE_prod <- list(
      "Data" = dat_DE_prod_st(),
      "Metadata" = dat_DE_prod_m(),
      "Data Completa" = dat_DE_prod_c()
    )
  })
  
  
  ## Data por año
  # Data para tabla y exportar
  dat_DE_prod_a <- reactive({
    
    if (input$indicador_DE_prod %in% vars_corte) {
      
      dat_DE_prod_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_DE_prod) %>%
        select(pais, fecha, names(dat_DE_prod()[,ncol(dat_DE_prod())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_DE_prod_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_DE_prod) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    }
  })
  
  # Lista para descarga
  list_dat_DE_prod_a <- reactive({
    list_dat_DE_prod <- list("Data" = dat_DE_prod_a(),
                             "Metadata" = dat_DE_prod_m(),
                             "Data Completa" = dat_DE_prod_c())
  })
  
  # Tablas en shiny
  output$tab_dat_DE_prod <- renderDT({
    
    if (input$indicador_DE_prod %in% vars_corte) {
      DT::datatable(
        dat_DE_prod_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Corte", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_DE_prod,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(4, '', mark = ",")
      
    } else if (input$visualizador_DE_prod == "Serie de tiempo") {
      DT::datatable(
        dat_DE_prod_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_DE_prod,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    } else if (input$visualizador_DE_prod %in% c("Anual gráfico", "Anual mapa")) {
      DT::datatable(
        dat_DE_prod_a(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_DE_prod,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    }
  })
  
  # Descarga tabla
  output$dl_tabla_dat_DE_prod <- downloadHandler(
    filename = function() {
      paste("resultados-", input$indicador_DE_prod, ".xlsx", sep = "")
    },
    content = function(file) {
      if (input$visualizador_DE_prod == "Serie de tiempo") {
        openxlsx::write.xlsx(list_dat_DE_prod_st(), file)
        
      } else if (input$visualizador_DE_prod %in% c("Anual gráfico", "Anual mapa")) {
        openxlsx::write.xlsx(list_dat_DE_prod_a(), file)
        
      }
    }
  )
  
  
  ##  7.  DE_estr (dat_DE_estr)   ====================================
  
  # Data DE_estr
  
  dat_DE_estr <- reactive({
    
    req(input$indicador_DE_estr)
    
    data %>%
      filter(nomindicador == input$indicador_DE_estr) %>% 
      janitor::remove_empty("cols")
    
  })
  
  
  # Titulo
  output$title_dat_DE_estr <- renderUI({
    helpText(HTML(unique(dat_DE_estr()$nomindicador)))
  })
  
  # Subtitulo
  output$subtitle_dat_DE_estr <- renderUI({
    helpText(HTML(unique(dat_DE_estr()$definicion)))
  })
  
  # Metodología
  output$info_dat_DE_estr <- renderUI({
    helpText(HTML(
      paste(
        "<b>Método de Agregación:</b>",
        unique(dat_DE_estr()$metodo_de_agregacion),
        "<b>Metodología:</b>",
        unique(dat_DE_estr()$concepto_estadistico_y_metodologia)
      )
    ))
  })
  
  # Relevancia:
  output$rel_dat_DE_estr <- renderUI({
    helpText(HTML(paste(
      "<b>Relvancia:</b>", unique(dat_DE_estr()$relevancia)
    )))
  })
  
  
  output$fecha_dat_DE_estr <- renderUI({
    if (input$visualizador_DE_estr == "Serie de tiempo") {
      tagList(
        tags$style(
          type = 'text/css',
          '#big_slider .irs-grid-text {font-size: 10px; transform: rotate(-90deg) translate(-15px);} ,.irs-grid-pol.large {height: 0px;}'
        ),
        div(
          id = 'big_slider',
          
          sliderTextInput(
            inputId = "fecha_dat_DE_estr",
            label = "Seleccione años",
            choices = sort(unique(dat_DE_estr()$fecha)),
            selected = c(min(dat_DE_estr()$fecha),
                         max(dat_DE_estr()$fecha)),
            hide_min_max = T,
            grid = TRUE,
            width = "100%"
          )
        )
      )
      
    } else {
      selectInput(
        inputId = "fecha_DE_estr",
        label = "Seleccione año:",
        choices = dat_DE_estr() %>%
          drop_na(valor) %>%
          select(fecha) %>%
          arrange(desc(fecha)) %>%
          unique() %>%
          pull(),
        selected = "2019"
      )
      
    }
    
  })
  
  
  output$corte_DE_estr <- renderUI({
    
    if (input$indicador_DE_estr %in% vars_corte) {
      
      selectInput(inputId = "corte_DE_estr",
                  label = "Seleccione categorías",
                  choices =  dat_DE_estr() %>%
                    distinct(get(names(dat_DE_estr()[,ncol(dat_DE_estr())]))) %>%
                    pull(),
                  selected = dat_DE_estr() %>%
                    filter(jerarquia == 1) %>% 
                    distinct(get(names(dat_DE_estr()[,ncol(dat_DE_estr())]))) %>%
                    pull()
      )
    } else {
      
      return(NULL)
    }
    
  })
  
  
  # Checkbox por pais
  output$sel_DE_estr_pais <- renderUI({
    if (input$visualizador_DE_estr == "Serie de tiempo") {
      dropdown(
        label = "Seleccione países",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_pais_DE_estr",
          label = "Seleccione países",
          inline = TRUE,
          choices = dat_DE_estr() %>%
            filter(region == 0) %>%
            filter(nomindicador == input$indicador_DE_estr) %>%
            distinct(cod_pais) %>%
            pull(),
          selected = c("URY", "ARG", "BRA", "PRY")
        )
      )
      
    } else {
      checkboxGroupInput(
        inputId = "chbox_pais_reg_DE_estr",
        label = "Mostrar:",
        inline = FALSE,
        choices = c("Países", "Regiones"),
        selected = "Países"
      )
      
    }
    
  })
  
  # Checkbox por región
  output$sel_DE_estr_region <- renderUI({
    if (input$visualizador_DE_estr == "Serie de tiempo") {
      dropdown(
        label = "Seleccione regiones",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_reg_DE_estr",
          label = "Seleccione regiones",
          inline = TRUE,
          choices = dat_DE_estr() %>%
            filter(region == 1) %>%
            filter(nomindicador == input$indicador_DE_estr) %>%
            distinct(pais) %>%
            pull(),
          selected = NULL
        )
      )
      
    } else {
      return(NULL)
      
    }
  })
  
  
  dat_DE_estr_anual <- reactive({
    
    if (input$indicador_DE_estr %in% vars_corte) {
      
      dat_DE_estr() %>%
        filter(get(names(dat_DE_estr()[,ncol(dat_DE_estr())])) %in% input$corte_DE_estr) %>%
        filter(fecha == input$fecha_DE_estr)
      
    } else {
      
      dat_DE_estr() %>%
        filter(fecha == input$fecha_DE_estr)
      
      
    }
  })
  
  dat_DE_estr_simple <- reactive({
    
    if (input$indicador_DE_estr %in% vars_corte) {
      
      dat_DE_estr() %>%
        filter(get(names(dat_DE_estr()[,ncol(dat_DE_estr())])) %in% input$corte_DE_estr) %>% 
        filter(fecha >= input$fecha_dat_DE_estr[1] & fecha <= input$fecha_dat_DE_estr[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_estr | pais %in% input$chbox_reg_DE_estr)
      
    } else {
      
      dat_DE_estr() %>%
        filter(fecha >= input$fecha_dat_DE_estr[1] & fecha <= input$fecha_dat_DE_estr[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_estr | pais %in% input$chbox_reg_DE_estr)        
    }
    
  })
  
  
  # Gráficos DE_estr
  output$p_dat_DE_estr <- renderPlot({
    
    if (input$visualizador_DE_estr == "Serie de tiempo" ) {
      
      req(input$fecha_dat_DE_estr, input$indicador_DE_estr)
      
      plot_DE_estr <- ggplot(
        data = dat_DE_estr_simple(), 
        aes(x = fecha, y = valor)
      ) +
        geom_line(aes(color = pais), size = 1, alpha = 0.5) +
        geom_point(aes(color = pais), size = 3) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_DE_estr_simple()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        scale_colour_manual(name = "", values = paleta_expandida) +
        if(input$indicador_DE_estr %in% vars_corte){
          
          ggtitle(paste0(input$indicador_DE_estr, " (", input$corte_DE_estr, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_DE_estr))
          
        }
      
      print(plot_DE_estr)
      ggsave(
        "www/indicador DE_estr.png",
        width = 30,
        height = 20,
        units = "cm"
      )
      
    } else if (input$visualizador_DE_estr == "Anual gráfico") {
      
      base_plot_DE_estr <- dat_DE_estr_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_DE_estr)
      
      validate(need(
        nrow(base_plot_DE_estr) > 0,
        'No hay datos disponible para esta búsqueda'
      ))
      
      plot_DE_estr <- ggplot(base_plot_DE_estr,
                             aes(x = fct_reorder(pais, valor), y = valor)) +
        geom_segment(
          aes(
            x = fct_reorder(pais, valor),
            xend = fct_reorder(pais, valor),
            y = 0,
            yend = valor
          ),
          size = 1,
          color = "#2c3e50"
        ) +
        geom_point(color = "#2c3e50", size = 4) +
        theme_light() +
        coord_flip() +
        theme(
          axis.text.y = element_text(size = 12),
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_DE_estr_anual()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        if(input$indicador_DE_estr %in% vars_corte){
          
          ggtitle(paste0(input$indicador_DE_estr, " (", input$corte_DE_estr, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_DE_estr))
          
        }
      
      print(plot_DE_estr)
      ggsave(
        "www/indicador DE_estr.png",
        width = 25,
        height = 30,
        units = "cm"
      )
      
    }
    
  })
  
  output$map_DE_estr <- renderLeaflet({
    # Read this shape file with the rgdal library.
    world_spdf <- readOGR(dsn = "data" ,
                          layer = "TM_WORLD_BORDERS_SIMPL-0.3",
                          verbose = FALSE)
    
    # Pegar datos
    world_spdf@data <- world_spdf@data %>%
      left_join(select(dat_DE_estr_anual(), pais_eng, valor_original),
                by = c("NAME" = "pais_eng"))
    
    # Library
    
    # Create a color palette with handmade bins.
    
    
    if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= -1) {
      round_number <- .1
      
    }
    
    min_val <-
      plyr::round_any(min(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      floor)
    max_val <-
      plyr::round_any(max(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      ceiling)
    dif <- max_val - min_val
    dif_5 <- dif / 5
    val_1 <- min_val + dif_5
    val_2 <- min_val + (dif_5 * 2)
    val_3 <- min_val + (dif_5 * 3)
    val_4 <- min_val + (dif_5 * 4)
    
    mybins <- c(min_val, val_1, val_2, val_3, val_4, max_val)
    
    mypalette <- colorBin(
      palette = "YlOrBr",
      domain = world_spdf@data$valor_original,
      na.color = "transparent",
      bins = mybins
    )
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "País: ",
      world_spdf@data$NAME,
      "<br/>",
      "Valor: ",
      round(world_spdf@data$valor_original, 2),
      sep = ""
    ) %>%
      lapply(htmltools::HTML)
    
    # Final Map
    leaflet(world_spdf) %>%
      addTiles()  %>%
      setView(lat = 30,
              lng = 0 ,
              zoom = 1) %>%
      addPolygons(
        fillColor = ~ mypalette(valor_original),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = mypalette,
        values =  ~ valor_original,
        opacity = 0.9,
        title = wrapit(input$indicador_DE_estr),
        position = "bottomleft"
      )
    
  })
  
  # Botón descarga grafico
  output$baja_plot_DE_estr <- downloadHandler(filename <- function() {
    paste("indicador DE_estr", "png", sep = ".")
  },
  
  content <- function(file) {
    file.copy("www/indicador DE_estr.png", file)
  },
  contentType = "www/indicador DE_estr")
  
  
  ## Data series temporal
  
  # Data para tabla y exportar
  dat_DE_estr_st <- reactive({
    
    if (input$indicador_DE_estr %in% vars_corte) {
      
      dat_DE_estr() %>%
        filter(fecha >= input$fecha_dat_DE_estr[1] & fecha <= input$fecha_dat_DE_estr[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_estr | pais %in% input$chbox_reg_DE_estr) %>%
        select(pais, fecha, names(dat_DE_estr()[,ncol(dat_DE_estr())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor)) 
      
    } else {
      
      dat_DE_estr() %>%
        filter(fecha >= input$fecha_dat_DE_estr[1] &
                 fecha <= input$fecha_dat_DE_estr[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_estr |
                 pais %in% input$chbox_reg_DE_estr) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
  })
  
  # Metadata
  dat_DE_estr_m <- reactive({
    dat_DE_estr() %>%
      select(
        nomindicador,
        definicion,
        metodo_de_agregacion,
        concepto_estadistico_y_metodologia,
        relevancia,
        limitaciones_y_excepciones
      ) %>%
      mutate(`Unidad de Métodos y Acceso a Datos (FCS -UdelaR)` = " ") %>%
      distinct() %>%
      gather(key = "", value = " ")
    
  })
  
  # Data completa
  dat_DE_estr_c <- reactive({
    
    if (input$indicador_DE_estr %in% vars_corte) {
      
      dat_DE_estr() %>%
        select(pais, fecha, names(dat_DE_estr()[,ncol(dat_DE_estr())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_DE_estr() %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
    
  })
  
  # Lista para descarga
  list_dat_DE_estr_st <- reactive({
    list_dat_DE_estr <- list(
      "Data" = dat_DE_estr_st(),
      "Metadata" = dat_DE_estr_m(),
      "Data Completa" = dat_DE_estr_c()
    )
  })
  
  
  ## Data por año
  # Data para tabla y exportar
  dat_DE_estr_a <- reactive({
    
    if (input$indicador_DE_estr %in% vars_corte) {
      
      dat_DE_estr_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_DE_estr) %>%
        select(pais, fecha, names(dat_DE_estr()[,ncol(dat_DE_estr())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_DE_estr_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_DE_estr) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    }
  })
  
  # Lista para descarga
  list_dat_DE_estr_a <- reactive({
    list_dat_DE_estr <- list("Data" = dat_DE_estr_a(),
                             "Metadata" = dat_DE_estr_m(),
                             "Data Completa" = dat_DE_estr_c())
  })
  
  # Tablas en shiny
  output$tab_dat_DE_estr <- renderDT({
    
    if (input$indicador_DE_estr %in% vars_corte) {
      DT::datatable(
        dat_DE_estr_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Corte", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_DE_estr,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(4, '', mark = ",")
      
    } else if (input$visualizador_DE_estr == "Serie de tiempo") {
      DT::datatable(
        dat_DE_estr_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_DE_estr,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    } else if (input$visualizador_DE_estr %in% c("Anual gráfico", "Anual mapa")) {
      DT::datatable(
        dat_DE_estr_a(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_DE_estr,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    }
  })
  
  # Descarga tabla
  output$dl_tabla_dat_DE_estr <- downloadHandler(
    filename = function() {
      paste("resultados-", input$indicador_DE_estr, ".xlsx", sep = "")
    },
    content = function(file) {
      if (input$visualizador_DE_estr == "Serie de tiempo") {
        openxlsx::write.xlsx(list_dat_DE_estr_st(), file)
        
      } else if (input$visualizador_DE_estr %in% c("Anual gráfico", "Anual mapa")) {
        openxlsx::write.xlsx(list_dat_DE_estr_a(), file)
        
      }
    }
  )
  
  ##  8.  DE_inv (dat_DE_inv)   ====================================
  
  # Data DE_inv
  
  dat_DE_inv <- reactive({
    
    req(input$indicador_DE_inv)
    
    data %>%
      filter(nomindicador == input$indicador_DE_inv) %>% 
      janitor::remove_empty("cols")
    
  })
  
  
  # Titulo
  output$title_dat_DE_inv <- renderUI({
    helpText(HTML(unique(dat_DE_inv()$nomindicador)))
  })
  
  # Subtitulo
  output$subtitle_dat_DE_inv <- renderUI({
    helpText(HTML(unique(dat_DE_inv()$definicion)))
  })
  
  # Metodología
  output$info_dat_DE_inv <- renderUI({
    helpText(HTML(
      paste(
        "<b>Método de Agregación:</b>",
        unique(dat_DE_inv()$metodo_de_agregacion),
        "<b>Metodología:</b>",
        unique(dat_DE_inv()$concepto_estadistico_y_metodologia)
      )
    ))
  })
  
  # Relevancia:
  output$rel_dat_DE_inv <- renderUI({
    helpText(HTML(paste(
      "<b>Relvancia:</b>", unique(dat_DE_inv()$relevancia)
    )))
  })
  
  
  output$fecha_dat_DE_inv <- renderUI({
    if (input$visualizador_DE_inv == "Serie de tiempo") {
      tagList(
        tags$style(
          type = 'text/css',
          '#big_slider .irs-grid-text {font-size: 10px; transform: rotate(-90deg) translate(-15px);} ,.irs-grid-pol.large {height: 0px;}'
        ),
        div(
          id = 'big_slider',
          
          sliderTextInput(
            inputId = "fecha_dat_DE_inv",
            label = "Seleccione años",
            choices = sort(unique(dat_DE_inv()$fecha)),
            selected = c(min(dat_DE_inv()$fecha),
                         max(dat_DE_inv()$fecha)),
            hide_min_max = T,
            grid = TRUE,
            width = "100%"
          )
        )
      )
      
    } else {
      selectInput(
        inputId = "fecha_DE_inv",
        label = "Seleccione año:",
        choices = dat_DE_inv() %>%
          drop_na(valor) %>%
          select(fecha) %>%
          arrange(desc(fecha)) %>%
          unique() %>%
          pull(),
        selected = "2019"
      )
      
    }
    
  })
  
  
  output$corte_DE_inv <- renderUI({
    
    if (input$indicador_DE_inv %in% vars_corte) {
      
      selectInput(inputId = "corte_DE_inv",
                  label = "Seleccione categorías",
                  choices =  dat_DE_inv() %>%
                    distinct(get(names(dat_DE_inv()[,ncol(dat_DE_inv())]))) %>%
                    pull(),
                  selected = dat_DE_inv() %>%
                    filter(jerarquia == 1) %>% 
                    distinct(get(names(dat_DE_inv()[,ncol(dat_DE_inv())]))) %>%
                    pull()
      )
    } else {
      
      return(NULL)
    }
    
  })
  
  
  # Checkbox por pais
  output$sel_DE_inv_pais <- renderUI({
    if (input$visualizador_DE_inv == "Serie de tiempo") {
      dropdown(
        label = "Seleccione países",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_pais_DE_inv",
          label = "Seleccione países",
          inline = TRUE,
          choices = dat_DE_inv() %>%
            filter(region == 0) %>%
            filter(nomindicador == input$indicador_DE_inv) %>%
            distinct(cod_pais) %>%
            pull(),
          selected = c("URY", "ARG", "BRA", "PRY")
        )
      )
      
    } else {
      checkboxGroupInput(
        inputId = "chbox_pais_reg_DE_inv",
        label = "Mostrar:",
        inline = FALSE,
        choices = c("Países", "Regiones"),
        selected = "Países"
      )
      
    }
    
  })
  
  # Checkbox por región
  output$sel_DE_inv_region <- renderUI({
    if (input$visualizador_DE_inv == "Serie de tiempo") {
      dropdown(
        label = "Seleccione regiones",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_reg_DE_inv",
          label = "Seleccione regiones",
          inline = TRUE,
          choices = dat_DE_inv() %>%
            filter(region == 1) %>%
            filter(nomindicador == input$indicador_DE_inv) %>%
            distinct(pais) %>%
            pull(),
          selected = NULL
        )
      )
      
    } else {
      return(NULL)
      
    }
  })
  
  
  dat_DE_inv_anual <- reactive({
    
    if (input$indicador_DE_inv %in% vars_corte) {
      
      dat_DE_inv() %>%
        filter(get(names(dat_DE_inv()[,ncol(dat_DE_inv())])) %in% input$corte_DE_inv) %>%
        filter(fecha == input$fecha_DE_inv)
      
    } else {
      
      dat_DE_inv() %>%
        filter(fecha == input$fecha_DE_inv)
      
      
    }
  })
  
  dat_DE_inv_simple <- reactive({
    
    if (input$indicador_DE_inv %in% vars_corte) {
      
      dat_DE_inv() %>%
        filter(get(names(dat_DE_inv()[,ncol(dat_DE_inv())])) %in% input$corte_DE_inv) %>% 
        filter(fecha >= input$fecha_dat_DE_inv[1] & fecha <= input$fecha_dat_DE_inv[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_inv | pais %in% input$chbox_reg_DE_inv)
      
    } else {
      
      dat_DE_inv() %>%
        filter(fecha >= input$fecha_dat_DE_inv[1] & fecha <= input$fecha_dat_DE_inv[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_inv | pais %in% input$chbox_reg_DE_inv)        
    }
    
  })
  
  
  # Gráficos DE_inv
  output$p_dat_DE_inv <- renderPlot({
    
    if (input$visualizador_DE_inv == "Serie de tiempo" ) {
      
      req(input$fecha_dat_DE_inv, input$indicador_DE_inv)
      
      plot_DE_inv <- ggplot(
        data = dat_DE_inv_simple(), 
        aes(x = fecha, y = valor)
      ) +
        geom_line(aes(color = pais), size = 1, alpha = 0.5) +
        geom_point(aes(color = pais), size = 3) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_DE_inv_simple()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        scale_colour_manual(name = "", values = paleta_expandida) +
        if(input$indicador_DE_inv %in% vars_corte){
          
          ggtitle(paste0(input$indicador_DE_inv, " (", input$corte_DE_inv, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_DE_inv))
          
        }
      
      print(plot_DE_inv)
      ggsave(
        "www/indicador DE_inv.png",
        width = 30,
        height = 20,
        units = "cm"
      )
      
    } else if (input$visualizador_DE_inv == "Anual gráfico") {
      
      base_plot_DE_inv <- dat_DE_inv_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_DE_inv)
      
      validate(need(
        nrow(base_plot_DE_inv) > 0,
        'No hay datos disponible para esta búsqueda'
      ))
      
      plot_DE_inv <- ggplot(base_plot_DE_inv,
                             aes(x = fct_reorder(pais, valor), y = valor)) +
        geom_segment(
          aes(
            x = fct_reorder(pais, valor),
            xend = fct_reorder(pais, valor),
            y = 0,
            yend = valor
          ),
          size = 1,
          color = "#2c3e50"
        ) +
        geom_point(color = "#2c3e50", size = 4) +
        theme_light() +
        coord_flip() +
        theme(
          axis.text.y = element_text(size = 12),
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_DE_inv_anual()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        if(input$indicador_DE_inv %in% vars_corte){
          
          ggtitle(paste0(input$indicador_DE_inv, " (", input$corte_DE_inv, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_DE_inv))
          
        }
      
      print(plot_DE_inv)
      ggsave(
        "www/indicador DE_inv.png",
        width = 25,
        height = 30,
        units = "cm"
      )
      
    }
    
  })
  
  output$map_DE_inv <- renderLeaflet({
    # Read this shape file with the rgdal library.
    world_spdf <- readOGR(dsn = "data" ,
                          layer = "TM_WORLD_BORDERS_SIMPL-0.3",
                          verbose = FALSE)
    
    # Pegar datos
    world_spdf@data <- world_spdf@data %>%
      left_join(select(dat_DE_inv_anual(), pais_eng, valor_original),
                by = c("NAME" = "pais_eng"))
    
    # Library
    
    # Create a color palette with handmade bins.
    
    
    if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= -1) {
      round_number <- .1
      
    }
    
    min_val <-
      plyr::round_any(min(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      floor)
    max_val <-
      plyr::round_any(max(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      ceiling)
    dif <- max_val - min_val
    dif_5 <- dif / 5
    val_1 <- min_val + dif_5
    val_2 <- min_val + (dif_5 * 2)
    val_3 <- min_val + (dif_5 * 3)
    val_4 <- min_val + (dif_5 * 4)
    
    mybins <- c(min_val, val_1, val_2, val_3, val_4, max_val)
    
    mypalette <- colorBin(
      palette = "YlOrBr",
      domain = world_spdf@data$valor_original,
      na.color = "transparent",
      bins = mybins
    )
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "País: ",
      world_spdf@data$NAME,
      "<br/>",
      "Valor: ",
      round(world_spdf@data$valor_original, 2),
      sep = ""
    ) %>%
      lapply(htmltools::HTML)
    
    # Final Map
    leaflet(world_spdf) %>%
      addTiles()  %>%
      setView(lat = 30,
              lng = 0 ,
              zoom = 1) %>%
      addPolygons(
        fillColor = ~ mypalette(valor_original),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = mypalette,
        values =  ~ valor_original,
        opacity = 0.9,
        title = wrapit(input$indicador_DE_inv),
        position = "bottomleft"
      )
    
  })
  
  # Botón descarga grafico
  output$baja_plot_DE_inv <- downloadHandler(filename <- function() {
    paste("indicador DE_inv", "png", sep = ".")
  },
  
  content <- function(file) {
    file.copy("www/indicador DE_inv.png", file)
  },
  contentType = "www/indicador DE_inv")
  
  
  ## Data series temporal
  
  # Data para tabla y exportar
  dat_DE_inv_st <- reactive({
    
    if (input$indicador_DE_inv %in% vars_corte) {
      
      dat_DE_inv() %>%
        filter(fecha >= input$fecha_dat_DE_inv[1] & fecha <= input$fecha_dat_DE_inv[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_inv | pais %in% input$chbox_reg_DE_inv) %>%
        select(pais, fecha, names(dat_DE_inv()[,ncol(dat_DE_inv())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor)) 
      
    } else {
      
      dat_DE_inv() %>%
        filter(fecha >= input$fecha_dat_DE_inv[1] &
                 fecha <= input$fecha_dat_DE_inv[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_inv |
                 pais %in% input$chbox_reg_DE_inv) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
  })
  
  # Metadata
  dat_DE_inv_m <- reactive({
    dat_DE_inv() %>%
      select(
        nomindicador,
        definicion,
        metodo_de_agregacion,
        concepto_estadistico_y_metodologia,
        relevancia,
        limitaciones_y_excepciones
      ) %>%
      mutate(`Unidad de Métodos y Acceso a Datos (FCS -UdelaR)` = " ") %>%
      distinct() %>%
      gather(key = "", value = " ")
    
  })
  
  # Data completa
  dat_DE_inv_c <- reactive({
    
    if (input$indicador_DE_inv %in% vars_corte) {
      
      dat_DE_inv() %>%
        select(pais, fecha, names(dat_DE_inv()[,ncol(dat_DE_inv())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_DE_inv() %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
    
  })
  
  # Lista para descarga
  list_dat_DE_inv_st <- reactive({
    list_dat_DE_inv <- list(
      "Data" = dat_DE_inv_st(),
      "Metadata" = dat_DE_inv_m(),
      "Data Completa" = dat_DE_inv_c()
    )
  })
  
  
  ## Data por año
  # Data para tabla y exportar
  dat_DE_inv_a <- reactive({
    
    if (input$indicador_DE_inv %in% vars_corte) {
      
      dat_DE_inv_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_DE_inv) %>%
        select(pais, fecha, names(dat_DE_inv()[,ncol(dat_DE_inv())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_DE_inv_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_DE_inv) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    }
  })
  
  # Lista para descarga
  list_dat_DE_inv_a <- reactive({
    list_dat_DE_inv <- list("Data" = dat_DE_inv_a(),
                             "Metadata" = dat_DE_inv_m(),
                            "Data Completa" = dat_DE_inv_c())
  })
  
  # Tablas en shiny
  output$tab_dat_DE_inv <- renderDT({
    
    if (input$indicador_DE_inv %in% vars_corte) {
      DT::datatable(
        dat_DE_inv_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Corte", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_DE_inv,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(4, '', mark = ",")
      
    } else if (input$visualizador_DE_inv == "Serie de tiempo") {
      DT::datatable(
        dat_DE_inv_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_DE_inv,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    } else if (input$visualizador_DE_inv %in% c("Anual gráfico", "Anual mapa")) {
      DT::datatable(
        dat_DE_inv_a(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_DE_inv,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    }
  })
  
  # Descarga tabla
  output$dl_tabla_dat_DE_inv <- downloadHandler(
    filename = function() {
      paste("resultados-", input$indicador_DE_inv, ".xlsx", sep = "")
    },
    content = function(file) {
      if (input$visualizador_DE_inv == "Serie de tiempo") {
        openxlsx::write.xlsx(list_dat_DE_inv_st(), file)
        
      } else if (input$visualizador_DE_inv %in% c("Anual gráfico", "Anual mapa")) {
        openxlsx::write.xlsx(list_dat_DE_inv_a(), file)
        
      }
    }
  )
  
  ##  9.  DE_ch (dat_DE_ch)   ====================================
  
  # Data DE_ch
  
  dat_DE_ch <- reactive({
    
    req(input$indicador_DE_ch)
    
    data %>%
      filter(nomindicador == input$indicador_DE_ch) %>% 
      janitor::remove_empty("cols")
    
  })
  
  
  # Titulo
  output$title_dat_DE_ch <- renderUI({
    helpText(HTML(unique(dat_DE_ch()$nomindicador)))
  })
  
  # Subtitulo
  output$subtitle_dat_DE_ch <- renderUI({
    helpText(HTML(unique(dat_DE_ch()$definicion)))
  })
  
  # Metodología
  output$info_dat_DE_ch <- renderUI({
    helpText(HTML(
      paste(
        "<b>Método de Agregación:</b>",
        unique(dat_DE_ch()$metodo_de_agregacion),
        "<b>Metodología:</b>",
        unique(dat_DE_ch()$concepto_estadistico_y_metodologia)
      )
    ))
  })
  
  # Relevancia:
  output$rel_dat_DE_ch <- renderUI({
    helpText(HTML(paste(
      "<b>Relvancia:</b>", unique(dat_DE_ch()$relevancia)
    )))
  })
  
  
  output$fecha_dat_DE_ch <- renderUI({
    if (input$visualizador_DE_ch == "Serie de tiempo") {
      tagList(
        tags$style(
          type = 'text/css',
          '#big_slider .irs-grid-text {font-size: 10px; transform: rotate(-90deg) translate(-15px);} ,.irs-grid-pol.large {height: 0px;}'
        ),
        div(
          id = 'big_slider',
          
          sliderTextInput(
            inputId = "fecha_dat_DE_ch",
            label = "Seleccione años",
            choices = sort(unique(dat_DE_ch()$fecha)),
            selected = c(min(dat_DE_ch()$fecha),
                         max(dat_DE_ch()$fecha)),
            hide_min_max = T,
            grid = TRUE,
            width = "100%"
          )
        )
      )
      
    } else {
      selectInput(
        inputId = "fecha_DE_ch",
        label = "Seleccione año:",
        choices = dat_DE_ch() %>%
          drop_na(valor) %>%
          select(fecha) %>%
          arrange(desc(fecha)) %>%
          unique() %>%
          pull(),
        selected = "2019"
      )
      
    }
    
  })
  
  
  output$corte_DE_ch <- renderUI({
    
    if (input$indicador_DE_ch %in% vars_corte) {
      
      selectInput(inputId = "corte_DE_ch",
                  label = "Seleccione categorías",
                  choices =  dat_DE_ch() %>%
                    distinct(get(names(dat_DE_ch()[,ncol(dat_DE_ch())]))) %>%
                    pull(),
                  selected = dat_DE_ch() %>%
                    filter(jerarquia == 1) %>% 
                    distinct(get(names(dat_DE_ch()[,ncol(dat_DE_ch())]))) %>%
                    pull()
      )
    } else {
      
      return(NULL)
    }
    
  })
  
  
  # Checkbox por pais
  output$sel_DE_ch_pais <- renderUI({
    if (input$visualizador_DE_ch == "Serie de tiempo") {
      dropdown(
        label = "Seleccione países",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_pais_DE_ch",
          label = "Seleccione países",
          inline = TRUE,
          choices = dat_DE_ch() %>%
            filter(region == 0) %>%
            filter(nomindicador == input$indicador_DE_ch) %>%
            distinct(cod_pais) %>%
            pull(),
          selected = c("URY", "ARG", "BRA", "PRY")
        )
      )
      
    } else {
      checkboxGroupInput(
        inputId = "chbox_pais_reg_DE_ch",
        label = "Mostrar:",
        inline = FALSE,
        choices = c("Países", "Regiones"),
        selected = "Países"
      )
      
    }
    
  })
  
  # Checkbox por región
  output$sel_DE_ch_region <- renderUI({
    if (input$visualizador_DE_ch == "Serie de tiempo") {
      dropdown(
        label = "Seleccione regiones",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_reg_DE_ch",
          label = "Seleccione regiones",
          inline = TRUE,
          choices = dat_DE_ch() %>%
            filter(region == 1) %>%
            filter(nomindicador == input$indicador_DE_ch) %>%
            distinct(pais) %>%
            pull(),
          selected = NULL
        )
      )
      
    } else {
      return(NULL)
      
    }
  })
  
  
  dat_DE_ch_anual <- reactive({
    
    if (input$indicador_DE_ch %in% vars_corte) {
      
      dat_DE_ch() %>%
        filter(get(names(dat_DE_ch()[,ncol(dat_DE_ch())])) %in% input$corte_DE_ch) %>%
        filter(fecha == input$fecha_DE_ch)
      
    } else {
      
      dat_DE_ch() %>%
        filter(fecha == input$fecha_DE_ch)
      
      
    }
  })
  
  dat_DE_ch_simple <- reactive({
    
    if (input$indicador_DE_ch %in% vars_corte) {
      
      dat_DE_ch() %>%
        filter(get(names(dat_DE_ch()[,ncol(dat_DE_ch())])) %in% input$corte_DE_ch) %>% 
        filter(fecha >= input$fecha_dat_DE_ch[1] & fecha <= input$fecha_dat_DE_ch[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_ch | pais %in% input$chbox_reg_DE_ch)
      
    } else {
      
      dat_DE_ch() %>%
        filter(fecha >= input$fecha_dat_DE_ch[1] & fecha <= input$fecha_dat_DE_ch[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_ch | pais %in% input$chbox_reg_DE_ch)        
    }
    
  })
  
  
  # Gráficos DE_ch
  output$p_dat_DE_ch <- renderPlot({
    
    if (input$visualizador_DE_ch == "Serie de tiempo" ) {
      
      req(input$fecha_dat_DE_ch, input$indicador_DE_ch)
      
      plot_DE_ch <- ggplot(
        data = dat_DE_ch_simple(), 
        aes(x = fecha, y = valor)
      ) +
        geom_line(aes(color = pais), size = 1, alpha = 0.5) +
        geom_point(aes(color = pais), size = 3) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_DE_ch_simple()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        scale_colour_manual(name = "", values = paleta_expandida) +
        if(input$indicador_DE_ch %in% vars_corte){
          
          ggtitle(paste0(input$indicador_DE_ch, " (", input$corte_DE_ch, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_DE_ch))
          
        }
      
      print(plot_DE_ch)
      ggsave(
        "www/indicador DE_ch.png",
        width = 30,
        height = 20,
        units = "cm"
      )
      
    } else if (input$visualizador_DE_ch == "Anual gráfico") {
      
      base_plot_DE_ch <- dat_DE_ch_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_DE_ch)
      
      validate(need(
        nrow(base_plot_DE_ch) > 0,
        'No hay datos disponible para esta búsqueda'
      ))
      
      plot_DE_ch <- ggplot(base_plot_DE_ch,
                            aes(x = fct_reorder(pais, valor), y = valor)) +
        geom_segment(
          aes(
            x = fct_reorder(pais, valor),
            xend = fct_reorder(pais, valor),
            y = 0,
            yend = valor
          ),
          size = 1,
          color = "#2c3e50"
        ) +
        geom_point(color = "#2c3e50", size = 4) +
        theme_light() +
        coord_flip() +
        theme(
          axis.text.y = element_text(size = 12),
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_DE_ch_anual()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        if(input$indicador_DE_ch %in% vars_corte){
          
          ggtitle(paste0(input$indicador_DE_ch, " (", input$corte_DE_ch, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_DE_ch))
          
        }
      
      print(plot_DE_ch)
      ggsave(
        "www/indicador DE_ch.png",
        width = 25,
        height = 30,
        units = "cm"
      )
      
    }
    
  })
  
  output$map_DE_ch <- renderLeaflet({
    # Read this shape file with the rgdal library.
    world_spdf <- readOGR(dsn = "data" ,
                          layer = "TM_WORLD_BORDERS_SIMPL-0.3",
                          verbose = FALSE)
    
    # Pegar datos
    world_spdf@data <- world_spdf@data %>%
      left_join(select(dat_DE_ch_anual(), pais_eng, valor_original),
                by = c("NAME" = "pais_eng"))
    
    # Library
    
    # Create a color palette with handmade bins.
    
    
    if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= -1) {
      round_number <- .1
      
    }
    
    min_val <-
      plyr::round_any(min(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      floor)
    max_val <-
      plyr::round_any(max(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      ceiling)
    dif <- max_val - min_val
    dif_5 <- dif / 5
    val_1 <- min_val + dif_5
    val_2 <- min_val + (dif_5 * 2)
    val_3 <- min_val + (dif_5 * 3)
    val_4 <- min_val + (dif_5 * 4)
    
    mybins <- c(min_val, val_1, val_2, val_3, val_4, max_val)
    
    mypalette <- colorBin(
      palette = "YlOrBr",
      domain = world_spdf@data$valor_original,
      na.color = "transparent",
      bins = mybins
    )
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "País: ",
      world_spdf@data$NAME,
      "<br/>",
      "Valor: ",
      round(world_spdf@data$valor_original, 2),
      sep = ""
    ) %>%
      lapply(htmltools::HTML)
    
    # Final Map
    leaflet(world_spdf) %>%
      addTiles()  %>%
      setView(lat = 30,
              lng = 0 ,
              zoom = 1) %>%
      addPolygons(
        fillColor = ~ mypalette(valor_original),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = mypalette,
        values =  ~ valor_original,
        opacity = 0.9,
        title = wrapit(input$indicador_DE_ch),
        position = "bottomleft"
      )
    
  })
  
  # Botón descarga grafico
  output$baja_plot_DE_ch <- downloadHandler(filename <- function() {
    paste("indicador DE_ch", "png", sep = ".")
  },
  
  content <- function(file) {
    file.copy("www/indicador DE_ch.png", file)
  },
  contentType = "www/indicador DE_ch")
  
  
  ## Data series temporal
  
  # Data para tabla y exportar
  dat_DE_ch_st <- reactive({
    
    if (input$indicador_DE_ch %in% vars_corte) {
      
      dat_DE_ch() %>%
        filter(fecha >= input$fecha_dat_DE_ch[1] & fecha <= input$fecha_dat_DE_ch[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_ch | pais %in% input$chbox_reg_DE_ch) %>%
        select(pais, fecha, names(dat_DE_ch()[,ncol(dat_DE_ch())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor)) 
      
    } else {
      
      dat_DE_ch() %>%
        filter(fecha >= input$fecha_dat_DE_ch[1] &
                 fecha <= input$fecha_dat_DE_ch[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_ch |
                 pais %in% input$chbox_reg_DE_ch) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
  })
  
  # Metadata
  dat_DE_ch_m <- reactive({
    dat_DE_ch() %>%
      select(
        nomindicador,
        definicion,
        metodo_de_agregacion,
        concepto_estadistico_y_metodologia,
        relevancia,
        limitaciones_y_excepciones
      ) %>%
      mutate(`Unidad de Métodos y Acceso a Datos (FCS -UdelaR)` = " ") %>%
      distinct() %>%
      gather(key = "", value = " ")
    
  })
  
  # Data completa
  dat_DE_ch_c <- reactive({
    
    if (input$indicador_DE_ch %in% vars_corte) {
      
      dat_DE_ch() %>%
        select(pais, fecha, names(dat_DE_ch()[,ncol(dat_DE_ch())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_DE_ch() %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
    
  })
  
  # Lista para descarga
  list_dat_DE_ch_st <- reactive({
    list_dat_DE_ch <- list(
      "Data" = dat_DE_ch_st(),
      "Metadata" = dat_DE_ch_m(),
      "Data Completa" = dat_DE_ch_c()
    )
  })
  
  
  ## Data por año
  # Data para tabla y exportar
  dat_DE_ch_a <- reactive({
    
    if (input$indicador_DE_ch %in% vars_corte) {
      
      dat_DE_ch_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_DE_ch) %>%
        select(pais, fecha, names(dat_DE_ch()[,ncol(dat_DE_ch())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_DE_ch_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_DE_ch) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    }
  })
  
  # Lista para descarga
  list_dat_DE_ch_a <- reactive({
    list_dat_DE_ch <- list("Data" = dat_DE_ch_a(),
                            "Metadata" = dat_DE_ch_m(),
                           "Data Completa" = dat_DE_ch_c())
  })
  
  # Tablas en shiny
  output$tab_dat_DE_ch <- renderDT({
    
    if (input$indicador_DE_ch %in% vars_corte) {
      DT::datatable(
        dat_DE_ch_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Corte", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_DE_ch,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(4, '', mark = ",")
      
    } else if (input$visualizador_DE_ch == "Serie de tiempo") {
      DT::datatable(
        dat_DE_ch_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_DE_ch,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    } else if (input$visualizador_DE_ch %in% c("Anual gráfico", "Anual mapa")) {
      DT::datatable(
        dat_DE_ch_a(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_DE_ch,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    }
  })
  
  # Descarga tabla
  output$dl_tabla_dat_DE_ch <- downloadHandler(
    filename = function() {
      paste("resultados-", input$indicador_DE_ch, ".xlsx", sep = "")
    },
    content = function(file) {
      if (input$visualizador_DE_ch == "Serie de tiempo") {
        openxlsx::write.xlsx(list_dat_DE_ch_st(), file)
        
      } else if (input$visualizador_DE_ch %in% c("Anual gráfico", "Anual mapa")) {
        openxlsx::write.xlsx(list_dat_DE_ch_a(), file)
        
      }
    }
  )
  
  
  
  ##  10.  DE_cti (dat_DE_cti)   ====================================
  
  # Data DE_cti
  
  dat_DE_cti <- reactive({
    
    req(input$indicador_DE_cti)
    
    data %>%
      filter(nomindicador == input$indicador_DE_cti) %>% 
      janitor::remove_empty("cols")
    
  })
  
  
  # Titulo
  output$title_dat_DE_cti <- renderUI({
    helpText(HTML(unique(dat_DE_cti()$nomindicador)))
  })
  
  # Subtitulo
  output$subtitle_dat_DE_cti <- renderUI({
    helpText(HTML(unique(dat_DE_cti()$definicion)))
  })
  
  # Metodología
  output$info_dat_DE_cti <- renderUI({
    helpText(HTML(
      paste(
        "<b>Método de Agregación:</b>",
        unique(dat_DE_cti()$metodo_de_agregacion),
        "<b>Metodología:</b>",
        unique(dat_DE_cti()$concepto_estadistico_y_metodologia)
      )
    ))
  })
  
  # Relevancia:
  output$rel_dat_DE_cti <- renderUI({
    helpText(HTML(paste(
      "<b>Relvancia:</b>", unique(dat_DE_cti()$relevancia)
    )))
  })
  
  
  output$fecha_dat_DE_cti <- renderUI({
    if (input$visualizador_DE_cti == "Serie de tiempo") {
      tagList(
        tags$style(
          type = 'text/css',
          '#big_slider .irs-grid-text {font-size: 10px; transform: rotate(-90deg) translate(-15px);} ,.irs-grid-pol.large {height: 0px;}'
        ),
        div(
          id = 'big_slider',
          
          sliderTextInput(
            inputId = "fecha_dat_DE_cti",
            label = "Seleccione años",
            choices = sort(unique(dat_DE_cti()$fecha)),
            selected = c(min(dat_DE_cti()$fecha),
                         max(dat_DE_cti()$fecha)),
            hide_min_max = T,
            grid = TRUE,
            width = "100%"
          )
        )
      )
      
    } else {
      selectInput(
        inputId = "fecha_DE_cti",
        label = "Seleccione año:",
        choices = dat_DE_cti() %>%
          drop_na(valor) %>%
          select(fecha) %>%
          arrange(desc(fecha)) %>%
          unique() %>%
          pull(),
        selected = "2019"
      )
      
    }
    
  })
  
  
  output$corte_DE_cti <- renderUI({
    
    if (input$indicador_DE_cti %in% vars_corte) {
      
      selectInput(inputId = "corte_DE_cti",
                  label = "Seleccione categorías",
                  choices =  dat_DE_cti() %>%
                    distinct(get(names(dat_DE_cti()[,ncol(dat_DE_cti())]))) %>%
                    pull(),
                  selected = dat_DE_cti() %>%
                    filter(jerarquia == 1) %>% 
                    distinct(get(names(dat_DE_cti()[,ncol(dat_DE_cti())]))) %>%
                    pull()
      )
    } else {
      
      return(NULL)
    }
    
  })
  
  
  # Checkbox por pais
  output$sel_DE_cti_pais <- renderUI({
    if (input$visualizador_DE_cti == "Serie de tiempo") {
      dropdown(
        label = "Seleccione países",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_pais_DE_cti",
          label = "Seleccione países",
          inline = TRUE,
          choices = dat_DE_cti() %>%
            filter(region == 0) %>%
            filter(nomindicador == input$indicador_DE_cti) %>%
            distinct(cod_pais) %>%
            pull(),
          selected = c("URY", "ARG", "BRA", "PRY")
        )
      )
      
    } else {
      checkboxGroupInput(
        inputId = "chbox_pais_reg_DE_cti",
        label = "Mostrar:",
        inline = FALSE,
        choices = c("Países", "Regiones"),
        selected = "Países"
      )
      
    }
    
  })
  
  # Checkbox por región
  output$sel_DE_cti_region <- renderUI({
    if (input$visualizador_DE_cti == "Serie de tiempo") {
      dropdown(
        label = "Seleccione regiones",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_reg_DE_cti",
          label = "Seleccione regiones",
          inline = TRUE,
          choices = dat_DE_cti() %>%
            filter(region == 1) %>%
            filter(nomindicador == input$indicador_DE_cti) %>%
            distinct(pais) %>%
            pull(),
          selected = NULL
        )
      )
      
    } else {
      return(NULL)
      
    }
  })
  
  
  dat_DE_cti_anual <- reactive({
    
    if (input$indicador_DE_cti %in% vars_corte) {
      
      dat_DE_cti() %>%
        filter(get(names(dat_DE_cti()[,ncol(dat_DE_cti())])) %in% input$corte_DE_cti) %>%
        filter(fecha == input$fecha_DE_cti)
      
    } else {
      
      dat_DE_cti() %>%
        filter(fecha == input$fecha_DE_cti)
      
      
    }
  })
  
  dat_DE_cti_simple <- reactive({
    
    if (input$indicador_DE_cti %in% vars_corte) {
      
      dat_DE_cti() %>%
        filter(get(names(dat_DE_cti()[,ncol(dat_DE_cti())])) %in% input$corte_DE_cti) %>% 
        filter(fecha >= input$fecha_dat_DE_cti[1] & fecha <= input$fecha_dat_DE_cti[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_cti | pais %in% input$chbox_reg_DE_cti)
      
    } else {
      
      dat_DE_cti() %>%
        filter(fecha >= input$fecha_dat_DE_cti[1] & fecha <= input$fecha_dat_DE_cti[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_cti | pais %in% input$chbox_reg_DE_cti)        
    }
    
  })
  
  
  # Gráficos DE_cti
  output$p_dat_DE_cti <- renderPlot({
    
    if (input$visualizador_DE_cti == "Serie de tiempo" ) {
      
      req(input$fecha_dat_DE_cti, input$indicador_DE_cti)
      
      plot_DE_cti <- ggplot(
        data = dat_DE_cti_simple(), 
        aes(x = fecha, y = valor)
      ) +
        geom_line(aes(color = pais), size = 1, alpha = 0.5) +
        geom_point(aes(color = pais), size = 3) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_DE_cti_simple()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        scale_colour_manual(name = "", values = paleta_expandida) +
        if(input$indicador_DE_cti %in% vars_corte){
          
          ggtitle(paste0(input$indicador_DE_cti, " (", input$corte_DE_cti, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_DE_cti))
          
        }
      
      print(plot_DE_cti)
      ggsave(
        "www/indicador DE_cti.png",
        width = 30,
        height = 20,
        units = "cm"
      )
      
    } else if (input$visualizador_DE_cti == "Anual gráfico") {
      
      base_plot_DE_cti <- dat_DE_cti_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_DE_cti)
      
      validate(need(
        nrow(base_plot_DE_cti) > 0,
        'No hay datos disponible para esta búsqueda'
      ))
      
      plot_DE_cti <- ggplot(base_plot_DE_cti,
                           aes(x = fct_reorder(pais, valor), y = valor)) +
        geom_segment(
          aes(
            x = fct_reorder(pais, valor),
            xend = fct_reorder(pais, valor),
            y = 0,
            yend = valor
          ),
          size = 1,
          color = "#2c3e50"
        ) +
        geom_point(color = "#2c3e50", size = 4) +
        theme_light() +
        coord_flip() +
        theme(
          axis.text.y = element_text(size = 12),
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_DE_cti_anual()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        if(input$indicador_DE_cti %in% vars_corte){
          
          ggtitle(paste0(input$indicador_DE_cti, " (", input$corte_DE_cti, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_DE_cti))
          
        }
      
      print(plot_DE_cti)
      ggsave(
        "www/indicador DE_cti.png",
        width = 25,
        height = 30,
        units = "cm"
      )
      
    }
    
  })
  
  output$map_DE_cti <- renderLeaflet({
    # Read this shape file with the rgdal library.
    world_spdf <- readOGR(dsn = "data" ,
                          layer = "TM_WORLD_BORDERS_SIMPL-0.3",
                          verbose = FALSE)
    
    # Pegar datos
    world_spdf@data <- world_spdf@data %>%
      left_join(select(dat_DE_cti_anual(), pais_eng, valor_original),
                by = c("NAME" = "pais_eng"))
    
    # Library
    
    # Create a color palette with handmade bins.
    
    
    if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= -1) {
      round_number <- .1
      
    }
    
    min_val <-
      plyr::round_any(min(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      floor)
    max_val <-
      plyr::round_any(max(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      ceiling)
    dif <- max_val - min_val
    dif_5 <- dif / 5
    val_1 <- min_val + dif_5
    val_2 <- min_val + (dif_5 * 2)
    val_3 <- min_val + (dif_5 * 3)
    val_4 <- min_val + (dif_5 * 4)
    
    mybins <- c(min_val, val_1, val_2, val_3, val_4, max_val)
    
    mypalette <- colorBin(
      palette = "YlOrBr",
      domain = world_spdf@data$valor_original,
      na.color = "transparent",
      bins = mybins
    )
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "País: ",
      world_spdf@data$NAME,
      "<br/>",
      "Valor: ",
      round(world_spdf@data$valor_original, 2),
      sep = ""
    ) %>%
      lapply(htmltools::HTML)
    
    # Final Map
    leaflet(world_spdf) %>%
      addTiles()  %>%
      setView(lat = 30,
              lng = 0 ,
              zoom = 1) %>%
      addPolygons(
        fillColor = ~ mypalette(valor_original),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = mypalette,
        values =  ~ valor_original,
        opacity = 0.9,
        title = wrapit(input$indicador_DE_cti),
        position = "bottomleft"
      )
    
  })
  
  # Botón descarga grafico
  output$baja_plot_DE_cti <- downloadHandler(filename <- function() {
    paste("indicador DE_cti", "png", sep = ".")
  },
  
  content <- function(file) {
    file.copy("www/indicador DE_cti.png", file)
  },
  contentType = "www/indicador DE_cti")
  
  
  ## Data series temporal
  
  # Data para tabla y exportar
  dat_DE_cti_st <- reactive({
    
    if (input$indicador_DE_cti %in% vars_corte) {
      
      dat_DE_cti() %>%
        filter(fecha >= input$fecha_dat_DE_cti[1] &
                 fecha <= input$fecha_dat_DE_cti[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_cti | 
                 pais %in% input$chbox_reg_DE_cti) %>%
        select(pais, fecha, names(dat_DE_cti()[,ncol(dat_DE_cti())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor)) 
      
    } else {
      
      dat_DE_cti() %>%
        filter(fecha >= input$fecha_dat_DE_cti[1] &
                 fecha <= input$fecha_dat_DE_cti[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_cti |
                 pais %in% input$chbox_reg_DE_cti) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
  })
  
  # Metadata
  dat_DE_cti_m <- reactive({
    dat_DE_cti() %>%
      select(
        nomindicador,
        definicion,
        metodo_de_agregacion,
        concepto_estadistico_y_metodologia,
        relevancia,
        limitaciones_y_excepciones
      ) %>%
      mutate(`Unidad de Métodos y Acceso a Datos (FCS -UdelaR)` = " ") %>%
      distinct() %>%
      gather(key = "", value = " ")
    
  })
  
  # Data completa
  dat_DE_cti_c <- reactive({
    
    if (input$indicador_DE_cti %in% vars_corte) {
      
      dat_DE_cti() %>%
        select(pais, fecha, names(dat_DE_cti()[,ncol(dat_DE_cti())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_DE_cti() %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
    
  })
  
  # Lista para descarga
  list_dat_DE_cti_st <- reactive({
    list_dat_DE_cti <- list(
      "Data" = dat_DE_cti_st(),
      "Metadata" = dat_DE_cti_m(),
      "Data Completa" = dat_DE_cti_c()
    )
  })
  
  
  ## Data por año
  # Data para tabla y exportar
  dat_DE_cti_a <- reactive({
    
    if (input$indicador_DE_cti %in% vars_corte) {
      
      dat_DE_cti_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_DE_cti) %>%
        select(pais, fecha, names(dat_DE_cti()[,ncol(dat_DE_cti())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_DE_cti_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_DE_cti) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    }
  })
  
  # Lista para descarga
  list_dat_DE_cti_a <- reactive({
    list_dat_DE_cti <- list("Data" = dat_DE_cti_a(),
                           "Metadata" = dat_DE_cti_m(),
                           "Data Completa" = dat_DE_cti_c())
  })
  
  # Tablas en shiny
  output$tab_dat_DE_cti <- renderDT({
    
    if (input$indicador_DE_cti %in% vars_corte) {
      DT::datatable(
        dat_DE_cti_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Corte", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_DE_cti,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(4, '', mark = ",")
      
    } else if (input$visualizador_DE_cti == "Serie de tiempo") {
      DT::datatable(
        dat_DE_cti_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_DE_cti,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    } else if (input$visualizador_DE_cti %in% c("Anual gráfico", "Anual mapa")) {
      DT::datatable(
        dat_DE_cti_a(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_DE_cti,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    }
  })
  
  # Descarga tabla
  output$dl_tabla_dat_DE_cti <- downloadHandler(
    filename = function() {
      paste("resultados-", input$indicador_DE_cti, ".xlsx", sep = "")
    },
    content = function(file) {
      if (input$visualizador_DE_cti == "Serie de tiempo") {
        openxlsx::write.xlsx(list_dat_DE_cti_st(), file)
        
      } else if (input$visualizador_DE_cti %in% c("Anual gráfico", "Anual mapa")) {
        openxlsx::write.xlsx(list_dat_DE_cti_a(), file)
        
      }
    }
  )
  
  ##  11.  DE_infra (dat_DE_infra)   ====================================
  
  # Data DE_infra
  
  dat_DE_infra <- reactive({
    
    req(input$indicador_DE_infra)
    
    data %>%
      filter(nomindicador == input$indicador_DE_infra) %>% 
      janitor::remove_empty("cols")
    
  })
  
  
  # Titulo
  output$title_dat_DE_infra <- renderUI({
    helpText(HTML(unique(dat_DE_infra()$nomindicador)))
  })
  
  # Subtitulo
  output$subtitle_dat_DE_infra <- renderUI({
    helpText(HTML(unique(dat_DE_infra()$definicion)))
  })
  
  # Metodología
  output$info_dat_DE_infra <- renderUI({
    helpText(HTML(
      paste(
        "<b>Método de Agregación:</b>",
        unique(dat_DE_infra()$metodo_de_agregacion),
        "<b>Metodología:</b>",
        unique(dat_DE_infra()$concepto_estadistico_y_metodologia)
      )
    ))
  })
  
  # Relevancia:
  output$rel_dat_DE_infra <- renderUI({
    helpText(HTML(paste(
      "<b>Relvancia:</b>", unique(dat_DE_infra()$relevancia)
    )))
  })
  
  
  output$fecha_dat_DE_infra <- renderUI({
    if (input$visualizador_DE_infra == "Serie de tiempo") {
      tagList(
        tags$style(
          type = 'text/css',
          '#big_slider .irs-grid-text {font-size: 10px; transform: rotate(-90deg) translate(-15px);} ,.irs-grid-pol.large {height: 0px;}'
        ),
        div(
          id = 'big_slider',
          
          sliderTextInput(
            inputId = "fecha_dat_DE_infra",
            label = "Seleccione años",
            choices = sort(unique(dat_DE_infra()$fecha)),
            selected = c(min(dat_DE_infra()$fecha),
                         max(dat_DE_infra()$fecha)),
            hide_min_max = T,
            grid = TRUE,
            width = "100%"
          )
        )
      )
      
    } else {
      selectInput(
        inputId = "fecha_DE_infra",
        label = "Seleccione año:",
        choices = dat_DE_infra() %>%
          drop_na(valor) %>%
          select(fecha) %>%
          arrange(desc(fecha)) %>%
          unique() %>%
          pull(),
        selected = "2019"
      )
      
    }
    
  })
  
  
  output$corte_DE_infra <- renderUI({
    
    if (input$indicador_DE_infra %in% vars_corte) {
      
      selectInput(inputId = "corte_DE_infra",
                  label = "Seleccione categorías",
                  choices =  dat_DE_infra() %>%
                    distinct(get(names(dat_DE_infra()[,ncol(dat_DE_infra())]))) %>%
                    pull(),
                  selected = dat_DE_infra() %>%
                    filter(jerarquia == 1) %>% 
                    distinct(get(names(dat_DE_infra()[,ncol(dat_DE_infra())]))) %>%
                    pull()
      )
    } else {
      
      return(NULL)
    }
    
  })
  
  
  # Checkbox por pais
  output$sel_DE_infra_pais <- renderUI({
    if (input$visualizador_DE_infra == "Serie de tiempo") {
      dropdown(
        label = "Seleccione países",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_pais_DE_infra",
          label = "Seleccione países",
          inline = TRUE,
          choices = dat_DE_infra() %>%
            filter(region == 0) %>%
            filter(nomindicador == input$indicador_DE_infra) %>%
            distinct(cod_pais) %>%
            pull(),
          selected = c("URY", "ARG", "BRA", "PRY")
        )
      )
      
    } else {
      checkboxGroupInput(
        inputId = "chbox_pais_reg_DE_infra",
        label = "Mostrar:",
        inline = FALSE,
        choices = c("Países", "Regiones"),
        selected = "Países"
      )
      
    }
    
  })
  
  # Checkbox por región
  output$sel_DE_infra_region <- renderUI({
    if (input$visualizador_DE_infra == "Serie de tiempo") {
      dropdown(
        label = "Seleccione regiones",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_reg_DE_infra",
          label = "Seleccione regiones",
          inline = TRUE,
          choices = dat_DE_infra() %>%
            filter(region == 1) %>%
            filter(nomindicador == input$indicador_DE_infra) %>%
            distinct(pais) %>%
            pull(),
          selected = NULL
        )
      )
      
    } else {
      return(NULL)
      
    }
  })
  
  
  dat_DE_infra_anual <- reactive({
    
    if (input$indicador_DE_infra %in% vars_corte) {
      
      dat_DE_infra() %>%
        filter(get(names(dat_DE_infra()[,ncol(dat_DE_infra())])) %in% input$corte_DE_infra) %>%
        filter(fecha == input$fecha_DE_infra)
      
    } else {
      
      dat_DE_infra() %>%
        filter(fecha == input$fecha_DE_infra)
      
      
    }
  })
  
  dat_DE_infra_simple <- reactive({
    
    if (input$indicador_DE_infra %in% vars_corte) {
      
      dat_DE_infra() %>%
        filter(get(names(dat_DE_infra()[,ncol(dat_DE_infra())])) %in% input$corte_DE_infra) %>% 
        filter(fecha >= input$fecha_dat_DE_infra[1] & fecha <= input$fecha_dat_DE_infra[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_infra | pais %in% input$chbox_reg_DE_infra)
      
    } else {
      
      dat_DE_infra() %>%
        filter(fecha >= input$fecha_dat_DE_infra[1] & fecha <= input$fecha_dat_DE_infra[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_infra | pais %in% input$chbox_reg_DE_infra)        
    }
    
  })
  
  
  # Gráficos DE_infra
  output$p_dat_DE_infra <- renderPlot({
    
    if (input$visualizador_DE_infra == "Serie de tiempo" ) {
      
      req(input$fecha_dat_DE_infra, input$indicador_DE_infra)
      
      plot_DE_infra <- ggplot(
        data = dat_DE_infra_simple(), 
        aes(x = fecha, y = valor)
      ) +
        geom_line(aes(color = pais), size = 1, alpha = 0.5) +
        geom_point(aes(color = pais), size = 3) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_DE_infra_simple()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        scale_colour_manual(name = "", values = paleta_expandida) +
        if(input$indicador_DE_infra %in% vars_corte){
          
          ggtitle(paste0(input$indicador_DE_infra, " (", input$corte_DE_infra, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_DE_infra))
          
        }
      
      print(plot_DE_infra)
      ggsave(
        "www/indicador DE_infra.png",
        width = 30,
        height = 20,
        units = "cm"
      )
      
    } else if (input$visualizador_DE_infra == "Anual gráfico") {
      
      base_plot_DE_infra <- dat_DE_infra_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_DE_infra)
      
      validate(need(
        nrow(base_plot_DE_infra) > 0,
        'No hay datos disponible para esta búsqueda'
      ))
      
      plot_DE_infra <- ggplot(base_plot_DE_infra,
                            aes(x = fct_reorder(pais, valor), y = valor)) +
        geom_segment(
          aes(
            x = fct_reorder(pais, valor),
            xend = fct_reorder(pais, valor),
            y = 0,
            yend = valor
          ),
          size = 1,
          color = "#2c3e50"
        ) +
        geom_point(color = "#2c3e50", size = 4) +
        theme_light() +
        coord_flip() +
        theme(
          axis.text.y = element_text(size = 12),
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_DE_infra_anual()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        if(input$indicador_DE_infra %in% vars_corte){
          
          ggtitle(paste0(input$indicador_DE_infra, " (", input$corte_DE_infra, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_DE_infra))
          
        }
      
      print(plot_DE_infra)
      ggsave(
        "www/indicador DE_infra.png",
        width = 25,
        height = 30,
        units = "cm"
      )
      
    }
    
  })
  
  output$map_DE_infra <- renderLeaflet({
    # Read this shape file with the rgdal library.
    world_spdf <- readOGR(dsn = "data" ,
                          layer = "TM_WORLD_BORDERS_SIMPL-0.3",
                          verbose = FALSE)
    
    # Pegar datos
    world_spdf@data <- world_spdf@data %>%
      left_join(select(dat_DE_infra_anual(), pais_eng, valor_original),
                by = c("NAME" = "pais_eng"))
    
    # Library
    
    # Create a color palette with handmade bins.
    
    
    if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= -1) {
      round_number <- .1
      
    }
    
    min_val <-
      plyr::round_any(min(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      floor)
    max_val <-
      plyr::round_any(max(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      ceiling)
    dif <- max_val - min_val
    dif_5 <- dif / 5
    val_1 <- min_val + dif_5
    val_2 <- min_val + (dif_5 * 2)
    val_3 <- min_val + (dif_5 * 3)
    val_4 <- min_val + (dif_5 * 4)
    
    mybins <- c(min_val, val_1, val_2, val_3, val_4, max_val)
    
    mypalette <- colorBin(
      palette = "YlOrBr",
      domain = world_spdf@data$valor_original,
      na.color = "transparent",
      bins = mybins
    )
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "País: ",
      world_spdf@data$NAME,
      "<br/>",
      "Valor: ",
      round(world_spdf@data$valor_original, 2),
      sep = ""
    ) %>%
      lapply(htmltools::HTML)
    
    # Final Map
    leaflet(world_spdf) %>%
      addTiles()  %>%
      setView(lat = 30,
              lng = 0 ,
              zoom = 1) %>%
      addPolygons(
        fillColor = ~ mypalette(valor_original),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = mypalette,
        values =  ~ valor_original,
        opacity = 0.9,
        title = wrapit(input$indicador_DE_infra),
        position = "bottomleft"
      )
    
  })
  
  # Botón descarga grafico
  output$baja_plot_DE_infra <- downloadHandler(filename <- function() {
    paste("indicador DE_infra", "png", sep = ".")
  },
  
  content <- function(file) {
    file.copy("www/indicador DE_infra.png", file)
  },
  contentType = "www/indicador DE_infra")
  
  
  ## Data series temporal
  
  # Data para tabla y exportar
  dat_DE_infra_st <- reactive({
    
    if (input$indicador_DE_infra %in% vars_corte) {
      
      dat_DE_infra() %>%
        filter(fecha >= input$fecha_dat_DE_infra[1] & fecha <= input$fecha_dat_DE_infra[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_infra | pais %in% input$chbox_reg_DE_infra) %>%
        select(pais, fecha, names(dat_DE_infra()[,ncol(dat_DE_infra())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor)) 
      
    } else {
      
      dat_DE_infra() %>%
        filter(fecha >= input$fecha_dat_DE_infra[1] &
                 fecha <= input$fecha_dat_DE_infra[2]) %>%
        filter(cod_pais %in% input$chbox_pais_DE_infra |
                 pais %in% input$chbox_reg_DE_infra) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
  })
  
  # Metadata
  dat_DE_infra_m <- reactive({
    dat_DE_infra() %>%
      select(
        nomindicador,
        definicion,
        metodo_de_agregacion,
        concepto_estadistico_y_metodologia,
        relevancia,
        limitaciones_y_excepciones
      ) %>%
      mutate(`Unidad de Métodos y Acceso a Datos (FCS -UdelaR)` = " ") %>%
      distinct() %>%
      gather(key = "", value = " ")
    
  })
  
  # Data completa
  dat_DE_infra_c <- reactive({
    
    if (input$indicador_DE_infra %in% vars_corte) {
      
      dat_DE_infra() %>%
        select(pais, fecha, names(dat_DE_infra()[,ncol(dat_DE_infra())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_DE_infra() %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
    
  })
  
  # Lista para descarga
  list_dat_DE_infra_st <- reactive({
    list_dat_DE_infra <- list(
      "Data" = dat_DE_infra_st(),
      "Metadata" = dat_DE_infra_m(),
      "Data Completa" = dat_DE_infra_c()
    )
  })
  
  
  ## Data por año
  # Data para tabla y exportar
  dat_DE_infra_a <- reactive({
    
    if (input$indicador_DE_infra %in% vars_corte) {
      
      dat_DE_infra_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_DE_infra) %>%
        select(pais, fecha, names(dat_DE_infra()[,ncol(dat_DE_infra())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_DE_infra_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_DE_infra) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    }
  })
  
  # Lista para descarga
  list_dat_DE_infra_a <- reactive({
    list_dat_DE_infra <- list("Data" = dat_DE_infra_a(),
                            "Metadata" = dat_DE_infra_m(),
                            "Data Completa" = dat_DE_infra_c())
  })
  
  # Tablas en shiny
  output$tab_dat_DE_infra <- renderDT({
    
    if (input$indicador_DE_infra %in% vars_corte) {
      DT::datatable(
        dat_DE_infra_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Corte", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_DE_infra,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(4, '', mark = ",")
      
    } else if (input$visualizador_DE_infra == "Serie de tiempo") {
      DT::datatable(
        dat_DE_infra_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_DE_infra,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    } else if (input$visualizador_DE_infra %in% c("Anual gráfico", "Anual mapa")) {
      DT::datatable(
        dat_DE_infra_a(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_DE_infra,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    }
  })
  
  # Descarga tabla
  output$dl_tabla_dat_DE_infra <- downloadHandler(
    filename = function() {
      paste("resultados-", input$indicador_DE_infra, ".xlsx", sep = "")
    },
    content = function(file) {
      if (input$visualizador_DE_infra == "Serie de tiempo") {
        openxlsx::write.xlsx(list_dat_DE_infra_st(), file)
        
      } else if (input$visualizador_DE_infra %in% c("Anual gráfico", "Anual mapa")) {
        openxlsx::write.xlsx(list_dat_DE_infra_a(), file)
        
      }
    }
  )
  
  ##  12.  SF_ahorro (dat_SF_ahorro)   ====================================
  
  # Data SF_ahorro
  
  dat_SF_ahorro <- reactive({
    
    req(input$indicador_SF_ahorro)
    
    data %>%
      filter(nomindicador == input$indicador_SF_ahorro) %>% 
      janitor::remove_empty("cols")
    
  })
  
  
  # Titulo
  output$title_dat_SF_ahorro <- renderUI({
    helpText(HTML(unique(dat_SF_ahorro()$nomindicador)))
  })
  
  # Subtitulo
  output$subtitle_dat_SF_ahorro <- renderUI({
    helpText(HTML(unique(dat_SF_ahorro()$definicion)))
  })
  
  # Metodología
  output$info_dat_SF_ahorro <- renderUI({
    helpText(HTML(
      paste(
        "<b>Método de Agregación:</b>",
        unique(dat_SF_ahorro()$metodo_de_agregacion),
        "<b>Metodología:</b>",
        unique(dat_SF_ahorro()$concepto_estadistico_y_metodologia)
      )
    ))
  })
  
  # Relevancia:
  output$rel_dat_SF_ahorro <- renderUI({
    helpText(HTML(paste(
      "<b>Relvancia:</b>", unique(dat_SF_ahorro()$relevancia)
    )))
  })
  
  
  output$fecha_dat_SF_ahorro <- renderUI({
    if (input$visualizador_SF_ahorro == "Serie de tiempo") {
      tagList(
        tags$style(
          type = 'text/css',
          '#big_slider .irs-grid-text {font-size: 10px; transform: rotate(-90deg) translate(-15px);} ,.irs-grid-pol.large {height: 0px;}'
        ),
        div(
          id = 'big_slider',
          
          sliderTextInput(
            inputId = "fecha_dat_SF_ahorro",
            label = "Seleccione años",
            choices = sort(unique(dat_SF_ahorro()$fecha)),
            selected = c(min(dat_SF_ahorro()$fecha),
                         max(dat_SF_ahorro()$fecha)),
            hide_min_max = T,
            grid = TRUE,
            width = "100%"
          )
        )
      )
      
    } else {
      selectInput(
        inputId = "fecha_SF_ahorro",
        label = "Seleccione año:",
        choices = dat_SF_ahorro() %>%
          drop_na(valor) %>%
          select(fecha) %>%
          arrange(desc(fecha)) %>%
          unique() %>%
          pull(),
        selected = "2019"
      )
      
    }
    
  })
  
  
  output$corte_SF_ahorro <- renderUI({
    
    if (input$indicador_SF_ahorro %in% vars_corte) {
      
      selectInput(inputId = "corte_SF_ahorro",
                  label = "Seleccione categorías",
                  choices =  dat_SF_ahorro() %>%
                    distinct(get(names(dat_SF_ahorro()[,ncol(dat_SF_ahorro())]))) %>%
                    pull(),
                  selected = dat_SF_ahorro() %>%
                    filter(jerarquia == 1) %>% 
                    distinct(get(names(dat_SF_ahorro()[,ncol(dat_SF_ahorro())]))) %>%
                    pull()
      )
    } else {
      
      return(NULL)
    }
    
  })
  
  
  # Checkbox por pais
  output$sel_SF_ahorro_pais <- renderUI({
    if (input$visualizador_SF_ahorro == "Serie de tiempo") {
      dropdown(
        label = "Seleccione países",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_pais_SF_ahorro",
          label = "Seleccione países",
          inline = TRUE,
          choices = dat_SF_ahorro() %>%
            filter(region == 0) %>%
            filter(nomindicador == input$indicador_SF_ahorro) %>%
            distinct(cod_pais) %>%
            pull(),
          selected = c("URY", "ARG", "BRA", "PRY")
        )
      )
      
    } else {
      checkboxGroupInput(
        inputId = "chbox_pais_reg_SF_ahorro",
        label = "Mostrar:",
        inline = FALSE,
        choices = c("Países", "Regiones"),
        selected = "Países"
      )
      
    }
    
  })
  
  # Checkbox por región
  output$sel_SF_ahorro_region <- renderUI({
    if (input$visualizador_SF_ahorro == "Serie de tiempo") {
      dropdown(
        label = "Seleccione regiones",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_reg_SF_ahorro",
          label = "Seleccione regiones",
          inline = TRUE,
          choices = dat_SF_ahorro() %>%
            filter(region == 1) %>%
            filter(nomindicador == input$indicador_SF_ahorro) %>%
            distinct(pais) %>%
            pull(),
          selected = NULL
        )
      )
      
    } else {
      return(NULL)
      
    }
  })
  
  
  dat_SF_ahorro_anual <- reactive({
    
    if (input$indicador_SF_ahorro %in% vars_corte) {
      
      dat_SF_ahorro() %>%
        filter(get(names(dat_SF_ahorro()[,ncol(dat_SF_ahorro())])) %in% input$corte_SF_ahorro) %>%
        filter(fecha == input$fecha_SF_ahorro)
      
    } else {
      
      dat_SF_ahorro() %>%
        filter(fecha == input$fecha_SF_ahorro)
      
      
    }
  })
  
  dat_SF_ahorro_simple <- reactive({
    
    if (input$indicador_SF_ahorro %in% vars_corte) {
      
      dat_SF_ahorro() %>%
        filter(get(names(dat_SF_ahorro()[,ncol(dat_SF_ahorro())])) %in% input$corte_SF_ahorro) %>% 
        filter(fecha >= input$fecha_dat_SF_ahorro[1] & fecha <= input$fecha_dat_SF_ahorro[2]) %>%
        filter(cod_pais %in% input$chbox_pais_SF_ahorro | pais %in% input$chbox_reg_SF_ahorro)
      
    } else {
      
      dat_SF_ahorro() %>%
        filter(fecha >= input$fecha_dat_SF_ahorro[1] & fecha <= input$fecha_dat_SF_ahorro[2]) %>%
        filter(cod_pais %in% input$chbox_pais_SF_ahorro | pais %in% input$chbox_reg_SF_ahorro)        
    }
    
  })
  
  
  # Gráficos SF_ahorro
  output$p_dat_SF_ahorro <- renderPlot({
    
    if (input$visualizador_SF_ahorro == "Serie de tiempo" ) {
      
      req(input$fecha_dat_SF_ahorro, input$indicador_SF_ahorro)
      
      plot_SF_ahorro <- ggplot(
        data = dat_SF_ahorro_simple(), 
        aes(x = fecha, y = valor)
      ) +
        geom_line(aes(color = pais), size = 1, alpha = 0.5) +
        geom_point(aes(color = pais), size = 3) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_SF_ahorro_simple()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        scale_colour_manual(name = "", values = paleta_expandida) +
        if(input$indicador_SF_ahorro %in% vars_corte){
          
          ggtitle(paste0(input$indicador_SF_ahorro, " (", input$corte_SF_ahorro, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_SF_ahorro))
          
        }
      
      print(plot_SF_ahorro)
      ggsave(
        "www/indicador SF_ahorro.png",
        width = 30,
        height = 20,
        units = "cm"
      )
      
    } else if (input$visualizador_SF_ahorro == "Anual gráfico") {
      
      base_plot_SF_ahorro <- dat_SF_ahorro_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_SF_ahorro)
      
      validate(need(
        nrow(base_plot_SF_ahorro) > 0,
        'No hay datos disponible para esta búsqueda'
      ))
      
      plot_SF_ahorro <- ggplot(base_plot_SF_ahorro,
                              aes(x = fct_reorder(pais, valor), y = valor)) +
        geom_segment(
          aes(
            x = fct_reorder(pais, valor),
            xend = fct_reorder(pais, valor),
            y = 0,
            yend = valor
          ),
          size = 1,
          color = "#2c3e50"
        ) +
        geom_point(color = "#2c3e50", size = 4) +
        theme_light() +
        coord_flip() +
        theme(
          axis.text.y = element_text(size = 12),
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_SF_ahorro_anual()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        if(input$indicador_SF_ahorro %in% vars_corte){
          
          ggtitle(paste0(input$indicador_SF_ahorro, " (", input$corte_SF_ahorro, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_SF_ahorro))
          
        }
      
      print(plot_SF_ahorro)
      ggsave(
        "www/indicador SF_ahorro.png",
        width = 25,
        height = 30,
        units = "cm"
      )
      
    }
    
  })
  
  output$map_SF_ahorro <- renderLeaflet({
    # Read this shape file with the rgdal library.
    world_spdf <- readOGR(dsn = "data" ,
                          layer = "TM_WORLD_BORDERS_SIMPL-0.3",
                          verbose = FALSE)
    
    # Pegar datos
    world_spdf@data <- world_spdf@data %>%
      left_join(select(dat_SF_ahorro_anual(), pais_eng, valor_original),
                by = c("NAME" = "pais_eng"))
    
    # Library
    
    # Create a color palette with handmade bins.
    
    
    if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= -1) {
      round_number <- .1
      
    }
    
    min_val <-
      plyr::round_any(min(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      floor)
    max_val <-
      plyr::round_any(max(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      ceiling)
    dif <- max_val - min_val
    dif_5 <- dif / 5
    val_1 <- min_val + dif_5
    val_2 <- min_val + (dif_5 * 2)
    val_3 <- min_val + (dif_5 * 3)
    val_4 <- min_val + (dif_5 * 4)
    
    mybins <- c(min_val, val_1, val_2, val_3, val_4, max_val)
    
    mypalette <- colorBin(
      palette = "YlOrBr",
      domain = world_spdf@data$valor_original,
      na.color = "transparent",
      bins = mybins
    )
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "País: ",
      world_spdf@data$NAME,
      "<br/>",
      "Valor: ",
      round(world_spdf@data$valor_original, 2),
      sep = ""
    ) %>%
      lapply(htmltools::HTML)
    
    # Final Map
    leaflet(world_spdf) %>%
      addTiles()  %>%
      setView(lat = 30,
              lng = 0 ,
              zoom = 1) %>%
      addPolygons(
        fillColor = ~ mypalette(valor_original),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = mypalette,
        values =  ~ valor_original,
        opacity = 0.9,
        title = wrapit(input$indicador_SF_ahorro),
        position = "bottomleft"
      )
    
  })
  
  # Botón descarga grafico
  output$baja_plot_SF_ahorro <- downloadHandler(filename <- function() {
    paste("indicador SF_ahorro", "png", sep = ".")
  },
  
  content <- function(file) {
    file.copy("www/indicador SF_ahorro.png", file)
  },
  contentType = "www/indicador SF_ahorro")
  
  
  ## Data series temporal
  
  # Data para tabla y exportar
  dat_SF_ahorro_st <- reactive({
    
    if (input$indicador_SF_ahorro %in% vars_corte) {
      
      dat_SF_ahorro() %>%
        filter(fecha >= input$fecha_dat_SF_ahorro[1] & 
                 fecha <= input$fecha_dat_SF_ahorro[2]) %>%
        filter(cod_pais %in% input$chbox_pais_SF_ahorro | 
                 pais %in% input$chbox_reg_SF_ahorro) %>%
        select(pais, fecha, names(dat_SF_ahorro()[,ncol(dat_SF_ahorro())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor)) 
      
    } else {
      
      dat_SF_ahorro() %>%
        filter(fecha >= input$fecha_dat_SF_ahorro[1] &
                 fecha <= input$fecha_dat_SF_ahorro[2]) %>%
        filter(cod_pais %in% input$chbox_pais_SF_ahorro |
                 pais %in% input$chbox_reg_SF_ahorro) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
  })
  
  # Metadata
  dat_SF_ahorro_m <- reactive({
    dat_SF_ahorro() %>%
      select(
        nomindicador,
        definicion,
        metodo_de_agregacion,
        concepto_estadistico_y_metodologia,
        relevancia,
        limitaciones_y_excepciones
      ) %>%
      mutate(`Unidad de Métodos y Acceso a Datos (FCS -UdelaR)` = " ") %>%
      distinct() %>%
      gather(key = "", value = " ")
    
  })
  
  # Data completa
  dat_SF_ahorro_c <- reactive({
    
    if (input$indicador_SF_ahorro %in% vars_corte) {
      
      dat_SF_ahorro() %>%
        select(pais, fecha, names(dat_SF_ahorro()[,ncol(dat_SF_ahorro())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_SF_ahorro() %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
    
  })
  
  # Lista para descarga
  list_dat_SF_ahorro_st <- reactive({
    list_dat_SF_ahorro <- list(
      "Data" = dat_SF_ahorro_st(),
      "Metadata" = dat_SF_ahorro_m(),
      "Data Completa" = dat_SF_ahorro_c()
    )
  })
  
  
  ## Data por año
  # Data para tabla y exportar
  dat_SF_ahorro_a <- reactive({
    
    if (input$indicador_SF_ahorro %in% vars_corte) {
      
      dat_SF_ahorro_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_SF_ahorro) %>%
        select(pais, fecha, names(dat_SF_ahorro()[,ncol(dat_SF_ahorro())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_SF_ahorro_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_SF_ahorro) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    }
  })
  
  # Lista para descarga
  list_dat_SF_ahorro_a <- reactive({
    list_dat_SF_ahorro <- list("Data" = dat_SF_ahorro_a(),
                              "Metadata" = dat_SF_ahorro_m(),
                              "Data Completa" = dat_SF_ahorro_c())
  })
  
  # Tablas en shiny
  output$tab_dat_SF_ahorro <- renderDT({
    
    if (input$indicador_SF_ahorro %in% vars_corte) {
      DT::datatable(
        dat_SF_ahorro_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Corte", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_SF_ahorro,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(4, '', mark = ",")
      
    } else if (input$visualizador_SF_ahorro == "Serie de tiempo") {
      DT::datatable(
        dat_SF_ahorro_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_SF_ahorro,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    } else if (input$visualizador_SF_ahorro %in% c("Anual gráfico", "Anual mapa")) {
      DT::datatable(
        dat_SF_ahorro_a(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_SF_ahorro,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    }
  })
  
  # Descarga tabla
  output$dl_tabla_dat_SF_ahorro <- downloadHandler(
    filename = function() {
      paste("resultados-", input$indicador_SF_ahorro, ".xlsx", sep = "")
    },
    content = function(file) {
      if (input$visualizador_SF_ahorro == "Serie de tiempo") {
        openxlsx::write.xlsx(list_dat_SF_ahorro_st(), file)
        
      } else if (input$visualizador_SF_ahorro %in% c("Anual gráfico", "Anual mapa")) {
        openxlsx::write.xlsx(list_dat_SF_ahorro_a(), file)
        
      }
    }
  )
  
  ##  13.  SF_externa (dat_SF_externa)   ====================================
  
  # Data SF_externa
  
  dat_SF_externa <- reactive({
    
    req(input$indicador_SF_externa)
    
    data %>%
      filter(nomindicador == input$indicador_SF_externa) %>% 
      janitor::remove_empty("cols")
    
  })
  
  
  # Titulo
  output$title_dat_SF_externa <- renderUI({
    helpText(HTML(unique(dat_SF_externa()$nomindicador)))
  })
  
  # Subtitulo
  output$subtitle_dat_SF_externa <- renderUI({
    helpText(HTML(unique(dat_SF_externa()$definicion)))
  })
  
  # Metodología
  output$info_dat_SF_externa <- renderUI({
    helpText(HTML(
      paste(
        "<b>Método de Agregación:</b>",
        unique(dat_SF_externa()$metodo_de_agregacion),
        "<b>Metodología:</b>",
        unique(dat_SF_externa()$concepto_estadistico_y_metodologia)
      )
    ))
  })
  
  # Relevancia:
  output$rel_dat_SF_externa <- renderUI({
    helpText(HTML(paste(
      "<b>Relvancia:</b>", unique(dat_SF_externa()$relevancia)
    )))
  })
  
  
  output$fecha_dat_SF_externa <- renderUI({
    if (input$visualizador_SF_externa == "Serie de tiempo") {
      tagList(
        tags$style(
          type = 'text/css',
          '#big_slider .irs-grid-text {font-size: 10px; transform: rotate(-90deg) translate(-15px);} ,.irs-grid-pol.large {height: 0px;}'
        ),
        div(
          id = 'big_slider',
          
          sliderTextInput(
            inputId = "fecha_dat_SF_externa",
            label = "Seleccione años",
            choices = sort(unique(dat_SF_externa()$fecha)),
            selected = c(min(dat_SF_externa()$fecha),
                         max(dat_SF_externa()$fecha)),
            hide_min_max = T,
            grid = TRUE,
            width = "100%"
          )
        )
      )
      
    } else {
      selectInput(
        inputId = "fecha_SF_externa",
        label = "Seleccione año:",
        choices = dat_SF_externa() %>%
          drop_na(valor) %>%
          select(fecha) %>%
          arrange(desc(fecha)) %>%
          unique() %>%
          pull(),
        selected = "2019"
      )
      
    }
    
  })
  
  
  output$corte_SF_externa <- renderUI({
    
    if (input$indicador_SF_externa %in% vars_corte) {
      
      selectInput(inputId = "corte_SF_externa",
                  label = "Seleccione categorías",
                  choices =  dat_SF_externa() %>%
                    distinct(get(names(dat_SF_externa()[,ncol(dat_SF_externa())]))) %>%
                    pull(),
                  selected = dat_SF_externa() %>%
                    filter(jerarquia == 1) %>% 
                    distinct(get(names(dat_SF_externa()[,ncol(dat_SF_externa())]))) %>%
                    pull()
      )
    } else {
      
      return(NULL)
    }
    
  })
  
  
  # Checkbox por pais
  output$sel_SF_externa_pais <- renderUI({
    if (input$visualizador_SF_externa == "Serie de tiempo") {
      dropdown(
        label = "Seleccione países",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_pais_SF_externa",
          label = "Seleccione países",
          inline = TRUE,
          choices = dat_SF_externa() %>%
            filter(region == 0) %>%
            filter(nomindicador == input$indicador_SF_externa) %>%
            distinct(cod_pais) %>%
            pull(),
          selected = c("URY", "ARG", "BRA", "PRY")
        )
      )
      
    } else {
      checkboxGroupInput(
        inputId = "chbox_pais_reg_SF_externa",
        label = "Mostrar:",
        inline = FALSE,
        choices = c("Países", "Regiones"),
        selected = "Países"
      )
      
    }
    
  })
  
  # Checkbox por región
  output$sel_SF_externa_region <- renderUI({
    if (input$visualizador_SF_externa == "Serie de tiempo") {
      dropdown(
        label = "Seleccione regiones",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_reg_SF_externa",
          label = "Seleccione regiones",
          inline = TRUE,
          choices = dat_SF_externa() %>%
            filter(region == 1) %>%
            filter(nomindicador == input$indicador_SF_externa) %>%
            distinct(pais) %>%
            pull(),
          selected = NULL
        )
      )
      
    } else {
      return(NULL)
      
    }
  })
  
  
  dat_SF_externa_anual <- reactive({
    
    if (input$indicador_SF_externa %in% vars_corte) {
      
      dat_SF_externa() %>%
        filter(get(names(dat_SF_externa()[,ncol(dat_SF_externa())])) %in% input$corte_SF_externa) %>%
        filter(fecha == input$fecha_SF_externa)
      
    } else {
      
      dat_SF_externa() %>%
        filter(fecha == input$fecha_SF_externa)
      
      
    }
  })
  
  dat_SF_externa_simple <- reactive({
    
    if (input$indicador_SF_externa %in% vars_corte) {
      
      dat_SF_externa() %>%
        filter(get(names(dat_SF_externa()[,ncol(dat_SF_externa())])) %in% input$corte_SF_externa) %>% 
        filter(fecha >= input$fecha_dat_SF_externa[1] & fecha <= input$fecha_dat_SF_externa[2]) %>%
        filter(cod_pais %in% input$chbox_pais_SF_externa | pais %in% input$chbox_reg_SF_externa)
      
    } else {
      
      dat_SF_externa() %>%
        filter(fecha >= input$fecha_dat_SF_externa[1] & fecha <= input$fecha_dat_SF_externa[2]) %>%
        filter(cod_pais %in% input$chbox_pais_SF_externa | pais %in% input$chbox_reg_SF_externa)        
    }
    
  })
  
  
  # Gráficos SF_externa
  output$p_dat_SF_externa <- renderPlot({
    
    if (input$visualizador_SF_externa == "Serie de tiempo" ) {
      
      req(input$fecha_dat_SF_externa, input$indicador_SF_externa)
      
      plot_SF_externa <- ggplot(
        data = dat_SF_externa_simple(), 
        aes(x = fecha, y = valor)
      ) +
        geom_line(aes(color = pais), size = 1, alpha = 0.5) +
        geom_point(aes(color = pais), size = 3) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_SF_externa_simple()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        scale_colour_manual(name = "", values = paleta_expandida) +
        if(input$indicador_SF_externa %in% vars_corte){
          
          ggtitle(paste0(input$indicador_SF_externa, " (", input$corte_SF_externa, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_SF_externa))
          
        }
      
      print(plot_SF_externa)
      ggsave(
        "www/indicador SF_externa.png",
        width = 30,
        height = 20,
        units = "cm"
      )
      
    } else if (input$visualizador_SF_externa == "Anual gráfico") {
      
      base_plot_SF_externa <- dat_SF_externa_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_SF_externa)
      
      validate(need(
        nrow(base_plot_SF_externa) > 0,
        'No hay datos disponible para esta búsqueda'
      ))
      
      plot_SF_externa <- ggplot(base_plot_SF_externa,
                              aes(x = fct_reorder(pais, valor), y = valor)) +
        geom_segment(
          aes(
            x = fct_reorder(pais, valor),
            xend = fct_reorder(pais, valor),
            y = 0,
            yend = valor
          ),
          size = 1,
          color = "#2c3e50"
        ) +
        geom_point(color = "#2c3e50", size = 4) +
        theme_light() +
        coord_flip() +
        theme(
          axis.text.y = element_text(size = 12),
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_SF_externa_anual()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        if(input$indicador_SF_externa %in% vars_corte){
          
          ggtitle(paste0(input$indicador_SF_externa, " (", input$corte_SF_externa, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_SF_externa))
          
        }
      
      print(plot_SF_externa)
      ggsave(
        "www/indicador SF_externa.png",
        width = 25,
        height = 30,
        units = "cm"
      )
      
    }
    
  })
  
  output$map_SF_externa <- renderLeaflet({
    # Read this shape file with the rgdal library.
    world_spdf <- readOGR(dsn = "data" ,
                          layer = "TM_WORLD_BORDERS_SIMPL-0.3",
                          verbose = FALSE)
    
    # Pegar datos
    world_spdf@data <- world_spdf@data %>%
      left_join(select(dat_SF_externa_anual(), pais_eng, valor_original),
                by = c("NAME" = "pais_eng"))
    
    # Library
    
    # Create a color palette with handmade bins.
    
    
    if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= -1) {
      round_number <- .1
      
    }
    
    min_val <-
      plyr::round_any(min(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      floor)
    max_val <-
      plyr::round_any(max(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      ceiling)
    dif <- max_val - min_val
    dif_5 <- dif / 5
    val_1 <- min_val + dif_5
    val_2 <- min_val + (dif_5 * 2)
    val_3 <- min_val + (dif_5 * 3)
    val_4 <- min_val + (dif_5 * 4)
    
    mybins <- c(min_val, val_1, val_2, val_3, val_4, max_val)
    
    mypalette <- colorBin(
      palette = "YlOrBr",
      domain = world_spdf@data$valor_original,
      na.color = "transparent",
      bins = mybins
    )
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "País: ",
      world_spdf@data$NAME,
      "<br/>",
      "Valor: ",
      round(world_spdf@data$valor_original, 2),
      sep = ""
    ) %>%
      lapply(htmltools::HTML)
    
    # Final Map
    leaflet(world_spdf) %>%
      addTiles()  %>%
      setView(lat = 30,
              lng = 0 ,
              zoom = 1) %>%
      addPolygons(
        fillColor = ~ mypalette(valor_original),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = mypalette,
        values =  ~ valor_original,
        opacity = 0.9,
        title = wrapit(input$indicador_SF_externa),
        position = "bottomleft"
      )
    
  })
  
  # Botón descarga grafico
  output$baja_plot_SF_externa <- downloadHandler(filename <- function() {
    paste("indicador SF_externa", "png", sep = ".")
  },
  
  content <- function(file) {
    file.copy("www/indicador SF_externa.png", file)
  },
  contentType = "www/indicador SF_externa")
  
  
  ## Data series temporal
  
  # Data para tabla y exportar
  dat_SF_externa_st <- reactive({
    
    if (input$indicador_SF_externa %in% vars_corte) {
      
      dat_SF_externa() %>%
        filter(fecha >= input$fecha_dat_SF_externa[1] & fecha <= input$fecha_dat_SF_externa[2]) %>%
        filter(cod_pais %in% input$chbox_pais_SF_externa | pais %in% input$chbox_reg_SF_externa) %>%
        select(pais, fecha, names(dat_SF_externa()[,ncol(dat_SF_externa())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor)) 
      
    } else {
      
      dat_SF_externa() %>%
        filter(fecha >= input$fecha_dat_SF_externa[1] &
                 fecha <= input$fecha_dat_SF_externa[2]) %>%
        filter(cod_pais %in% input$chbox_pais_SF_externa |
                 pais %in% input$chbox_reg_SF_externa) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
  })
  
  # Metadata
  dat_SF_externa_m <- reactive({
    dat_SF_externa() %>%
      select(
        nomindicador,
        definicion,
        metodo_de_agregacion,
        concepto_estadistico_y_metodologia,
        relevancia,
        limitaciones_y_excepciones
      ) %>%
      mutate(`Unidad de Métodos y Acceso a Datos (FCS -UdelaR)` = " ") %>%
      distinct() %>%
      gather(key = "", value = " ")
    
  })
  
  # Data completa
  dat_SF_externa_c <- reactive({
    
    if (input$indicador_SF_externa %in% vars_corte) {
      
      dat_SF_externa() %>%
        select(pais, fecha, names(dat_SF_externa()[,ncol(dat_SF_externa())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_SF_externa() %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
    
  })
  
  # Lista para descarga
  list_dat_SF_externa_st <- reactive({
    list_dat_SF_externa <- list(
      "Data" = dat_SF_externa_st(),
      "Metadata" = dat_SF_externa_m(),
      "Data Completa" = dat_SF_externa_c()
    )
  })
  
  
  ## Data por año
  # Data para tabla y exportar
  dat_SF_externa_a <- reactive({
    
    if (input$indicador_SF_externa %in% vars_corte) {
      
      dat_SF_externa_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_SF_externa) %>%
        select(pais, fecha, names(dat_SF_externa()[,ncol(dat_SF_externa())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_SF_externa_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_SF_externa) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    }
  })
  
  # Lista para descarga
  list_dat_SF_externa_a <- reactive({
    list_dat_SF_externa <- list("Data" = dat_SF_externa_a(),
                              "Metadata" = dat_SF_externa_m(),
                              "Data Completa" = dat_SF_externa_c())
  })
  
  # Tablas en shiny
  output$tab_dat_SF_externa <- renderDT({
    
    if (input$indicador_SF_externa %in% vars_corte) {
      DT::datatable(
        dat_SF_externa_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Corte", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_SF_externa,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(4, '', mark = ",")
      
    } else if (input$visualizador_SF_externa == "Serie de tiempo") {
      DT::datatable(
        dat_SF_externa_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_SF_externa,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    } else if (input$visualizador_SF_externa %in% c("Anual gráfico", "Anual mapa")) {
      DT::datatable(
        dat_SF_externa_a(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_SF_externa,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    }
  })
  
  # Descarga tabla
  output$dl_tabla_dat_SF_externa <- downloadHandler(
    filename = function() {
      paste("resultados-", input$indicador_SF_externa, ".xlsx", sep = "")
    },
    content = function(file) {
      if (input$visualizador_SF_externa == "Serie de tiempo") {
        openxlsx::write.xlsx(list_dat_SF_externa_st(), file)
        
      } else if (input$visualizador_SF_externa %in% c("Anual gráfico", "Anual mapa")) {
        openxlsx::write.xlsx(list_dat_SF_externa_a(), file)
        
      }
    }
  )
  
  ##  14.  SF_inv (dat_SF_inv)   ====================================
  
  # Data SF_inv
  
  dat_SF_inv <- reactive({
    
    req(input$indicador_SF_inv)
    
    data %>%
      filter(nomindicador == input$indicador_SF_inv) %>% 
      janitor::remove_empty("cols")
    
  })
  
  
  # Titulo
  output$title_dat_SF_inv <- renderUI({
    helpText(HTML(unique(dat_SF_inv()$nomindicador)))
  })
  
  # Subtitulo
  output$subtitle_dat_SF_inv <- renderUI({
    helpText(HTML(unique(dat_SF_inv()$definicion)))
  })
  
  # Metodología
  output$info_dat_SF_inv <- renderUI({
    helpText(HTML(
      paste(
        "<b>Método de Agregación:</b>",
        unique(dat_SF_inv()$metodo_de_agregacion),
        "<b>Metodología:</b>",
        unique(dat_SF_inv()$concepto_estadistico_y_metodologia)
      )
    ))
  })
  
  # Relevancia:
  output$rel_dat_SF_inv <- renderUI({
    helpText(HTML(paste(
      "<b>Relvancia:</b>", unique(dat_SF_inv()$relevancia)
    )))
  })
  
  
  output$fecha_dat_SF_inv <- renderUI({
    if (input$visualizador_SF_inv == "Serie de tiempo") {
      tagList(
        tags$style(
          type = 'text/css',
          '#big_slider .irs-grid-text {font-size: 10px; transform: rotate(-90deg) translate(-15px);} ,.irs-grid-pol.large {height: 0px;}'
        ),
        div(
          id = 'big_slider',
          
          sliderTextInput(
            inputId = "fecha_dat_SF_inv",
            label = "Seleccione años",
            choices = sort(unique(dat_SF_inv()$fecha)),
            selected = c(min(dat_SF_inv()$fecha),
                         max(dat_SF_inv()$fecha)),
            hide_min_max = T,
            grid = TRUE,
            width = "100%"
          )
        )
      )
      
    } else {
      selectInput(
        inputId = "fecha_SF_inv",
        label = "Seleccione año:",
        choices = dat_SF_inv() %>%
          drop_na(valor) %>%
          select(fecha) %>%
          arrange(desc(fecha)) %>%
          unique() %>%
          pull(),
        selected = "2019"
      )
      
    }
    
  })
  
  
  output$corte_SF_inv <- renderUI({
    
    if (input$indicador_SF_inv %in% vars_corte) {
      
      selectInput(inputId = "corte_SF_inv",
                  label = "Seleccione categorías",
                  choices =  dat_SF_inv() %>%
                    distinct(get(names(dat_SF_inv()[,ncol(dat_SF_inv())]))) %>%
                    pull(),
                  selected = dat_SF_inv() %>%
                    filter(jerarquia == 1) %>% 
                    distinct(get(names(dat_SF_inv()[,ncol(dat_SF_inv())]))) %>%
                    pull()
      )
    } else {
      
      return(NULL)
    }
    
  })
  
  
  # Checkbox por pais
  output$sel_SF_inv_pais <- renderUI({
    if (input$visualizador_SF_inv == "Serie de tiempo") {
      dropdown(
        label = "Seleccione países",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_pais_SF_inv",
          label = "Seleccione países",
          inline = TRUE,
          choices = dat_SF_inv() %>%
            filter(region == 0) %>%
            filter(nomindicador == input$indicador_SF_inv) %>%
            distinct(cod_pais) %>%
            pull(),
          selected = c("URY", "ARG", "BRA", "PRY")
        )
      )
      
    } else {
      checkboxGroupInput(
        inputId = "chbox_pais_reg_SF_inv",
        label = "Mostrar:",
        inline = FALSE,
        choices = c("Países", "Regiones"),
        selected = "Países"
      )
      
    }
    
  })
  
  # Checkbox por región
  output$sel_SF_inv_region <- renderUI({
    if (input$visualizador_SF_inv == "Serie de tiempo") {
      dropdown(
        label = "Seleccione regiones",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_reg_SF_inv",
          label = "Seleccione regiones",
          inline = TRUE,
          choices = dat_SF_inv() %>%
            filter(region == 1) %>%
            filter(nomindicador == input$indicador_SF_inv) %>%
            distinct(pais) %>%
            pull(),
          selected = NULL
        )
      )
      
    } else {
      return(NULL)
      
    }
  })
  
  
  dat_SF_inv_anual <- reactive({
    
    if (input$indicador_SF_inv %in% vars_corte) {
      
      dat_SF_inv() %>%
        filter(get(names(dat_SF_inv()[,ncol(dat_SF_inv())])) %in% input$corte_SF_inv) %>%
        filter(fecha == input$fecha_SF_inv)
      
    } else {
      
      dat_SF_inv() %>%
        filter(fecha == input$fecha_SF_inv)
      
      
    }
  })
  
  dat_SF_inv_simple <- reactive({
    
    if (input$indicador_SF_inv %in% vars_corte) {
      
      dat_SF_inv() %>%
        filter(get(names(dat_SF_inv()[,ncol(dat_SF_inv())])) %in% input$corte_SF_inv) %>% 
        filter(fecha >= input$fecha_dat_SF_inv[1] & fecha <= input$fecha_dat_SF_inv[2]) %>%
        filter(cod_pais %in% input$chbox_pais_SF_inv | pais %in% input$chbox_reg_SF_inv)
      
    } else {
      
      dat_SF_inv() %>%
        filter(fecha >= input$fecha_dat_SF_inv[1] & fecha <= input$fecha_dat_SF_inv[2]) %>%
        filter(cod_pais %in% input$chbox_pais_SF_inv | pais %in% input$chbox_reg_SF_inv)        
    }
    
  })
  
  
  # Gráficos SF_inv
  output$p_dat_SF_inv <- renderPlot({
    
    if (input$visualizador_SF_inv == "Serie de tiempo" ) {
      
      req(input$fecha_dat_SF_inv, input$indicador_SF_inv)
      
      plot_SF_inv <- ggplot(
        data = dat_SF_inv_simple(), 
        aes(x = fecha, y = valor)
      ) +
        geom_line(aes(color = pais), size = 1, alpha = 0.5) +
        geom_point(aes(color = pais), size = 3) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_SF_inv_simple()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        scale_colour_manual(name = "", values = paleta_expandida) +
        if(input$indicador_SF_inv %in% vars_corte){
          
          ggtitle(paste0(input$indicador_SF_inv, " (", input$corte_SF_inv, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_SF_inv))
          
        }
      
      print(plot_SF_inv)
      ggsave(
        "www/indicador SF_inv.png",
        width = 30,
        height = 20,
        units = "cm"
      )
      
    } else if (input$visualizador_SF_inv == "Anual gráfico") {
      
      base_plot_SF_inv <- dat_SF_inv_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_SF_inv)
      
      validate(need(
        nrow(base_plot_SF_inv) > 0,
        'No hay datos disponible para esta búsqueda'
      ))
      
      plot_SF_inv <- ggplot(base_plot_SF_inv,
                              aes(x = fct_reorder(pais, valor), y = valor)) +
        geom_segment(
          aes(
            x = fct_reorder(pais, valor),
            xend = fct_reorder(pais, valor),
            y = 0,
            yend = valor
          ),
          size = 1,
          color = "#2c3e50"
        ) +
        geom_point(color = "#2c3e50", size = 4) +
        theme_light() +
        coord_flip() +
        theme(
          axis.text.y = element_text(size = 12),
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_SF_inv_anual()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        if(input$indicador_SF_inv %in% vars_corte){
          
          ggtitle(paste0(input$indicador_SF_inv, " (", input$corte_SF_inv, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_SF_inv))
          
        }
      
      print(plot_SF_inv)
      ggsave(
        "www/indicador SF_inv.png",
        width = 25,
        height = 30,
        units = "cm"
      )
      
    }
    
  })
  
  output$map_SF_inv <- renderLeaflet({
    # Read this shape file with the rgdal library.
    world_spdf <- readOGR(dsn = "data" ,
                          layer = "TM_WORLD_BORDERS_SIMPL-0.3",
                          verbose = FALSE)
    
    # Pegar datos
    world_spdf@data <- world_spdf@data %>%
      left_join(select(dat_SF_inv_anual(), pais_eng, valor_original),
                by = c("NAME" = "pais_eng"))
    
    # Library
    
    # Create a color palette with handmade bins.
    
    
    if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= -1) {
      round_number <- .1
      
    }
    
    min_val <-
      plyr::round_any(min(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      floor)
    max_val <-
      plyr::round_any(max(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      ceiling)
    dif <- max_val - min_val
    dif_5 <- dif / 5
    val_1 <- min_val + dif_5
    val_2 <- min_val + (dif_5 * 2)
    val_3 <- min_val + (dif_5 * 3)
    val_4 <- min_val + (dif_5 * 4)
    
    mybins <- c(min_val, val_1, val_2, val_3, val_4, max_val)
    
    mypalette <- colorBin(
      palette = "YlOrBr",
      domain = world_spdf@data$valor_original,
      na.color = "transparent",
      bins = mybins
    )
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "País: ",
      world_spdf@data$NAME,
      "<br/>",
      "Valor: ",
      round(world_spdf@data$valor_original, 2),
      sep = ""
    ) %>%
      lapply(htmltools::HTML)
    
    # Final Map
    leaflet(world_spdf) %>%
      addTiles()  %>%
      setView(lat = 30,
              lng = 0 ,
              zoom = 1) %>%
      addPolygons(
        fillColor = ~ mypalette(valor_original),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = mypalette,
        values =  ~ valor_original,
        opacity = 0.9,
        title = wrapit(input$indicador_SF_inv),
        position = "bottomleft"
      )
    
  })
  
  # Botón descarga grafico
  output$baja_plot_SF_inv <- downloadHandler(filename <- function() {
    paste("indicador SF_inv", "png", sep = ".")
  },
  
  content <- function(file) {
    file.copy("www/indicador SF_inv.png", file)
  },
  contentType = "www/indicador SF_inv")
  
  
  ## Data series temporal
  
  # Data para tabla y exportar
  dat_SF_inv_st <- reactive({
    
    if (input$indicador_SF_inv %in% vars_corte) {
      
      dat_SF_inv() %>%
        filter(fecha >= input$fecha_dat_SF_inv[1] & fecha <= input$fecha_dat_SF_inv[2]) %>%
        filter(cod_pais %in% input$chbox_pais_SF_inv | pais %in% input$chbox_reg_SF_inv) %>%
        select(pais, fecha, names(dat_SF_inv()[,ncol(dat_SF_inv())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor)) 
      
    } else {
      
      dat_SF_inv() %>%
        filter(fecha >= input$fecha_dat_SF_inv[1] &
                 fecha <= input$fecha_dat_SF_inv[2]) %>%
        filter(cod_pais %in% input$chbox_pais_SF_inv |
                 pais %in% input$chbox_reg_SF_inv) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
  })
  
  # Metadata
  dat_SF_inv_m <- reactive({
    dat_SF_inv() %>%
      select(
        nomindicador,
        definicion,
        metodo_de_agregacion,
        concepto_estadistico_y_metodologia,
        relevancia,
        limitaciones_y_excepciones
      ) %>%
      mutate(`Unidad de Métodos y Acceso a Datos (FCS -UdelaR)` = " ") %>%
      distinct() %>%
      gather(key = "", value = " ")
    
  })
  
  # Data completa
  dat_SF_inv_c <- reactive({
    
    if (input$indicador_SF_inv %in% vars_corte) {
      
      dat_SF_inv() %>%
        select(pais, fecha, names(dat_SF_inv()[,ncol(dat_SF_inv())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_SF_inv() %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
    
  })
  
  # Lista para descarga
  list_dat_SF_inv_st <- reactive({
    list_dat_SF_inv <- list(
      "Data" = dat_SF_inv_st(),
      "Metadata" = dat_SF_inv_m(),
      "Data Completa" = dat_SF_inv_c()
    )
  })
  
  
  ## Data por año
  # Data para tabla y exportar
  dat_SF_inv_a <- reactive({
    
    if (input$indicador_SF_inv %in% vars_corte) {
      
      dat_SF_inv_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_SF_inv) %>%
        select(pais, fecha, names(dat_SF_inv()[,ncol(dat_SF_inv())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_SF_inv_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_SF_inv) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    }
  })
  
  # Lista para descarga
  list_dat_SF_inv_a <- reactive({
    list_dat_SF_inv <- list("Data" = dat_SF_inv_a(),
                              "Metadata" = dat_SF_inv_m(),
                            "Data Completa" = dat_SF_inv_c())
  })
  
  # Tablas en shiny
  output$tab_dat_SF_inv <- renderDT({
    
    if (input$indicador_SF_inv %in% vars_corte) {
      DT::datatable(
        dat_SF_inv_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Corte", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_SF_inv,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(4, '', mark = ",")
      
    } else if (input$visualizador_SF_inv == "Serie de tiempo") {
      DT::datatable(
        dat_SF_inv_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_SF_inv,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    } else if (input$visualizador_SF_inv %in% c("Anual gráfico", "Anual mapa")) {
      DT::datatable(
        dat_SF_inv_a(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_SF_inv,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    }
  })
  
  # Descarga tabla
  output$dl_tabla_dat_SF_inv <- downloadHandler(
    filename = function() {
      paste("resultados-", input$indicador_SF_inv, ".xlsx", sep = "")
    },
    content = function(file) {
      if (input$visualizador_SF_inv == "Serie de tiempo") {
        openxlsx::write.xlsx(list_dat_SF_inv_st(), file)
        
      } else if (input$visualizador_SF_inv %in% c("Anual gráfico", "Anual mapa")) {
        openxlsx::write.xlsx(list_dat_SF_inv_a(), file)
        
      }
    }
  )
  
  ##  15.  SF_mon (dat_SF_mon)   ====================================
  
  # Data SF_mon
  
  dat_SF_mon <- reactive({
    
    req(input$indicador_SF_mon)
    
    data %>%
      filter(nomindicador == input$indicador_SF_mon) %>% 
      janitor::remove_empty("cols")
    
  })
  
  
  # Titulo
  output$title_dat_SF_mon <- renderUI({
    helpText(HTML(unique(dat_SF_mon()$nomindicador)))
  })
  
  # Subtitulo
  output$subtitle_dat_SF_mon <- renderUI({
    helpText(HTML(unique(dat_SF_mon()$definicion)))
  })
  
  # Metodología
  output$info_dat_SF_mon <- renderUI({
    helpText(HTML(
      paste(
        "<b>Método de Agregación:</b>",
        unique(dat_SF_mon()$metodo_de_agregacion),
        "<b>Metodología:</b>",
        unique(dat_SF_mon()$concepto_estadistico_y_metodologia)
      )
    ))
  })
  
  # Relevancia:
  output$rel_dat_SF_mon <- renderUI({
    helpText(HTML(paste(
      "<b>Relvancia:</b>", unique(dat_SF_mon()$relevancia)
    )))
  })
  
  
  output$fecha_dat_SF_mon <- renderUI({
    if (input$visualizador_SF_mon == "Serie de tiempo") {
      tagList(
        tags$style(
          type = 'text/css',
          '#big_slider .irs-grid-text {font-size: 10px; transform: rotate(-90deg) translate(-15px);} ,.irs-grid-pol.large {height: 0px;}'
        ),
        div(
          id = 'big_slider',
          
          sliderTextInput(
            inputId = "fecha_dat_SF_mon",
            label = "Seleccione años",
            choices = sort(unique(dat_SF_mon()$fecha)),
            selected = c(min(dat_SF_mon()$fecha),
                         max(dat_SF_mon()$fecha)),
            hide_min_max = T,
            grid = TRUE,
            width = "100%"
          )
        )
      )
      
    } else {
      selectInput(
        inputId = "fecha_SF_mon",
        label = "Seleccione año:",
        choices = dat_SF_mon() %>%
          drop_na(valor) %>%
          select(fecha) %>%
          arrange(desc(fecha)) %>%
          unique() %>%
          pull(),
        selected = "2019"
      )
      
    }
    
  })
  
  
  output$corte_SF_mon <- renderUI({
    
    if (input$indicador_SF_mon %in% vars_corte) {
      
      selectInput(inputId = "corte_SF_mon",
                  label = "Seleccione categorías",
                  choices =  dat_SF_mon() %>%
                    distinct(get(names(dat_SF_mon()[,ncol(dat_SF_mon())]))) %>%
                    pull(),
                  selected = dat_SF_mon() %>%
                    filter(jerarquia == 1) %>% 
                    distinct(get(names(dat_SF_mon()[,ncol(dat_SF_mon())]))) %>%
                    pull()
      )
    } else {
      
      return(NULL)
    }
    
  })
  
  
  # Checkbox por pais
  output$sel_SF_mon_pais <- renderUI({
    if (input$visualizador_SF_mon == "Serie de tiempo") {
      dropdown(
        label = "Seleccione países",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_pais_SF_mon",
          label = "Seleccione países",
          inline = TRUE,
          choices = dat_SF_mon() %>%
            filter(region == 0) %>%
            filter(nomindicador == input$indicador_SF_mon) %>%
            distinct(cod_pais) %>%
            pull(),
          selected = c("URY", "ARG", "BRA", "PRY")
        )
      )
      
    } else {
      checkboxGroupInput(
        inputId = "chbox_pais_reg_SF_mon",
        label = "Mostrar:",
        inline = FALSE,
        choices = c("Países", "Regiones"),
        selected = "Países"
      )
      
    }
    
  })
  
  # Checkbox por región
  output$sel_SF_mon_region <- renderUI({
    if (input$visualizador_SF_mon == "Serie de tiempo") {
      dropdown(
        label = "Seleccione regiones",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_reg_SF_mon",
          label = "Seleccione regiones",
          inline = TRUE,
          choices = dat_SF_mon() %>%
            filter(region == 1) %>%
            filter(nomindicador == input$indicador_SF_mon) %>%
            distinct(pais) %>%
            pull(),
          selected = NULL
        )
      )
      
    } else {
      return(NULL)
      
    }
  })
  
  
  dat_SF_mon_anual <- reactive({
    
    if (input$indicador_SF_mon %in% vars_corte) {
      
      dat_SF_mon() %>%
        filter(get(names(dat_SF_mon()[,ncol(dat_SF_mon())])) %in% input$corte_SF_mon) %>%
        filter(fecha == input$fecha_SF_mon)
      
    } else {
      
      dat_SF_mon() %>%
        filter(fecha == input$fecha_SF_mon)
      
      
    }
  })
  
  dat_SF_mon_simple <- reactive({
    
    if (input$indicador_SF_mon %in% vars_corte) {
      
      dat_SF_mon() %>%
        filter(get(names(dat_SF_mon()[,ncol(dat_SF_mon())])) %in% input$corte_SF_mon) %>% 
        filter(fecha >= input$fecha_dat_SF_mon[1] & fecha <= input$fecha_dat_SF_mon[2]) %>%
        filter(cod_pais %in% input$chbox_pais_SF_mon | pais %in% input$chbox_reg_SF_mon)
      
    } else {
      
      dat_SF_mon() %>%
        filter(fecha >= input$fecha_dat_SF_mon[1] & fecha <= input$fecha_dat_SF_mon[2]) %>%
        filter(cod_pais %in% input$chbox_pais_SF_mon | pais %in% input$chbox_reg_SF_mon)        
    }
    
  })
  
  
  # Gráficos SF_mon
  output$p_dat_SF_mon <- renderPlot({
    
    if (input$visualizador_SF_mon == "Serie de tiempo" ) {
      
      req(input$fecha_dat_SF_mon, input$indicador_SF_mon)
      
      plot_SF_mon <- ggplot(
        data = dat_SF_mon_simple(), 
        aes(x = fecha, y = valor)
      ) +
        geom_line(aes(color = pais), size = 1, alpha = 0.5) +
        geom_point(aes(color = pais), size = 3) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_SF_mon_simple()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        scale_colour_manual(name = "", values = paleta_expandida) +
        if(input$indicador_SF_mon %in% vars_corte){
          
          ggtitle(paste0(input$indicador_SF_mon, " (", input$corte_SF_mon, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_SF_mon))
          
        }
      
      print(plot_SF_mon)
      ggsave(
        "www/indicador SF_mon.png",
        width = 30,
        height = 20,
        units = "cm"
      )
      
    } else if (input$visualizador_SF_mon == "Anual gráfico") {
      
      base_plot_SF_mon <- dat_SF_mon_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_SF_mon)
      
      validate(need(
        nrow(base_plot_SF_mon) > 0,
        'No hay datos disponible para esta búsqueda'
      ))
      
      plot_SF_mon <- ggplot(base_plot_SF_mon,
                              aes(x = fct_reorder(pais, valor), y = valor)) +
        geom_segment(
          aes(
            x = fct_reorder(pais, valor),
            xend = fct_reorder(pais, valor),
            y = 0,
            yend = valor
          ),
          size = 1,
          color = "#2c3e50"
        ) +
        geom_point(color = "#2c3e50", size = 4) +
        theme_light() +
        coord_flip() +
        theme(
          axis.text.y = element_text(size = 12),
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_SF_mon_anual()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        if(input$indicador_SF_mon %in% vars_corte){
          
          ggtitle(paste0(input$indicador_SF_mon, " (", input$corte_SF_mon, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_SF_mon))
          
        }
      
      print(plot_SF_mon)
      ggsave(
        "www/indicador SF_mon.png",
        width = 25,
        height = 30,
        units = "cm"
      )
      
    }
    
  })
  
  output$map_SF_mon <- renderLeaflet({
    # Read this shape file with the rgdal library.
    world_spdf <- readOGR(dsn = "data" ,
                          layer = "TM_WORLD_BORDERS_SIMPL-0.3",
                          verbose = FALSE)
    
    # Pegar datos
    world_spdf@data <- world_spdf@data %>%
      left_join(select(dat_SF_mon_anual(), pais_eng, valor_original),
                by = c("NAME" = "pais_eng"))
    
    # Library
    
    # Create a color palette with handmade bins.
    
    
    if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= -1) {
      round_number <- .1
      
    }
    
    min_val <-
      plyr::round_any(min(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      floor)
    max_val <-
      plyr::round_any(max(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      ceiling)
    dif <- max_val - min_val
    dif_5 <- dif / 5
    val_1 <- min_val + dif_5
    val_2 <- min_val + (dif_5 * 2)
    val_3 <- min_val + (dif_5 * 3)
    val_4 <- min_val + (dif_5 * 4)
    
    mybins <- c(min_val, val_1, val_2, val_3, val_4, max_val)
    
    mypalette <- colorBin(
      palette = "YlOrBr",
      domain = world_spdf@data$valor_original,
      na.color = "transparent",
      bins = mybins
    )
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "País: ",
      world_spdf@data$NAME,
      "<br/>",
      "Valor: ",
      round(world_spdf@data$valor_original, 2),
      sep = ""
    ) %>%
      lapply(htmltools::HTML)
    
    # Final Map
    leaflet(world_spdf) %>%
      addTiles()  %>%
      setView(lat = 30,
              lng = 0 ,
              zoom = 1) %>%
      addPolygons(
        fillColor = ~ mypalette(valor_original),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = mypalette,
        values =  ~ valor_original,
        opacity = 0.9,
        title = wrapit(input$indicador_SF_mon),
        position = "bottomleft"
      )
    
  })
  
  # Botón descarga grafico
  output$baja_plot_SF_mon <- downloadHandler(filename <- function() {
    paste("indicador SF_mon", "png", sep = ".")
  },
  
  content <- function(file) {
    file.copy("www/indicador SF_mon.png", file)
  },
  contentType = "www/indicador SF_mon")
  
  
  ## Data series temporal
  
  # Data para tabla y exportar
  dat_SF_mon_st <- reactive({
    
    if (input$indicador_SF_mon %in% vars_corte) {
      
      dat_SF_mon() %>%
        filter(fecha >= input$fecha_dat_SF_mon[1] & fecha <= input$fecha_dat_SF_mon[2]) %>%
        filter(cod_pais %in% input$chbox_pais_SF_mon | pais %in% input$chbox_reg_SF_mon) %>%
        select(pais, fecha, names(dat_SF_mon()[,ncol(dat_SF_mon())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor)) 
      
    } else {
      
      dat_SF_mon() %>%
        filter(fecha >= input$fecha_dat_SF_mon[1] &
                 fecha <= input$fecha_dat_SF_mon[2]) %>%
        filter(cod_pais %in% input$chbox_pais_SF_mon |
                 pais %in% input$chbox_reg_SF_mon) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
  })
  
  # Metadata
  dat_SF_mon_m <- reactive({
    dat_SF_mon() %>%
      select(
        nomindicador,
        definicion,
        metodo_de_agregacion,
        concepto_estadistico_y_metodologia,
        relevancia,
        limitaciones_y_excepciones
      ) %>%
      mutate(`Unidad de Métodos y Acceso a Datos (FCS -UdelaR)` = " ") %>%
      distinct() %>%
      gather(key = "", value = " ")
    
  })
  
  # Data completa
  dat_SF_mon_c <- reactive({
    
    if (input$indicador_SF_mon %in% vars_corte) {
      
      dat_SF_mon() %>%
        select(pais, fecha, names(dat_SF_mon()[,ncol(dat_SF_mon())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_SF_mon() %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
    
  })
  
  # Lista para descarga
  list_dat_SF_mon_st <- reactive({
    list_dat_SF_mon <- list(
      "Data" = dat_SF_mon_st(),
      "Metadata" = dat_SF_mon_m(),
      "Data Completa" = dat_SF_mon_c()
    )
  })
  
  
  ## Data por año
  # Data para tabla y exportar
  dat_SF_mon_a <- reactive({
    
    if (input$indicador_SF_mon %in% vars_corte) {
      
      dat_SF_mon_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_SF_mon) %>%
        select(pais, fecha, names(dat_SF_mon()[,ncol(dat_SF_mon())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_SF_mon_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_SF_mon) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    }
  })
  
  # Lista para descarga
  list_dat_SF_mon_a <- reactive({
    list_dat_SF_mon <- list("Data" = dat_SF_mon_a(),
                              "Metadata" = dat_SF_mon_m(),
                            "Data Completa" = dat_SF_mon_c())
  })
  
  # Tablas en shiny
  output$tab_dat_SF_mon <- renderDT({
    
    if (input$indicador_SF_mon %in% vars_corte) {
      DT::datatable(
        dat_SF_mon_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Corte", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_SF_mon,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(4, '', mark = ",")
      
    } else if (input$visualizador_SF_mon == "Serie de tiempo") {
      DT::datatable(
        dat_SF_mon_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_SF_mon,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    } else if (input$visualizador_SF_mon %in% c("Anual gráfico", "Anual mapa")) {
      DT::datatable(
        dat_SF_mon_a(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_SF_mon,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    }
  })
  
  # Descarga tabla
  output$dl_tabla_dat_SF_mon <- downloadHandler(
    filename = function() {
      paste("resultados-", input$indicador_SF_mon, ".xlsx", sep = "")
    },
    content = function(file) {
      if (input$visualizador_SF_mon == "Serie de tiempo") {
        openxlsx::write.xlsx(list_dat_SF_mon_st(), file)
        
      } else if (input$visualizador_SF_mon %in% c("Anual gráfico", "Anual mapa")) {
        openxlsx::write.xlsx(list_dat_SF_mon_a(), file)
        
      }
    }
  )
  
  ##  16.  EN_consumo (dat_EN_consumo)   ====================================
  
  # Data EN_consumo
  
  dat_EN_consumo <- reactive({
    
    req(input$indicador_EN_consumo)
    
    data %>%
      filter(nomindicador == input$indicador_EN_consumo) %>% 
      janitor::remove_empty("cols")
    
  })
  
  
  # Titulo
  output$title_dat_EN_consumo <- renderUI({
    helpText(HTML(unique(dat_EN_consumo()$nomindicador)))
  })
  
  # Subtitulo
  output$subtitle_dat_EN_consumo <- renderUI({
    helpText(HTML(unique(dat_EN_consumo()$definicion)))
  })
  
  # Metodología
  output$info_dat_EN_consumo <- renderUI({
    helpText(HTML(
      paste(
        "<b>Método de Agregación:</b>",
        unique(dat_EN_consumo()$metodo_de_agregacion),
        "<b>Metodología:</b>",
        unique(dat_EN_consumo()$concepto_estadistico_y_metodologia)
      )
    ))
  })
  
  # Relevancia:
  output$rel_dat_EN_consumo <- renderUI({
    helpText(HTML(paste(
      "<b>Relvancia:</b>", unique(dat_EN_consumo()$relevancia)
    )))
  })
  
  
  output$fecha_dat_EN_consumo <- renderUI({
    if (input$visualizador_EN_consumo == "Serie de tiempo") {
      tagList(
        tags$style(
          type = 'text/css',
          '#big_slider .irs-grid-text {font-size: 10px; transform: rotate(-90deg) translate(-15px);} ,.irs-grid-pol.large {height: 0px;}'
        ),
        div(
          id = 'big_slider',
          
          sliderTextInput(
            inputId = "fecha_dat_EN_consumo",
            label = "Seleccione años",
            choices = sort(unique(dat_EN_consumo()$fecha)),
            selected = c(min(dat_EN_consumo()$fecha),
                         max(dat_EN_consumo()$fecha)),
            hide_min_max = T,
            grid = TRUE,
            width = "100%"
          )
        )
      )
      
    } else {
      selectInput(
        inputId = "fecha_EN_consumo",
        label = "Seleccione año:",
        choices = dat_EN_consumo() %>%
          drop_na(valor) %>%
          select(fecha) %>%
          arrange(desc(fecha)) %>%
          unique() %>%
          pull(),
        selected = "2019"
      )
      
    }
    
  })
  
  
  output$corte_EN_consumo <- renderUI({
    
    if (input$indicador_EN_consumo %in% vars_corte) {
      
      selectInput(inputId = "corte_EN_consumo",
                  label = "Seleccione categorías",
                  choices =  dat_EN_consumo() %>%
                    distinct(get(names(dat_EN_consumo()[,ncol(dat_EN_consumo())]))) %>%
                    pull(),
                  selected = dat_EN_consumo() %>%
                    filter(jerarquia == 1) %>% 
                    distinct(get(names(dat_EN_consumo()[,ncol(dat_EN_consumo())]))) %>%
                    pull()
      )
    } else {
      
      return(NULL)
    }
    
  })
  
  
  # Checkbox por pais
  output$sel_EN_consumo_pais <- renderUI({
    if (input$visualizador_EN_consumo == "Serie de tiempo") {
      dropdown(
        label = "Seleccione países",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_pais_EN_consumo",
          label = "Seleccione países",
          inline = TRUE,
          choices = dat_EN_consumo() %>%
            filter(region == 0) %>%
            filter(nomindicador == input$indicador_EN_consumo) %>%
            distinct(cod_pais) %>%
            pull(),
          selected = c("URY", "ARG", "BRA", "PRY")
        )
      )
      
    } else {
      checkboxGroupInput(
        inputId = "chbox_pais_reg_EN_consumo",
        label = "Mostrar:",
        inline = FALSE,
        choices = c("Países", "Regiones"),
        selected = "Países"
      )
      
    }
    
  })
  
  # Checkbox por región
  output$sel_EN_consumo_region <- renderUI({
    if (input$visualizador_EN_consumo == "Serie de tiempo") {
      dropdown(
        label = "Seleccione regiones",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_reg_EN_consumo",
          label = "Seleccione regiones",
          inline = TRUE,
          choices = dat_EN_consumo() %>%
            filter(region == 1) %>%
            filter(nomindicador == input$indicador_EN_consumo) %>%
            distinct(pais) %>%
            pull(),
          selected = NULL
        )
      )
      
    } else {
      return(NULL)
      
    }
  })
  
  
  dat_EN_consumo_anual <- reactive({
    
    if (input$indicador_EN_consumo %in% vars_corte) {
      
      dat_EN_consumo() %>%
        filter(get(names(dat_EN_consumo()[,ncol(dat_EN_consumo())])) %in% input$corte_EN_consumo) %>%
        filter(fecha == input$fecha_EN_consumo)
      
    } else {
      
      dat_EN_consumo() %>%
        filter(fecha == input$fecha_EN_consumo)
      
      
    }
  })
  
  dat_EN_consumo_simple <- reactive({
    
    if (input$indicador_EN_consumo %in% vars_corte) {
      
      dat_EN_consumo() %>%
        filter(get(names(dat_EN_consumo()[,ncol(dat_EN_consumo())])) %in% input$corte_EN_consumo) %>% 
        filter(fecha >= input$fecha_dat_EN_consumo[1] & fecha <= input$fecha_dat_EN_consumo[2]) %>%
        filter(cod_pais %in% input$chbox_pais_EN_consumo | pais %in% input$chbox_reg_EN_consumo)
      
    } else {
      
      dat_EN_consumo() %>%
        filter(fecha >= input$fecha_dat_EN_consumo[1] & fecha <= input$fecha_dat_EN_consumo[2]) %>%
        filter(cod_pais %in% input$chbox_pais_EN_consumo | pais %in% input$chbox_reg_EN_consumo)        
    }
    
  })
  
  
  # Gráficos EN_consumo
  output$p_dat_EN_consumo <- renderPlot({
    
    if (input$visualizador_EN_consumo == "Serie de tiempo" ) {
      
      req(input$fecha_dat_EN_consumo, input$indicador_EN_consumo)
      
      plot_EN_consumo <- ggplot(
        data = dat_EN_consumo_simple(), 
        aes(x = fecha, y = valor)
      ) +
        geom_line(aes(color = pais), size = 1, alpha = 0.5) +
        geom_point(aes(color = pais), size = 3) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_EN_consumo_simple()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        scale_colour_manual(name = "", values = paleta_expandida) +
        if(input$indicador_EN_consumo %in% vars_corte){
          
          ggtitle(paste0(input$indicador_EN_consumo, " (", input$corte_EN_consumo, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_EN_consumo))
          
        }
      
      print(plot_EN_consumo)
      ggsave(
        "www/indicador EN_consumo.png",
        width = 30,
        height = 20,
        units = "cm"
      )
      
    } else if (input$visualizador_EN_consumo == "Anual gráfico") {
      
      base_plot_EN_consumo <- dat_EN_consumo_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_EN_consumo)
      
      validate(need(
        nrow(base_plot_EN_consumo) > 0,
        'No hay datos disponible para esta búsqueda'
      ))
      
      plot_EN_consumo <- ggplot(base_plot_EN_consumo,
                            aes(x = fct_reorder(pais, valor), y = valor)) +
        geom_segment(
          aes(
            x = fct_reorder(pais, valor),
            xend = fct_reorder(pais, valor),
            y = 0,
            yend = valor
          ),
          size = 1,
          color = "#2c3e50"
        ) +
        geom_point(color = "#2c3e50", size = 4) +
        theme_light() +
        coord_flip() +
        theme(
          axis.text.y = element_text(size = 12),
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_EN_consumo_anual()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        if(input$indicador_EN_consumo %in% vars_corte){
          
          ggtitle(paste0(input$indicador_EN_consumo, " (", input$corte_EN_consumo, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_EN_consumo))
          
        }
      
      print(plot_EN_consumo)
      ggsave(
        "www/indicador EN_consumo.png",
        width = 25,
        height = 30,
        units = "cm"
      )
      
    }
    
  })
  
  output$map_EN_consumo <- renderLeaflet({
    # Read this shape file with the rgdal library.
    world_spdf <- readOGR(dsn = "data" ,
                          layer = "TM_WORLD_BORDERS_SIMPL-0.3",
                          verbose = FALSE)
    
    # Pegar datos
    world_spdf@data <- world_spdf@data %>%
      left_join(select(dat_EN_consumo_anual(), pais_eng, valor_original),
                by = c("NAME" = "pais_eng"))
    
    # Library
    
    # Create a color palette with handmade bins.
    
    
    if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= -1) {
      round_number <- .1
      
    }
    
    min_val <-
      plyr::round_any(min(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      floor)
    max_val <-
      plyr::round_any(max(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      ceiling)
    dif <- max_val - min_val
    dif_5 <- dif / 5
    val_1 <- min_val + dif_5
    val_2 <- min_val + (dif_5 * 2)
    val_3 <- min_val + (dif_5 * 3)
    val_4 <- min_val + (dif_5 * 4)
    
    mybins <- c(min_val, val_1, val_2, val_3, val_4, max_val)
    
    mypalette <- colorBin(
      palette = "YlOrBr",
      domain = world_spdf@data$valor_original,
      na.color = "transparent",
      bins = mybins
    )
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "País: ",
      world_spdf@data$NAME,
      "<br/>",
      "Valor: ",
      round(world_spdf@data$valor_original, 2),
      sep = ""
    ) %>%
      lapply(htmltools::HTML)
    
    # Final Map
    leaflet(world_spdf) %>%
      addTiles()  %>%
      setView(lat = 30,
              lng = 0 ,
              zoom = 1) %>%
      addPolygons(
        fillColor = ~ mypalette(valor_original),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = mypalette,
        values =  ~ valor_original,
        opacity = 0.9,
        title = wrapit(input$indicador_EN_consumo),
        position = "bottomleft"
      )
    
  })
  
  # Botón descarga grafico
  output$baja_plot_EN_consumo <- downloadHandler(filename <- function() {
    paste("indicador EN_consumo", "png", sep = ".")
  },
  
  content <- function(file) {
    file.copy("www/indicador EN_consumo.png", file)
  },
  contentType = "www/indicador EN_consumo")
  
  
  ## Data series temporal
  
  # Data para tabla y exportar
  dat_EN_consumo_st <- reactive({
    
    if (input$indicador_EN_consumo %in% vars_corte) {
      
      dat_EN_consumo() %>%
        filter(fecha >= input$fecha_dat_EN_consumo[1] & fecha <= input$fecha_dat_EN_consumo[2]) %>%
        filter(cod_pais %in% input$chbox_pais_EN_consumo | pais %in% input$chbox_reg_EN_consumo) %>%
        select(pais, fecha, names(dat_EN_consumo()[,ncol(dat_EN_consumo())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor)) 
      
    } else {
      
      dat_EN_consumo() %>%
        filter(fecha >= input$fecha_dat_EN_consumo[1] &
                 fecha <= input$fecha_dat_EN_consumo[2]) %>%
        filter(cod_pais %in% input$chbox_pais_EN_consumo |
                 pais %in% input$chbox_reg_EN_consumo) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
  })
  
  # Metadata
  dat_EN_consumo_m <- reactive({
    dat_EN_consumo() %>%
      select(
        nomindicador,
        definicion,
        metodo_de_agregacion,
        concepto_estadistico_y_metodologia,
        relevancia,
        limitaciones_y_excepciones
      ) %>%
      mutate(`Unidad de Métodos y Acceso a Datos (FCS -UdelaR)` = " ") %>%
      distinct() %>%
      gather(key = "", value = " ")
    
  })
  
  # Data completa
  dat_EN_consumo_c <- reactive({
    
    if (input$indicador_EN_consumo %in% vars_corte) {
      
      dat_EN_consumo() %>%
        select(pais, fecha, names(dat_EN_consumo()[,ncol(dat_EN_consumo())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_EN_consumo() %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
    
  })
  
  # Lista para descarga
  list_dat_EN_consumo_st <- reactive({
    list_dat_EN_consumo <- list(
      "Data" = dat_EN_consumo_st(),
      "Metadata" = dat_EN_consumo_m(),
      "Data Completa" = dat_EN_consumo_c()
    )
  })
  
  
  ## Data por año
  # Data para tabla y exportar
  dat_EN_consumo_a <- reactive({
    
    if (input$indicador_EN_consumo %in% vars_corte) {
      
      dat_EN_consumo_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_EN_consumo) %>%
        select(pais, fecha, names(dat_EN_consumo()[,ncol(dat_EN_consumo())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_EN_consumo_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_EN_consumo) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    }
  })
  
  # Lista para descarga
  list_dat_EN_consumo_a <- reactive({
    list_dat_EN_consumo <- list("Data" = dat_EN_consumo_a(),
                            "Metadata" = dat_EN_consumo_m(),
                            "Data Completa" = dat_EN_consumo_c())
  })
  
  # Tablas en shiny
  output$tab_dat_EN_consumo <- renderDT({
    
    if (input$indicador_EN_consumo %in% vars_corte) {
      DT::datatable(
        dat_EN_consumo_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Corte", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_EN_consumo,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(4, '', mark = ",")
      
    } else if (input$visualizador_EN_consumo == "Serie de tiempo") {
      DT::datatable(
        dat_EN_consumo_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_EN_consumo,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    } else if (input$visualizador_EN_consumo %in% c("Anual gráfico", "Anual mapa")) {
      DT::datatable(
        dat_EN_consumo_a(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_EN_consumo,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    }
  })
  
  # Descarga tabla
  output$dl_tabla_dat_EN_consumo <- downloadHandler(
    filename = function() {
      paste("resultados-", input$indicador_EN_consumo, ".xlsx", sep = "")
    },
    content = function(file) {
      if (input$visualizador_EN_consumo == "Serie de tiempo") {
        openxlsx::write.xlsx(list_dat_EN_consumo_st(), file)
        
      } else if (input$visualizador_EN_consumo %in% c("Anual gráfico", "Anual mapa")) {
        openxlsx::write.xlsx(list_dat_EN_consumo_a(), file)
        
      }
    }
  )
  
  ##  17.  EN_emisiones (dat_EN_emisiones)   ====================================
  
  # Data EN_emisiones
  
  dat_EN_emisiones <- reactive({
    
    req(input$indicador_EN_emisiones)
    
    data %>%
      filter(nomindicador == input$indicador_EN_emisiones) %>% 
      janitor::remove_empty("cols")
    
  })
  
  
  # Titulo
  output$title_dat_EN_emisiones <- renderUI({
    helpText(HTML(unique(dat_EN_emisiones()$nomindicador)))
  })
  
  # Subtitulo
  output$subtitle_dat_EN_emisiones <- renderUI({
    helpText(HTML(unique(dat_EN_emisiones()$definicion)))
  })
  
  # Metodología
  output$info_dat_EN_emisiones <- renderUI({
    helpText(HTML(
      paste(
        "<b>Método de Agregación:</b>",
        unique(dat_EN_emisiones()$metodo_de_agregacion),
        "<b>Metodología:</b>",
        unique(dat_EN_emisiones()$concepto_estadistico_y_metodologia)
      )
    ))
  })
  
  # Relevancia:
  output$rel_dat_EN_emisiones <- renderUI({
    helpText(HTML(paste(
      "<b>Relvancia:</b>", unique(dat_EN_emisiones()$relevancia)
    )))
  })
  
  
  output$fecha_dat_EN_emisiones <- renderUI({
    if (input$visualizador_EN_emisiones == "Serie de tiempo") {
      tagList(
        tags$style(
          type = 'text/css',
          '#big_slider .irs-grid-text {font-size: 10px; transform: rotate(-90deg) translate(-15px);} ,.irs-grid-pol.large {height: 0px;}'
        ),
        div(
          id = 'big_slider',
          
          sliderTextInput(
            inputId = "fecha_dat_EN_emisiones",
            label = "Seleccione años",
            choices = sort(unique(dat_EN_emisiones()$fecha)),
            selected = c(min(dat_EN_emisiones()$fecha),
                         max(dat_EN_emisiones()$fecha)),
            hide_min_max = T,
            grid = TRUE,
            width = "100%"
          )
        )
      )
      
    } else {
      selectInput(
        inputId = "fecha_EN_emisiones",
        label = "Seleccione año:",
        choices = dat_EN_emisiones() %>%
          drop_na(valor) %>%
          select(fecha) %>%
          arrange(desc(fecha)) %>%
          unique() %>%
          pull(),
        selected = "2019"
      )
      
    }
    
  })
  
  
  output$corte_EN_emisiones <- renderUI({
    
    if (input$indicador_EN_emisiones %in% vars_corte) {
      
      selectInput(inputId = "corte_EN_emisiones",
                  label = "Seleccione categorías",
                  choices =  dat_EN_emisiones() %>%
                    distinct(get(names(dat_EN_emisiones()[,ncol(dat_EN_emisiones())]))) %>%
                    pull(),
                  selected = dat_EN_emisiones() %>%
                    filter(jerarquia == 1) %>% 
                    distinct(get(names(dat_EN_emisiones()[,ncol(dat_EN_emisiones())]))) %>%
                    pull()
      )
    } else {
      
      return(NULL)
    }
    
  })
  
  
  # Checkbox por pais
  output$sel_EN_emisiones_pais <- renderUI({
    if (input$visualizador_EN_emisiones == "Serie de tiempo") {
      dropdown(
        label = "Seleccione países",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_pais_EN_emisiones",
          label = "Seleccione países",
          inline = TRUE,
          choices = dat_EN_emisiones() %>%
            filter(region == 0) %>%
            filter(nomindicador == input$indicador_EN_emisiones) %>%
            distinct(cod_pais) %>%
            pull(),
          selected = c("URY", "ARG", "BRA", "PRY")
        )
      )
      
    } else {
      checkboxGroupInput(
        inputId = "chbox_pais_reg_EN_emisiones",
        label = "Mostrar:",
        inline = FALSE,
        choices = c("Países", "Regiones"),
        selected = "Países"
      )
      
    }
    
  })
  
  # Checkbox por región
  output$sel_EN_emisiones_region <- renderUI({
    if (input$visualizador_EN_emisiones == "Serie de tiempo") {
      dropdown(
        label = "Seleccione regiones",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_reg_EN_emisiones",
          label = "Seleccione regiones",
          inline = TRUE,
          choices = dat_EN_emisiones() %>%
            filter(region == 1) %>%
            filter(nomindicador == input$indicador_EN_emisiones) %>%
            distinct(pais) %>%
            pull(),
          selected = NULL
        )
      )
      
    } else {
      return(NULL)
      
    }
  })
  
  
  dat_EN_emisiones_anual <- reactive({
    
    if (input$indicador_EN_emisiones %in% vars_corte) {
      
      dat_EN_emisiones() %>%
        filter(get(names(dat_EN_emisiones()[,ncol(dat_EN_emisiones())])) %in% input$corte_EN_emisiones) %>%
        filter(fecha == input$fecha_EN_emisiones)
      
    } else {
      
      dat_EN_emisiones() %>%
        filter(fecha == input$fecha_EN_emisiones)
      
      
    }
  })
  
  dat_EN_emisiones_simple <- reactive({
    
    if (input$indicador_EN_emisiones %in% vars_corte) {
      
      dat_EN_emisiones() %>%
        filter(get(names(dat_EN_emisiones()[,ncol(dat_EN_emisiones())])) %in% input$corte_EN_emisiones) %>% 
        filter(fecha >= input$fecha_dat_EN_emisiones[1] & fecha <= input$fecha_dat_EN_emisiones[2]) %>%
        filter(cod_pais %in% input$chbox_pais_EN_emisiones | pais %in% input$chbox_reg_EN_emisiones)
      
    } else {
      
      dat_EN_emisiones() %>%
        filter(fecha >= input$fecha_dat_EN_emisiones[1] & fecha <= input$fecha_dat_EN_emisiones[2]) %>%
        filter(cod_pais %in% input$chbox_pais_EN_emisiones | pais %in% input$chbox_reg_EN_emisiones)        
    }
    
  })
  
  
  # Gráficos EN_emisiones
  output$p_dat_EN_emisiones <- renderPlot({
    
    if (input$visualizador_EN_emisiones == "Serie de tiempo" ) {
      
      req(input$fecha_dat_EN_emisiones, input$indicador_EN_emisiones)
      
      plot_EN_emisiones <- ggplot(
        data = dat_EN_emisiones_simple(), 
        aes(x = fecha, y = valor)
      ) +
        geom_line(aes(color = pais), size = 1, alpha = 0.5) +
        geom_point(aes(color = pais), size = 3) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_EN_emisiones_simple()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        scale_colour_manual(name = "", values = paleta_expandida) +
        if(input$indicador_EN_emisiones %in% vars_corte){
          
          ggtitle(paste0(input$indicador_EN_emisiones, " (", input$corte_EN_emisiones, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_EN_emisiones))
          
        }
      
      print(plot_EN_emisiones)
      ggsave(
        "www/indicador EN_emisiones.png",
        width = 30,
        height = 20,
        units = "cm"
      )
      
    } else if (input$visualizador_EN_emisiones == "Anual gráfico") {
      
      base_plot_EN_emisiones <- dat_EN_emisiones_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_EN_emisiones)
      
      validate(need(
        nrow(base_plot_EN_emisiones) > 0,
        'No hay datos disponible para esta búsqueda'
      ))
      
      plot_EN_emisiones <- ggplot(base_plot_EN_emisiones,
                            aes(x = fct_reorder(pais, valor), y = valor)) +
        geom_segment(
          aes(
            x = fct_reorder(pais, valor),
            xend = fct_reorder(pais, valor),
            y = 0,
            yend = valor
          ),
          size = 1,
          color = "#2c3e50"
        ) +
        geom_point(color = "#2c3e50", size = 4) +
        theme_light() +
        coord_flip() +
        theme(
          axis.text.y = element_text(size = 12),
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_EN_emisiones_anual()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        if(input$indicador_EN_emisiones %in% vars_corte){
          
          ggtitle(paste0(input$indicador_EN_emisiones, " (", input$corte_EN_emisiones, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_EN_emisiones))
          
        }
      
      print(plot_EN_emisiones)
      ggsave(
        "www/indicador EN_emisiones.png",
        width = 25,
        height = 30,
        units = "cm"
      )
      
    }
    
  })
  
  output$map_EN_emisiones <- renderLeaflet({
    # Read this shape file with the rgdal library.
    world_spdf <- readOGR(dsn = "data" ,
                          layer = "TM_WORLD_BORDERS_SIMPL-0.3",
                          verbose = FALSE)
    
    # Pegar datos
    world_spdf@data <- world_spdf@data %>%
      left_join(select(dat_EN_emisiones_anual(), pais_eng, valor_original),
                by = c("NAME" = "pais_eng"))
    
    # Library
    
    # Create a color palette with handmade bins.
    
    
    if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= -1) {
      round_number <- .1
      
    }
    
    min_val <-
      plyr::round_any(min(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      floor)
    max_val <-
      plyr::round_any(max(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      ceiling)
    dif <- max_val - min_val
    dif_5 <- dif / 5
    val_1 <- min_val + dif_5
    val_2 <- min_val + (dif_5 * 2)
    val_3 <- min_val + (dif_5 * 3)
    val_4 <- min_val + (dif_5 * 4)
    
    mybins <- c(min_val, val_1, val_2, val_3, val_4, max_val)
    
    mypalette <- colorBin(
      palette = "YlOrBr",
      domain = world_spdf@data$valor_original,
      na.color = "transparent",
      bins = mybins
    )
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "País: ",
      world_spdf@data$NAME,
      "<br/>",
      "Valor: ",
      round(world_spdf@data$valor_original, 2),
      sep = ""
    ) %>%
      lapply(htmltools::HTML)
    
    # Final Map
    leaflet(world_spdf) %>%
      addTiles()  %>%
      setView(lat = 30,
              lng = 0 ,
              zoom = 1) %>%
      addPolygons(
        fillColor = ~ mypalette(valor_original),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = mypalette,
        values =  ~ valor_original,
        opacity = 0.9,
        title = wrapit(input$indicador_EN_emisiones),
        position = "bottomleft"
      )
    
  })
  
  # Botón descarga grafico
  output$baja_plot_EN_emisiones <- downloadHandler(filename <- function() {
    paste("indicador EN_emisiones", "png", sep = ".")
  },
  
  content <- function(file) {
    file.copy("www/indicador EN_emisiones.png", file)
  },
  contentType = "www/indicador EN_emisiones")
  
  
  ## Data series temporal
  
  # Data para tabla y exportar
  dat_EN_emisiones_st <- reactive({
    
    if (input$indicador_EN_emisiones %in% vars_corte) {
      
      dat_EN_emisiones() %>%
        filter(fecha >= input$fecha_dat_EN_emisiones[1] & fecha <= input$fecha_dat_EN_emisiones[2]) %>%
        filter(cod_pais %in% input$chbox_pais_EN_emisiones | pais %in% input$chbox_reg_EN_emisiones) %>%
        select(pais, fecha, names(dat_EN_emisiones()[,ncol(dat_EN_emisiones())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor)) 
      
    } else {
      
      dat_EN_emisiones() %>%
        filter(fecha >= input$fecha_dat_EN_emisiones[1] &
                 fecha <= input$fecha_dat_EN_emisiones[2]) %>%
        filter(cod_pais %in% input$chbox_pais_EN_emisiones |
                 pais %in% input$chbox_reg_EN_emisiones) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
  })
  
  # Metadata
  dat_EN_emisiones_m <- reactive({
    dat_EN_emisiones() %>%
      select(
        nomindicador,
        definicion,
        metodo_de_agregacion,
        concepto_estadistico_y_metodologia,
        relevancia,
        limitaciones_y_excepciones
      ) %>%
      mutate(`Unidad de Métodos y Acceso a Datos (FCS -UdelaR)` = " ") %>%
      distinct() %>%
      gather(key = "", value = " ")
    
  })
  
  # Data completa
  dat_EN_emisiones_c <- reactive({
    
    if (input$indicador_EN_emisiones %in% vars_corte) {
      
      dat_EN_emisiones() %>%
        select(pais, fecha, names(dat_EN_emisiones()[,ncol(dat_EN_emisiones())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_EN_emisiones() %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
    
  })
  
  # Lista para descarga
  list_dat_EN_emisiones_st <- reactive({
    list_dat_EN_emisiones <- list(
      "Data" = dat_EN_emisiones_st(),
      "Metadata" = dat_EN_emisiones_m(),
      "Data Completa" = dat_EN_emisiones_c()
    )
  })
  
  
  ## Data por año
  # Data para tabla y exportar
  dat_EN_emisiones_a <- reactive({
    
    if (input$indicador_EN_emisiones %in% vars_corte) {
      
      dat_EN_emisiones_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_EN_emisiones) %>%
        select(pais, fecha, names(dat_EN_emisiones()[,ncol(dat_EN_emisiones())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_EN_emisiones_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_EN_emisiones) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    }
  })
  
  # Lista para descarga
  list_dat_EN_emisiones_a <- reactive({
    list_dat_EN_emisiones <- list("Data" = dat_EN_emisiones_a(),
                            "Metadata" = dat_EN_emisiones_m(),
                            "Data Completa" = dat_EN_emisiones_c())
  })
  
  # Tablas en shiny
  output$tab_dat_EN_emisiones <- renderDT({
    
    if (input$indicador_EN_emisiones %in% vars_corte) {
      DT::datatable(
        dat_EN_emisiones_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Corte", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_EN_emisiones,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(4, '', mark = ",")
      
    } else if (input$visualizador_EN_emisiones == "Serie de tiempo") {
      DT::datatable(
        dat_EN_emisiones_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_EN_emisiones,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    } else if (input$visualizador_EN_emisiones %in% c("Anual gráfico", "Anual mapa")) {
      DT::datatable(
        dat_EN_emisiones_a(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_EN_emisiones,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    }
  })
  
  # Descarga tabla
  output$dl_tabla_dat_EN_emisiones <- downloadHandler(
    filename = function() {
      paste("resultados-", input$indicador_EN_emisiones, ".xlsx", sep = "")
    },
    content = function(file) {
      if (input$visualizador_EN_emisiones == "Serie de tiempo") {
        openxlsx::write.xlsx(list_dat_EN_emisiones_st(), file)
        
      } else if (input$visualizador_EN_emisiones %in% c("Anual gráfico", "Anual mapa")) {
        openxlsx::write.xlsx(list_dat_EN_emisiones_a(), file)
        
      }
    }
  )
  
  ##  18.  EN_prod (dat_EN_prod)   ====================================
  
  # Data EN_prod
  
  dat_EN_prod <- reactive({
    
    req(input$indicador_EN_prod)
    
    data %>%
      filter(nomindicador == input$indicador_EN_prod) %>% 
      janitor::remove_empty("cols")
    
  })
  
  
  # Titulo
  output$title_dat_EN_prod <- renderUI({
    helpText(HTML(unique(dat_EN_prod()$nomindicador)))
  })
  
  # Subtitulo
  output$subtitle_dat_EN_prod <- renderUI({
    helpText(HTML(unique(dat_EN_prod()$definicion)))
  })
  
  # Metodología
  output$info_dat_EN_prod <- renderUI({
    helpText(HTML(
      paste(
        "<b>Método de Agregación:</b>",
        unique(dat_EN_prod()$metodo_de_agregacion),
        "<b>Metodología:</b>",
        unique(dat_EN_prod()$concepto_estadistico_y_metodologia)
      )
    ))
  })
  
  # Relevancia:
  output$rel_dat_EN_prod <- renderUI({
    helpText(HTML(paste(
      "<b>Relvancia:</b>", unique(dat_EN_prod()$relevancia)
    )))
  })
  
  
  output$fecha_dat_EN_prod <- renderUI({
    if (input$visualizador_EN_prod == "Serie de tiempo") {
      tagList(
        tags$style(
          type = 'text/css',
          '#big_slider .irs-grid-text {font-size: 10px; transform: rotate(-90deg) translate(-15px);} ,.irs-grid-pol.large {height: 0px;}'
        ),
        div(
          id = 'big_slider',
          
          sliderTextInput(
            inputId = "fecha_dat_EN_prod",
            label = "Seleccione años",
            choices = sort(unique(dat_EN_prod()$fecha)),
            selected = c(min(dat_EN_prod()$fecha),
                         max(dat_EN_prod()$fecha)),
            hide_min_max = T,
            grid = TRUE,
            width = "100%"
          )
        )
      )
      
    } else {
      selectInput(
        inputId = "fecha_EN_prod",
        label = "Seleccione año:",
        choices = dat_EN_prod() %>%
          drop_na(valor) %>%
          select(fecha) %>%
          arrange(desc(fecha)) %>%
          unique() %>%
          pull(),
        selected = "2019"
      )
      
    }
    
  })
  
  
  output$corte_EN_prod <- renderUI({
    
    if (input$indicador_EN_prod %in% vars_corte) {
      
      selectInput(inputId = "corte_EN_prod",
                  label = "Seleccione categorías",
                  choices =  dat_EN_prod() %>%
                    distinct(get(names(dat_EN_prod()[,ncol(dat_EN_prod())]))) %>%
                    pull(),
                  selected = dat_EN_prod() %>%
                    filter(jerarquia == 1) %>% 
                    distinct(get(names(dat_EN_prod()[,ncol(dat_EN_prod())]))) %>%
                    pull()
      )
    } else {
      
      return(NULL)
    }
    
  })
  
  
  # Checkbox por pais
  output$sel_EN_prod_pais <- renderUI({
    if (input$visualizador_EN_prod == "Serie de tiempo") {
      dropdown(
        label = "Seleccione países",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_pais_EN_prod",
          label = "Seleccione países",
          inline = TRUE,
          choices = dat_EN_prod() %>%
            filter(region == 0) %>%
            filter(nomindicador == input$indicador_EN_prod) %>%
            distinct(cod_pais) %>%
            pull(),
          selected = c("URY", "ARG", "BRA", "PRY")
        )
      )
      
    } else {
      checkboxGroupInput(
        inputId = "chbox_pais_reg_EN_prod",
        label = "Mostrar:",
        inline = FALSE,
        choices = c("Países", "Regiones"),
        selected = "Países"
      )
      
    }
    
  })
  
  # Checkbox por región
  output$sel_EN_prod_region <- renderUI({
    if (input$visualizador_EN_prod == "Serie de tiempo") {
      dropdown(
        label = "Seleccione regiones",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_reg_EN_prod",
          label = "Seleccione regiones",
          inline = TRUE,
          choices = dat_EN_prod() %>%
            filter(region == 1) %>%
            filter(nomindicador == input$indicador_EN_prod) %>%
            distinct(pais) %>%
            pull(),
          selected = NULL
        )
      )
      
    } else {
      return(NULL)
      
    }
  })
  
  
  dat_EN_prod_anual <- reactive({
    
    if (input$indicador_EN_prod %in% vars_corte) {
      
      dat_EN_prod() %>%
        filter(get(names(dat_EN_prod()[,ncol(dat_EN_prod())])) %in% input$corte_EN_prod) %>%
        filter(fecha == input$fecha_EN_prod)
      
    } else {
      
      dat_EN_prod() %>%
        filter(fecha == input$fecha_EN_prod)
      
      
    }
  })
  
  dat_EN_prod_simple <- reactive({
    
    if (input$indicador_EN_prod %in% vars_corte) {
      
      dat_EN_prod() %>%
        filter(get(names(dat_EN_prod()[,ncol(dat_EN_prod())])) %in% input$corte_EN_prod) %>% 
        filter(fecha >= input$fecha_dat_EN_prod[1] & fecha <= input$fecha_dat_EN_prod[2]) %>%
        filter(cod_pais %in% input$chbox_pais_EN_prod | pais %in% input$chbox_reg_EN_prod)
      
    } else {
      
      dat_EN_prod() %>%
        filter(fecha >= input$fecha_dat_EN_prod[1] & fecha <= input$fecha_dat_EN_prod[2]) %>%
        filter(cod_pais %in% input$chbox_pais_EN_prod | pais %in% input$chbox_reg_EN_prod)        
    }
    
  })
  
  
  # Gráficos EN_prod
  output$p_dat_EN_prod <- renderPlot({
    
    if (input$visualizador_EN_prod == "Serie de tiempo" ) {
      
      req(input$fecha_dat_EN_prod, input$indicador_EN_prod)
      
      plot_EN_prod <- ggplot(
        data = dat_EN_prod_simple(), 
        aes(x = fecha, y = valor)
      ) +
        geom_line(aes(color = pais), size = 1, alpha = 0.5) +
        geom_point(aes(color = pais), size = 3) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_EN_prod_simple()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        scale_colour_manual(name = "", values = paleta_expandida) +
        if(input$indicador_EN_prod %in% vars_corte){
          
          ggtitle(paste0(input$indicador_EN_prod, " (", input$corte_EN_prod, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_EN_prod))
          
        }
      
      print(plot_EN_prod)
      ggsave(
        "www/indicador EN_prod.png",
        width = 30,
        height = 20,
        units = "cm"
      )
      
    } else if (input$visualizador_EN_prod == "Anual gráfico") {
      
      base_plot_EN_prod <- dat_EN_prod_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_EN_prod)
      
      validate(need(
        nrow(base_plot_EN_prod) > 0,
        'No hay datos disponible para esta búsqueda'
      ))
      
      plot_EN_prod <- ggplot(base_plot_EN_prod,
                            aes(x = fct_reorder(pais, valor), y = valor)) +
        geom_segment(
          aes(
            x = fct_reorder(pais, valor),
            xend = fct_reorder(pais, valor),
            y = 0,
            yend = valor
          ),
          size = 1,
          color = "#2c3e50"
        ) +
        geom_point(color = "#2c3e50", size = 4) +
        theme_light() +
        coord_flip() +
        theme(
          axis.text.y = element_text(size = 12),
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        labs(
          x = "",
          y = "",
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_EN_prod_anual()$fuente)))) +
        scale_y_continuous(labels = addUnits) +
        if(input$indicador_EN_prod %in% vars_corte){
          
          ggtitle(paste0(input$indicador_EN_prod, " (", input$corte_EN_prod, ")"))
          
        } else { 
          
          ggtitle(paste(input$indicador_EN_prod))
          
        }
      
      print(plot_EN_prod)
      ggsave(
        "www/indicador EN_prod.png",
        width = 25,
        height = 30,
        units = "cm"
      )
      
    }
    
  })
  
  output$map_EN_prod <- renderLeaflet({
    # Read this shape file with the rgdal library.
    world_spdf <- readOGR(dsn = "data" ,
                          layer = "TM_WORLD_BORDERS_SIMPL-0.3",
                          verbose = FALSE)
    
    # Pegar datos
    world_spdf@data <- world_spdf@data %>%
      left_join(select(dat_EN_prod_anual(), pais_eng, valor_original),
                by = c("NAME" = "pais_eng"))
    
    # Library
    
    # Create a color palette with handmade bins.
    
    
    if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) <= -1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1000) {
      round_number <- 1000
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 100) {
      round_number <- 100
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 10) {
      round_number <- 10
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= 1) {
      round_number <- 1
      
    } else if (mean(world_spdf@data$valor_original, na.rm = TRUE) >= -1) {
      round_number <- .1
      
    }
    
    min_val <-
      plyr::round_any(min(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      floor)
    max_val <-
      plyr::round_any(max(world_spdf@data$valor_original, na.rm = T),
                      round_number,
                      ceiling)
    dif <- max_val - min_val
    dif_5 <- dif / 5
    val_1 <- min_val + dif_5
    val_2 <- min_val + (dif_5 * 2)
    val_3 <- min_val + (dif_5 * 3)
    val_4 <- min_val + (dif_5 * 4)
    
    mybins <- c(min_val, val_1, val_2, val_3, val_4, max_val)
    
    mypalette <- colorBin(
      palette = "YlOrBr",
      domain = world_spdf@data$valor_original,
      na.color = "transparent",
      bins = mybins
    )
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "País: ",
      world_spdf@data$NAME,
      "<br/>",
      "Valor: ",
      round(world_spdf@data$valor_original, 2),
      sep = ""
    ) %>%
      lapply(htmltools::HTML)
    
    # Final Map
    leaflet(world_spdf) %>%
      addTiles()  %>%
      setView(lat = 30,
              lng = 0 ,
              zoom = 1) %>%
      addPolygons(
        fillColor = ~ mypalette(valor_original),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = mypalette,
        values =  ~ valor_original,
        opacity = 0.9,
        title = wrapit(input$indicador_EN_prod),
        position = "bottomleft"
      )
    
  })
  
  # Botón descarga grafico
  output$baja_plot_EN_prod <- downloadHandler(filename <- function() {
    paste("indicador EN_prod", "png", sep = ".")
  },
  
  content <- function(file) {
    file.copy("www/indicador EN_prod.png", file)
  },
  contentType = "www/indicador EN_prod")
  
  
  ## Data series temporal
  
  # Data para tabla y exportar
  dat_EN_prod_st <- reactive({
    
    if (input$indicador_EN_prod %in% vars_corte) {
      
      dat_EN_prod() %>%
        filter(fecha >= input$fecha_dat_EN_prod[1] & fecha <= input$fecha_dat_EN_prod[2]) %>%
        filter(cod_pais %in% input$chbox_pais_EN_prod | pais %in% input$chbox_reg_EN_prod) %>%
        select(pais, fecha, names(dat_EN_prod()[,ncol(dat_EN_prod())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor)) 
      
    } else {
      
      dat_EN_prod() %>%
        filter(fecha >= input$fecha_dat_EN_prod[1] &
                 fecha <= input$fecha_dat_EN_prod[2]) %>%
        filter(cod_pais %in% input$chbox_pais_EN_prod |
                 pais %in% input$chbox_reg_EN_prod) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
  })
  
  # Metadata
  dat_EN_prod_m <- reactive({
    dat_EN_prod() %>%
      select(
        nomindicador,
        definicion,
        metodo_de_agregacion,
        concepto_estadistico_y_metodologia,
        relevancia,
        limitaciones_y_excepciones
      ) %>%
      mutate(`Unidad de Métodos y Acceso a Datos (FCS -UdelaR)` = " ") %>%
      distinct() %>%
      gather(key = "", value = " ")
    
  })
  
  # Data completa
  dat_EN_prod_c <- reactive({
    
    if (input$indicador_EN_prod %in% vars_corte) {
      
      dat_EN_prod() %>%
        select(pais, fecha, names(dat_EN_prod()[,ncol(dat_EN_prod())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_EN_prod() %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
    }
    
  })
  
  # Lista para descarga
  list_dat_EN_prod_st <- reactive({
    list_dat_EN_prod <- list(
      "Data" = dat_EN_prod_st(),
      "Metadata" = dat_EN_prod_m(),
      "Data Completa" = dat_EN_prod_c()
    )
  })
  
  
  ## Data por año
  # Data para tabla y exportar
  dat_EN_prod_a <- reactive({
    
    if (input$indicador_EN_prod %in% vars_corte) {
      
      dat_EN_prod_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_EN_prod) %>%
        select(pais, fecha, names(dat_EN_prod()[,ncol(dat_EN_prod())]), valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    } else {
      
      dat_EN_prod_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_EN_prod) %>%
        select(pais, fecha, valor) %>%
        arrange(desc(fecha), fct_reorder(pais,-valor))
      
    }
  })
  
  # Lista para descarga
  list_dat_EN_prod_a <- reactive({
    list_dat_EN_prod <- list("Data" = dat_EN_prod_a(),
                            "Metadata" = dat_EN_prod_m(),
                            "Data Completa" = dat_EN_prod_c())
  })
  
  # Tablas en shiny
  output$tab_dat_EN_prod <- renderDT({
    
    if (input$indicador_EN_prod %in% vars_corte) {
      DT::datatable(
        dat_EN_prod_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Corte", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_EN_prod,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(4, '', mark = ",")
      
    } else if (input$visualizador_EN_prod == "Serie de tiempo") {
      DT::datatable(
        dat_EN_prod_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_EN_prod,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    } else if (input$visualizador_EN_prod %in% c("Anual gráfico", "Anual mapa")) {
      DT::datatable(
        dat_EN_prod_a(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_EN_prod,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    }
  })
  
  # Descarga tabla
  output$dl_tabla_dat_EN_prod <- downloadHandler(
    filename = function() {
      paste("resultados-", input$indicador_EN_prod, ".xlsx", sep = "")
    },
    content = function(file) {
      if (input$visualizador_EN_prod == "Serie de tiempo") {
        openxlsx::write.xlsx(list_dat_EN_prod_st(), file)
        
      } else if (input$visualizador_EN_prod %in% c("Anual gráfico", "Anual mapa")) {
        openxlsx::write.xlsx(list_dat_EN_prod_a(), file)
        
      }
    }
  )
}

shinyApp(ui = ui, server = server)
