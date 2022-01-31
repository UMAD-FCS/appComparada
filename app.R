

## **************************************************************************
## Shiny APP Comparada (economía & social) -
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
  secondary = "#3E6C9A",
  base_font = font_google("Roboto"),
  heading_font = font_google("Roboto"),
  code_font = font_google("Space Mono")
)

# bs_theme_preview(tema_umad)

##  1. PREPARAR DATA  =======================================================

load("data/data.rda")

# Data Economía
data_eco <- data %>%
  filter(tipo == "eco")


# Data social
data_soc <- data %>%
  filter(tipo == "soc")


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
    
    # * 2.1. Pestaña 1 ------------------------------------------------------
    
    title = "Pestaña 1",
    icon = icon("dollar-sign"),
    
    tabsetPanel(
      type = "pills",
      id   = "CP",
      
      
      tabPanel(
        "Sub - Pestaña 1",
        icon = icon("fas fa-chart-bar"),
        
        br(),
        
        fluidRow(
          sidebarPanel(
            width = 3,
            # style = "position:fixed;width:22%;",
            
            selectInput(
              inputId = "visualizador_eco",
              label = "Visualización",
              choices = c("Serie de tiempo",
                          "Anual gráfico",
                          "Anual mapa"),
              selected = 2019
            ),
            
            selectInput(
              inputId = "indicador_eco",
              label = "Indicador",
              choices = sort(unique(data_eco$nomindicador)),
              selected = 2019
            ),
            
            uiOutput("fecha_dat_eco"),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_eco != 'Anual mapa'",
                             uiOutput("sel_eco_pais")),
            
            br(),
            
            uiOutput("sel_eco_region"),
            
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
                    uiOutput("title_dat_eco")),
            
            div(
              style = "display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("calculator", lib = "font-awesome"),
                uiOutput("info_dat_eco")
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
                uiOutput("rel_dat_eco")
              )
            ),
            
            tags$h5(uiOutput("subtitle_dat_eco")),
            
            br(),
            
            conditionalPanel(condition = "input.visualizador_eco != 'Anual mapa'",
                             withSpinner(
                               plotOutput("p_dat_eco", height = "600px"),
                               type = 2
                             )),
            
            conditionalPanel(condition = "input.visualizador_eco == 'Anual mapa'",
                             withSpinner(leafletOutput("map_eco"),
                                         type = 2)),
            
            conditionalPanel(
              condition = "input.visualizador_eco != 'Anual mapa'",
              downloadButton(outputId = "baja_plot_eco",
                             label = "Descarga el gráfico")
            ),
            br(),
            br(),
            withSpinner(DTOutput("tab_dat_eco"),
                        type = 2),
            br(),
            downloadButton("dl_tabla_dat_eco", "Descarga la tabla"),
            br(),
            br()
          )
        )
      ),
      
      tabPanel(
        "Sub - Pestaña 2",
        icon = icon("fas fa-chart-bar"),
        
        br(),
        
        fluidRow(
          sidebarPanel(
            width = 3,
            # style = "position:fixed;width:22%;",
            
            # selectInput(
            #   inputId = "visualizador_eco",
            #   label = "Visualización",
            #   choices = c("Serie de tiempo",
            #               "Anual gráfico",
            #               "Anual mapa"),
            #   selected = 2019
            # ),
            # 
            # selectInput(
            #   inputId = "indicador_eco",
            #   label = "Indicador",
            #   choices = sort(unique(data_eco$nomindicador)),
            #   selected = 2019
            # ),
            # 
            # uiOutput("fecha_dat_eco"),
            # 
            # br(),
            # 
            # conditionalPanel(condition = "input.visualizador_eco != 'Anual mapa'",
            #                  uiOutput("sel_eco_pais")),
            # 
            # br(),
            # 
            # uiOutput("sel_eco_region"),
            # 
            # br(),
            
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
            # tags$h3(style = "display:inline-block",
            #         uiOutput("title_dat_eco")),
            # 
            # div(
            #   style = "display:inline-block",
            #   dropdown(
            #     style = "minimal",
            #     status = "primary",
            #     width = "500px",
            #     right = TRUE,
            #     icon = icon("calculator", lib = "font-awesome"),
            #     uiOutput("info_dat_eco")
            #   )
            # ),
            # 
            # div(
            #   style = "display:inline-block",
            #   dropdown(
            #     style = "minimal",
            #     status = "primary",
            #     width = "500px",
            #     right = TRUE,
            #     icon = icon("exclamation", lib = "font-awesome"),
            #     uiOutput("rel_dat_eco")
            #   )
            # ),
            # 
            # tags$h5(uiOutput("subtitle_dat_eco")),
            # 
            # br(),
            # 
            # conditionalPanel(condition = "input.visualizador_eco != 'Anual mapa'",
            #                  withSpinner(
            #                    plotOutput("p_dat_eco", height = "600px"),
            #                    type = 2
            #                  )),
            # 
            # conditionalPanel(condition = "input.visualizador_eco == 'Anual mapa'",
            #                  withSpinner(leafletOutput("map_eco"),
            #                              type = 2)),
            # 
            # conditionalPanel(
            #   condition = "input.visualizador_eco != 'Anual mapa'",
            #   downloadButton(outputId = "baja_plot_eco",
            #                  label = "Descarga el gráfico")
            # ),
            # br(),
            # br(),
            # withSpinner(DTOutput("tab_dat_eco"),
            #             type = 2),
            # br(),
            # downloadButton("dl_tabla_dat_eco", "Descarga la tabla"),
            # br(),
            # br()
          )
        )
      )
    ),
  ),
  
  tabPanel(
    
    # * 2.2. Pestaña 2 ------------------------------------------------------
    
    title = "Pestaña 2",
    icon = icon("people-arrows"),
    
    tabsetPanel(
      type = "pills",
      id   = "CP",
      
      
      tabPanel(
        "Sub - Pestaña 3",
        icon = icon("fas fa-chart-bar"),
        
        br(),
        
        fluidRow(
          sidebarPanel(
            width = 3,
            # style = "position:fixed;width:22%;",
            
          #   selectInput(
          #     inputId = "visualizador_eco",
          #     label = "Visualización",
          #     choices = c("Serie de tiempo",
          #                 "Anual gráfico",
          #                 "Anual mapa"),
          #     selected = 2019
          #   ),
          #   
          #   selectInput(
          #     inputId = "indicador_eco",
          #     label = "Indicador",
          #     choices = sort(unique(data_eco$nomindicador)),
          #     selected = 2019
          #   ),
          #   
          #   uiOutput("fecha_dat_eco"),
          #   
          #   br(),
          #   
          #   conditionalPanel(condition = "input.visualizador_eco != 'Anual mapa'",
          #                    uiOutput("sel_eco_pais")),
          #   
          #   br(),
          #   
          #   uiOutput("sel_eco_region"),
          #   
          #   br(),
          #   
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
          # 
          # mainPanel(
          #   tags$h3(style = "display:inline-block",
          #           uiOutput("title_dat_eco")),
          #   
          #   div(
          #     style = "display:inline-block",
          #     dropdown(
          #       style = "minimal",
          #       status = "primary",
          #       width = "500px",
          #       right = TRUE,
          #       icon = icon("calculator", lib = "font-awesome"),
          #       uiOutput("info_dat_eco")
          #     )
          #   ),
          #   
          #   div(
          #     style = "display:inline-block",
          #     dropdown(
          #       style = "minimal",
          #       status = "primary",
          #       width = "500px",
          #       right = TRUE,
          #       icon = icon("exclamation", lib = "font-awesome"),
          #       uiOutput("rel_dat_eco")
          #     )
          #   ),
          #   
          #   tags$h5(uiOutput("subtitle_dat_eco")),
          #   
          #   br(),
          #   
          #   conditionalPanel(condition = "input.visualizador_eco != 'Anual mapa'",
          #                    withSpinner(
          #                      plotOutput("p_dat_eco", height = "600px"),
          #                      type = 2
          #                    )),
          #   
          #   conditionalPanel(condition = "input.visualizador_eco == 'Anual mapa'",
          #                    withSpinner(leafletOutput("map_eco"),
          #                                type = 2)),
          #   
          #   conditionalPanel(
          #     condition = "input.visualizador_eco != 'Anual mapa'",
          #     downloadButton(outputId = "baja_plot_eco",
          #                    label = "Descarga el gráfico")
          #   ),
          #   br(),
          #   br(),
          #   withSpinner(DTOutput("tab_dat_eco"),
          #               type = 2),
          #   br(),
          #   downloadButton("dl_tabla_dat_eco", "Descarga la tabla"),
          #   br(),
          #   br()
          # )
        )
      ),
      
      tabPanel(
        "Sub - Pestaña 4",
        icon = icon("fas fa-chart-bar"),
        
        br(),
        
        fluidRow(
          sidebarPanel(
            width = 3,
            # style = "position:fixed;width:22%;",
            
            # selectInput(
            #   inputId = "visualizador_eco",
            #   label = "Visualización",
            #   choices = c("Serie de tiempo",
            #               "Anual gráfico",
            #               "Anual mapa"),
            #   selected = 2019
            # ),
            # 
            # selectInput(
            #   inputId = "indicador_eco",
            #   label = "Indicador",
            #   choices = sort(unique(data_eco$nomindicador)),
            #   selected = 2019
            # ),
            # 
            # uiOutput("fecha_dat_eco"),
            # 
            # br(),
            # 
            # conditionalPanel(condition = "input.visualizador_eco != 'Anual mapa'",
            #                  uiOutput("sel_eco_pais")),
            # 
            # br(),
            # 
            # uiOutput("sel_eco_region"),
            # 
            # br(),
            
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
            # tags$h3(style = "display:inline-block",
            #         uiOutput("title_dat_eco")),
            # 
            # div(
            #   style = "display:inline-block",
            #   dropdown(
            #     style = "minimal",
            #     status = "primary",
            #     width = "500px",
            #     right = TRUE,
            #     icon = icon("calculator", lib = "font-awesome"),
            #     uiOutput("info_dat_eco")
            #   )
            # ),
            # 
            # div(
            #   style = "display:inline-block",
            #   dropdown(
            #     style = "minimal",
            #     status = "primary",
            #     width = "500px",
            #     right = TRUE,
            #     icon = icon("exclamation", lib = "font-awesome"),
            #     uiOutput("rel_dat_eco")
            #   )
            # ),
            # 
            # tags$h5(uiOutput("subtitle_dat_eco")),
            # 
            # br(),
            # 
            # conditionalPanel(condition = "input.visualizador_eco != 'Anual mapa'",
            #                  withSpinner(
            #                    plotOutput("p_dat_eco", height = "600px"),
            #                    type = 2
            #                  )),
            # 
            # conditionalPanel(condition = "input.visualizador_eco == 'Anual mapa'",
            #                  withSpinner(leafletOutput("map_eco"),
            #                              type = 2)),
            # 
            # conditionalPanel(
            #   condition = "input.visualizador_eco != 'Anual mapa'",
            #   downloadButton(outputId = "baja_plot_eco",
            #                  label = "Descarga el gráfico")
            # ),
            # br(),
            # br(),
            # withSpinner(DTOutput("tab_dat_eco"),
            #             type = 2),
            # br(),
            # downloadButton("dl_tabla_dat_eco", "Descarga la tabla"),
            # br(),
            # br()
          )
        )
      )
    ),
  )
)


server <- function(session, input, output) {
  
  ##  3.  ECONOMÍA (dat_eco)   ============================================
  
  # Data Económica
  
  dat_eco <- reactive({
    req(input$indicador_eco)
    
    data_eco %>%
      filter(nomindicador == input$indicador_eco)
  })
  
  
  # Titulo
  output$title_dat_eco <- renderUI({
    helpText(HTML(unique(dat_eco()$nomindicador)))
  })
  
  # Subtitulo
  output$subtitle_dat_eco <- renderUI({
    helpText(HTML(unique(dat_eco()$definicion)))
  })
  
  # Metodología
  output$info_dat_eco <- renderUI({
    helpText(HTML(
      paste(
        "<b>Método de Agregación:</b>",
        unique(dat_eco()$metodo_de_agregacion),
        "<b>Metodología:</b>",
        unique(dat_eco()$concepto_estadistico_y_metodologia)
      )
    ))
  })
  
  # Relevancia:
  output$rel_dat_eco <- renderUI({
    helpText(HTML(paste(
      "<b>Relvancia:</b>", unique(dat_eco()$relevancia)
    )))
  })
  
  
  output$fecha_dat_eco <- renderUI({
    if (input$visualizador_eco == "Serie de tiempo") {
      tagList(
        tags$style(
          type = 'text/css',
          '#big_slider .irs-grid-text {font-size: 10px; transform: rotate(-90deg) translate(-15px);} ,.irs-grid-pol.large {height: 0px;}'
        ),
        div(
          id = 'big_slider',
          
          sliderTextInput(
            inputId = "fecha_dat_eco",
            label = "Seleccione años",
            choices = sort(unique(dat_eco()$fecha)),
            selected = c(min(dat_eco()$fecha),
                         max(dat_eco()$fecha)),
            hide_min_max = T,
            grid = TRUE,
            width = "100%"
          )
        )
      )
      
    } else {
      selectInput(
        inputId = "fecha_eco",
        label = "Seleccione año:",
        choices = dat_eco() %>%
          drop_na(valor) %>%
          select(fecha) %>%
          arrange(desc(fecha)) %>%
          unique() %>%
          pull(),
        selected = "2019"
      )
      
    }
    
  })
  
  
  # Checkbox por pais
  output$sel_eco_pais <- renderUI({
    if (input$visualizador_eco == "Serie de tiempo") {
      dropdown(
        label = "Seleccione país",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_pais_eco",
          label = "Seleccione país",
          inline = TRUE,
          choices = dat_eco() %>%
            filter(region == 0) %>%
            filter(nomindicador == input$indicador_eco) %>%
            distinct(cod_pais) %>%
            pull(),
          selected = c("URY", "ARG", "BRA", "PRY")
        )
      )
      
    } else {
      checkboxGroupInput(
        inputId = "chbox_pais_reg_eco",
        label = "Mostrar:",
        inline = FALSE,
        choices = c("Países", "Regiones"),
        selected = "Países"
      )
      
    }
    
  })
  
  # Checkbox por pais
  output$sel_eco_region <- renderUI({
    if (input$visualizador_eco == "Serie de tiempo") {
      dropdown(
        label = "Seleccione región",
        status = "default",
        width = 400,
        circle = F,
        
        checkboxGroupInput(
          inputId = "chbox_reg_eco",
          label = "Seleccione región",
          inline = TRUE,
          choices = dat_eco() %>%
            filter(region == 1) %>%
            filter(nomindicador == input$indicador_eco) %>%
            distinct(pais) %>%
            pull(),
          selected = NULL
        )
      )
      
    } else {
      return(NULL)
      
    }
  })
  
  dat_eco_anual <- reactive({
    dat_eco() %>%
      filter(fecha == input$fecha_eco)
    
  })
  
  
  # Gráficos CP_comp
  output$p_dat_eco <- renderPlot({
    if (input$visualizador_eco == "Serie de tiempo") {
      req(input$fecha_dat_eco, input$indicador_eco)
      
      plot_eco <- ggplot(
        data = dat_eco() %>%
          filter(
            fecha >= input$fecha_dat_eco[1] &
              fecha <= input$fecha_dat_eco[2]
          ) %>%
          filter(
            cod_pais %in% input$chbox_pais_eco |
              pais %in% input$chbox_reg_eco
          ),
        aes(x = fecha, y = valor)
      ) +
        geom_line(aes(color = pais), size = 1, alpha = 0.5) +
        geom_point(aes(color = pais), size = 3) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(
          x = "",
          y = "",
          title = input$indicador_eco,
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_eco()$fuente)))) +
        scale_y_continuous(labels = addUnits)
      
      print(plot_eco)
      ggsave(
        "www/indicador eco.png",
        width = 30,
        height = 20,
        units = "cm"
      )
      
    } else if (input$visualizador_eco == "Anual gráfico") {
      base_plot_eco <- dat_eco_anual() %>%
        filter(pais_region %in% input$chbox_pais_reg_eco)
      
      validate(need(
        nrow(base_plot_eco) > 0,
        'No hay datos disponible para esta búsqueda'
      ))
      
      plot_eco <- ggplot(base_plot_eco,
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
          title = input$indicador_eco,
          caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                                 unique(dat_eco_anual()$fuente)))) +
        scale_y_continuous(labels = addUnits)
      
      print(plot_eco)
      ggsave(
        "www/indicador eco.png",
        width = 25,
        height = 30,
        units = "cm"
      )
      
    }
    
  })
  
  output$map_eco <- renderLeaflet({
    # Read this shape file with the rgdal library.
    world_spdf <- readOGR(dsn = "data" ,
                          layer = "TM_WORLD_BORDERS_SIMPL-0.3",
                          verbose = FALSE)
    
    # Pegar datos
    world_spdf@data <- world_spdf@data %>%
      left_join(select(dat_eco_anual(), pais_eng, valor_original),
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
        title = wrapit(input$indicador_eco),
        position = "bottomleft"
      )
    
  })
  
  # Botón descarga grafico
  output$baja_plot_eco <- downloadHandler(filename <- function() {
    paste("indicador eco", "png", sep = ".")
  },
  
  content <- function(file) {
    file.copy("www/indicador eco.png", file)
  },
  contentType = "www/indicador eco")
  
  
  ## Data series temporal
  
  # Data para tabla y exportar
  dat_eco_st <- reactive({
    dat_eco() %>%
      filter(fecha >= input$fecha_dat_eco[1] &
               fecha <= input$fecha_dat_eco[2]) %>%
      filter(cod_pais %in% input$chbox_pais_eco |
               pais %in% input$chbox_reg_eco) %>%
      select(pais, fecha, valor) %>%
      arrange(desc(fecha), fct_reorder(pais,-valor))
  })
  
  # Metadata
  dat_eco_m <- reactive({
    dat_eco() %>%
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
  dat_eco_c <- reactive({
    dat_eco() %>%
      select(pais, fecha, valor) %>%
      arrange(desc(fecha), fct_reorder(pais,-valor))
    
  })
  
  # Lista para descarga
  list_dat_eco_st <- reactive({
    list_dat_eco <- list(
      "Data" = dat_eco_st(),
      "Metadata" = dat_eco_m(),
      "Data Completa" = dat_eco_c()
    )
  })
  
  
  ## Data por año
  # Data para tabla y exportar
  dat_eco_a <- reactive({
    dat_eco_anual() %>%
      filter(pais_region %in% input$chbox_pais_reg_eco) %>%
      select(pais, fecha, valor) %>%
      arrange(desc(fecha), fct_reorder(pais,-valor))
  })
  
  # Lista para descarga
  list_dat_eco_a <- reactive({
    list_dat_eco <- list("Data" = dat_eco_a(),
                         "Metadata" = dat_eco_m())
  })
  
  # Tablas en shiny
  output$tab_dat_eco <- renderDT({
    if (input$visualizador_eco == "Serie de tiempo") {
      DT::datatable(
        dat_eco_st(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_eco,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    } else if (input$visualizador_eco %in% c("Anual gráfico", "Anual mapa")) {
      DT::datatable(
        dat_eco_a(),
        rownames = FALSE,
        colnames = c("País/Región", "Fecha", "Valor"),
        options = list(columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )),
        caption = htmltools::tags$caption(input$indicador_eco,
                                          style = "color:black; font-size:110%;")
      ) %>%
        formatCurrency(3, '', mark = ",")
      
    }
  })
  
  # Descarga tabla
  output$dl_tabla_dat_eco <- downloadHandler(
    filename = function() {
      paste("resultados-", input$indicador_eco, ".xlsx", sep = "")
    },
    content = function(file) {
      if (input$visualizador_eco == "Serie de tiempo") {
        openxlsx::write.xlsx(list_dat_eco_st(), file)
        
      } else if (input$visualizador_eco %in% c("Anual gráfico", "Anual mapa")) {
        openxlsx::write.xlsx(list_dat_eco_a(), file)
        
      }
    }
  )
  
  
  ## SOCIAL  ==============================================================
  
  # Data Social
  
  dat_soc <- reactive({
    req(input$indicador_soc)
    
    data_soc %>%
      filter(nomindicador == input$indicador_soc)
    
  })
  
  
  # Titulo
  output$title_dat_soc <- renderUI({
    helpText(HTML(unique(dat_soc()$nomindicador)))
  })
  
  # Subtitulo
  output$subtitle_dat_soc <- renderUI({
    helpText(HTML(unique(dat_soc()$definicion)))
  })
  
  # Metodología
  output$info_dat_soc <- renderUI({
    helpText(HTML(
      paste(
        "<b>Método de Agregación:</b>",
        unique(dat_soc()$metodo_de_agregacion),
        "<b>Metodología:</b>",
        unique(dat_soc()$concepto_estadistico_y_metodologia)
      )
    ))
  })
  
  # Relevancia:
  output$rel_dat_soc <- renderUI({
    helpText(HTML(paste(
      "<b>Relvancia:</b>", unique(dat_soc()$relevancia)
    )))
  })
  
  
  output$fecha_dat_soc <- renderUI({
    selectInput(
      inputId = "fecha_soc",
      label = "Seleccione año:",
      choices = dat_soc() %>%
        drop_na(valor) %>%
        select(fecha) %>%
        arrange(desc(fecha)) %>%
        unique() %>%
        pull(),
      selected = "2019"
    )
    
  })
  
  
  output$p_dat_soc <- renderPlot({
    base_plot_soc <- dat_soc() %>%
      filter(pais_region %in% input$chbox_pais_reg_soc) %>%
      filter(fecha == input$fecha_soc)
    
    validate(need(
      nrow(base_plot_soc) > 0,
      'No hay datos disponible para esta búsqueda'
    ))
    
    plot_soc <- ggplot(base_plot_soc,
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
        title = input$indicador_soc,
        caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
                               unique(dat_soc()$fuente)))) +
      scale_y_continuous(labels = addUnits)
    
    print(plot_soc)
    ggsave(
      "www/indicador soc.png",
      width = 15,
      height = 30,
      units = "cm"
    )
    
  })
  
  
}

shinyApp(ui = ui, server = server)
