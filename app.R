
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

rm(list=ls())

source("utils.R")

theme_set(theme_bdd(base_size = 12))
update_geom_defaults("text", list(family = theme_get()$text$family))

dir.create('~/.fonts')
file.copy("www/Titillium Web.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')

# Spinner options 
options(spinner.color = "#21618C",
        spinner.color.background="#ffffff", 
        spinner.size = 2)


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
  theme = shinytheme("flatly"),
  collapsible = TRUE,
  fluid = TRUE,

  tabPanel(
    
    # * 2.1. Economía --------------------------------------------------------

    title = "Economía", 
    icon = icon("dollar-sign"),

        sidebarPanel(
          
          width = 3,
          style = "position:fixed;width:22%;",
          
           selectInput(inputId = "indicador_eco",
                       label = "Indicador",
                       choices = sort(unique(data_eco$nomindicador)),
                       selected = 2019
                       ),

          uiOutput("fecha_dat_eco"),
          
          br(),

          uiOutput("sel_eco_pais"),

          br(),

           tags$a(href="https://umad.cienciassociales.edu.uy/",
                   "Unidad de Métodos y Acceso a Datos",
                   style = "font-size:12px; color:Navy;
                   text-decoration:underline;"),
           br(),
           br(),
           img(src = "logo_umad.png", height="70%",
               width = "70%", align = "left"),
           style = "display:inline-block;",
                    
        ),
        
        mainPanel(
          
          tags$h3(style="display:inline-block",
                  uiOutput("title_dat_eco")),
          
          div(style="display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("calculator", lib = "font-awesome"),
                uiOutput("info_dat_eco"))
          ),
          
          div(style="display:inline-block",
              dropdown(
                style = "minimal",
                status = "primary",
                width = "500px",
                right = TRUE,
                icon = icon("exclamation", lib = "font-awesome"),
                uiOutput("rel_dat_eco"))
          ),
          
          tags$h5(uiOutput("subtitle_dat_eco")),
          
          br(),
          
          withSpinner(plotOutput("p_dat_eco", height = "500px"),type = 2),
          # downloadButton(outputId = "baja_p_dat_eco",
          #                label = "Descarga el gráfico"),
          br(),
          br(),
          # withSpinner(uiOutput("p_CP_comp_t5", width = "100%", height = "800px"),
          #             type = 2),
          # conditionalPanel(
          #   condition = paste0(paste0("[",toString(paste0("'",c(list_barras_CP_comp, list_barras_CP_comp_men, list_barras_CP_comp_anual),"'")),"]"),".includes(input.indicador_CP_comp)"),
          #   downloadButton(outputId = "baja_p_CP_comp_t5", 
          #                  label = "Descarga el gráfico")),
          # br(),
          # br(),
          # DTOutput("tab_CP_comp"),
          # br(),
          # downloadButton("dl_tabla_CP_comp", "Descarga la tabla"), 
          # br(),
          # br()
          
        )
        
      ),
  
  tabPanel(
    
    # * 2.2. Social ---------------------------------------------------------
    
    title = "Social", 
    icon = icon("people-arrows"),
    
        sidebarPanel(
          
          width = 3,
          style = "position:fixed;width:22%;",
          
          selectInput(
            inputId = "indicador_soc",
            label = "Indicador",
            choices = sort(unique(data_soc$nomindicador)),
            selected = 2019
          ),
          
          # uiOutput("selector_CDE_crec"),
          # 
          # br(),
          # 
          # tags$a(href="https://umad.cienciassociales.edu.uy/", 
          #        "Unidad de Métodos y Acceso a Datos",
          #        style = "font-size:12px; color:Navy;
          #                    text-decoration:underline;"),
          # br(),
          # br(),
          # img(src = "logo_umad.png", height="70%",
          #     width = "70%", align = "left"),
          # style = "display:inline-block;",
          
        ),
    
        mainPanel(
          
        )
      ),
    )


server <- function(session, input, output) {
  
  ##  3.  COMPETITIVIDAD (CP_comp)   ========================================
  
  
referencias <- "<br><b>Referencias:</b> K = miles; M = millones; B = billones. <br>
                * Los valores usan el separador decimal inglés: los números luego
                de puntos son decimales, y las comas separan los miles. "

  ## CP_comp

  # Data CP_comp

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
    helpText(HTML(paste("<b>Metodología:</b>",
                        unique(dat_eco()$concepto_estadistico_y_metodologia),
                        referencias)))
  })

  # Relevancia:
  output$rel_dat_eco <- renderUI({
    helpText(HTML(paste("<b>Relvancia:</b>", unique(dat_eco()$relevancia))))
  })

  
  output$fecha_dat_eco <- renderUI({
    
    tagList(
      
      tags$style(type = 'text/css', 
               '#big_slider .irs-grid-text {font-size: 10px; transform: rotate(-90deg) translate(-15px);} ,.irs-grid-pol.large {height: 0px;}'),
      div(id = 'big_slider',
          
          sliderTextInput(inputId = "fecha_dat_eco",
                          label = "Seleccione años",
                          choices = sort(unique(dat_eco()$fecha)),
                          selected = c(min(dat_eco()$fecha),
                                       max(dat_eco()$fecha)),
                          hide_min_max = T,
                          grid = TRUE,
                          width = "100%")
          )
      )
  
  })
  
  
  # Checkbox por pais
  output$sel_eco_pais <- renderUI({
    
      checkboxGroupInput(
        inputId = "chbox_pais_eco",
        label = "Seleccione países",
        inline = TRUE,
        choices = dat_eco() %>%
          distinct(pais) %>%
          pull(),
        selected = c("Uruguay", "Argentina", "Brasil", "Chile"))
    
    })
  
    # Gráficos CP_comp
    output$p_dat_eco <- renderPlot({

      req(input$fecha_dat_eco, input$indicador_eco)

        plot_eco <- ggplot(data = dat_eco() %>%
                             filter(fecha >= input$fecha_dat_eco[1] &
                                      fecha <= input$fecha_dat_eco[2]) %>%
                             filter(pais %in% input$chbox_pais_eco),
                               aes(x = fecha, y = valor)) +
          geom_line(aes(color = pais), size = 1, alpha = 0.5) +
          geom_point(aes(color = pais), size = 3) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",
               y = "",
               title = input$indicador_eco,
               caption = wrapit("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de")) +
          scale_y_continuous(labels = addUnits)

        print(plot_eco)
        ggsave("www/indicador eco.png", width = 30, height = 20, units = "cm")
        
    })

  
#   # Gráficos CP_comp
#   output$p_CP_comp <- renderPlot({
#     
#     req(input$fecha_CP_comp, input$indicador_CP_comp)
#     
#     if(input$indicador_CP_comp %in% list_barras_CP_comp) {
#       
#       req(input$anio_CP_comp)
#       
#       base_plot_CP_comp <-  CP_comp() %>% 
#         filter(anio == input$anio_CP_comp)  
#       
#       validate(need(nrow(base_plot_CP_comp) > 0, 
#                     'No hay datos disponible para esta búsqueda'))
#       
#       plot_CP_comp  <- base_plot_CP_comp %>%      
#         top_n(20, VALOR) %>% 
#         filter(categoria != "TOTAL GENERAL",
#                categoria != "Total general",
#                categoria != "Total empresas",
#                categoria != "Total",
#                categoria != "TOTAL") %>% 
#         mutate(categoria = str_trunc(categoria, 75)) %>% 
#         ggplot(aes(x = fct_reorder(categoria, VALOR), y = VALOR)) +
#         geom_bar(stat = "identity", color = "black", fill = "#2c3e50", alpha =.9) +
#         coord_flip() +
#         theme(axis.text.x = element_text(angle = 0),
#               legend.position = "none") +
#         labs(x = "",
#              y = "",
#              title = paste(input$indicador_CP_comp, "(top 20)"),
#              caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
#                                     unique(CP_comp()$FUENTE)))) +
#         scale_y_continuous(labels = addUnits)
#       
#       print(plot_CP_comp)
#       ggsave("www/indicador CP.png", width = 30, height = 30, units = "cm")
#       
#       } else if(input$indicador_CP_comp %in% list_barras_CP_comp_men) {
#         
#         req(input$mes_anio_CP_comp)
#         
#         base_plot_CP_comp <-  CP_comp() %>% 
#           filter(mes_anio == input$mes_anio_CP_comp)  
#         
#         validate(need(nrow(base_plot_CP_comp) > 0, 'No hay datos disponible para esta búsqueda'))
#         
#         plot_CP_comp  <- base_plot_CP_comp %>%      
#           top_n(20, VALOR) %>% 
#           filter(categoria != "TOTAL IMPORTACIONES CIF",
#                  categoria != "TOTAL IMPORTACIONES CIF") %>% 
#           mutate(categoria = str_trunc(categoria, 75)) %>% 
#           ggplot(aes(x = fct_reorder(categoria, VALOR), y = VALOR)) +
#           geom_bar(stat = "identity", color = "black", fill = "#2c3e50", alpha =.9) +
#           coord_flip() +
#           theme(axis.text.x = element_text(angle = 0),
#                 legend.position = "none") +
#           labs(x = "",
#                y = "",
#                title = paste(input$indicador_CP_comp, "(top 20)"),
#                caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
#                                       unique(CP_comp()$FUENTE)))) +
#           scale_y_continuous(labels = addUnits)
#         
#         
#         print(plot_CP_comp)
#         ggsave("www/indicador CP.png", width = 30, height = 30, units = "cm")
#         
#     } else if(input$indicador_CP_comp %in% list_barras_CP_comp_anual){ 
# 
#       plot_CP_comp <- ggplot(data =  CP_comp() %>% 
#                                filter(fecha_num >= input$fecha_CP_comp[1] &
#                                         fecha_num <= input$fecha_CP_comp[2]) %>% 
#                                 filter(categoria %in% input$chbox_filtro_cpcomp),
#                               aes(x = fecha, y = VALOR, color = categoria)) +
#         geom_line(size = 1, alpha = 0.5) +
#         geom_point(size = 3) +
#         theme(axis.text.x = element_text(angle = 0),
#               legend.position = "bottom") +
#         labs(x = "",
#              y = "",
#              title = input$indicador_SP_deuda,
#              caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de",
#                                     unique(CP_comp()$FUENTE)))) +
#         scale_y_continuous(labels = addUnits) +
#         scale_color_viridis(name = " ", discrete = T, direction = 1)
#       
#       print(plot_CP_comp)
#       ggsave("www/indicador CP.png", width = 30, height = 20, units = "cm")
#       
#     } else if(input$indicador_CP_comp %in% lista_simple_mensual){ 
# 
#       req(input$fecha_CP_comp_mensual, input$indicador_CP_comp)
#       
#       plot_CP_comp <- ggplot(data =  CP_comp() %>%
#                                filter(fecha >= as.POSIXct(zoo::as.yearmon(input$fecha_CP_comp_mensual[1])) &
#                                         fecha <= as.POSIXct(zoo::as.yearmon(input$fecha_CP_comp_mensual[2]))),
#                              aes(x = fecha, y = VALOR)) +
#         geom_line(size = 1, color = "#476481", alpha = 0.5) +
#         geom_point(color = "#476481", size = 3) +
#         theme(axis.text.x = element_text(angle = 0),
#               legend.position = "none") +
#         labs(x = "",
#              y = "",
#              title = input$indicador_CP_comp,
#              caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de",
#                                     unique(CP_comp()$FUENTE)))) +
#         scale_y_continuous(labels = addUnits)
#       
#       print(plot_CP_comp)
#       ggsave("www/indicador CP.png", width = 30, height = 20, units = "cm")
# 
#             
#     } else if(input$indicador_CP_comp %in% lista_simple_CP_comp){ 
#       
#       req(input$fecha_CP_comp, input$indicador_CP_comp)
#       
#       plot_CP_comp <- ggplot(data =  CP_comp() %>% 
#                                filter(fecha_num >= input$fecha_CP_comp[1] &
#                                         fecha_num <= input$fecha_CP_comp[2]),
#                              aes(x = fecha, y = VALOR)) +
#         geom_line(size = 1, color = "#476481", alpha = 0.5) +
#         geom_point(color = "#476481", size = 3) +
#         theme(axis.text.x = element_text(angle = 0),
#               legend.position = "none") +
#         labs(x = "",
#              y = "",
#              title = input$indicador_CP_comp,
#              caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de",
#                                     unique(CP_comp()$FUENTE)))) +
#         scale_y_continuous(labels = addUnits)
#       
#       print(plot_CP_comp)
#       ggsave("www/indicador CP.png", width = 30, height = 20, units = "cm")
#     
#       }
#   })
#   
#     
# output$p_CP_comp_t5_p <- renderPlot({
#     
#   if(input$indicador_CP_comp == "Empresas exportadoras (USD corrientes) según Uruguay XXI") {
#     
#     req(input$fecha_CP_comp, input$indicador_CP_comp)
#     
#     base_plot_CP_comp <-  CP_comp() %>% 
#       filter(categoria == "Total empresas") 
#     
#     plot_CP_comp_t5  <- base_plot_CP_comp %>%      
#       ggplot(aes(x = fecha, y = VALOR, color = categoria)) +
#       geom_line(size = 1, alpha = 0.5) +
#       geom_point(size = 3) +
#       theme(axis.text.x = element_text(angle = 0),
#             legend.position = "bottom") +
#       labs(x = "",
#            y = "",
#            title = input$indicador_CP_comp,
#            caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
#                                   unique(CP_comp()$FUENTE)))) +
#       scale_y_continuous(labels = addUnits) +
#       scale_color_viridis(name = "", discrete=TRUE) 
#     
#     print(plot_CP_comp_t5)
#     ggsave("www/indicador CP_t5.png", width = 40, height = 20, units = "cm")
#     
#       
#   } else { 
# 
#     
#     req(input$fecha_CP_comp, input$indicador_CP_comp)
#     
#     base_plot_CP_comp <-  CP_comp() %>% 
#       filter(categoria != "TOTAL GENERAL",
#              categoria != "Total general",
#              categoria != "Total empresas",
#              categoria != "Total",
#              categoria != "TOTAL") %>% 
#       mutate(categoria = str_trunc(categoria, 75)) %>%
#       add_count(categoria, wt = VALOR, name = "temp") %>%
#       mutate(temp = dense_rank(desc(temp))) %>%
#       filter(temp %in% 1:5) %>%
#       select(-temp)
#     
#     plot_CP_comp_t5  <- base_plot_CP_comp %>%      
#       ggplot(aes(x = fecha, y = VALOR, color = categoria)) +
#       geom_line(size = 1, alpha = 0.5) +
#       geom_point(size = 3) +
#       theme(axis.text.x = element_text(angle = 0),
#             legend.position = "bottom") +
#       labs(x = "",
#            y = "",
#            title = paste(input$indicador_CP_comp, "(top 5 histórico)"),
#            caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
#                                   unique(CP_comp()$FUENTE)))) +
#       scale_y_continuous(labels = addUnits) +
#       scale_color_viridis(name = "", discrete=TRUE) 
#     
#     print(plot_CP_comp_t5)
#     ggsave("www/indicador CP_t5.png", width = 40, height = 20, units = "cm")
#   
#     }
#   
#   })
# 
# 
# output$p_CP_comp_api<- renderPlot({
#   
#   req(input$fecha_CP_comp, input$indicador_CP_comp)
#   
# plot_CP_comp  <- ggplot(data = CP_comp() %>% 
#                           filter(fecha_num >= input$fecha_CP_comp[1] &
#                                    fecha_num <= input$fecha_CP_comp[2]) %>%
#                           filter(categoria != "Total"),
#                         aes(x = fecha, y = VALOR, fill = categoria)) +
#   geom_col(color = "black", alpha = .8) +
#   theme(axis.text.x = element_text(angle = 0),
#         legend.position = "bottom") +
#   labs(x = "",
#        y = "",
#        title = input$indicador_CP_comp,
#        caption = wrapit(paste("Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de", 
#                               unique(CP_comp()$FUENTE)))) +
#   scale_y_continuous(labels = addUnits) +
#   scale_fill_brewer(name = "Flujo de IMPORTACION", palette = "Blues")
# 
# print(plot_CP_comp)
# ggsave("www/indicador CP.png", width = 30, height = 30, units = "cm")
# 
# })
# 
# 
#   output$p_CP_comp_t5 <- renderUI({
#   
#     if(input$indicador_CP_comp %in% c(list_barras_CP_comp, list_barras_CP_comp_men)) {
#       
#     plotOutput("p_CP_comp_t5_p")
#       
#     } else if(input$indicador_CP_comp %in% list_barras_CP_comp_anual){ 
#      
#        plotOutput("p_CP_comp_api")
#   
#     } else {
#       
#       NULL
#     }
#       })
#     
#   # Botón descarga grafico
#   output$baja_p_CP_comp <- downloadHandler(
#     filename <- function() {
#       paste("indicador CP", "png", sep = ".")
#     },
#     
#     content <- function(file) {
#       file.copy("www/indicador CP.png", file)
#     },
#     contentType = "www/indicador CP"
#   )
#   
#   
#   output$baja_p_CP_comp_t5 <- downloadHandler(
#     filename <- function() {
#       paste("indicador CP_t5", "png", sep = ".")
#     },
#     
#     content <- function(file) {
#       file.copy("www/indicador CP_t5.png", file)
#     },
#     contentType = "www/indicador CP_t5"
#   )
# 
#   
#   
#   # Data para tabla y exportar 
#   CP_comp_1 <- reactive({
#     
#     req(input$anio_CP_comp)
#     
#     CP_comp() %>%
#       mutate(anio = format(fecha, format = "%Y")) %>%
#       filter(anio == input$anio_CP_comp) %>%
#       select(anio, categoria, VALOR_ORIGINAL) %>%
#       arrange(anio, fct_reorder(categoria, -VALOR_ORIGINAL)) 
#   })
#   
#   CP_comp_2 <- reactive({
#     
#     req(input$mes_anio_CP_comp)
#     
#     CP_comp() %>%
#       mutate(anio = format(fecha, format = "%Y-%m")) %>%
#       filter(mes_anio == input$mes_anio_CP_comp) %>% 
#       select(mes_anio, categoria, VALOR_ORIGINAL) %>%
#       arrange(mes_anio, fct_reorder(categoria, -VALOR_ORIGINAL)) 
#     
#   })
#   
#   CP_comp_3 <- reactive({
#     CP_comp() %>%
#       filter(fecha_num >= input$fecha_CP_comp[1] &
#                fecha_num <= input$fecha_CP_comp[2]) %>% 
#       mutate(fecha = format(fecha, format = "%Y")) %>% 
#       select(fecha, categoria, VALOR_ORIGINAL) %>%
#       arrange(desc(fecha), fct_reorder(categoria, -VALOR_ORIGINAL)) %>% 
#       pivot_wider(names_from = categoria,
#                   values_from = VALOR_ORIGINAL)
#     
#   })
# 
#     CP_comp_4 <- reactive({
#     CP_comp() %>%
#         filter(fecha_num >= input$fecha_CP_comp[1] &
#                  fecha_num <= input$fecha_CP_comp[2]) %>% 
#       mutate(fecha = format(fecha, format = "%Y")) %>% 
#       select(fecha, VALOR_ORIGINAL) %>%
#       arrange(desc(fecha))
#   })
# 
#     CP_comp_5 <- reactive({
#       CP_comp() %>%
#         filter(fecha >= as.POSIXct(zoo::as.yearmon(input$fecha_CP_comp_mensual[1])) &
#                  fecha <= as.POSIXct(zoo::as.yearmon(input$fecha_CP_comp_mensual[2]))) %>% 
#         mutate(fecha = format(fecha, format = "%Y-%m")) %>% 
#         select(fecha, VALOR_ORIGINAL) %>%
#         arrange(desc(fecha))
#     })
#     
#     
#     
#   # Data completa 
#   CP_comp_1_c <- reactive({
#     d_CP_comp %>%
#       mutate(fecha = format(fecha, format = "%Y")) %>%
#       filter(NOMINDICADOR == input$indicador_CP_comp) %>% 
#       select(anio, categoria, VALOR_ORIGINAL) %>%
#       arrange(anio, fct_reorder(categoria, -VALOR_ORIGINAL)) %>% 
#       pivot_wider(names_from = categoria,
#                   values_from = VALOR_ORIGINAL)
#   })
#   
#   CP_comp_2_c <- reactive({
#     d_CP_comp %>%
#       mutate(fecha = format(fecha, format = "%Y-%m")) %>%
#       filter(NOMINDICADOR == input$indicador_CP_comp) %>% 
#       select(fecha, categoria, VALOR_ORIGINAL) %>%
#       arrange(desc(fecha), fct_reorder(categoria, -VALOR_ORIGINAL)) %>% 
#       pivot_wider(names_from = categoria,
#                   values_from = VALOR_ORIGINAL)
#     
#   })
#   
#   CP_comp_3_c <- reactive({
#     d_CP_comp %>%
#       mutate(fecha = format(fecha, format = "%Y")) %>%
#       filter(NOMINDICADOR == input$indicador_CP_comp) %>% 
#       select(fecha, categoria, VALOR_ORIGINAL) %>%
#       arrange(desc(fecha), fct_reorder(categoria, -VALOR_ORIGINAL)) %>% 
#       pivot_wider(names_from = categoria,
#                   values_from = VALOR_ORIGINAL)
#     
#   })
#   
#   CP_comp_4_c <- reactive({
#     d_CP_comp %>%
#       mutate(fecha = format(fecha, format = "%Y")) %>%
#       filter(NOMINDICADOR == input$indicador_CP_comp) %>% 
#       select(fecha, VALOR_ORIGINAL) %>%
#       arrange(desc(fecha))
#   })
#   
#   CP_comp_5_c <- reactive({
#     d_CP_comp %>%
#       mutate(fecha = format(fecha, format = "%Y-%m")) %>%
#       filter(NOMINDICADOR == input$indicador_CP_comp) %>% 
#       select(fecha, VALOR_ORIGINAL) %>%
#       arrange(desc(fecha))
#   })
# 
#     # Metadata 
#   CP_comp_b <- reactive({
#     CP_comp() %>%
#       select(NOMINDICADOR, FUENTE, DEFINICION, `FORMA DE CALCULO`, RELEVANCIA) %>% 
#       mutate(`Unidad de Métodos y Acceso a Datos (FCS -UdelaR)` = " ") %>% 
#       distinct() %>% 
#       gather(key = "", value = " ")
#     
#   })
#   
#   list_df_CP_comp1 <- reactive({
#     list_df_CP_comp1 <- list("Data" = CP_comp_1(),
#                              "Metadata" = CP_comp_b(),
#                              "Data Completa" = CP_comp_1_c())
#   })
#   
#   list_df_CP_comp2 <- reactive({
#     list_df_CP_comp1 <- list("Data" = CP_comp_2(),
#                              "Metadata" = CP_comp_b(),
#                              "Data Completa" = CP_comp_2_c())
#   })
#   
#   list_df_CP_comp3 <- reactive({
#     list_df_CP_comp3 <- list("Data" = CP_comp_3(), 
#                              "Metadata" = CP_comp_b(),
#                              "Data Completa" = CP_comp_3_c())
#   })
#   
#   list_df_CP_comp4 <- reactive({
#     list_df_CP_comp4 <- list("Data" = CP_comp_4(),
#                              "Metadata" = CP_comp_b(),
#                              "Data Completa" = CP_comp_4_c())
#   })
# 
#   list_df_CP_comp5 <- reactive({
#     list_df_CP_comp5 <- list("Data" = CP_comp_5(),
#                              "Metadata" = CP_comp_b(),
#                              "Data Completa" = CP_comp_5_c())
#   })
#   
#   # Tablas en shiny
#   output$tab_CP_comp <- renderDT({
#     
#     if(input$indicador_CP_comp %in% list_barras_CP_comp) {
#       
#       DT::datatable(CP_comp_1(), 
#                 rownames = FALSE,
#                 colnames = c("Año", "Categoría", "Valor"),
#                 caption = htmltools::tags$caption(input$indicador_CP_comp,
#                                                   style = "color:black; font-size:110%;"))  %>% 
#         formatCurrency(3, '', interval =3, mark = ",") 
#       
#       } else if(input$indicador_CP_comp %in% list_barras_CP_comp_men) {
#         
#       DT::datatable(CP_comp_2(), 
#                 rownames = FALSE,
#                 colnames = c("Año", "Categoría", "Valor"), 
#                 caption = htmltools::tags$caption(input$indicador_CP_comp,
#                                                     style = "color:black; font-size:110%;"))  %>% 
#         formatCurrency(3, '', mark = ",") 
#       
#     } else if(input$indicador_CP_comp %in% list_barras_CP_comp_anual){ 
#       
#       DT::datatable(CP_comp_3(), 
#                 rownames = FALSE,
#                 # colnames = c("Fecha", "Categoría", "Valor"),
#                 caption = htmltools::tags$caption(input$indicador_CP_comp,
#                                                   style = "color:black; font-size:110%;"))  
#       
#     } else if(input$indicador_CP_comp %in% lista_simple_mensual){ 
#       
#       DT::datatable(CP_comp_5(), 
#                     rownames = FALSE,
#                     colnames = c("Fecha", "Valor"),
#                     caption = htmltools::tags$caption(input$indicador_CP_comp,
#                                                       style = "color:black; font-size:110%;")) %>% 
#         formatCurrency(2, '', mark = ",")
#     
#     } else if(input$indicador_CP_comp %in% lista_simple_CP_comp){ 
#       
#       DT::datatable(CP_comp_4(), 
#                 rownames = FALSE,
#                 colnames = c("Fecha", "Valor"),
#                 caption = htmltools::tags$caption(input$indicador_CP_comp,
#                                                   style = "color:black; font-size:110%;")) %>% 
#         formatCurrency(2, '', mark = ",")
#       
#     }
#     
#   })
# 
#   # Descarga tabla
#   output$dl_tabla_CP_comp <- downloadHandler(
#     
#     filename = function() {
#       paste("resultados-", input$indicador_CP_comp, ".xlsx", sep = "")
#     },
#     content = function(file) {
#       if(input$indicador_CP_comp %in% list_barras_CP_comp) {
#         
#         openxlsx::write.xlsx(list_df_CP_comp1(), file)
#         
#       } else if(input$indicador_CP_comp %in% list_barras_CP_comp_men) {
#           
#           openxlsx::write.xlsx(list_df_CP_comp2(), file)
#         
#       } else if(input$indicador_CP_comp == "Importaciones anuales de bienes y servicios CIF (millones USD corrientes)"){ 
#         
#         openxlsx::write.xlsx(list_df_CP_comp3(), file) 
#       
#         } else if(input$indicador_CP_comp %in% lista_simple_CP_comp){ 
#         
#         openxlsx::write.xlsx(list_df_CP_comp4(), file) 
#        
#         } else if(input$indicador_CP_comp %in% lista_simple_CP_comp){ 
# 
#           openxlsx::write.xlsx(list_df_CP_comp5(), file) 
#           
#       }
#     }
#   )
  
}  

shinyApp(ui = ui, server = server)

