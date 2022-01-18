
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

rm(list=ls())

source("utils.R")

theme_set(theme_bdd(base_size = 12))
update_geom_defaults("text", list(family = theme_get()$text$family))

dir.create('~/.fonts')
file.copy("www/Titillium Web.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')

# Spinner options 
options(spinner.color = "#476481",
        spinner.color.background="#ffffff", 
        spinner.size = 2)

# Shiny app customized theme
tema_umad <- bs_theme(version = 5,
                      bg = "#FFFFFF",
                      fg = "#085792",
                      primary = "#3E6C9A",
                      secondary = "#3E6C9A",
                      base_font = font_google("Roboto"),
                      heading_font = font_google("Roboto"),
                      code_font = font_google("Space Mono"))

# bs_theme_preview(tema_umad)

##  1. PREPARAR DATA  =======================================================

load("data/data.rda")

datita <- data %>% 
  filter(nomindicador == "Acceso a la electricidad, sector rural (% de la población rural) según WDI")
table(datita$pais)


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
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  
  tabPanel(
    
    # * 2.1. Economía --------------------------------------------------------

    title = "Economía", 
    icon = icon("dollar-sign"),
    
    fluidRow(
      
        sidebarPanel(
          
          width = 3,
          # style = "position:fixed;width:22%;",
          
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
          downloadButton(outputId = "baja_plot_eco",
                         label = "Descarga el gráfico"),
          br(),
          br(),
          DTOutput("tab_dat_eco"),
          br(),
          downloadButton("dl_tabla_dat_eco", "Descarga la tabla"),
          br(),
          br()
          
        )
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
  
  ##  3.  ECONOMÍA (dat_eco)   ============================================
  
  
referencias <- "<br><b>Referencias:</b> K = miles; M = millones; B = billones. <br>
                * Los valores usan el separador decimal inglés: los números luego
                de puntos son decimales, y las comas separan los miles. "

  ## dat_eco

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
    
    if(input$indicador_eco == "Acceso a la electricidad, sector rural (% de la población rural) según WDI") {
    
      checkboxGroupInput(
        inputId = "chbox_pais_eco",
        label = "Seleccione país o región",
        inline = TRUE,
        choices = dat_eco() %>%
          filter(nomindicador == input$indicador_eco) %>%
          distinct(cod_pais) %>%
          pull(),
        selected = c("URY", "ARG", "BRA", "CHI")
        )
      
    } else if (input$indicador_eco == "Acceso a la electricidad, sector urbano (% de la población urbana) según WDI"){

      checkboxGroupInput(
        inputId = "chbox_pais_eco",
        label = "Seleccione país o región",
        inline = TRUE,
        choices = dat_eco() %>%
          filter(nomindicador == input$indicador_eco) %>%
          distinct(pais) %>%
          pull(),
        selected = c("Uruguay", "Argentina", "Brasil", "Chile")
      )
      
    } else {
      
      dropdown(
        
        label = "Seleccione país o región",
        status = "default",
        width = 400,
        circle = F,
        # icon = icon("flag", lib = "font-awesome"),
        # tooltipOptions(placement = "right", title = "Máximo 10"),
        
        checkboxGroupInput(
          inputId = "chbox_pais_eco",
          label = "Seleccione país o región",
          inline = TRUE,
          choices = dat_eco() %>%
            filter(nomindicador == input$indicador_eco) %>%
            distinct(cod_pais) %>%
            pull(),
          selected = c("URY", "ARG", "BRA", "CHI")
        )
        
      )
    }
    
    })
  
    # Gráficos CP_comp
    output$p_dat_eco <- renderPlot({

      req(input$fecha_dat_eco, input$indicador_eco)

        plot_eco <- ggplot(data = dat_eco() %>%
                             filter(fecha >= input$fecha_dat_eco[1] &
                                      fecha <= input$fecha_dat_eco[2]) %>%
                             filter(cod_pais %in% input$chbox_pais_eco),
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

  # Botón descarga grafico
  output$baja_plot_eco <- downloadHandler(
    filename <- function() {
      paste("indicador eco", "png", sep = ".")
    },

    content <- function(file) {
      file.copy("www/indicador eco.png", file)
    },
    contentType = "www/indicador eco"
  )



  # Data para tabla y exportar
  dat_eco_t <- reactive({
    
    dat_eco() %>%
      filter(fecha >= input$fecha_dat_eco[1] & 
               fecha <= input$fecha_dat_eco[2]) %>%
      filter(cod_pais %in% input$chbox_pais_eco) %>% 
      mutate(fecha = format(fecha, format = "%Y")) %>% 
      select(fecha, pais, valor) %>%
      arrange(desc(fecha), fct_reorder(pais, -valor))
  })
  
  # Metadata
  dat_eco_m <- reactive({
    
    dat_eco() %>%
      select(nomindicador, definicion, concepto_estadistico_y_metodologia, relevancia) %>%
      mutate(`Unidad de Métodos y Acceso a Datos (FCS -UdelaR)` = " ") %>%
      distinct() %>%
      gather(key = "", value = " ")

    })
  
  # Data completa
  dat_eco_c <- reactive({
    
    dat_eco %>%
      mutate(fecha = format(fecha, format = "%Y")) %>%
      filter(nomindicador == input$indicador_eco) %>%
      select(fecha, pais, valor_original) %>%
      arrange(desc(fecha), fct_reorder(pais, -valor_original)) %>% 
      pivot_wider(names_from = pais,
                  values_from = valor_original)
    })
  
  # Lista para descarga
  list_dat_eco <- reactive({
    
      list_dat_eco <- list("Data" = dat_eco_t(),
                           "Metadata" = dat_eco_m(),
                           "Data Completa" = dat_eco_c())
      })
  
  
  # Tablas en shiny
  output$tab_dat_eco <- renderDT({

      DT::datatable(dat_eco_t(),
                rownames = FALSE,
                caption = htmltools::tags$caption(input$indicador_eco,
                                                  style = "color:black; font-size:110%;")) %>% 
      formatCurrency(3, '', mark = ",")

  })

  # Descarga tabla
  output$dl_tabla_dat_eco <- downloadHandler(

    filename = function() {
      paste("resultados-", input$indicador_eco, ".xlsx", sep = "")
    },
    content = function(file) {
      
      openxlsx::write.xlsx(list_dat_eco(), file)

    }
  )
  
}  

shinyApp(ui = ui, server = server)

