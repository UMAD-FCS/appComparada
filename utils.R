set.seed(1989)

import_titillium_web <- function() {
  
  tw_font_dir <- system.file("fonts", "titillium-web", package = "hrbrthemes")
  
  suppressWarnings(suppressMessages(extrafont::font_import(tw_font_dir, prompt = FALSE)))
  
  message(
    sprintf(
      "You will likely need to install these fonts on your system as well.\n\nYou can find them in [%s]",
      tw_font_dir)
  )
  
}

# theme_bdd

theme_bdd <- function(
  base_family = "Titillium Web", 
  base_size = 14,
  plot_title_family = if (.Platform$OS.type == "windows") "Titillium Web" else "Titillium-Web Bold",
  plot_title_size = 16,
  plot_title_face = "bold",
  plot_title_margin = 12,
  subtitle_family = if (.Platform$OS.type == "windows") "Titillium Web" else "Titillium Web Light",
  subtitle_size = 13,
  subtitle_face = "plain",
  subtitle_margin = 15,
  strip_text_family = base_family,
  strip_text_size = 12,
  strip_text_face = "plain",
  caption_family = if (.Platform$OS.type == "windows") "Titillium Web" else "Titillium Web",
  caption_size = 10,
  caption_face = "plain", 
  caption_margin = 10,
  axis_text_size = base_size,
  axis_title_family = base_family,
  axis_title_size = 10,
  axis_title_face = "plain",
  axis_title_just = "rt",
  plot_margin = margin(30, 30, 30, 30)) {
  
  ggplot2::theme_minimal(base_family = base_family, base_size = base_size)

}

blue_bdd <- "#0075BE"
blue_bdd2 <- "#3290cb"
blue_bdd3 <- "#4c9ed1"

theme_ggparliament_bdd <- function(legend = TRUE,
                                   background_colour = FALSE,
                                   border = FALSE) {
  basic_theme <- theme_void(base_family = "Titillium Web", base_size = 12)
  
  
  if (legend == TRUE) {
    basic_theme <- basic_theme + ggplot2::theme(legend.position = "bottom")
  } else {
    basic_theme <- basic_theme + ggplot2::theme(legend.position = "none")
  }
  
  
  
  if (!background_colour) {
    basic_theme <- basic_theme
  } else {
    basic_theme <- basic_theme + 
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#F5F5F5", colour = NA)) # white smoke fill
  }
  
  
  if (!border) {
    basic_theme <- basic_theme
  } else {
    basic_theme <- basic_theme + 
      ggplot2::theme(panel.border = ggplot2::element_rect(colour = "#F5F5F5", fill = NA)) # white smoke colour
  }
  
  basic_theme
}  


coloriza_partidos <- function(vector) {
  levs <- if(is.factor(vector)) 
    levels(vector) 
  else 
    levels(factor(vector))
  
  predefinidos <- c("FA", "PN", "PC", "CA", "PI", "UP", "Cifra", "Equipos", 
                    "Factum", "Interconsult", "Opcion", "Radar", "OtrosP.", "VB/VA",
                    "Frente Amplio", "Partido Nacional", "Partido Colorado", "Cabildo Abierto", "Partido Independiente", "Unidad Popular",
                    "Aprueba", "Desaprueba", "Ni aprueba ni desaprueba", "NS/NC", "Saldo",
                    "Sanguinetti 2", "Lacalle", "Batlle", "Vazquez 1", "Mujica", "Vazquez 2", "Lacalle Pou")
  
  pal <- c("#013197", "#99ccff", "#BA0200", "#F8BC1E", "#663399", "#00913C", "#4C9ED1", 
           "#5C8DCC", "#737AC0", "#8865AC", "#984E91", "#9F3770", "grey35", "grey75",
           "#013197", "#99ccff", "#BA0200", "#F8BC1E", "#663399", "#00913C",
           "#4c9ed1", "#BA0200", "#808080", "#D3D3D3", "#4c9ed1",
           "#BA0200", "#4c9ed1", "#BA0200", "#013197", "#013197", "#013197", "#4c9ed1")
  
  pal <- pal[match(levs, predefinidos)]
  
  blanks <- which(is.na(pal))
  
  pal[blanks] <- sample(colours(100), length(blanks))
  
  pal
}

# creamos una función para graficar parlamento

par_uy <- function(datos, titulo, mayoria, ...){
  
  ggplot(datos, aes(x, y, colour = party_long)) +
    geom_parliament_seats() + 
    geom_highlight_government(government == 1) +
    draw_majoritythreshold(n = mayoria, label = FALSE, type = 'semicircle') +
    #geom_parliament_bar(colour = datos$colour, party = datos$party_long) + 
    theme_ggparliament_bdd() +
    labs(colour = NULL, 
         title = titulo,
         subtitle = "") +
    scale_colour_manual(values = datos$colour,
                        limits = datos$party_long)
}

wrapit <- function(x, n = 120) {
  wtext <- paste(strwrap(x, width = n), collapse=" \n ")
  return(wtext)
}

`%notin%` <- Negate(`%in%`)


addUnits <- function(n) {
  labels <- ifelse(n >= 0 & n < 1000, n,  # si es menos de 1000 less than thousands
                   ifelse(n >= 1000 & n < 1e6, paste0(round(n/1e3), 'K'),  # si es menor a un millon in thousands
                          ifelse(n >= 1e6 & n < 1e12, paste0(round(n/1e6), 'M'),  # si es menor a 1000 millones in millions
                                 ifelse(n >= 1e12 & n < 1e15, paste0(round(n/1e12), 'B'), # in billions
                                       ifelse(n < 0 & n > -1000, n,  # si es menos de 1000 less than thousands
                                              ifelse(n < -1000 & n > -1000000, paste0(round(n/1e3), 'K'),  # si es menor a un millon in thousands
                                                     ifelse(n < -1000000 & n  > -1000000000000, paste0(round(n/1e6), 'M'),  # si es menor a 1000 millones in millions
                                                            ifelse(n < -1000000000000 & n > -1000000000000000, paste0(round(n/1e12), 'B'), # in billions
                                                                   "error"))))))))
  return(labels)
}
