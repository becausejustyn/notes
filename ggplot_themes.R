# Data Viz

#https://github.com/nsgrantham/ggdark/blob/master/R/themes.R
#https://github.com/nsgrantham/ggdark/blob/master/R/dark_mode.R

# https://github.com/koundy/ggplot_theme_Publication/blob/master/ggplot_theme_Publication-2.R

library(grid)
library(ggthemes)

###############################
###############################  Theme Dark Grey
###############################

theme_dark_grey <- function(base_size = 14, base_family = "sans") {
  (theme_foundation(base_size = base_size, base_family = base_family)
   + theme(
     plot.title = element_text(
       face = "bold", colour = "#ffffb3",
       size = rel(1.2), hjust = 0.5, margin = margin(0, 0, 20, 0)
     ),
     text = element_text(),
     panel.background = element_rect(colour = NA, fill = "grey20"),
     plot.background = element_rect(colour = NA, fill = "#262626"),
     panel.border = element_rect(colour = NA),
     axis.title = element_text(face = "bold", size = rel(1), colour = "white"),
     axis.title.y = element_text(angle = 90, vjust = 2),
     axis.title.x = element_text(vjust = -0.2),
     axis.text = element_text(colour = "grey70"),
     axis.line.x = element_line(colour = "grey70"),
     axis.line.y = element_line(colour = "grey70"),
     axis.ticks = element_line(colour = "grey70"),
     panel.grid.major = element_line(colour = "#262626"),
     panel.grid.minor = element_blank(),
     legend.background = element_rect(fill = "#262626"),
     legend.text = element_text(color = "white"),
     legend.key = element_rect(colour = NA, fill = "#262626"),
     legend.position = "bottom",
     legend.direction = "horizontal",
     legend.box = "vetical",
     legend.key.size = unit(0.5, "cm"),
     # legend.margin = unit(0, "cm"),
     legend.title = element_text(face = "italic", colour = "white"),
     plot.margin = unit(c(10, 5, 5, 5), "mm"),
     strip.background = element_rect(colour = "#2D3A4C", fill = "#2D3A4C"),
     strip.text = element_text(face = "bold", colour = "white")
   ))
}

###############################
###############################  Theme Dark Blue
###############################

theme_dark_blue <- function(base_size = 14, base_family = "sans") {
  (theme_foundation(base_size = base_size, base_family = base_family)
   + theme(
     plot.title = element_text(
       face = "bold", colour = "#ffffb3",
       size = rel(1.2), hjust = 0.5, margin = margin(0, 0, 20, 0)
     ),
     text = element_text(),
     panel.background = element_rect(colour = NA, fill = "#282C33"),
     plot.background = element_rect(colour = NA, fill = "#282C33"),
     panel.border = element_rect(colour = NA),
     axis.title = element_text(face = "bold", size = rel(1), colour = "white"),
     axis.title.y = element_text(angle = 90, vjust = 2),
     axis.title.x = element_text(vjust = -0.2),
     axis.text = element_text(colour = "grey70"),
     axis.line.x = element_line(colour = "grey70"),
     axis.line.y = element_line(colour = "grey70"),
     axis.ticks = element_line(colour = "grey70"),
     panel.grid.major = element_line(colour = "#343840"),
     panel.grid.minor = element_blank(),
     legend.background = element_rect(fill = "#282C33"),
     legend.text = element_text(color = "white"),
     legend.key = element_rect(colour = NA, fill = "#282C33"),
     legend.position = "bottom",
     legend.direction = "horizontal",
     legend.box = "vetical",
     legend.key.size = unit(0.5, "cm"),
     # legend.margin = unit(0, "cm"),
     legend.title = element_text(face = "italic", colour = "white"),
     plot.margin = unit(c(10, 5, 5, 5), "mm"),
     strip.background = element_rect(colour = "#2D3A4C", fill = "#2D3A4C"),
     strip.text = element_text(face = "bold", colour = "white")
   ))
}


###############################
###############################  Theme Black
###############################


theme_black <- function(base_size = 12, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),
      axis.text.x = element_text(size = base_size * 0.8, color = "white", lineheight = 0.9),
      axis.text.y = element_text(size = base_size * 0.8, color = "white", lineheight = 0.9),
      axis.ticks = element_line(color = "white", size = 0.2),
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),
      axis.ticks.length = unit(0.3, "lines"),
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),
      legend.key = element_rect(color = "white", fill = "black"),
      legend.key.size = unit(1.2, "lines"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.text = element_text(size = base_size * 0.8, color = "white"),
      legend.title = element_text(size = base_size * 0.8, face = "bold", hjust = 0, color = "white"),
      legend.position = "right",
      legend.text.align = NULL,
      legend.title.align = NULL,
      legend.direction = "vertical",
      legend.box = NULL,
      # Specify panel options
      panel.background = element_rect(fill = "black", color = NA),
      panel.border = element_rect(fill = NA, color = "white"),
      panel.grid.major = element_line(color = "grey35"),
      panel.grid.minor = element_line(color = "grey20"),
      panel.spacing = unit(0.5, "lines"),
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),
      strip.text.x = element_text(size = base_size * 0.8, color = "white"),
      strip.text.y = element_text(size = base_size * 0.8, color = "white", angle = -90),
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),
      plot.title = element_text(size = base_size * 1.2, color = "white"),
      plot.margin = unit(rep(1, 4), "lines")
    )
}


###############################
###############################  Theme 538
###############################

theme_fivethirtyeight <- function(base_size = 13, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      
      # Base elements which are not used directly but inherited by others
      line = element_line(
        colour = "#DADADA", size = 0.75,
        linetype = 1, lineend = "butt"
      ),
      rect = element_rect(
        fill = "#F0F0F0", colour = "#F0F0F0",
        size = 0.5, linetype = 1
      ),
      text = element_text(
        family = base_family, face = "plain",
        colour = "#656565", size = base_size,
        hjust = 0.5, vjust = 0.5, angle = 0,
        lineheight = 0.9
      ),
      
      # Modified inheritance structure of text element
      plot.title = element_text(
        size = rel(1.5), family = "",
        face = "bold", hjust = -0.05,
        vjust = 1.5, colour = "#3B3B3B"
      ),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(),
      
      # Modified inheritance structure of line element
      axis.ticks = element_line(),
      panel.grid.major = element_line(),
      panel.grid.minor = element_blank(),
      
      # Modified inheritance structure of rect element
      plot.background = element_rect(),
      panel.background = element_rect(),
      legend.key = element_rect(colour = "#DADADA"),
      
      # Modifiying legend.position
      legend.position = "none",
      complete = TRUE
    )
}