#' Learning and Work Institute discrete colour palette
#'
#' @param fill Use the fill palette
#'
#' @family colour lwi
#'
#' @export
lwi_pal <- function (fill = TRUE) {

  if (fill) {
    function(n) {
      if (n == 1) {
        i <- "#ee7e3b"
      }
      else if (n == 2) {
        i <- c("#264c59", "#ee7e3b")
      }
      else if (n == 3) {
        i <- c("#f6c65b", "#ee7e3b", "#264c59")
      }
      else if (n == 4) {
        i <- c("#f6c65b", "#ee7e3b", "#264c59",
               "#3fbfad")
      }
      else if (n %in% 5:6) {
        i <- c("#f6c65b", "#ee7e3b", "#5d3754",
               "#264c59", "#3fbfad", "#a02140")
      }
      else if (n == 7) {
        i <- c("#f6c65b", "#ee7e3b", "#264c59",
               "#5d3754", "#a02140", "#3fbfad", "#77b7d4")
      }
      else if (n == 8) {
        i <- c("#f6c65b", "#ee7e3b", "#264c59",
               "#5d3754", "#a02140",
               "#c85d15", "#3fbfad", "#77b7d4")
      }
      else if (n == 9) {
        i <- c("#f6c65b", "#ee7e3b", "#264c59",
               "#5d3754", "#a02140",
               "#c85d15", "#3fbfad", "#77b7d4", "#0086d4")
      }
      else if (n == 10) {
        i <- c("#f6c65b", "#ee7e3b", "#264c59",
               "#5d3754", "#a02140",
               "#c85d15", "#3fbfad", "#77b7d4", "#0086d4",
               "#624494")
      }
      else if (n == 11) {
        i <- c("#f6c65b", "#ee7e3b", "#264c59",
               "#5d3754", "#a02140",
               "#c85d15", "#3fbfad", "#77b7d4", "#0086d4",
               "#624494", "#e32110")
      }
      else if (n == 12) {
        i <- c("#f6c65b", "#ee7e3b", "#264c59",
               "#5d3754", "#a02140",
               "#c85d15", "#3fbfad", "#77b7d4", "#0086d4",
               "#624494", "#e32110", "#1b5dc8")
      }
      else if (n == 13) {
        i <- c("#f6c65b", "#ee7e3b", "#264c59",
               "#5d3754", "#a02140",
               "#c85d15", "#3fbfad", "#77b7d4", "#0086d4",
               "#624494", "#e32110", "#1b5dc8", "#1b5633")
      }
    }
  }
  else {
    function(n) {
      if (n <= 3) {
        i <- c("#ee7e3b", "#264c59", "#5d3754")
      }
      else if (n %in% 4:5) {
        i <- c("#f6c65b", "#ee7e3b", "#5d3754",
               "#264c59", "#3fbfad")
      }
      else if (n == 6) {
        i <- c("#f6c65b", "#ee7e3b", "#264c59",
               "#5d3754", "#a02140", "#3fbfad")
      }
      else if (n > 6) {
        i <- c("#f6c65b", "#ee7e3b", "#264c59",
               "#5d3754", "#a02140", "#3fbfad", "#c85d15","#77b7d4", "#0086d4")
      }
    }
  }
}

#' Learning and Work Institute colour scales
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @inheritParams lwi_pal
#'
#' @family colour lwi
#'
#' @rdname scale_lwi
#'
#' @export
scale_colour_lwi <- function (...) {
  ggplot2::discrete_scale("colour", "lwi", lwi_pal(), ...)
}

#' @rdname scale_lwi
#'
#' @export
scale_fill_lwi <- function (...) {
  ggplot2::discrete_scale("fill", "lwi", lwi_pal(), ...)
}

#' Learning and Work Institute ggplot theme
#'
#' If Maven Pro Medium is installed (https://fonts.google.com/download?family=Maven%20Pro)
#' and accessible by R through library(extrafont), the theme will use this font,
#' otherwise it will default to Arial.
#'
#' @inheritParams ggplot2::theme_grey
#' @param horizontal Default is TRUE for horizontal gridlines only. If FALSE, displays vertical gridlines, useful in conjunction with coord_flip()
#'
#' @return An object of class \code{\link[ggplot2]{theme}()}
#' @export
lwi_theme <- function(base_size = 12, base_family = "sans",
                      horizontal = TRUE) {

  lwiorange <- "#ee7e3b"
  lwiblue <- "#264c59"
  lwigrey <- "#4d4d4d"
  lwi_l_grey <- "#fafafa"

  ## From measurements
  ## Ticks = 1 / 32 in, with margin about 1.5 / 32
  ## Title = 3 / 32 in (6 pt)
  ## Legend Labels = 2.5 / 32 in (5pt)
  ## Axis Labels = 2
  ## Axis Titles and other text ~ 2
  ## Margins: Top / Bottom = 6 / 32, sides = 5 / 32
  ret <-
    theme_foundation(base_size = base_size, base_family = base_family) +
    theme(line = element_line(colour = lwigrey),
          rect = element_rect(fill = "white", colour = NA,
                              linetype = 1),
          text = element_text(colour = lwigrey, size = base_size),
          ## Axis
          axis.line = element_line(size = rel(1)),
          axis.text = element_text(size = rel(1)),
          axis.text.x = element_text(vjust = 0,
                                     margin = margin(t = base_size,
                                                     unit = "pt")),
          axis.text.x.top = element_text(vjust = 0, margin = margin(b = base_size, unit = "pt")),
          axis.text.y = element_text(hjust = 0,
                                     margin = margin(r = base_size,
                                                     unit = "pt")),
          axis.ticks = element_line(),
          axis.title = element_text(size = rel(1.25)),
          axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
          axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"), angle = 90),
          axis.ticks.length = unit(base_size * 0.5, "points"),
          legend.background = element_rect(linetype = 0),
          legend.spacing = unit(base_size * 1.25, "points"),
          legend.key = element_rect(linetype = 0),
          legend.key.size = unit(1.25, "lines"),
          legend.key.height = NULL,
          legend.key.width = NULL,
          legend.text = element_text(size = rel(1.25)),
          legend.text.align = NULL,
          legend.title = element_text(size = rel(1),  hjust = 0),
          legend.title.align = NULL,
          legend.position = "top",
          legend.direction = NULL,
          legend.justification = "center",

          panel.background = element_rect(fill = "white",
                                          colour = "white", linetype = 0,
                                          size = rel(1)),
          panel.border = element_blank(),
          panel.grid.major = element_line(colour = lwigrey, size = rel(1), linetype = "dotted"),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(0.25, "lines"),
          strip.background = element_rect(fill = lwi_l_grey,
                                          colour = NA, linetype = 0),
          strip.text = element_text(size = rel(1.25)),
          strip.text.x = element_text(),
          strip.text.y = element_text(angle = -90),
          plot.background = element_rect(fill = "white",
                                         colour = NA),
          plot.title = element_text(size = rel(1.5),
                                    hjust = 0, face = "bold",
                                    colour = lwiblue),
          plot.margin = unit(c(6, 5, 6, 5) * 2, "points"),
          complete = TRUE)

  if(grDevices::windowsFonts()["Maven Pro Medium"] == "Maven Pro Medium") {
    ret <- ret + theme(text = element_text(family = "Maven Pro Medium"))
  }

  if (horizontal) {
    ret <- ret + theme(panel.grid.major.x = element_blank())
  } else {
    ret <- ret + theme(panel.grid.major.y = element_blank())
  }

  ret

}
