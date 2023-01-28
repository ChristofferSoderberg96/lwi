

#' Automatically save a collection of ggplot objects to a specified file directory as PNGs
#'
#' @param plot_list List of ggplot objects from global environment
#' @param filepath Desired file path for the PNGs
#' @param width Width of the PNG output, in mm
#' @param height Height of the PNG output, in mm
#' @param res Resolution of the PNG output (default 72)
#' @param scaling Scaling factor of the PNG output (default 1)
#'
#' @return Returns nothing to the global environment
#' @export
#'
#' @examples
#' \dontrun{
#' chart1 <- ggplot(mtcars, aes(x = mpg, y = disp)) + geom_point()
#' chart2 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
#'
#' plot_print(mget(c("chart1", "chart2")), "Charts", width = 200, height = 200)
#' }
plot_print <- function (plot_list, filepath, width, height, res = 72, scaling = 1) {

  if (length(plot_list) > 0) {

    if(!dir.exists(filepath)) {
      dir.create(filepath)
    }

    home <- here::here() # get current directory

    setwd(filepath) # set to specified directory for PNGs

    for (i in 1:length(plot_list)) {

      namestring <- paste(names(plot_list[i]), ".png", sep="")
      ragg::agg_png(namestring,
                    unit = "mm",
                    res = res,
                    width = width,
                    height = height,
                    scaling = scaling)
      print(plot_list[i])
      grDevices::dev.off()

    }

    setwd(home) # set working directory to what it was before function call

  } else {

    warning("No objects specified")

  }

}
