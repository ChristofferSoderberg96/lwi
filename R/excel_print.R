#' Automatically save a collection of dataframes to a specified file directory in .xlsx format
#'
#' @param df_list Global environment list of dataframes
#' @param filepath Desired file path for the .xlsx files
#' @param ggplots Default FALSE - if TRUE, assumes df_list is a global environment list of ggplot objects, useful for 1:1 outputs with charts
#'
#' @return Returns nothing to the global environment
#' @export
#'
#' @examples
#' \dontrun{
#' df1 <- data.frame(x = c(1,2,3), y = c("A", "B", "C"))
#' df2 <- data.frame(x = c(4,5,6), y = c("D", "E", "F"))
#'
#' excel_print(mget(c("df1", "df2")), "Excel")
#'
#' chart1 <- ggplot(mtcars, aes(x = mpg, y = disp)) + geom_point()
#' chart2 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
#'
#' excel_print(mget(c("chart1", "chart2")), "Excel", ggplots = TRUE)
#'
#' }
excel_print <- function (df_list, filepath, ggplots = FALSE) {

  if (stringr::str_length(filepath) > 0) {

    if (length(df_list) > 0) {

      if(!dir.exists(filepath)) {
        dir.create(filepath)
      }

      home <- here::here() # get current directory

      setwd(filepath) # set to specified directory for excel files

      for (i in 1:length(df_list)) {

        namestring <- paste(names(df_list[i]), ".xlsx", sep = "")

        if (ggplots) {
          writexl::write_xlsx(df_list[[i]]$data, namestring)
        } else {
          writexl::write_xlsx(df_list[i], namestring)
        }

      }

      setwd(home) # set working directory to what it was before function call

    } else {

      warning("No objects specified")

    }

  } else {

    warning("No filepath specified")

  }

}
