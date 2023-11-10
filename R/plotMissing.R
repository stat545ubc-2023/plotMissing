#' @title Plot counts of missing and recorded values in each selected column
#'
#' @description
#' This function returns a stacked column plot which depicts the number of missing (`NA`)
#' and recorded (non-`NA`) observations in each specified column of a tibble.
#'
#' @param .data A tibble in R which contains data. This parameter is named `.data` because it is a generic name for a dataset, and this name matches the convention of other tidyverse functions. The `.` allows this function to be called using a pipe, as opposed to requiring the dataset to be explicitly given in the function call.
#' @param ... A collection of additional graphical parameters which will be passed to `ggplot2`. This parameter gives us extra flexibility in creating the plots by dding desired graphical features after the creation of the "basic" plot.
#' @param cols <[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)> The columns in `.data` which will be shown in the plot. This parameter is named `cols` because it is a selection of columns, and this name matches the convention of other tidyverse functions.
#' @param count_names A vector of two strings, which will be the names given to the counts of missing and recorded values, respectively. This parameter is named `count_names` because the two values in this vector are the names corresponding to the counts of missing and recorded values.
#' @param bar_colours A vector of two colour codes, which are used for depicting the missing and recorded values, respectively. For more information on valid colour codes, see [grDevices::colors()]. This parameter is named `bar_colours` because that is the proper Canadian spelling, and because the two values in this vector are the colours in the plot which correspond to the missing missing and recorded values.
#'
#' @returns
#' A stacked column plot which displays the number of missing and recorded observations per column
#' in `cols`. This plot has a legend with the group names given in `count_names`, where the colours
#' for each group are provided in `bar_colours`, and the plot also includes any additional
#' graphical parameters which were provided using the `...` argument.
#'
#' @examples
#' # This function does not require every single tidyverse library
#' # but it is convenient to load them all at once
#' library(tidyverse)
#' # load in a built-in dataset in R and convert it to a tibble
#' airquality <- tibble::as_tibble(airquality)
#' # displays a plot with the number of missing/recorded observations in each column
#' plotMissing(airquality)
#' # subsetting columns, changing the group names, and changing the bar colours
#' plotMissing(airquality,
#'             cols = c(Ozone, Solar.R),
#'             count_names = c("No", "Yes"),
#'             bar_colours = c("purple", "orange"))
#' # adding additional graphical parameters (axis labels, theme, and centering the title)
#' plotMissing(airquality,
#'             labs(x = "Column Name", y = "Number of Rows",
#'                  title = "Count of Missing Values in Selected Columns"),
#'             theme_bw(),
#'             theme(plot.title = element_text(hjust = 0.5)),
#'             cols = c(Ozone, Solar.R),
#'             count_names = c("No", "Yes"),
#'             bar_colours = c("purple", "red"))
#'
#' @import tibble
#' @import ggplot2
#' @import rlang
#' @importFrom tidyselect everything
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_longer
#' @importFrom forcats fct_rev
#' @importFrom magrittr %>%
#'
#' @export

plotMissing <- function(.data, ...,
                        cols = tidyselect::everything(),
                        count_names = c("Missing", "Recorded"),
                        bar_colours = c("red", "darkgreen")
                        ){
  # Check that the provided dataset is a tibble
  if(!tibble::is_tibble(.data)){
    stop(paste("This function requires the provided dataset to be a tibble.",
               "The data which you provided is not in tibble format."))
  }

  # Check that count_names has exactly 2 values
  if(length(count_names) != 2){
    stop(paste("The names for the counts data must have exactly two values.",
               "You provided a list of", length(count_names), "names instead."))
  }

  # Check that bar_colours has exactly 2 values
  if(length(bar_colours) != 2){
    stop(paste("The colors for the counts data must have exactly two values.",
               "You provided a list of", length(bar_colours), "colors instead."))
  }

  # Extract the names and colours from the count_names and bar_colours arguments
  missing_data_name = count_names[1]
  recorded_data_name = count_names[2]
  missing_data_colour = bar_colours[1]
  recorded_data_colour = bar_colours[2]

  # Select only the desired columns from the dataset
  data_subset <- .data %>%
    dplyr::select({{ cols }})

  # Create a new tibble with the counts of recorded and missing observations per column
  data_counts <- tibble::tibble(variable = colnames(data_subset),
                                {{ missing_data_name }} := colSums(is.na(data_subset)),
                                {{ recorded_data_name }} := colSums(!is.na(data_subset)))

  # Use pivot_longer to put the counts in one column
  data_counts <- data_counts %>%
    tidyr::pivot_longer(cols = !c("variable"),
                        names_to = "presence",
                        values_to = "count")

  # Turn "presence" into a factor so the count of recorded values is always
  # on the left instead of showing group names based on alphabetical order
  data_counts <- data_counts %>%
    dplyr::mutate(presence = factor(data_counts$presence, levels = count_names))

  # Extract any additional ggplot parameters from the ellipsis
  ggplot_extra_params <- list(...)

  # Create the column plot to visualize the counts
  missing_data_plot <- data_counts %>%
    ggplot2::ggplot(ggplot2::aes(x = forcats::fct_rev(data_counts$variable),
                                 y = data_counts$count,
                                 fill = data_counts$presence)) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::scale_fill_manual(values = bar_colours) +
    ggplot2::labs(x = "Variable", y = "Count", fill = "Presence of Data") +
    ggplot2::coord_flip() +
    ggplot_extra_params # Add any extra parameters which were provided

  # Return the plot which was constructed above
  return(missing_data_plot)
}
