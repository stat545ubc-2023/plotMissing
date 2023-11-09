#' @title Plot counts of missing and recorded values in each selected column
#'
#' @details This function returns a stacked column plot which depicts the number of missing (`NA`) and recorded (not `NA`) observations in each specified column of a tibble.
#'
#' @param `.data` A tibble in R which contains data. This parameter is named `.data` because it is a generic name for a dataset, and this name matches the convention of other tidyverse functions. The `.` allows this function to be called using a pipe, as opposed to requiring the dataset to be explicitly given in the function call.
#' @param `...` A collection of additional graphical parameters which will be passed to `ggplot2`. This parameter gives us extra flexibility in creating the plots by dding desired graphical features after the creation of the "basic" plot.
#' @param `cols` <[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)> The columns in `.data` which will be shown in the plot. This parameter is named `cols` because it is a selection of columns, and this name matches the convention of other tidyverse functions.
#' @param `count_names` A vector of two strings, which will be the names given to the counts of missing and recorded values, respectively. This parameter is named `count_names` because the two values in this vector are the names corresponding to the counts of missing and recorded values.
#' @param `bar_colours` A vector of two colour codes, which are used for depicting the missing and recorded values, respectively. For more information on valid colour codes, see `colors()`. This parameter is named `bar_colours` because that is the proper Canadian spelling, and because the two values in this vector are the colours in the plot which correspond to the missing missing and recorded values.
#'
#' @return A stacked column plot displaying the number of missing and recorded observations in each column in `cols`, with the names given in `count_names`, the colours selected in `bar_colours`, and any additional graphical parameters which were provided in `...`.
#' @export
#'
#' @examples
#' library(tidyverse) # this function does not require every single tidyverse library, but it is convenient to load them all
#' airquality <- as_tibble(airquality) # load in a built-in dataset in R and convert it to a tibble
#' # displays a plot with the number of missing/recorded observations in each column
#' plotMissing(airquality)
#' # displays the same plot as above but called using a pipe
#' airquality %>%
#'   plotMissing()
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

plotMissing <- function(.data, ..., cols = everything(),
                        count_names = c("Missing", "Recorded"),
                        bar_colours = c("red", "darkgreen")
                        ){
  # Check that the provided dataset is a tibble
  if(!is_tibble(.data)){
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
    select({{ cols }})

  # Create a new tibble with the counts of recorded and missing observations per column
  data_counts <- tibble(variable = colnames(data_subset),
                        {{ missing_data_name }} := colSums(is.na(data_subset)),
                        {{ recorded_data_name }} := colSums(!is.na(data_subset)))

  # Use pivot_longer to put the counts in one column
  data_counts <- data_counts %>%
    pivot_longer(cols = !variable, names_to = "presence", values_to = "count")

  # Turn presence"into a factor so that the count of recorded values is always
  # shown first (on the left)
  data_counts <- data_counts %>%
    mutate(presence = factor(presence, levels = count_names))

  # Extract any additional ggplot parameters from the ellipsis
  ggplot_extra_params <- list(...)

  # Create the column plot to visualize the counts
  missing_data_plot <- data_counts %>%
    ggplot(aes(x = forcats::fct_rev(variable), y = count, fill = presence)) +
    geom_col(position = "stack") +
    scale_fill_manual(values = bar_colours) +
    labs(x = "Variable", y = "Count", fill = "Presence of Data") +
    coord_flip() +
    ggplot_extra_params # Add any extra parameters which were provided

  # Return the plot which was constructed above
  return(missing_data_plot)
}
