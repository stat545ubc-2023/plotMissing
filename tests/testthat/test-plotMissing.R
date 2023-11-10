# Load in a dataset which we can use as the input for plotMissing
airquality <- datasets::airquality

test_that("plotMissing throws an error when the data is not a tibble", {
  expect_error(plotMissing(airquality))
})

# Now that we know the function fails on non-tibbles, we convert our data to a tibble
airquality <- tibble::tibble(datasets::airquality)

test_that("plotMissing throws an error when count_names does not have exactly 2 values", {
  # Only one name provided
  expect_error(plotMissing(airquality, count_names = c("Everything")))
  # Three names provided
  expect_error(
    plotMissing(airquality, count_names = c("Missing", "Recorded",
                                            "Another secret third thing"))
  )
})

test_that("plotMissing throws an error when bar_colours does not have exactly 2 values", {
  # Only one colour provided
  expect_error(plotMissing(airquality, bar_colours = c("goldenrod2")))
  # Three colours provided
  expect_error(
    plotMissing(airquality, bar_colours = c("violet", "maroon", "limegreen"))
  )
})

test_that("plotMissing generates a column plot instead of another type of plot
           (e.g. a histogram or a scatter plot)", {
  test_plot <- plotMissing(airquality)
  expect_true("GeomCol" %in% class(test_plot$layers[[1]]$geom))
  expect_false("GeomHist" %in% class(test_plot$layers[[1]]$geom))
  expect_false("GeomPoint" %in% class(test_plot$layers[[1]]$geom))
  expect_false("GeomBoxplot" %in% class(test_plot$layers[[1]]$geom))
})

test_that("plotMissing does not alter the dataset which was passed into the function", {
  # Create a copy of the air quality dataset (to compare)
  airquality_copy <- airquality

  # Check that the datasets are still identical after the function is run
  air_quality_missing_plot <- plotMissing(airquality)
  expect_identical(airquality, airquality_copy)
})

test_that("plotMissing yields the same result when called via the pipe", {
  test_plot_1 <- plotMissing(airquality)
  test_plot_2 <- airquality %>%
    plotMissing()

  # Check that the expression for the x-axes is the same in both plots
  expect_true(rlang::get_expr(test_plot_1$mapping$x) ==
                rlang::get_expr(test_plot_2$mapping$x))

  # Check that the expression for the y-axes is the same in both plots
  expect_true(rlang::get_expr(test_plot_1$mapping$y) ==
                rlang::get_expr(test_plot_2$mapping$y))

  # Check that the expression for the fill is the same in both plots
  expect_true(rlang::get_expr(test_plot_1$mapping$fill) ==
                rlang::get_expr(test_plot_2$mapping$fill))

  # Check that the axis labels (and legend title) are the same in both plots
  expect_true(identical(test_plot_1$labels, test_plot_2$labels))
})

test_that("plotMissing yields the same result when columns are selected as an argument
           and when called on a subset of the original dataset", {
  test_plot_1 <- plotMissing(airquality, cols = tidyselect::contains("o"))
  test_plot_2 <- airquality %>%
    dplyr::select(tidyselect::contains("o")) %>%
    plotMissing()

  # Check that the expression for the x-axes is the same in both plots
  expect_true(rlang::get_expr(test_plot_1$mapping$x) ==
                rlang::get_expr(test_plot_2$mapping$x))

  # Check that the expression for the y-axes is the same in both plots
  expect_true(rlang::get_expr(test_plot_1$mapping$y) ==
                rlang::get_expr(test_plot_2$mapping$y))

  # Check that the expression for the fill is the same in both plots
  expect_true(rlang::get_expr(test_plot_1$mapping$fill) ==
                rlang::get_expr(test_plot_2$mapping$fill))

  # Check that the axis labels (and legend title) are the same in both plots
  expect_true(identical(test_plot_1$labels, test_plot_2$labels))

  # Check that the data itself used for generating the plots is equal
  expect_equal(test_plot_1$data, test_plot_2$data)
})

test_that("plotMissing yields the same result when additional graphical parameters
           are provided with the `...` argument and when added after the function call", {
  test_plot_1 <- plotMissing(airquality,
    ggplot2::labs(x = "Column Name", y = "Number of Rows",
                  title = "Count of Missing Values in Selected Columns"),
    ggplot2::theme_bw())
  test_plot_2 <- plotMissing(airquality) +
    ggplot2::labs(x = "Column Name", y = "Number of Rows",
                  title = "Count of Missing Values in Selected Columns") +
    ggplot2::theme_bw()

  # Check that the expression for the x-axes is the same in both plots
  expect_true(rlang::get_expr(test_plot_1$mapping$x) ==
                rlang::get_expr(test_plot_2$mapping$x))

  # Check that the expression for the y-axes is the same in both plots
  expect_true(rlang::get_expr(test_plot_1$mapping$y) ==
                rlang::get_expr(test_plot_2$mapping$y))

  # Check that the expression for the fill is the same in both plots
  expect_true(rlang::get_expr(test_plot_1$mapping$fill) ==
                rlang::get_expr(test_plot_2$mapping$fill))

  # Check that the axis labels (and legend title) are the same in both plots
  expect_true(identical(test_plot_1$labels, test_plot_2$labels))

  # Check that the data itself used for generating the plots is equal
  expect_equal(test_plot_1$data, test_plot_2$data)
})

# remove the dataset which we loaded
rm(airquality)
