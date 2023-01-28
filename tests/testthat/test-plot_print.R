test_that("empty plot list raises warning", {
  expect_warning(plot_print(list(), "", width = 200, height = 200))
})
