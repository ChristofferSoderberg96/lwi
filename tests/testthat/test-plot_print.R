test_that("Empty plot list raises warning", {
  expect_warning(plot_print(list(), "", width = 200, height = 200))
})

test_that("Empty filepath string raises warning", {
  expect_warning(plot_print(chart, "", width = 200, height = 200))
})
