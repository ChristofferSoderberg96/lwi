test_that("Empty dataframe list raises warning", {
  expect_warning(excel_print(list(), ""))
})

test_that("Empty filepath string raises warning", {
  expect_warning(excel_print(df, ""))
})
