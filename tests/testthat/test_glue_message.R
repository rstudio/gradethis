context("Glue Message")

test_that("Check NULL, NA", {
  testthat::expect_equal(glue_message("{.is_bool}", .is_bool = NA), "NA")
  testthat::expect_equal(glue_message("{.is_bool}", .is_bool = NULL), character(0L))

  testthat::expect_equal(glue_message("{.char}", .char = NULL), "")
  testthat::expect_equal(glue_message("{.char}", .char = NULL), "")
})

test_that("Check message construction", {
  testthat::expect_equal(glue_message("{.is_bool} {.char}",
                                      .is_bool = TRUE,
                                      .char = "hello"),
                         "TRUE hello")
})
