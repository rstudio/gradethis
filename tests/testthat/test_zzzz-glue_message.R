test_that("Check NULL, NA", {
  testthat::expect_equal(glue_message("{.is_bool}", .is_bool = NA), "NA")
  testthat::expect_equal(glue_message("{.is_bool1}", .is_bool1 = NULL, .is_bool2 = NULL), "NA")

  testthat::expect_equal(glue_message("{.num_thing}", .num_thing = NA), "NA")
  testthat::expect_equal(glue_message("{.num_thing1}", .num_thing1 = NULL, .num_thing2 = NULL), "NA")

  testthat::expect_equal(glue_message("{.char}", .char = NA), "NA")
  testthat::expect_equal(glue_message("{.char}", .char = NULL), "")
})

test_that("Check message construction", {
  testthat::expect_equal(glue_message("{.is_bool} {.char}",
    .is_bool = TRUE,
    .char = "hello"),
  "TRUE hello")
})

test_that("glue_message() returns a scalar string", {
  # vector params are concatenated with no space
  expect_equal(glue_message("{x} {y}", x = 1:3, y = 4), "123 4")
  expect_equal(glue_message("{x} {y}", x = letters[1:3], y = 4), "abc 4")
  expect_equal(glue_message("{x} {y}", x = c("a ", "b ", "c"), y = 4), "a b c 4")

  expect_equal(
    glue_message("{x} {y}", x = htmltools::p(htmltools::strong("a")), y = 1),
    "<p>\n  <strong>a</strong>\n</p> 1"
  )

  expect_equal(
    glue_message_with_env(new.env(), c("This ", "message ", "has several lines.")),
    "This message has several lines."
  )
})
