test_that("infix works", {
  expect_true(is_infix("1+1"))
  expect_true(is_infix("1*1"))
  expect_true(is_infix("a %in% b"))
  expect_false(is_infix("1"))

  })
test_that("infix is robust", {
  expect_false(is_infix("[azeazer]"))
  expect_false(is_infix("[azeazer"))
  expect_false(is_infix("[azeazer*"))
  })
