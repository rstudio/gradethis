context("Unpipe pipes")

test_that("unpipe() strips off the top level of piping", {
  pipe <- quote(iris %>% filter(Species == "Virginica") %>% select(Sepal.Length)) # nolint
  func <- quote(select(iris %>% filter(Species == "Virginica"), Sepal.Length))

  expect_equal(unpipe(pipe), func)
})

test_that("unpipe() recognizes . syntax", {
  pipe2 <- quote(iris %>% lm(Sepal.Length ~ Sepal.Width, data = .))
  func2 <- quote(lm(Sepal.Length ~ Sepal.Width, data = iris))

  expect_equal(unpipe(pipe2), func2)
})


test_that("unpipe() does not alter unpiped code", {
  func2 <- quote(lm(Sepal.Length ~ Sepal.Width, data = iris))

  expect_equal(unpipe(func2), func2)
})
