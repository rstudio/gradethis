test_that("gradethis_equal.list() checks names", {
  expect_false(gradethis_equal(list(pi, letters), list(a = pi, b = letters)))
})

test_that("gradethis_equal uses methods from ggcheck", {
  skip_if_not_installed("ggcheck", "0.0.5")
  skip_if_not_installed("ggplot2")

  withr::local_package("ggplot2")
  withr::local_package("ggcheck")

  p1 <- ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point()
  p2 <- ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point()

  withr::with_pdf(NULL, print(p1))

  expect_true(gradethis_equal(p1, p2))
})

test_that("gradethis_equal.list uses methods from ggcheck", {
  skip_if_not_installed("ggcheck", "0.0.5")
  skip_if_not_installed("ggplot2")

  withr::local_package("ggplot2")
  withr::local_package("ggcheck")

  l1 <- list(
    ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point(),
    ggplot(mpg, aes(displ, hwy, colour = manufacturer)) + geom_point()
  )
  l2 <- list(
    ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point(),
    ggplot(mpg, aes(displ, hwy, colour = manufacturer)) + geom_point()
  )

  capture.output(withr::with_pdf(NULL, print(l1)))

  expect_true(gradethis_equal(l1, l2))
})
