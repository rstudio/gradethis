context("Unpipe pipes")
library(tidyverse)

test_that("unpipe() strips off the top level of piping", {
  pipe <- quote(iris %>% filter(Species == "Virginica") %>% select(Sepal.Length))
  func <- quote(select(iris %>% filter(Species == "Virginica"), Sepal.Length))
  
  expect_equal(unpipe(pipe), func)
})

test_that("unpipe() recognizes top level . syntax", {
  pipe2 <- quote(iris %>% lm(Sepal.Length ~ Sepal.Width, data = .))
  func2 <- quote(lm(Sepal.Length ~ Sepal.Width, data = iris))
  
  expect_equal(unpipe(pipe2), func2)
})

test_that("unpipe() does not alter unpiped code", {
  func2 <- quote(lm(Sepal.Length ~ Sepal.Width, data = iris))
  
  expect_equal(unpipe(func2), func2)
})

test_that("unpipe() preserves argument names", {
  
  expect_equal(unpipe(quote(iris %>% mutate(species = Species %>% tolower()))), 
               quote(mutate(iris, species = Species %>% tolower())))
})

test_that("unpipe_all() completely unpipes code with multiple pipes", {
  
  expect_equal(unpipe_all(quote(a %>% b() %>% c())), quote(c(b(a))))
  expect_equal(unpipe_all(quote(a %>% b())), quote(b(a)))
  expect_equal(unpipe_all(quote(a)), quote(a))
  expect_equal(unpipe_all(quote(iris %>% pluck("Sepal.Length") %>% mean())),
               quote(mean(pluck(iris, "Sepal.Length"))))
})

test_that("unpipe_all() unpipes pipes that occur within arguments", {
  
  expect_equal(unpipe_all(quote(iris %>% mutate(species = Species %>% tolower()))),
               quote(mutate(iris, species = tolower(Species))))
})


test_that("unpipe_all() recognizes top level . syntax", {
  pipe2 <- quote(iris %>% lm(Sepal.Length ~ Sepal.Width, data = .) %>% summary())
  func2 <- quote(summary(lm(Sepal.Length ~ Sepal.Width, data = iris)))
  
  expect_equal(unpipe_all(pipe2), func2)
})

test_that("unpipe_all does not alter unpiped code", {
  func2 <- quote(lm(Sepal.Length ~ Sepal.Width, data = iris))
  
  expect_equal(unpipe_all(func2), func2)
})