test_that("split solutions", {
  code <- '# one ----
runif(1)
# --- two ----
    runif( 2 )
  # ------- three ----------
runif( 3 ) '
  
  expect_equal(
    solutions_prepare(code),
    gradethis_solutions(
      one   = "runif(1)",
      two   = "runif(2)",
      three = "runif(3)"
    )
  )
  
  code <- '
runif(1)
# --- two ----
    runif( 2 )
  # ----------
runif( 3 ) '
  
  expect_equal(
    solutions_prepare(code),
    gradethis_solutions(
      solution00 = "runif(1)",
      two        = "runif(2)",
      solution02 = "runif(3)"
    )
  )
  
  
  code <- '
runif(1)
# ----
    runif( 2 )
  # ----------
runif( 3 ) '
  
  expect_equal(
    solutions_prepare(code),
    gradethis_solutions(
      solution00 = "runif(1)",
      solution01 = "runif(2)",
      solution02 = "runif(3)"
    )
  )
  
  code <- "runif(1)"
  expect_equal(solutions_prepare(code), list("runif(1)"))
  expect_equal(solutions_prepare(code)[[1]], code[[1]])
})

test_that("split solutions with unparseable code", {
  code <- '
runif(1)
# ----
    runif( 
  # ----------
runif( 3 ) '
  
  # throws both an error and a condition
  expect_error(solutions_prepare(code))
  expect_condition(solutions_prepare(code), class = "gradethis_graded")
})

test_that("split solutions with empty code", {
  expect_null(solutions_prepare(NULL))
  expect_null(solutions_prepare(""))
  expect_null(solutions_prepare("      "))
  expect_null(solutions_prepare("\n\n\n\n"))
  expect_null(solutions_prepare(c(" ", "", "\n", "\t\t")))
})
