test_that("split solutions", {
  code <- '# one ----
runif(1)
# --- two ----
    runif( 2 )
  # ------- three ----------
runif( 3 ) '
  
  expect_equal(
    solutions_prepare(code),
    list(one = "runif(1)", two = "runif(2)", three = "runif(3)")
  )
  
  code <- '
runif(1)
# --- two ----
    runif( 2 )
  # ----------
runif( 3 ) '
  
  expect_equal(
    solutions_prepare(code),
    list(solution00 = "runif(1)", two = "runif(2)", solution02 = "runif(3)")
  )
  
  
  code <- '
runif(1)
# ----
    runif( 2 )
  # ----------
runif( 3 ) '
  
  expect_equal(
    solutions_prepare(code),
    list(solution00 = "runif(1)", solution01 = "runif(2)", solution02 = "runif(3)")
  )
  
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
