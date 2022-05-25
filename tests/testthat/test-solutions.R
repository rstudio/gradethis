local_edition(3)

test_that("split solutions", {
  code <- '# one ----
runif(1)
# --- two ----
    runif( 2 )
### ------- three ----------
runif( 3 ) '

  expect_equal(
    solutions_prepare(code),
    gradethis_solutions(
      one   = "runif(1)",
      two   = "    runif( 2 )",
      three = "runif( 3 ) "
    )
  )

  code <- '
runif(1)
# --- two ----
    runif( 2 )
## ----------
runif( 3 ) '

  expect_equal(
    solutions_prepare(code),
    gradethis_solutions(
      solution00 = "runif(1)",
      two        = "    runif( 2 )",
      solution02 = "runif( 3 ) "
    )
  )


  code <- '
runif(1)
# ----
    runif( 2 )
##     ----------
runif( 3 ) '

  expect_equal(
    solutions_prepare(code),
    gradethis_solutions(
      solution00 = "runif(1)",
      solution01 = "    runif( 2 )",
      solution02 = "runif( 3 ) "
    )
  )

  code <- "runif(1)"
  expect_equal(solutions_prepare(code), list("runif(1)"))
  expect_equal(solutions_prepare(code)[[1]], code[[1]])
})

test_that("split solutions with empty code", {
  expect_null(solutions_prepare(NULL))
  expect_null(solutions_prepare(""))
  expect_null(solutions_prepare("      "))
  expect_null(solutions_prepare("\n\n\n\n"))
  expect_null(solutions_prepare(c(" ", "", "\n", "\t\t")))
})

test_that("split solutions with code comments", {
  code <- '
# one ----

# this solution is fine
runif(1)

# two ----
# but maybe this one would work, too
runif(2 - 1)'

  code_split <- solutions_prepare(code)
  expect_equal(
    code_split$one,
    "# this solution is fine\nrunif(1)"
  )

  expect_equal(
    code_split$two,
    "# but maybe this one would work, too\nrunif(2 - 1)"
  )
})

test_that("split_code_headers()", {
  target <- list(one = "1", two = "2")

  # No whitespace after dashes
  expect_equal(
    split_code_headers(
      "# one ----
1
# two ----
2"
    ),
    target
  )


  # Whitespace after first header
  expect_equal(
    split_code_headers(
      "# one ----
1
# two ----
2"
    ),
    target
  )

  # Whitespace after subsequent headers
  expect_equal(
    split_code_headers(
      "# one ----
1
# two ----
2"
    ),
    target
  )
})
