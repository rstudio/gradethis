#' Compare the values of two objects to check whether they are equal
#'
#' @param x,y Two objects to compare
#' @param ... Additional arguments passed to methods
#'
#' @return A [logical] value of length one, or an internal gradethis error.
#' @export
#'
#' @examples
#' gradethis_equal(mtcars[mtcars$cyl == 6, ], mtcars[mtcars$cyl == 6, ])
#' gradethis_equal(mtcars[mtcars$cyl == 6, ], mtcars[mtcars$cyl == 4, ])
gradethis_equal <- function(x = .result, y = .solution, ...) {
  if (is_placeholder(x) || is_placeholder(y)) {
    x <- resolve_placeholder(x)
    y <- resolve_placeholder(y)
    return(gradethis_equal(x, y, ...))
  }

  UseMethod("gradethis_equal")
}

#' @describeIn gradethis_equal
#'   The default comparison method, which uses [waldo::compare]
#' @inheritParams waldo::compare
#' @export
gradethis_equal.default <- function(
  x,
  y,
  tolerance = sqrt(.Machine$double.eps),
  ...
) {
  # First check with `identical()`, since it's much faster than `waldo::compare()`
  if (identical(x, y)) {
    return(TRUE)
  }

  # If `identical()` returned `FALSE`, try `waldo::compare()`,
  # since `identical()` is prone to false negatives
  local_options_waldo_compare()
  compare_message <- try(
    waldo::compare(x, y, tolerance = tolerance),
    silent = TRUE
  )

  if (is_graded(compare_message)) {
    # An internal grading problem occurred with waldo::compare()
    return(compare_message)
  }

  # If `waldo::compare()` found no differences, `x` and `y` are equal
  length(compare_message) == 0
}
