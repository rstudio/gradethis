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
gradethis_equal <- function(x, y, ...) {
  UseMethod("gradethis_equal")
}

#' @describeIn gradethis_equal
#'   The default comparison method, which uses [waldo::compare]
#' @inheritParams waldo::compare
#' @export
gradethis_equal.default <- function(x, y, tolerance, ...) {
  local_options_waldo_compare()

  tryCatch(
    {
      compare_message <- try_with_timelimit(
        waldo::compare(x, y, tolerance = tolerance)
      )

      if (is_graded(compare_message)) {
        # an internal grading problem occurred with waldo::compare()
        return(compare_message)
      }

      length(compare_message) == 0
    },
    error = function(e) {
      # waldo::compare() takes into account a lot of the things we'd have to
      # think about in comparing two objects, but its goal is to create a
      # readable diff. Since we're engaging in some off-label usage of these
      # functions, they will sometimes error or take longer than desired when we
      # give them unusual inputs. In these cases, we fall back to `identical()`.
      # Since we aren't (currently) interested in reporting the differences
      # between `x` and `y`, we mark them "different" if they aren't identical.
      identical(x, y)
    }
  )
}
