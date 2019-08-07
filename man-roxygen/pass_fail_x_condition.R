#' @param x A formula, function, or value, that returns \code{TRUE} or \code{FALSE}.
#'    When comparing objects that are greater than length 1 (e.g., vectors, dataframes, matricies, etc)
#'    A boolean vector will be returned if the user uses \code{==}, not a single boolean value.
#'    \code{grader} will run the vector through \code{all(..., na.rm = TRUE)} to check for the boolean value.
#'    It is advised that the user use \code{identical()} instead of \code{==} in this case.
