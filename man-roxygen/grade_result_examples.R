#' @examples
#' grade_result(
#'   pass_if(~ identical(.result, 5), "This is a correct message"),
#'           grader_args = list(),
#'           learnr_args = list(last_value = 5, envir_prep = new.env())
#' )
#'
#' grade_result(
#'   pass_if(function(x) identical(x, 5), "This is a correct message"),
#'   learnr_args = list(last_value = 5)
#' )
#'
#' grade_result(
#'   pass_if(5, "This is a correct message"),
#'           learnr_args = list(last_value = 5)
#' )
#'
#' grade_result(
#'   fail_if(5, "You were supposed to do this other thing!"),
#'           pass_if(~ TRUE, "should never reach here"),
#'           learnr_args = list(last_value = 5)
#' )
