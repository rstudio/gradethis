#' Random praise and encouragement
#'
#' Generate a random praise or encouragement phrase.
#' This can be used in conjunction with
#' `glue::[glue][glue::glue()]` to generate praise or encouragement within feedback to users.
#'
#' @examples
#' replicate(5, glue::glue("Random praise: {random_praise()}"))
#' replicate(5, glue::glue("Random encouragement: {random_encouragement()}"))
#' @export
#' @describeIn praise Returns a random praise message
random_praise <- function() {
  sub("^Correct! ", "", learnr::random_praise())
}

#' @export
#' @describeIn praise Returns a random encouragement message, included for 
#'   backwards compatibility. 
random_encourage <- function() {
  lifecycle::deprecate_soft("0.2.1", "random_encourage()", "random_encouragement()")
  random_encouragement()
}

#' @export
#' @describeIn praise Returns a random encouragement message
random_encouragement <- learnr::random_encouragement
