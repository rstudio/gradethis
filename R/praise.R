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


#' @describeIn praise Returns a random encouragement message
#' @export
learnr::random_encouragement
