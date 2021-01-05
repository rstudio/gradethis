#' Random praise and encouragement
#'
#' Generate a random praise or encouragement phrase.
#' This can be used in conjunction with
#' [glue::glue()] to generate praise or encouragement within feedback to users.
#'
#' @examples
#' replicate(5, glue::glue("Random praise: {random_praise()}"))
#' replicate(5, glue::glue("Random encouragement: {random_encouragement()}"))
#' 
#' @seealso [learnr::random_praise()], [learnr::random_encouragement()]
#' @name praise
NULL

#' @importFrom learnr random_praise
#' @export
learnr::random_praise

#' @importFrom learnr random_encouragement
#' @export
learnr::random_encouragement
