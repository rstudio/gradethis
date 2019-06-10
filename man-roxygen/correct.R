#' @param correct A character string to display if the student answer matches
#'   a known answer.
#'   This character string will be run through \code{glue::\link[glue]{glue_data}} with \code{list(correct = TRUE, message = "<result message>")}. where message is the matched result message.
#'   \code{matched} is a boolean value about whether the submitted is found in a set of provided values.
