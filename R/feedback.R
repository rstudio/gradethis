

#' Provide grade feedback
#'
#' Creates a feedback object suitable for returning in a learnr checking function
#' (e.g., the `exercise.checker` option in [learnr::tutorial_options()])
#'
#' @param grade a [graded()] object.
#' @param type Feedback type (visual presentation style). Can be "auto", "success", "info", "warning", "error", or "custom".
#' Note that "custom" implies that the "message" field is custom HTML rather than a character vector.
#' @param location Location for feedback ("append", "prepend", or "replace").
#' @noRd
feedback <- function(
  grade,
  type = c("auto", "success", "info", "warning", "error", "custom"),
  location = c("append", "prepend", "replace")
) {
  # do not allow grade objects to throw
  grade <- capture_graded(grade)

  if (!is_graded(grade)) {
    stop("`grade` must be a `graded` object", call. = FALSE)
  }

  type <- match.arg(type)

  if (identical("auto", type)) {
    type <- if (grade$correct) "success" else "error"
  }

  structure(
    list(
      message = message_md(grade$message),
      correct = grade$correct,
      type = type,
      location = match.arg(location)
    ),
    class = "gradethis_feedback"
  )
}


is_feedback <- function(x) {
  inherits(x, "gradethis_feedback")
}


message_md <- function(message = NULL) {
  message <- message %||% ""
  
  if (!is.character(message)) {
    message <- paste(as.character(message), collapse = "")
  }
  
  md <- commonmark::markdown_html(
    disallow_tags(message),
    smart = TRUE,
    normalize = TRUE,
    extensions = c("tagfilter", "strikethrough", "table", "autolink")
  )
  
  htmltools::HTML(md)
}

disallow_tags <- function(md) {
  # In theory, these tags should be filtered by the `tagfilter` cmark-gfm extension
  # but in practice they were not. The list of tags was taken from the GFM specs:
  # https://github.com/github/cmark-gfm/blob/85d895289c5ab67f988ca659493a64abb5fec7b4/test/spec.txt#L9661-L9672
  bad_tags <- c(
    "title", "textarea", "style", "xmp", "iframe", "noembed", "noframes", 
    "script", "plaintext"
  )
  
  gsub(
    pattern = glue::glue("<(/?({tags}))", tags = paste(bad_tags, collapse = "|")),
    replacement = "&lt;\\1",
    md,
    ignore.case = TRUE
  )
}
