

#' Provide grade feedback
#'
#' Creates a feedback object suitable for returning in a learnr checking function
#' (e.g., the `exercise.checker` option in [learnr::tutorial_options()])
#'
#' @param grade a [graded()] object. Note that if `grade` contains `type` or
#'   `location` items, these values will override the values provided to
#'   the `feedback()` call.
#' @param type Feedback type (visual presentation style). Can be "auto",
#'   "success", "info", "warning", "error", or "custom". Note that "custom"
#'   implies that the "message" field is custom HTML rather than a character
#'   vector.
#' @param location Location for feedback ("append", "prepend", or
#'   "replace").
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

  type <- grade$type %||% type %||% "auto"
  type <- match.arg(type)
  
  location <- grade$location %||% location %||% "append"
  location <- match.arg(location)

  if (identical("auto", type)) {
    type <- 
      if (length(grade$correct)) {
        if (grade$correct) "success" else "error"
      } else {
        "custom"
      }
  }
  
  feedback <- list(
    message = message_md(grade$message),
    correct = grade$correct,
    type = type,
    location = location
  )
  
  learnr_std_feedback <- which(names(grade) %in% c("message", "correct", "type", "location"))
  extra <- grade[-learnr_std_feedback]
  
  if (length(extra)) {
    checkmate::assert_names(names(extra), "unique", .var.name = "extra data in `grade`")
    feedback <- c(feedback, extra)
  }

  structure(feedback, class = "gradethis_feedback")
}


is_feedback <- function(x) {
  inherits(x, "gradethis_feedback")
}

# Process the graded message using {commonmark}
# 
# 1. htmltools tags and tagLists are passed through untouched. Authors should
#    not use unescaped user-generated results in graded messages, but at least
#    htmltools escapes text input by default.
# 2. messages marked "AsIs" by I() are collapsed with new lines and then
#    HTML escaped and returned without markdown processing.
# 3. All other messages are processed with commonmark into HTML, stripped of
#    disallowed tags, and then returned as HTML.
message_md <- function(message = NULL) {
  if (is_tag_like(message)) {
    return(message)
  }
  if (is_AsIs(message)) {
    # AsIs messages are collapsed with new lines to match markdown_html()
    return(paste(message, collapse = "\n"))
  }
  if (is.null(message)) {
    return("")
  }
  
  md <- commonmark::markdown_html(
    message,
    smart = TRUE,
    normalize = TRUE,
    extensions = c("tagfilter", "strikethrough", "table", "autolink")
  )
  
  htmltools::HTML(remove_dangerous_html_tags(md))
}

remove_dangerous_html_tags <- function(md) {
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

feedback_grading_problem <- function(message = NULL, type = "error") {
  feedback(
    fail(message %||% "A problem occurred with your teacher's grading code. Defaulting to _incorrect_."),
    type = type
  )
}
