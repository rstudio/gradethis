#' Random praise and encouragement
#'
#' Generate a random praise or encouragement phrase. These functions are
#' designed for use within [pass()] or [fail()] messages, or anywhere else that
#' \pkg{gradethis} provides feedback to the student.
#'
#' @examples
#' replicate(5, glue::glue("Random praise: {random_praise()}"))
#' replicate(5, glue::glue("Random encouragement: {random_encouragement()}"))
#'
#' # give_praise() adds praise to passing grade messages
#' give_praise(pass("That's absolutely correct."))
#'
#' # give_encouragement() encouragement to failing grade messages
#' give_encouragement(fail("Sorry, but no."))
#' @return
#'   - `random_praise()` and `random_encouragement()` each return a length-one
#'     string with a praising or encouraging phrase.
#'   - `give_praise()` and `give_encouragement()` add praise or encouragement
#'     phrases to passing and failing grades, respectively.
#'
#' @name praise
NULL

#' @describeIn praise Random praising phrase
#' @export
random_praise <- function() {
  if (!isTRUE(getOption("gradethis.__praise", TRUE))) return("")
  learnr::random_praise()
}

#' @describeIn praise Random encouraging phrase
#' @export
random_encouragement <- function() {
  if (!isTRUE(getOption("gradethis.__encouragement", TRUE))) return("")
  learnr::random_encouragement()
}

with_praise <- function(value, expr) {
  with_options(
    list("gradethis.__praise" = isTRUE(value)),
    expr
  )
}

with_encouragement <- function(value, expr) {
  with_options(
    list("gradethis.__encouragement" = isTRUE(value)),
    expr
  )
}

#' @describeIn praise Add praising message to a passing grade.
#'
#' @param expr A `graded()` grade or helper function, or a grading function —
#'   like [grade_this()] or [grade_result()] — or a character string. Praise
#'   will be added to any passing grades and encouragement will be added to any
#'   failing grade. If `expr` is a character string, then `"{random_praise()}"`
#'   or `"{random_encouragement()}"` is pasted before or after the string
#'   according to `location`.
#' @param location Should the praise or encouragement be added before or after
#'   the grade message?
#' @param before,after Text to be added before or after the praise or
#'   encouragement phrase.
#' @param ... Ignored.
#'
#' @export
give_praise <- function(
  expr,
  ...,
  location = "before",
  before = NULL,
  after = NULL
) {
  ellipsis::check_dots_empty()
  placement <- tryCatch(
    set_placement(location, before, after),
    gradethis_location_error = function(e) {
      rlang::abort("`give_praise()` expects `location` of 'before' or 'after'")
    }
  )

  # evaluate expression in gradethis context to catch any grades
  # and also turn off  so that it isn't repeated twice
  env <- parent.frame()
  expr_q <- rlang::get_expr(rlang::enquo(expr))
  res <- with_praise(
    FALSE,
    eval_gradethis(rlang::eval_bare(expr_q, env))
  )

  # then dispatch on input type internally
  give_random_phrase(
    x = res,
    location = placement$location,
    before = placement$before,
    after = placement$after,
    type = "praise"
  )
}

#' @describeIn praise Add encouraging message to a failing grade.
#'
#' @export
give_encouragement <- function(
  expr,
  ...,
  location = "after",
  before = NULL,
  after = NULL
) {
  ellipsis::check_dots_empty()
  placement <- tryCatch(
    set_placement(location, before, after),
    gradethis_location_error = function(e) {
      rlang::abort("`give_encouragement()` expects `location` of 'before' or 'after'")
    }
  )

  # evaluate expression in gradethis context to catch any grades
  # and also turn off encouragement so that it isn't repeated twice
  env <- parent.frame()
  expr_q <- rlang::get_expr(rlang::enquo(expr))
  res <- with_praise(
    FALSE,
    eval_gradethis(rlang::eval_bare(expr_q, env))
  )

  # then dispatch on input type internally
  give_random_phrase(
    x = res,
    location = placement$location,
    before = placement$before,
    after = placement$after,
    type = "encouragement"
  )
}

set_placement <- function(location = c("after", "before"), before = NULL, after = NULL) {
  location <- tryCatch(
    match.arg(location),
    error = function(e) {
      rlang::abort(location, class = "gradethis_location_error")
    }
  )
  if (!is.null(before) && !is.null(after)) {
    return(list(location = location, before = before, after = after))
  }

  if (is.null(before) && is.null(after)) {
    before <- if (location == "after") " " else ""
    after <- if (location == "before") " " else ""
  }

  list(
    location = location,
    before = before,
    after = after
  )
}

give_random_phrase <- function(
  x,
  ...,
  location = c("after", "before"),
  type = c("praise", "encouragement"),
  before = NULL,
  after = NULL
) {
  UseMethod("give_random_phrase", x)
}

#' @export
give_random_phrase.character <- function(
  x,
  ...,
  location = "after",
  type = c("praise", "encouragement"),
  before = NULL,
  after = NULL
) {
  # This just inlines praise/encouragement
  txt <- switch(
    match.arg(type),
    praise = "{random_praise()}",
    encouragement = "{random_encouragement()}"
  )
  txt <- paste0(before, txt, after)
  add_before <- identical(location, "before")
  paste0(if (add_before) txt, x, if (!add_before) txt)
}

#' @export
give_random_phrase.function <- function(
  x,
  ...,
  location = "after",
  type = c("praise", "encouragement"),
  before = NULL,
  after = NULL
) {
  type <- match.arg(type)

  function(check_env) {
    # get original grade without praise/encouragement
    grade <- capture_graded(
      switch(
        type,
        praise = with_praise(FALSE, x(check_env)),
        encouragement = with_encouragement(FALSE, x(check_env))
      )
    )

    give_random_phrase(
      x = grade,
      location = location,
      type = type,
      before = before,
      after = after
    )
  }
}

#' @export
give_random_phrase.gradethis_graded <- function(
  x,
  ...,
  location = c("after", "before"),
  type = c("praise", "encouragement"),
  before = NULL,
  after = NULL
) {
  type <- match.arg(type)
  location <- match.arg(location)
  grade <- x

  phrase <-
    switch(
      type,
      praise = {
        if (!identical(grade$correct, TRUE)) {
          signal_grade(grade)
        }
        with_praise(TRUE, random_praise())
      },
      encouragement = {
        if (!identical(grade$correct, FALSE)) {
          signal_grade(grade)
        }
        with_encouragement(TRUE, random_encouragement())
      },
      signal_grade(grade)
    )

  phrase <- paste0(before, phrase, after)
  add_before <- identical(location, "before")

  grade$message <- paste0(
    if (add_before) phrase,
    grade$message,
    if (!add_before) phrase
  )

  signal_grade(grade)
}

#' @export
give_random_phrase.NULL <- function(x, ...) {
  invisible(NULL)
}

#' @export
give_random_phrase.default <- function(x, ...) {
  stop(
    "give_random_phrase() expected a character, function, or grade.",
    call. = FALSE
  )
}
