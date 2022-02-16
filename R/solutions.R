# @staticimports pkg:learnr
#   split_code_headers
#   str_trim

solutions_prepare <- function(code) {
  code <- code_standardize_string(code, scalar = FALSE)

  if (is.null(code)) {
    return(NULL)
  }

  solutions <- split_code_headers(code, prefix = "solution")
  if (length(solutions) == 1 && is_null(names(solutions))) {
    return(solutions)
  }

  gradethis_solutions(.list = solutions)
}

gradethis_solutions <- function(..., .list = list()) {
  ret <- c(.list, list(...))
  structure(ret, class = "gradethis_solutions")
}

#' @export
print.gradethis_solutions <- function(x, ...) {
  if (!rlang::has_length(x)) {
    cat("No solutions")
  } else {
    cat(format(x))
  }
}

#' @export
format.gradethis_solutions <- function(x, ...) {
  ret <- c()
  for (i in seq_along(x)) {
    header <- names(x)[i]
    width_remaining <- min(getOption("width", 80), 80) - nchar(header) - 3
    header <- paste("#", header, strrep("-", width_remaining))
    ret <- c(ret, header, "", x[[i]], "")
  }
  paste(ret, collapse = "\n")
}

code_standardize_string <- function(code, scalar = TRUE) {
  if (is_null(code)) {
    return(NULL)
  }

  code <- paste(code, collapse = "\n")
  if (!nzchar(trimws(code))) {
    return(NULL)
  }

  code <- strsplit(code, "\n")[[1]]
  if (is_true(scalar)) {
    paste(code, collapse = "\n")
  } else {
    code
  }
}
