solutions_prepare <- function(code) {
  if (inherits(code, "gradethis_solutions")) {
    return(code)
  }

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

  if (!rlang::is_named(x)) {
    names(x) <- sprintf("solution%02d", seq_along(x))
  }

  for (name in names(x)) {
    header <- name
    width_remaining <- min(getOption("width", 80), 80) - nchar(header) - 3
    header <- paste("#", header, strrep("-", width_remaining))
    ret <- c(ret, header, "", x[[name]], "")
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

split_code_headers <- function(code, prefix = "section") {
  if (is.null(code)) {
    return(NULL)
  }

  code <- paste(code, collapse = "\n")
  code <- str_trim(code, character = "[\r\n]")
  code <- strsplit(code, "\n")[[1]]

  rgx_header <- "^(#+)([ -]*)(.+?)?\\s*----+\\s*$"
  headers <- regmatches(code, regexec(rgx_header, code, perl = TRUE))
  lines_headers <- which(vapply(headers, length, integer(1)) > 0)

  if (length(lines_headers) > 0 && max(lines_headers) == length(code)) {
    # nothing after last heading
    lines_headers <- lines_headers[-length(lines_headers)]
  }

  if (!length(lines_headers)) {
    return(list(paste(code, collapse = "\n")))
  }

  # header names are 3rd group, so 4th place in match since 1st is the whole match
  header_names <- vapply(headers[lines_headers], `[[`, character(1), 4)
  header_names <- str_trim(header_names)
  if (any(!nzchar(header_names))) {
    header_names[!nzchar(header_names)] <- sprintf(
      paste0(prefix, "%02d"),
      which(!nzchar(header_names))
    )
  }

  rgx_header_line <- gsub("[$^]", "(^|\n|$)", rgx_header)
  sections <- strsplit(paste(code, collapse = "\n"), rgx_header_line, perl = TRUE)[[1]]
  if (length(sections) > length(header_names)) {
    header_names <- c(paste0(prefix, "00"), header_names)
  }
  names(sections) <- header_names

  # trim leading/trailing new lines from code section
  sections <- str_trim(sections, character = "[\r\n]")
  # drop any sections that don't have anything in them
  sections <- sections[nzchar(str_trim(sections))]

  as.list(sections)
}
