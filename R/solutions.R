solutions_prepare <- function(code) {
  code <- code_standardize_string(code, scalar = FALSE)

  if (is.null(code)) {
    return(NULL)
  }

  solutions <- solutions_split_headers(code)
  if (rlang::is_character(solutions)) {
    return(list(solutions))
  }

  solutions <- lapply(solutions, r_format_code)

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

solutions_split_headers <- function(code, prefix = "solution") {
  code <- paste(code, collapse = "\n")
  code <- trimws(code)
  code <- strsplit(code, "\n")[[1]]

  rgx_header <- "^\\s*#+[ -]*(.+?)\\s*----+$"
  headers <- regmatches(code, regexec(rgx_header, code))
  lines_headers <- which(vapply(headers, length, integer(1)) > 0)

  if (length(lines_headers) > 0 && max(lines_headers) == length(code)) {
    # nothing after last heading
    lines_headers <- lines_headers[-length(lines_headers)]
  }

  if (!length(lines_headers)) {
    return(paste(code, collapse = "\n"))
  }

  header_names <- vapply(headers[lines_headers], `[[`, character(1), 2)
  header_names <- trimws(header_names)
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
  sections <- trimws(sections)
  as.list(sections[nzchar(sections)])
}
