solutions_prepare <- function(code) {
  code <- code_standardize_string(code, scalar = FALSE)
  
  if (is.null(code)) {
    return(NULL)
  }
  
  headers <- solutions_detect_headers(code)
  if (is_null(headers)) {
    return(list(code))
  }
  
  idx_code_splits <- c(1, headers, length(code) + 1L)
  names(idx_code_splits) <- c("solution00", names(headers), "__ignored__")
  
  if (all(idx_code_splits[1:2] == 1)) {
    idx_code_splits <- idx_code_splits[-1]
  }
  
  ret <- list()
  for (i in seq_along(idx_code_splits)[-1]) {
    split_name <- names(idx_code_splits)[i - 1L]
    line_start <- idx_code_splits[i - 1L]
    line_end <- idx_code_splits[i] - 1L

    split_code <- code[seq(line_start, line_end)]
    split_code <- paste(split_code, collapse = "\n")
    split_code <- r_format_code(split_code)
    
    if (nzchar(split_code)) {
      ret[split_name] <- split_code
    }
  }
  
  gradethis_solutions(.list = ret)
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

solutions_detect_headers <- function(code) {
  # lines with only a comment and ending in 4+ dashes
  idx <- grep("^\\s*#.+----+\\s*$", code)
  if (!length(idx)) {
    return(NULL)
  }
  
  names(idx) <- sprintf("solution%02d", seq_len(length(idx)))
  for (i in seq_along(idx)) {
    idx_name <- code[idx[i]]
    idx_name <- sub("^\\s*#([ -]+)?", "", idx_name)
    idx_name <- sub("-+\\s*$", "", idx_name)
    idx_name <- trimws(idx_name)
    if (nzchar(idx_name)) {
      names(idx)[i] <- idx_name
    }
  }
  
  idx
}

r_format_code <- function(code, name = "solution") {
  tryCatch({
    x <- lapply(rlang::parse_exprs(code), rlang::expr_text)
    paste(unlist(x), collapse = "\n")
  }, error = function(err) {
    msg <- glue::glue("Unable to parse {name} code")
    grade_grading_problem(message = msg, error = err)
    rlang::abort(msg, parent = err)
  })
}

