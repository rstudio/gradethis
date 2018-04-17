# get all of the chunks with
extract_chunks <- function(file) {
  knitr::pat_md()
  knitr::render_markdown()
  on.exit({
    knitr::knit_patterns$restore()
    knitr::knit_hooks$restore()
    knitr:::knit_code$restore()
  }, add = TRUE)
  knitr:::split_file(readLines(file, encoding = "UTF-8"))
  knitr:::knit_code$get()
}

#' Test Solutions
#'
#' @param file The filepath to a learnr tutorial
#'
#' @export
test_solutions <- function(file, show.answers = FALSE) {
  test_solution <- function(solution, label) {
    exercise_label <- sub("-solution$", "", label)
    if (!(exercise_label %in% names(chunks))) {
      stop(paste(label, "not associated with an exercise chunk."), call. = FALSE)
    }

    # Does the solution rely on a setup chunk?
    # If so, you will need to evaluate the setup
    # chunk before the solution
    exercise <- chunks[[exercise_label]]
    exercise_setup <- attr(exercise, "chunk_opts")$exercise.setup
    setup_label <- paste0(exercise_label, "-setup")

    if (!purrr::is_null(exercise_setup)) {
      eval(parse(text = chunks[[exercise_setup]]))
    } else if (setup_label %in% names(chunks)) {
      eval(parse(text = chunks[[setup_label]]))
    }

    eval(parse(text = solution))
  }

  chunks <- extract_chunks(file)

  # If there is a global setup chunk you will 
  # want to run it once before running each 
  # solution in a child environment
  if ("setup" %in% names(chunks)) {
    eval(parse(text = unlist(chunks[["setup"]])))
  }

  solutions <- chunks[grep("-solution$", names(chunks))]
  safe_test <- purrr::safely(quietly(test_solution), otherwise = NULL)
  results <- purrr::imap(solutions, safe_test)
  
  if (show.answers) {
    structure(make_pretty_full(results), class = "solutions_test_full")
  } else {
    structure(make_pretty(results), class = "solutions_test")
  }
}

make_pretty <- function(res) {
  final <- purrr::imap(res, ~ {
    if (is.null(.x$error)) {
      if (length(.x$result$warnings)) {
        structure(.x$result$warnings, class = "solution_warning")
      } else {
        .x$result$result
      }
    } else {
      .x$error
    }
  })
  names(final) <- sub("-solution$", "", names(final))
  final
}

make_pretty_full <- function(res) {
  final <- purrr::imap(res, ~ {
    if (is.null(.x$error)) {
      if (length(.x$result$warnings)) {
        structure(list(result = .x$result$result, warning = .x$result$warnings), class = "solution_warning")
      } else {
        .x$result$result
      }
    } else {
      .x$error
    }
  })
  names(final) <- sub("-solution$", "", names(final))
  final
}

#' Format solutions test output
#'
#' @param x 
#' @param ... 
#'
#' @export
format.solutions_test <- function(x, ...) {
  labels <- names(x)
  for (i in seq_along(x)) {
    cat(labels[i], crayon::silver(": "), sep = "")
    if (inherits(x[[i]], "error")) {
      cat(crayon::red(clisymbols::symbol$cross), crayon::red(conditionMessage(x[[i]])), "\n")
    } else if (inherits(x[[i]], "solution_warning")) {
      cat(crayon::yellow(clisymbols::symbol$tick), crayon::yellow(x[[i]]), "\n")
    } else {
      cat(crayon::green(clisymbols::symbol$tick), "\n")
    }
  }
}

#' Format solutions test output
#'
#' @param x 
#' @param ... 
#'
#' @export
format.solutions_test_full <- function(x, ...) {
  labels <- names(x)
  for (i in seq_along(x)) {
    cat(labels[i], crayon::silver(": "), sep = "")
    if (inherits(x[[i]], "error")) {
      cat(crayon::red(clisymbols::symbol$cross), 
          crayon::red(conditionMessage(x[[i]])), "\n")
    } else if (inherits(x[[i]], "solution_warning")) {
      cat(crayon::yellow(clisymbols::symbol$tick), 
          crayon::yellow(x[[i]]$warning), "\n")
      print(x[[i]]$result)
    } else {
      cat(crayon::green(clisymbols::symbol$tick), "\n") 
      print(x[[i]])
    }
  }
}

#' Print method for solutions test
#'
#' @param x 
#' @param ... 
#'
#' @export
print.solutions_test <- function(x, ...) {
  format.solutions_test(x)
}

#' Print method for solutions test
#'
#' @param x 
#' @param ... 
#'
#' @export
print.solutions_test_full <- function(x, ...) {
  format.solutions_test_full(x)
}
  

  
  
  