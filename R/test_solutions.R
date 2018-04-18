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
test_solutions <- function(file,
                           show.answers = FALSE,
                           .params = NULL) {
  
  safe_test <- purrr::safely(purrr::quietly(eval))
  recursive_test <- function(label, env = parent.frame()) {
    label_root <- sub("-solution", "", label)
    if (grepl("-solution", label) && !(label_root %in% names(chunks))) {
      stop(paste(label, "not associated with an exercise chunk."), call. = FALSE)
    }

    # Does the chunk require a setup chunk?
    setup_option <- attr(chunks[[label_root]], "chunk_opts")$exercise.setup
    setup_suffix <- paste0(label_root, "-setup")

    if (!is.null(setup_option)) {
      env <- recursive_test(setup_option)
    } else if (setup_suffix %in% names(chunks)) {
      env <- recursive_test(setup_suffix)
    }

    result <- safe_test(parse(text = chunks[[label]]), envir = env)

    # print pretty
    cat(label, crayon::silver(": "), sep = "")
    if (!is.null(result$error)) {
      cat(
        crayon::red(clisymbols::symbol$cross),
        crayon::red(conditionMessage(result$error)), "\n"
      )
    } else if (length(result$result$warnings)) {
      cat(
        crayon::yellow(clisymbols::symbol$tick),
        crayon::yellow(result$result$warnings), "\n"
      )
      if (show.answers) print(result$result$result)
    } else {
      cat(crayon::green(clisymbols::symbol$tick), "\n")
      if (show.answers) print(result$result$result)
    }

    env
  }


  # Exercises have access to all computations
  # performed at render time
  rmarkdown::render(demo,
    params = .params,
    quiet = TRUE,
    envir = parent.frame()
  )

  chunks <- extract_chunks(file)
  labels <- names(chunks)
  solutions <- grep("-solution$", labels, value = TRUE)

  for (solution in solutions) {
    recursive_test(solution, env = parent.frame())
  }
}
