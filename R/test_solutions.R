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
  
  safe_eval <- purrr::safely(purrr::quietly(eval))
  
  safe_test <- function(label, envir) {
    safe_eval(parse(text = chunks[[label]]), envir = parent.frame(2))
  }
  
  test_solution <- function(label) {
    exercise <- sub("-solution", "", label)
    if (grepl("-solution$", label) && !(exercise %in% names(chunks))) {
      stop(paste(label, "not associated with an exercise chunk."), call. = FALSE)
    }

    code <- chunks[[label]]
    # Does the chunk require a setup chunk?
    setup_option <- attr(chunks[[exercise]], "chunk_opts")$exercise.setup
    setup_suffix <- paste0(exercise, "-setup")

    if (!is.null(setup_option)) {
      setup <- safe_test(setup_option)
      print_result(setup_option, 
                   setup, 
                   show.answers = show.answers)
    } else if (setup_suffix %in% names(chunks)) {
      setup <- safe_test(setup_suffix)
      print_result(setup_suffix, 
                   setup,
                   show.answers = show.answers)
    }

    result <- safe_test(label)
    print_result(label, 
                 result,
                 show.answers = show.answers)
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
    test_solution(solution)
  }
}

print_result <- function(label, result, show.answers = FALSE) {    
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
}

