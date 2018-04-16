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
#' @return
#' @export
test_solutions <- function(file) {

  # test solution
  test_solution <- function(solution, label) {

    # Does the solution rely on a setup chunk?
    exercise_label <- sub("-solution$", "", label)
    exercise <- chunks[[exercise_label]]
    exercise_setup <- attr(exercise, "chunk_opts")$exercise.setup

    setup_label <- paste0(exercise_label, "-setup")
    setup_chunk <- chunks[[setup_label]]

    # If so, evaluate first
    if (!is_null(exercise_setup)) {
      eval(parse(text = chunks[[exercise_setup]]))
    } else if (!is.null(setup_chunk)) {
      eval(parse(text = setup_chunk))
    }

    # Evaluate solution
    eval(parse(text = solution))
  }

  # Run the global setup chunk and then the solutions
  chunks <- extract_chunks(file)

  if ("setup" %in% names(chunks)) {
    eval(parse(text = unlist(chunks[["setup"]])))
  }

  # test solutions
  solutions <- chunks[grep("-solution$", names(chunks))]
  imap(solutions, test_solution)
}
