# yoink <- function(pkg, fn) {
#   do.call("getFromNamespace", list(fn, pkg))
# }

# get all of the chunks with
# extract_chunks <- function(file) {
#   knitr_knit_code <- yoink("knitr", "knit_code")
#   knitr_split_file <- yoink("knitr", "split_file")
#   knitr::pat_md()
#   knitr::render_markdown()
#   on.exit({
#     knitr::knit_patterns$restore()
#     knitr::knit_hooks$restore()
#     knitr_knit_code$restore()
#   }, add = TRUE) # nolint
#   knitr_split_file(readLines(file, encoding = "UTF-8"))
#   knitr_knit_code$get()
# }

#' Test Solutions
#'
#' \code{test_solutions} checks that the solution and setup code provided in a
#' learnr tutorial runs without errors or warnings. When checking code chunks,
#'
#' \code{test_solutions} applies the same scoping rules that are applied within
#' the tutorial: every solution chunk has access to the results of evaluated
#' knitr chunks that are not associated with learnr exercises. If a solution is
#' associated with an exercise that has a setup chunk, the setup chunk is
#' evaluated (and tested) before the solution chunk.
#'
#' @param file The filepath to a learnr tutorial. If a file path is not provided
#'   and only a single .Rmd file exists in the current directory,
#'   \code{test_solutions} will test that .Rmd file.
#' @param show_answers TRUE or FALSE. Should solution results be printed in the
#'   output?
# ' @param .params A list of parameters to use when evauating code in a
# '   parameterized R Markdown document. This should be identical to the list
# '   that you would use to render the document.
# '
#' @return \code{test_solutions} does not return a value; it prints an
#'   informative summary of the testing results. Each solution and setup block
#'   is listed by name alongside a result status. Within the RStudio IDE, chunks
#'   that run without an error or warning are printed with a green checkmark.
#'   Chunks that run but return a warning message are printed with a yellow
#'   checkmark and the warning message. Chunks that produce an error are printed
#'   with a red x and the error message. Chunks that contain an error related to
#'   how learnr processes the chunk contain are printed with a purple error
#'   symbol followed by a message.
#'
#' @export
# test_solutions <- function(file = NULL,
#                            show_answers = FALSE
#                            ) {

#   if (is.null(file)) {
#     files <- dir()
#     rmds <- files[grepl("(.Rmd|.rmd)$", files)]
#     if (length(rmds) == 0) {
#       stop("No .Rmd file found in the current directory. Please provide a file path.",
#            call. = FALSE)
#     } else if (length(rmds) > 1) {
#       stop("Multiple .Rmd files found in the current directory. Please provide a file path.",
#            call. = FALSE)
#     } else {
#       file <- rmds
#     }
#   }

#   # the functions that test the solutions are defined here so they can find the
#   # results of the global setup chunk through lexical scoping
#   safe_eval <- purrr::safely(purrr::quietly(eval))

#   safe_test <- function(label, chunks) {
#     safe_eval(parse(text = chunks[[label]]), envir = parent.frame(1))
#   }

#   test_solution <- function(label, chunks, show_answer = FALSE) {
#     exercise <- sub("-solution", "", label)
#     if (grepl("-solution$", label) && !(exercise %in% names(chunks))) {
#       stop(paste(label, "not associated with an exercise chunk."), call. = FALSE)
#     }

#     # Does the chunk require a setup chunk?
#     setup_option <- attr(chunks[[exercise]], "chunk_opts")$exercise.setup
#     setup_suffix <- paste0(exercise, "-setup")

#     if (!is.null(setup_option)) {
#       if (!is.character(setup_option)) {
#         print_author_error(label, "exercise.setup chunk_opt is not a single string.")
#         return()
#       } else if (!(setup_option %in% names(chunks))) {
#         print_author_error(label, "exercise.setup chunk_opt points to unknown chunk.")
#         return()
#       } else if (setup_suffix %in% names(chunks)) {
#         print_author_error(label, "More than one setup chunk is assigned to exercise.")
#         return()
#       } else {
#         setup <- safe_test(setup_option, chunks = chunks)
#         print_result(setup_option,
#                      setup,
#                      show_answers = show_answer)
#       }
#     } else if (setup_suffix %in% names(chunks)) {
#       setup <- safe_test(setup_suffix, chunks = chunks)
#       print_result(setup_suffix,
#                    setup,
#                    show_answers = show_answer)
#     }

#     result <- safe_test(label, chunks = chunks)
#     print_result(label,
#                  result,
#                  show_answers = show_answer)
#   }

#   chunks <- extract_chunks(file)
#   labels <- names(chunks)

#   # Exercises have access to the computations
#   # performed in the setup chunk
#   if ("setup" %in% labels) {
#     setup_result <- safe_test("setup", chunks = chunks)
#     print_result("setup",
#                  setup_result,
#                  show_answers = show_answers)
#   }

#   solutions <- grep("-solution$", labels, value = TRUE)

#   purrr::walk(solutions,
#               test_solution,
#               chunks = chunks,
#               show_answer = show_answers)
# }

# print_result <- function(label, result, show_answers = FALSE) {
#   cat(label, crayon::silver(": "), sep = "")
#   if (!is.null(result$error)) {
#     cat(
#       crayon::red(clisymbols::symbol$cross),
#       crayon::red(conditionMessage(result$error)), "\n"
#     )
#   } else if (length(result$result$warnings)) {
#     cat(
#       crayon::yellow(clisymbols::symbol$tick),
#       crayon::yellow(result$result$warnings), "\n"
#     )
#     if (show_answers) print(result$result$result)
#   } else {
#     cat(crayon::green(clisymbols::symbol$tick), "\n")
#     if (show_answers) print(result$result$result)
#   }
# }

# print_author_error <- function(label, message){
#   purple <- crayon::make_style("purple")
#   cat(label, crayon::silver(": "), sep = "")
#   cat(
#     purple(clisymbols::symbol$warning),
#     purple(message), "\n"
#   )
# }
