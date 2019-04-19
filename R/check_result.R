
# TODO document
#' @export
check_result <- function(
  results,
  correct = "{paste0(random_praise(), if (!is.null(message)) paste0(\" \", message))}",
  incorrect = "{paste0(if(!is.null(message)) paste0(message, \" \"), random_encourage())}",
  # TODO check for empty answer
  empty_msg = "I did not notice a result. Does your code return one?",
  user = NULL, # provided by `grade_learnr`
  solution = NULL,
  ... # ignored / extra params
) {
  chkm8_class(results, "grader_results")
  chkm8_single_character(correct)
  chkm8_single_character(incorrect)
  chkm8_single_character(empty_msg)

  user_val <- get_user_code(user)
  for (resu in results) {
    if (identical(resu$x, user_val)) {
      message <- glue::glue_data(
        list(
          message = resu$message
        ),
        ifelse(resu$correct, correct, incorrect)
      )

      # print("user_val == ans$x")
      return(result(
        x = resu$x,
        message = message,
        correct = resu$correct
      ))
    }
  }
  # print("not found")
  return(result(
    x = structure("answer not found", class = c("exercise_answer_not_found", "character")),
    message = glue::glue_data(
      list(
        message = NULL
      ),
      incorrect
    ),
    correct = FALSE
  ))
}
