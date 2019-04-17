
#' @export
#' @importFrom rlang get_expr
check_result <- function(
  results,
  correct = NULL,
  incorrect = NULL,
  strict = correct,
  empty_msg = "I did not notice a result. Does your code return one?",
  solution = NULL, user = NULL # provided by `grade_learnr`
) {

  same_info <- code_is_same(user = user, solution = solution)
  if (same_info$correct) {
    return(result(
      x = solution,
      message = strict,
      correct = TRUE
    ))
  }

  if (is.null(solution)) {
    stop("'solution' not provided")
  }
  user_val <- rlang::eval_tidy(user)
  solution_val <- rlang::eval_tidy(solution)
  if (identical(user_val, solution_val)) {
    print("user_val == solution_val")
    return(result(
      x = solution_val,
      message = correct,
      correct = TRUE
    ))
  }
  for (resu in results) {
    if (!inherits(resu, "grader_result")) {
      stop("All results must be produced by 'grader::result'")
    }
    if (identical(resu$x, user_val)) {
      print("user_val == ans$x")
      return(ans)
    }
  }
  print("not found")
  return(result(
    x = structure("answer not found", class = "exercise_answer_not_found"),
    message = incorrect,
    correct = FALSE
  ))
}
