#' @export
fail_if <- function(x, message) {
  condi(x, message, correct = FALSE)
}

#' @export
pass_if <- function(x, message) {
  condi(x, message, correct = TRUE)
}

condi <- function(x, message, correct) {
  type <- 
    if (rlang::is_formula(x)) {
      "formula"
    } else if (rlang::is_function(x)) {
      "function"
    } else {
      "value"
    }
  
  ret <- list(
    x = x, 
    message = message,
    correct = correct,
    type = type
  )
  class(ret) <- "grader_condition"
  ret
}


evaluate_condi <- function(condi_x, grader_args, learnr_args) {
  
  checkmate::assert_class(condi_x, "grader_condition")
  
  switch(condi_x$type,
         "formula" = evaluate_condi_formula,
         "function" = evalutate_condi_function,
         "value" = evaluate_condi_value
         )
}


tryCatch( {
  ret <- fn(.last_value)
  if (!inherits(ret, "grader_graded")) {
    if (ret) {
      graded(TRUE, correct)
    } else {
      graded(FALSE< incorrect)
    }
  } else {
    ret
  }
}, error = function(e) {
  message <- ... error = as.character(e)
  graded(FALSE, message)
})


pass_if(checkmate::assert_numeric)