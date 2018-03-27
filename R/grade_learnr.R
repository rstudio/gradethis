grade_learnr <- function(label = NULL, 
                         solution_code = NULL,
                         user_code = NULL, 
                         check_code = NULL, 
                         envir_result = NULL, 
                         evaluate_result = NULL,
                         ...) {
  
  # Run checking code to get feedback
  grading_code <- parse(text = check_code)[[1]]
  grading_code$user <- rlang::as_quosure(parse(text = user_code)[[1]])
  grading_code$solution <- rlang::as_quosure(parse(text = solution_code)[[1]])
  
  feedback <- eval(grading_code)
  
  # Check that the student submission was correct
  if(feedback == grading_code$success) {
    
    result <- list(message = feedback,
                   correct = TRUE,
                   type = "success",
                   location = "append")
  } else {
    result <- list(message = feedback,
                   correct = FALSE,
                   type = "error",
                   location = "append")
  }
  result
}

