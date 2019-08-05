#' @param grader_args A list of parameters passed to \code{grader} functions (provided by \code{grade_learnr}).
#'   This contains: \describe{
#'    \item{\code{user_quo}}{Quoted R code submitted by the user.  Ex: \code{rlang::\link[rlang]{quo}(1)} }
#'    \item{\code{solution_quo}}{[Optional] Quoted solution R code provided by the \code{*-solution} chunk for an exercise.}
#'   }
