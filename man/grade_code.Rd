% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grade_code.R
\name{grade_code}
\alias{grade_code}
\title{Grade student code against a solution (Legacy)}
\usage{
grade_code(
  correct = NULL,
  incorrect = NULL,
  ...,
  allow_partial_matching = getOption("gradethis.allow_partial_matching", TRUE),
  glue_correct = getOption("gradethis.glue_correct"),
  glue_incorrect = getOption("gradethis.glue_incorrect"),
  glue_pipe = deprecated(),
  grader_args = deprecated(),
  learnr_args = deprecated()
)
}
\arguments{
\item{correct}{A character string to display if the student answer matches a
known correct answer.}

\item{incorrect}{A character string to display if the student answer matches
a known incorrect answer.}

\item{...}{ignored. Should be empty}

\item{allow_partial_matching}{A logical. If \code{FALSE}, the partial matching of
argument names is not allowed and e.g. \code{runif(1, mi = 0)} will return a
message indicating that the full formal name \code{min} should be used. The
default is set via the \code{gradethis.allow_partial_matching} option, or by
\code{\link[=gradethis_setup]{gradethis_setup()}}.}

\item{glue_correct}{A glue string that returns the final correct message
displayed. Defaults to \code{getOption("gradethis_glue_correct")}.}

\item{glue_incorrect}{A glue string that returns the final incorrect message
displayed. Defaults to \code{getOption("gradethis_glue_incorrect")}.}

\item{glue_pipe}{A glue string that returns the final message displayed when
the student uses a pipe, \verb{\%>\%}. Defaults to
\code{getOption("gradethis.pipe_warning")}.}

\item{grader_args}{A list of parameters passed to \code{grader} functions
(provided by \code{\link[=gradethis_exercise_checker]{gradethis_exercise_checker()}}). This contains:
\itemize{
\item \code{user_quo}: Quoted R code submitted by the user. For example
\code{\link[rlang:quo]{rlang::quo(1)}}
\item \code{solution_quo}: (Optional) Quoted solution R code provided by the
\verb{*-solution} chunk for an exercise.
}}

\item{learnr_args}{A list of all parameters passed to
\code{\link[=gradethis_exercise_checker]{gradethis_exercise_checker()}} by \code{learnr}. See
\url{https://rstudio.github.io/learnr/exercises.html#exercise_checking} for
more details.}
}
\value{
a function whose first parameter should be an environment that contains
all necessary information to compare the code.  The result of the returned
function will be a \code{\link[=graded]{graded()}} object. An incorrect message will describe the
first way that the answer differs, the message will be the content of the \code{glue_pipe}
argument.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#superseded}{\figure{lifecycle-superseded.svg}{options: alt='[Superseded]'}}}{\strong{[Superseded]}} Please use \code{\link[=grade_this_code]{grade_this_code()}}.
}
\details{
Checks the code expression or the code result against a solution.

\code{grade_code()} compares student code to a solution (i.e. model code) and
describes the first way that the student code differs. If the student code
exactly matches the solution, \code{grade_code()} returns a customizable success
message (\code{correct}). If the student code does not match the solution, a
customizable incorrect message (\code{incorrect}) can also be provided.

\code{grade_code()} provides a \emph{strict} check in that the student code must
exactly match the solution. It is not enough for the student code to be
equivalent to the solution code (e.g. to return the same result as the
solution).

You can provide solution code for \code{grade_code()} to use in two ways:
\enumerate{
\item Pass code as a character string or a quoted expression to the solution
argument of \code{grade_code()}
\item Make a "-solution" code chunk for the exercise to be checked in a learnr
document. There is no need to supply a solution argument for \code{grade_code()}
if you call it from the "-check" chunk of the same exercise. Likewise, there
is no need to supply a student submitted code argument when you call
\code{grade_code()} from a learnr document (learnr will provide the code that the
student submits when it runs \code{grade_code()}.
}

For best results, name all arguments provided in the solution code.
}
\examples{
\dontrun{
gradethis_demo()
}

# This is a manual example, see grading demo for `learnr` tutorial usage
y <- expression(sqrt(log(2)))
z <- expression(sqrt(log(1)))
grade_code(grader_args = list(user_quo = y, solution_quo = z))
}
\seealso{
\code{\link[=grade_this_code]{grade_this_code()}}, \code{\link[=grade_code]{grade_code()}}
}
\keyword{internal}
