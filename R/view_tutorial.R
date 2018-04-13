#' View tutorial
#'
#' \code{view_tutorial()} opens a tutorial in the RStudio viewer window. You can
#' place it in a .Rprofile file to cause a project to open with a tutorial
#' visible. This makes a convenient way to guide students through sophisticated
#' case studies, or to transition students from the learnr environment to the
#' IDE. However, it only works for teaching purposes if students can still use
#' the IDE command line. How to open the tutorial in a second R process?
#'
#' @param name A character string. The name of a tutorial saved in a package.
#' @param package A character string. The name of the package that contains a tutorial.
#'
#' @export
view_tutorial <- function(name, package) {
  learnr::run_tutorial(
    name = name,
    package = package,
    shiny_args = list(
      launch.browser = rstudioapi::viewer
    )
  )
}

#' Add a tutorial to a project
#'
#' \code{add_tutorial()} inserts a call to \link[grader]{\code{view_tutorial()}}
#' into the .Rprofile file contained in the current working directory. As a
#' result, R will launch the tutorial in the RStudio IDE viewer pane whenever
#' the current project is opened or re-opened. 
#'
#' If an .Rprofile file does not exist in the current working directory,
#' \code{add_tutorial()} will create one. If the .Rprofile file in the current
#' working directory already contains a call to \code{view_tutorial()},
#' \code{add_tutorial()} will return an informative error message. Note that a
#' user may also have .Rprofile files in both their home directory and their R
#' home directory. If one of these files contains a call to
#' \code{view_tutorial()}, \code{add_tutorial} will override the call without a
#' warning message.
#' 
#' @seealso \link[grader]{\code{view_tutorial}}
#'
#' @param name A character string. The name of a tutorial saved in a package.
#' @param package A character string. The name of the package that contains a tutorial.
#'
#' @export
add_tutorial <- function(name, package) {
  rprofile <- paste0(getwd(), "/.Rprofile")

  # Check that the .Rprofile does not already load a tutorial
  if (file.exists(rprofile)) {
    text <- readr::read_file(rprofile)
    if (stringr::str_detect(text, "view_tutorial\\(")) {
      stop("This project already loads a tutorial upon opening. Please remove the tutorial before proceeding by opening the .Rprofile file in the project's working directory and removing the call to view_tutorial.")
    }
  }
  cat(glue::glue("view_tutorial(name = {name}, package = {package})"),
    file = rprofile,
    sep = "\n",
    append = TRUE
  )
}

