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
#' TODO need to export when feature is implement in RStudio IDE
#' @noRd
#' @keywords internal
#' @importFrom utils browseURL
view_tutorial <- function(name, package) {
  # launch in separate R session
  r2 <- callr::r_bg(function(name, package) {
    learnr_run_tutorial <- eval(call(":::", "learnr", "run_tutorial")) # removes learnr depedency
    learnr_run_tutorial(
      name = name,
      package = package,
      shiny_args = list(
        launch.browser = FALSE,
        port = 8000,
        host = "127.0.0.1")
    )
  },
  supervise = TRUE,
  args = list(name = name, package = package)
  )

  # If you open the viewer before the app loads, it will
  # display a blank screen until you click refresh
  status <- r2$read_error()
  n <- 1
  while (!grepl("Listening", status)) {
    status <- r2$read_error()
    Sys.sleep(0.5)
    n <- n + 1
    if (n == 20) {
      print("Click refresh in the Viewer pane if you do not see a tutorial.")
      break
    }
  }

  if (rstudioapi::isAvailable()) {
    rstudioapi::viewer(
      url = "http://localhost:8000",
      height = "maximize"
    )
  } else {
    browseURL("http://localhost:8000")
  }
}


#' Add a tutorial to a project
#'
#' Warning: \code{add_tutorial} does not work as written because it assumes that
#' R will recognize that RStudio IDE is running when R evaluates .Rprofile. This
#' is an incorrect assumption. However, \code{add_tutorial} and
#' \code{remove_tutorial} should be easy to adapt to a an RStudio-specific
#' startup file should RStudio implement one.
#'
#' \code{add_tutorial()} inserts a call to \code{\link{view_tutorial}()}
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
#' @seealso \code{\link{view_tutorial}}
#'
#' @param name A character string. The name of a tutorial saved in a package.
#' @param package A character string. The name of the package that contains a
#'   tutorial.
#'
#' TODO need to export when feature is implement in RStudio IDE
#' @noRd
#' @keywords internal
add_tutorial <- function(name, package) {
  rprofile <- paste0(getwd(), "/.Rprofile") # nolint

  # load packages

  # Check that the .Rprofile does not already load a tutorial
  if (file.exists(rprofile)) {
    text <- readr::read_file(rprofile)
    if (grepl("view_tutorial(", text, fixed = TRUE)) {
      stop(
        "This project already loads a tutorial upon opening.\n",
        "Please remove the tutorial by manually opening the ",
        ".Rprofile file in the project's working directory and ",
        "removing the view_tutorial call (safer). Or by running ",
        "remove_tutorial (less safe)."
      )
    }
  }
  cat(paste0(
    'gradethis::view_tutorial(name = "',
    name, '", package = "', package,
    '")  ## Learnr tutorial added on ',
    Sys.Date()
  ),
  file = rprofile,
  sep = "\n",
  append = TRUE
  )

  # return message
}

remove_tutorial <- function(dir = NULL) {
  if (!is.null(dir)) dir <- getwd()
  rprofile <- paste0(dir, "/.Rprofile") # nolint

  if (file.exists(rprofile)) {
    text <- readr::read_lines(rprofile)
  } else {
    stop("Directory does not have a .Rprofile ",
         "file to remove tutorial from.")
  }

  tutorial_calls <- grepl("\\)  ## Learnr tutorial added on ", text)
  if (!any(tutorial_calls)) {
    message("No tutorials detected to remove.")
  } else {
    text <- text[!tutorial_calls]
    readr::write_lines(text, path = rprofile)
    message("Tutorial removed.")
  }
}
