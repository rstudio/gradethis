#' Functions for interacting with objects created by student code
#'
#' @example man/examples/example-user_object.R
#'
#' @inheritParams utils::ls.str
#' @param x An object name, given as a quoted [character] string.
#' @param envir An [environment] in which to search for objects.
#'   Defaults to [`.envir_result`].
#' @param exclude_envir An [environment].
#'   Objects that appear in both `envir` and `exclude_envir` will be excluded.
#'   Defaults to [`.envir_prep`].
#'   Use `exclude_envir = NULL` to include all objects.
#' @param ... Additional arguments passed to underlying functions:
#'   - For `user_object_exists()`, [exists()]
#'   - For `user_object_get()`, [get()]
#'   - For `user_object_list()`, [`ls.str()`][utils::ls.str]
#' @param gradethis_env The [environment] from which to retrieve
#'   [`.envir_result`] and [`.envir_prep`].
#'   Most users of \pkg{gradethis} will not need to use this argument.
#'
#' @usage NULL
user_object <- function(x, mode, envir, exclude_envir, ..., gradethis_env) NULL

#' @rdname user_object
#' @export
#' @return For `user_object_get()`, the object.
#'   If the object is not found, an error.
user_object_get <- function(
  x, mode = "any", envir = .envir_result, ..., gradethis_env = parent.frame()
) {
  envir <- resolve_placeholder(envir, gradethis_env)
  get(x, envir = envir, mode = mode, ...)
}

#' @rdname user_object
#' @export
#' @return For `user_object_exists()`, a [`TRUE`]/[`FALSE`] value.
user_object_exists <- function(
  x, mode = "any", envir = .envir_result, ..., gradethis_env = parent.frame()
) {
  envir <- resolve_placeholder(envir, gradethis_env)
  exists(x, envir = envir, mode = mode, ...)
}

#' @rdname user_object
#' @export
#' @return For `user_object_list()`, a [character] vector
#'   giving the names of the objects in `envir`.
user_object_list <- function(
  mode = "any",
  envir = .envir_result,
  exclude_envir = .envir_prep,
  ...,
  gradethis_env = parent.frame()
) {
  envir <- resolve_placeholder(envir, gradethis_env)
  exclude_envir <- resolve_placeholder(exclude_envir, gradethis_env)

  objects <- as.character(
    utils::ls.str(envir = envir, mode = mode, ...)
  )

  if (!is.null(exclude_envir)) {
    exclude_objects <- as.character(
      utils::ls.str(envir = exclude_envir, mode = mode, ...)
    )

    objects <- setdiff(objects, exclude_objects)
  }

  objects
}
