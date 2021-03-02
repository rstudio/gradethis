
with_gradethis_setup <- function(expr, ...) {
  old_opts <- gradethis_setup(...)
  on.exit(options(old_opts), add = TRUE)
  force(expr)
}

with_seed <- function(seed, expr) {
  set.seed(seed)
  expr
}
