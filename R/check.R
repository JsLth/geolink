check_string <- function(x, null = FALSE) {
  if (is.null(x) && isTRUE(null)) return(invisible())

  cond <- is.character(x) && length(x) == 1
  if (!cond) {
    name <- deparse(substitute(x))
    cli::cli_abort("Argument `{name}` must be a single string.")
  }
}


check_function <- function(x, null = FALSE) {
  if (is.null(x) && isTRUE(null)) return(invisible())

  cond <- is.function(x)
  if (!cond) {
    name <- deparse(substitute(x))
    cli::cli_abort("Argument `{name}` must be a function.")
  }
}


check_args <- function(x, arg_names = NULL, arg_n = NULL, null = FALSE) {
  if (is.null(x) && isTRUE(null)) return(invisible())

  name <- deparse(substitute(x))
  args <- if (!is.null(x)) formals(x) else list()

  if (!is.null(arg_n)) {
    cond <- length(args) == arg_n
    if (!cond) {
      cli::cli_abort("Function `{name}` must have exactly {arg_n} formal argument{?s}.")
    }
  }

  if (!is.null(arg_names)) {
    cond <- arg_names %in% names(args)
    if (!cond) {
      cli::cli_abort("Function `{name}` must have the following formal arguments: {.var {arg_names}}")
    }
  }
}


check_installed <- function(pkg, purpose) {
  pkg_raw <- pkg
  if (grepl("/", pkg, fixed = TRUE)) {
    pkg_raw <- strsplit(pkg, "/")[[1]][2]
  }

  if (!is_installed(pkg_raw)) {
    cli::cli_abort(c(
      "Package {pkg_raw} is needed {purpose}.",
      "i" = "You can install it by running `pak::pkg_install(\"{pkg}\")`."
    ))
  }
}
