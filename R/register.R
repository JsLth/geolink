geolinkers <- new.env(parent = emptyenv())


register_geolinker <- function(name,
                               link,
                               check = NULL,
                               parse = NULL,
                               priority = NULL) {
  check_function(link)
  check_function(check, null = TRUE)
  check_function(parse, null = TRUE)

  check_args(link, arg_names = ".data")
  check_args(check, arg_n = 1, null = TRUE)
  check_args(parse, arg_n = 1, null = TRUE)

  geolinker <- list(link = link, check = check, parse = parse)
  assign(name, geolinker, envir = geolinkers)

  if (!is.null(priority)) {
    order <- get0("order", envir = geolinkers)

    if (identical(priority, "last")) {
      priority <- length(order)
    }

    order <- append(order, name, after = priority)
    assign("order", order, envir = geolinkers)
  }

  invisible(geolinker)
}


unregister_geolinker <- function(name) {
  rm(name, envir = geolinkers)
}


all_geolinkers <- function() {
  ls(envir = geolinkers)
}


priorities <- function() {
  get0("order", envir = geolinkers)
}


get_linker <- function(name, fun = NULL) {
  geolinker <- get(name, envir = geolinkers)

  if (!is.null(fun)) {
    geolinker[[fun]] %||% function(...) list()
  } else {
    geolinker
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
