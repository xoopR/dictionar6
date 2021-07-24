string_as_set <- function(str) {
    if (!is.null(str)) {
        paste0("{", paste0(str, collapse = ", "), "}")
    }
}

is_r6 <- function(x) {
    inherits(x, "R6")
}

assert_named_list <- function(x, types = NULL) {
    if (!is.list(x)) {
      stop("'x' must be a list")
    } else if (!length(names(x))) {
      stop("'x' must be named")
    } else if (any(duplicated(names(x)))) {
      stop("names of 'x' must be unique")
    }

    if (!is.null(types)) {
      if (!all(vapply(x, inherits, logical(1), what = types))) {
        stop(sprintf(
          "All elements of 'x' should inherit from %s",
          string_as_set(types)
        ))
      }
    }

  invisible(x)
}


#' @export
as.character.R6 <- function(x, ...) {
  class(x)[[1]]
}
