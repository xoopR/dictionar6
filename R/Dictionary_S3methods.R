#' @title Extract an element from a Dictionary
#' @description This is simply a wrapper around `[Dictionary]$get_list(i)`
#' @param object ([Dictionary])
#' @param i (`character()`) \cr
#' Keys of items to get.
#' @export
`[.Dictionary` <- function(object, i) {
  object$get_list(i)
}

#' @title Set an element in a Dictionary
#' @description This is simply a wrapper around `[Dictionary]$add(i, value)`
#' @details Cannot be used to re-value an item.
#' @param object ([Dictionary])
#' @param i (`character()`) \cr
#' Keys of items to add.
#' @param value (`character()`) \cr
#' Values of items to add.
#' @export
`[.Dictionary<-` <- function(object, i, value) {
  object$add(keys = i, values = value)
}

#' @title Get length of a Dictionary
#' @description This is simply a wrapper around `[Dictionary]$length` to get the
#' number of elements in a [Dictionary].
#' @param x ([Dictionary])
#' @export
length.Dictionary <- function(x) {
  x$length
}

#' @title Summarise a Dictionary
#' @description This is simply a wrapper around `[Dictionary]$summary(n)`.
#' @param object ([Dictionary])
#' @param n (`integer(1)`) \cr
#' Number of items to print on either side of ellipsis.
#' @param ... (`ANY`) \cr Other arguments, currently unused.
#' @export
summary.Dictionary <- function(object, n = 2, ...) {
  object$summary(n = n)
}

#' @title Create a string representation of an R6 class
#' @description Creates a string representation of an [R6::R6Class] if not
#' already defined.
#' @param x ([R6::R6Class])
#' @export
as.character.R6 <- function(x) {
  class(x)[[1]]
}

#' @title Create a string representation of a Dictionary
#' @description Creates a string representation of a [Dictionary] used in
#' printing.
#' @param x ([Dictionary])
#' @param n (`integer(1)`) \cr
#' Number of items to print on either side of ellipsis.
#' @param ... (`ANY`) \cr Other arguments, currently unused.
#' @export
as.character.Dictionary <- function(x, n = 2, ...) { # nolint
  keys <- x$keys
  values <- vapply(x$values, as.character, character(1), USE.NAMES = FALSE)

  lng <- x$length
  if (lng > (2 * n)) {
    string <- paste0(paste(keys[1:n], values[1:n], sep = ": ",
                           collapse = ", "),
                     ", ..., ", paste(keys[(lng - n + 1):lng],
                                      values[(lng - n + 1):lng],
                                      sep = ": ", collapse = ", "))
  } else {
    string <- paste(keys, values, sep = ": ", collapse = ", ")
  }
  return(paste0("{", string, "}"))
}

#' @title Concatenate multiple Dictionary objects
#' @description Creates a new [Dictionary] from the elements of provided
#' [Dictionary] objects, first checks keys are unique.
#' @param ... ([Dictionary]) \cr Dictionaries to concatenate.
#' @export
c.Dictionary <- function(...) {
  x <- list(...)
  types <- vapply(x, function(.x) list(.x$typed, length(.x$types), .x$types),
                  vector("list", 3))
  # different typing
  if (length(unique(types[1, ])) > 1) {
    stop("Can only combine Dictionaries if all typed or all untyped.")
    # all typed or untyped
  } else {
    # untyped
    if (!unlist(types[1, 1])) {
      Dictionary$new(x = unlist(lapply(x, "[[", "items"), recursive = FALSE))
      # typed
    } else {
      # different type lengths
      if (length(unique(types[2, ])) > 1) {
        stop("Can only combine typed Dictionaries of the same type(s).")
      } else {
        if (length(unique(unlist(types[3, ]))) != types[2, 1][[1]]) {
          stop("Can only combine typed Dictionaries of the same type(s).")
        } else {
          Dictionary$new(x = unlist(lapply(x, "[[", "items"),
                                    recursive = FALSE),
                         types = unlist(types[3, 1]))
        }
      }
    }
  }
}
