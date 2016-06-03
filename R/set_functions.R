#' @title set functions for multiple column alterations using data.tables
#'
#' @description This function updates by reference multiple columns by applying
#'   a desired function such as \code{numeric}, \code{factor} using functions in
#'   the same manior as \code{apply()}, or with \code{.} as a placeholder.
#'
#' @param x A data.table. Or, set() accepts data.frame, too.
#'
#' \code{i} Is left out of this function.
#'
#' @param j Column name(s) (character) or number(s) (integer) to be assigned
#'   value when column(s) already exist, and only column name(s) if they are to
#'   be added newly. Additionally, assigning \code{j = 'all'} will automatically
#'   reference all columns in the data.table.
#' @param value  A list of replacement values to assign by reference to
#'   \code{x[i, j]}. In addition, functions can be passed into the value
#'   argument as well with subsquent arguements in the same way as apply
#'   functions.
#' @param ... Is used to pass arguments into the value "function" if you choose
#'   to apply a function instead of updating the data with a replacement vector
#'   or list. If you choose to place a \code{.} at any point within the
#'   sequence of additional arguements passed into the \code{value} arguement,
#'   the \code{.} will act as a placeholder for the jth column.
#'
#' @section Notes: These functions are for convenience only.
#'
#' @name set_functions
NULL

#' @rdname set_functions
#' @importFrom data.table set
#' @examples
#' dt <- d.t(x = 0:10, y = 10:20)
#' setf(dt, j = c("x","y"), value = paste0,  ., "hello", "would")
#' print(dt)
#' @section TODO:
#' \itemize{
#'    \item Consider to.function joining.
#' }
#' @export
setf <- function (x, j, value, ...) {
  if (!'all' %in% names(x) && j == "all") j <- names(x)
  if (is.atomic(value)) {
    set(x, i = NULL, j, value)
  } else if (is.function(value)) {
    args <- as.list(substitute(list(...)))[-1L]
    ph_index <- which(args == ".")
    if (!length(ph_index)) {
      ph_index <- 1
      clist <- append(list(value), args)
    } else {
      clist <- append(list(value), args[-ph_index])
    }
    FUN <- as.call(append(clist, quote(x[[jj]]), after = ph_index))
    for(jj in j) {
      eval(substitute(set(x, i=NULL, j=jj, value = f), list(f = FUN)))
    }
  }
}


