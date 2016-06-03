#' data.table shortened alternatives
#'
#' Shortented versions of data.table's functions \code{data.table}, \code{rbindlist}, and base's \code{data.frame}.
#'
#' @importFrom data.table rbindlist data.table
#' @inheritParams data.table::data.table
#' @inheritParams data.table::rbindlist
#' @inheritParams base::data.frame
#' @inheritParams base::unique
#' @inheritParams data.table::uniqueN
#'
#' @examples
#' # Normal example #
#' dt <- d.t(x = c(1:3), y = letters[1:3], key = 'y')
#' key(dt)
#' # Without argument name #
#' d.t(c(1:3))
#'
#' @seealso
#' \href{https://cran.r-project.org/web/packages/data.table/index.html}{data.table}
#' @return
#' Functions return the same objects as the original functions.
#' @section TODO:
#' \itemize{
#'    \item Improve and/or potentally merge function into other files.
#' }
#' @name d.t
#' @export
d.t <- data.table::data.table
# function(..., keep.rownames = FALSE, check.names = FALSE, key = NULL) {
#   CALL <- match.call()
#   CALL[1] <- call('data.table')
#   eval(CALL, parent.frame())
# }

#' @rdname d.t
#' @examples
#' d.f(c(1:3)) # Normal data.frame
#' @export
d.f <- base::data.frame
#   function(..., row.names = NULL, check.rows = FALSE, check.names = TRUE,
#                 stringsAsFactors = default.stringsAsFactors()) {
#   CALL <- match.call()
#   CALL[1] <- call('data.frame')
#   eval(CALL, parent.frame())
# }

#' @rdname d.t
#' @examples
#' rbl(list(d.t(a = 1, b = 1), d.t(a = 2, b = 2)))
#' @export
rbl <- data.table::rbindlist
#   function(l, use.names = fill, fill = FALSE) {
#   CALL <- match.call()
#   CALL[1] <- call('rbindlist')
#   eval(CALL, parent.frame())
# }



