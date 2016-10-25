#' Interval Closure Classification
#'
#' @description An alternative to sQuote, ideally for passing quoted values to
#' another R function. When you want fancy quotes (for visual appeal),
#' use sQuote.
#'
#' @param x any character object to be be surrounded with quotation marks. If
#' x is not a character object and can be coerced to a character object, it
#' will, but with a warning message.
#' @param qmark Any character to 'surround' x. Defaults to single quotation
#' marks (not fancy).
#'
#' @return a character vector in single quotes (default behavior). Changing
#' qmark will result in different character(s) surrouding 'x'.
#'
#' @note sQuote is very similar but to get the same behavior options must
#' be changed. (i.e. options(useFancyQuotes = FALSE))
#'
#' @examples
#' add_quote("10/1/2012")
#' add_quote(1)
#'
#' @name add_quote
NULL

# R CMD checker appeasement

#' @rdname add_quote
#' @export
add_quote <- function(x, qmark = "'") {
  if (class(x) != "character") {
    x <- as.character(x)
    warning("You added quotes to non-character class object;
            class of 'x' is now character.")
  }
  Reduce(paste0, list(qmark, x, qmark))
  }