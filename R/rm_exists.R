#' @title Remove if object exists
#'
#' @description A function which tests object(s) existence, and then removes it
#' if it exists.
#'
#' @param vector A vector of strings references the objects that you want to have removed.
#' @param envir The environment this should be completed within.
#' @return Nothing is returned but the removal of the object.
#' @examples
#' A <- 1
#' B <- 2
#' D <- 4
#' rm_exists(vector = LETTERS[1:4])
#' @note We will remove rm.exists on or after 11/1/2017; replaced with
#' rm_exists.
#'
#' @export
rm_exists <- function(vector, envir = parent.frame()) {
  for (i in seq_along(vector)) {
    if (exists(vector[i], envir = envir, inherits = FALSE)) {
      rm(list = vector[i], envir = envir)
    }
  }
}

# this is now deprecated ---
rm.exists <- function(vector, envir = parent.frame()) {
  for (i in seq_along(vector)) {
    if (exists(vector[i], envir = envir, inherits = FALSE)) {
      rm(list = vector[i], envir = envir)
    }
  }
  .Deprecated(new = "rm_exists", package = "Emisc",
              msg = paste("rm.exists is deprecated.",
                          "It is scheduled for complete removal 11/1/2017."))
}