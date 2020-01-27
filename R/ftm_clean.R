#' Clean data of a given class
#'
#' To use this, you must write a method that is specific to that data class.
#' Typically, data will have been loaded, run through `assign_data_type()` to
#' have the class assigned, and then run through `ftm_clean()` to clean any
#' idiosyncracies of how data may not be clean from the source.  `ftm_clean()`
#' is not typically used to change column names or make other modifications.
#'
#' @param data The data to clean
#' @param ... options for cleaning passed to methods
#' @return The cleaned data
#' @export
ftm_clean <- function(data, ...)
  UseMethod("ftm_clean")

#' @export
ftm_clean.default <- function(data, ...) {
  stop("ftm_clean does not have a method to clean the class that was provided: ",
       paste(class(data), collapse=", "))
}
