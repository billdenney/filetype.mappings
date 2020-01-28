#' Generate a function to store and retrieve data column names for use with
#' assign_data_type
#' 
#' @return A function to store and retrieve data column names
#' @export
#' @examples
#' my_data_types <- register_data_type()
#' my_data_types(data_source_1=c("A", "B"))
#' my_data_types()
register_data_type <- function() {
  ftm_columns <- list()
  function(.l=list(), ..., overwrite=FALSE, reset=FALSE) {
    # Combine all arguments
    args <- append(.l, list(...))
    if (reset) {
      if (length(args)) {
        warning("Arguments other than reset are ignored when reset is TRUE.")
      }
      ftm_columns <<- list()
      invisible(ftm_columns)
    } else if (length(args) == 0) {
      # return the column names
      ftm_columns
    } else {
      # Confirm that the input is all named
      if (is.null(names(args)) || any(names(args) %in% "")) {
        stop("All arguments must be named, including that list input must be a named list.")
      } else if (any(duplicated(names(args)))) {
        stop("No names may be duplicated in input arguments including between list and ... arguments.")
      }
      overwrite_names <- intersect(names(ftm_columns), names(args))
      if (length(overwrite_names)) {
        overwrite_message <-
          paste(
            "overwrite the following definition(s):",
            paste(overwrite_names, collapse=", ")
          )
        if (overwrite) {
          warning(paste("Did", overwrite_message))
        } else {
          stop(paste("Attempted to", overwrite_message))
        }
      }
      # Overwrite/append the new arguments
      ftm_columns[names(args)] <<- args
      invisible(ftm_columns)
    }
  }
}

#' A specific example of how to use `register_data_type()`.  Do not use
#' `register_data_type_instance()` in your own package; generate your own
#' instance as shown in the example.
#' 
#' @param .l A a named list of data types.  Names are the name of the data
#'   source and values are column names.
#' @param ... Named arguments interpreted the same as `.l`
#' @param overwrite Overwrite definitions if they already exist (with a warning)
#'   or give an error if a name already exists.
#' @param reset Remove all defined data types.
#'
#' @return When called with no arguments, returns the currently stored list of
#'   data types.  When called with a combination of `.l` and/or `...` arguments,
#'   updates the list and returns it invisibly.
#' @rdname register_data_type
#' @export
register_data_type_instance <- register_data_type()
