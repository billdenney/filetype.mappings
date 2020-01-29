#' Generate a function to store and retrieve data column names for use with
#' assign_data_type
#'
#' @param overwrite Overwrite definitions if they already exist (with a warning)
#'   or give an error if a name already exists.
#' @param reset Remove all defined data types.
#' @param only_good_names Should only names that make good R class names (starts
#'   with a letter, only contains letters, numbers, and underscores) be allowed?
#'
#' @return `register_data_type()`: A function to store and retrieve data column
#'   names.  The function returned by `register_data_type()`: When called with
#'   no arguments, returns the currently stored list of data types.  When called
#'   with a combination of `.l` and/or `...` arguments, updates the list and
#'   returns it invisibly.
#' @export
#' @examples
#' my_data_types <- register_data_type()
#' my_data_types(data_source_1=c("A", "B"))
#' my_data_types()
register_data_type <- function() {
  ftm_columns <- list()
  function(.l=list(), ..., overwrite=FALSE, only_good_names=TRUE, reset=FALSE) {
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
      pattern_good_name <- "^[A-Za-z][A-Za-z0-9_]*$"
      # Detect and warn about names that will not be good class names
      bad_names <- grep(x=names(args), pattern=pattern_good_name, invert=TRUE, value=TRUE)
      if (length(bad_names)) {
        bad_name_message <-
          paste0(
            "The following name(s) do not make good R class names: ",
            paste0('"', bad_names, '"', collapse=", ")
          )
        if (only_good_names) {
          stop(bad_name_message)
        } else {
          warning(bad_name_message)
        }
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
#' @rdname register_data_type
#' @export
register_data_type_instance <- register_data_type()
