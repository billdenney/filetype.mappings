#' Assign a type of data given a set of data types to match.
#' 
#' @details To detect a data type, first `col_names` is sorted by number of
#'   columns provided so that more matching names are matched rather than fewer.
#'   Then, the first among the sorted `col_names` list is selected.
#'
#' @param x A data.frame to detect the type
#' @param col_names A named list where the names are the data type and the
#'   values are required column names (additional column names may be present in
#'   the data).
#' @param no_match What should be done if no data type matches `x`?  (Typically
#'   this will be one of `stop`, `warning`, or `message`, but it can be any
#'   function taking a single character argument.)
#' @return `x` with classes of the data type and "filetype_mappings" added.
#' @export
assign_data_type <- function(x, col_names, no_match=warning)
  UseMethod("assign_data_type")

#' @export
assign_data_type.data.frame <- function(x, col_names, no_match=warning) {
  stopifnot(is.data.frame(x))
  col_names_sorted <- sort_col_names(col_names)
  mapped_type <- NULL
  idx <- 0
  while (is.null(mapped_type) && idx < length(col_names_sorted)) {
    idx <- idx + 1
    if (all(col_names_sorted[[idx]] %in% colnames(x))) {
      mapped_type <- names(col_names_sorted)[[idx]]
      class(x) <- c(mapped_type, "filetype_mappings", class(x))
    }
  }
  if (is.null(mapped_type)) {
    no_match("Data did not match any data type for assignment.")
  }
  x
}

#' @export
assign_data_type.list <- function(x, col_names, no_match=warning) {
  lapply(
    x,
    assign_data_type,
    col_names=col_names, no_match=no_match
  )
}

assign_data_type.NULL <- function(x, col_names, no_match=warning) {
  no_match("NULL values cannot match a data_type")
  NULL
}

sort_col_names <- function(col_names) {
  if (!is.list(col_names)) {
    stop("`col_names` must be a list.")
  } else if (length(col_names) < 1) {
    stop("`col_names` cannot be empty.")
  } else if (!all(sapply(col_names, is.character))) {
    stop("`col_names` must be a list of character vectors.")
  } else if (any(sapply(col_names, FUN=function(x) any(is.na(x))))) {
    stop("No value in `col_names` may be NA.")
  } else if (is.null(names(col_names)) || any(names(col_names) == "")) {
    stop("`col_names` must be a named list where all elements are named.")
  } else if (any(duplicated(names(col_names)))) {
    stop("`names(col_names)` must be unique.")
  }
  cn_len <- sapply(col_names, length)
  ret <- col_names[order(cn_len, decreasing=TRUE)]
  ret_len <- sapply(ret, length)
  # Verify that the values are unique
  for (current_len in unique(ret_len)) {
    current_idx <- which(ret_len == current_len)
    for (start_idx in (seq_len(length(current_idx) - 1))) {
      idx_1 <- current_idx[start_idx]
      for (end_idx in (seq_len(length(current_idx) - start_idx) + start_idx)) {
        idx_2 <- current_idx[end_idx]
        # Sort the results so that c("A", "B") and c("B", "A") will be caught
        if (identical(sort(ret[[idx_1]]), sort(ret[[idx_2]]))) {
          stop(
            "`col_names` cannot have the same vector of character strings (it would match the same data two ways).  ",
            "Column names duplicated are: ",
            paste0("`", sort(ret[[idx_1]]), "`", collapse=", ")
          )
        }
      }
    }
  }
  ret
}