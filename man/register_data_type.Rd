% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/register_data_type.R
\name{register_data_type}
\alias{register_data_type}
\alias{register_data_type_instance}
\title{Generate a function to store and retrieve data column names for use with
assign_data_type}
\usage{
register_data_type()

register_data_type_instance(
  .l = list(),
  ...,
  overwrite = FALSE,
  only_good_names = TRUE,
  reset = FALSE
)
}
\arguments{
\item{.l}{A a named list of data types.  Names are the name of the data
source and values are column names.}

\item{...}{Named arguments interpreted the same as `.l`}

\item{overwrite}{Overwrite definitions if they already exist (with a warning)
or give an error if a name already exists.}

\item{only_good_names}{Should only names that make good R class names (starts
with a letter, only contains letters, numbers, and underscores) be allowed?}

\item{reset}{Remove all defined data types.}
}
\value{
`register_data_type()`: A function to store and retrieve data column
  names.  The function returned by `register_data_type()`: When called with
  no arguments, returns the currently stored list of data types.  When called
  with a combination of `.l` and/or `...` arguments, updates the list and
  returns it invisibly.
}
\description{
Generate a function to store and retrieve data column names for use with
assign_data_type

A specific example of how to use `register_data_type()`.  Do not use
`register_data_type_instance()` in your own package; generate your own
instance as shown in the example.
}
\examples{
my_data_types <- register_data_type()
my_data_types(data_source_1=c("A", "B"))
my_data_types()
}
