% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/targeter.R
\name{detect_variables_types}
\alias{detect_variables_types}
\title{Detect and filter variables for analysis}
\usage{
detect_variables_types(
  data,
  target,
  select_vars = NULL,
  exclude_vars = NULL,
  naming_conventions = FALSE,
  autoguess_nrows = 1000,
  num_as_categorical_nval = 5,
  verbose = FALSE
)

detect_variables_types(
  data,
  target,
  select_vars = NULL,
  exclude_vars = NULL,
  naming_conventions = FALSE,
  autoguess_nrows = 1000,
  num_as_categorical_nval = 5,
  verbose = FALSE
)
}
\arguments{
\item{data}{data.table with data to analyze}

\item{target}{character - name of the target variable}

\item{select_vars}{character - vector of variables to include}

\item{exclude_vars}{character - vector of variables to exclude}

\item{naming_conventions}{logical - whether to enforce naming conventions}

\item{autoguess_nrows}{numeric - number of rows to use for type detection}

\item{num_as_categorical_nval}{numeric - threshold for numeric as categorical}

\item{verbose}{logical - whether to print verbose output}
}
\value{
list with variable classification and messages

list with variable classification and messages
}
\description{
This function handles variable type detection and filtering based on
naming conventions and other criteria.

This function handles variable type detection and filtering based on
naming conventions and other criteria.
}
