% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/targeter.R
\name{treat_tab_labels}
\alias{treat_tab_labels}
\title{Process table labels for display}
\usage{
treat_tab_labels(tab, variable, is_numeric, cutpoints_list, dec = 2)

treat_tab_labels(tab, variable, is_numeric, cutpoints_list, dec = 2)
}
\arguments{
\item{tab}{data.table - table to process}

\item{variable}{character - variable name}

\item{is_numeric}{logical - whether variable is numeric}

\item{cutpoints_list}{list - cutpoints for numeric variables}

\item{dec}{integer - decimals to show}
}
\value{
processed table with formatted row names

processed table with formatted row names
}
\description{
Helper function to format variable labels for display

Helper function to format variable labels for display
}
