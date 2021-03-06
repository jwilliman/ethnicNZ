% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NZEthnicity.R
\name{check_ethnicity}
\alias{check_ethnicity}
\title{Check for errors in free text ethnicity field}
\usage{
check_ethnicity(data = NULL, id_col = "id", ethnicity = "ethnicity", sep = ",")
}
\arguments{
\item{data}{Data frame containing subject identifier and free text ethnicity field to be checked.}

\item{ethnicity}{Character giving name of the free text ethnicity column.}

\item{id}{Character giving name of the subject identifier column.}
}
\value{
A dataframe with identifier, input ethnicity, and mapped ethnicity classifications.
}
\description{
Check if the values entered in the free text ethnicity field match those
listed in the source dataset. By default the source dataset is taken from the
Statistics New Zealand Ethnicity Standard Classification 2005 version 2.
}
