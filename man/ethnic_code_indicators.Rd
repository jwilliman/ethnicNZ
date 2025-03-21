% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NZEthnicity_2.R
\name{ethnic_code_indicators}
\alias{ethnic_code_indicators}
\title{Provides StatsNZ codes for ethnicity recorded in indicator (TRUE/FALSE, 1/0) columns}
\usage{
ethnic_code_indicators(data, cols, eth_codes = census_2013_question_codes)
}
\arguments{
\item{data}{A data frame containing ethnicity questions to code.}

\item{cols}{<tidy-select> Columns containing ethnicity indicator columns}

\item{eth_codes}{A vector containing the StatsNZ codes for the ethnicity indicator columns. The length of codes should match the number of columns listed, and be in the same order. Default is as per the standard New Zealand census ethnicity question (used in 2001, 2006, 2013, and 2018). I.e. New Zealand European = 111, Māori = 211, ... .}
}
\description{
Provides StatsNZ codes for ethnicity recorded in indicator (TRUE/FALSE, 1/0) columns
}
