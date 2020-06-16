#' Check for internal data
#'
#' Provides access to the internal data listing all NHANES files
#'
#' @return A data frame with the list of files that can be accessed through the NHANES website.  Should not generally be used.  Present for debugging purposes and transparency.
#' @export
find_data <- function(){
  ethnic05$v2
}