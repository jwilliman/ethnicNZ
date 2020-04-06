#' Check for errors in free text ethnicity field
#'
#' Check if the values entered in the free text ethnicity field match those
#' listed in the source dataset. By default the source dataset is taken from the
#' Statistics New Zealand Ethnicity Standard Classification 2005 version 2.
#'
#' @param data Data frame containing subject identifier and free text ethnicity field to be checked.
#' @param id Character giving name of the subject identifier column.
#' @param ethnicity Character giving name of the free text ethnicity column.
#'
#' @return A dataframe with identifier, input ethnicity, and mapped ethnicity classifications.
#' @export
#'
check_ethnicity <- function(data = NULL, id_col = "id", ethnicity = "ethnicity", sep = ",") {

  assertthat::assert_that(!is.null(id_col) & !is.null(ethnicity))

  if(is.null(data)) {
    data <- data.frame(idcol = id_col, ethnicity = ethnicity)
  } else {
    assertthat::assert_that(
      id_col %in% names(data) & ethnicity %in% names(data),
      msg = paste0(
        "Columns '", id_col , "' or '", ethnicity, "' not found in dataset."))
    data <- setNames(
      data[, c(id_col, ethnicity)],
      c("id", "ethnicity"))
  }


  # Drop records without other ethnicity listed
  data <- data[!is.na(data$ethnicity),]


  if(nrow(data) > 0) {

    if(is.factor(data$ethnicity))
      data$ethnicity <- as.character(data$ethnicity)

    # Create individual records for multiple other ethnicities
    list_eth <- strsplit(x = data$ethnicity, split = sep)
    names(list_eth) <- data$id

    dat_long <- setNames(
      dplyr::bind_rows(
        lapply(list_eth, as.data.frame, stringsAsFactors = FALSE),
        .id = "id"),
      c("id", "ethnicity"))

  } else {

    dat_long <- data

  }
    # Trim white space and lower case each word
    dat_long$ethnicity <- trimws(dat_long$ethnicity)
    dat_long$eth_low  <- tolower(dat_long$ethnicity)

    ### Merge with classification file  ----
    ethnic05$v2$eth_low <- tolower(ethnic05$v2$l4_label)

    dat_nfd <- ethnic05$v2[grepl("(nfd)$", ethnic05$v2$l4_label),]
    dat_nfd$eth_low <- trimws(gsub("(nfd)$", "", dat_nfd$eth_low))

    # Merge character ethnicity with StatsNZ coding scheme
    dat_out <- merge(
      dat_long,
      rbind(ethnic05$v2, dat_nfd), by = "eth_low",
      all.x = TRUE, all.y = FALSE)[,-1]

    if(is.numeric(data$id))
      class(dat_out$id) <- class(data$id)

    if(is.character(id_col) & is.character(ethnicity))
      names(dat_out)[1:2] <- c(id_col, ethnicity)


  return(dat_out)

}


#' Title
#'
#' @param data Data collected as per the standard New Zealand census ethnicity
#'   question (used in 2001, 2006, 2013, and 2018).
#' @param cols Vector of numeric or character indices indicating positions or
#'   names of ethnicity variables. Columns should be in the following order
#'   (names aren't important); New Zealand European, Maori, Samoan, Cook Island
#'   Maori, Tongan, Niuean, Chinese, Indian, Other, Other specified, (Refused)
#'   Columns should be logical or coercable as such (coded as 0 = No or 1 =
#'   Yes). Other specified should be a text field with multiple ethnicities
#'   separated by a comma.
#' @param sep A character string used to seperate different ethnicities list in the 'Other' field.
#' @param id_col Names of one or more variables to include in output dataset.  
#' @param base_levels Names of input variables, as recorded in the reference dataset.
#' @param eth_levels Names of output columns.

#'
#' @return A data.frame with ethnicity formatted according to Statistics NZ levels.
#' @export
#'
tidy_ethnicity <- function(
  data, cols = 1:10, sep = ",", id_col = NULL,
  base_levels = c(
    "New Zealand European", "M\U101ori", "Samoan", "Cook Island Maori",
    "Tongan", "Niuean", "Chinese", "Indian", "Other"),
  eth_levels = c("European", "M\u0101ori", "Pacific", "Asian", "MELAA", "Other", "Residual")) {

  if(is.numeric(cols))
    cols <- names(data)[cols]

  # Check   
  assertthat::assert_that(length(cols) == 10)
  assertthat::assert_that(
    length(intersect(id_col, names(data)[1:10])) == 0
    , msg = "id_col and cols overlap")
  
  
  ## Subset data to essential columns only
  dat <- data.frame(cbind(id = 1:nrow(data), data[, cols]))
  id_row <- "id"

  cols <- match(cols, names(dat))

  # Initial cleaning of the dataset
  # data[,1] <- as.character(data[,1])
  names(dat)[cols] <- c(base_levels, "OtherSpec")


  ## Correct column formats
  dat[, base_levels] <- lapply(dat[base_levels], as.logical)

  dat[,"OtherSpec"] <- as.character(dat[,"OtherSpec"])


  ## Identify rows with no ethnicity recorded
  dat[!is.na(dat$OtherSpec), rev(base_levels)[1]] <- TRUE
  dat$`Not Stated` <- rowSums(dat[, base_levels]) == 0



  ## Code Standard ethnicities ----

  # Reshape data from wide to long, (excluding 'other' ethnicities for now)
  dat_eth_core <- reshape(
    data = dat[,c(id_row, base_levels[1:8], "Not Stated")],
    varying = c(base_levels[1:8], "Not Stated"), # Dataset columns containing ethnicites.
    v.names = "value",
    timevar = "l4_label",
    times = c(base_levels[1:8], "Not Stated"),
    idvar = id_row,
    direction = "long")


  # Only keep rows indicating presence of ethnicity
  dat_eth_core <- dat_eth_core[dat_eth_core$value,]


  # Combine with reference datasets
  dat_eth_core <- merge(dat_eth_core, ethnic05$v2, by = "l4_label")[, c(id_row, names(ethnic05$v2))]


  ## Code Other ethnicities and merge with core ethnicities --------------------
  # If other is yes but not specified - code as such
  dat$OtherSpec[dat$Other & (is.na(dat$OtherSpec) | dat$Other %in% "")] <- "Other Ethnicity nec"

  dat_eth_other <- check_ethnicity(dat, id_col = id_row, ethnicity = "OtherSpec", sep = sep)

  dat_eth <- suppressWarnings(dplyr::bind_rows(dat_eth_core, dat_eth_other))
  dat_eth <- dat_eth[order(dat_eth[, id_row]),]

  ### Create dataset in wide format --------------------------------------------
  # dat_eth_l1w <- dat_eth %>%
  #   distinct(scn_id, l1_code, l1_label) %>%
  #   mutate(value = TRUE) %>%
  #   tidyr::pivot_wider("scn_id", names_from = "l1_label", values_from = "value",
  #                      values_fill = list(value = FALSE)) %>%

  dat_eth_l1 <- unique(dat_eth[, c(id_row, "l1_code", "l1_label")])
  dat_eth_l1 <- dat_eth_l1[order(dat_eth_l1$l1_code, dat_eth_l1[, id_row]),]
  dat_eth_l1$value <- TRUE

  levels(dat_eth_l1$l1_label) <- eth_levels

  dat_eth_l1w <- reshape(
    dat_eth_l1,
    drop = "l1_code",
    timevar = "l1_label",
    v.names = "value",
    idvar   = id_row,
    direction = "wide")

  # Code NA to FALSE
  dat_eth_l1w[is.na(dat_eth_l1w)] <- FALSE
  
  # Correct column names and add missing columns
  names(dat_eth_l1w) <- gsub("value\\.", "", names(dat_eth_l1w))
  col_miss <- setdiff(eth_levels, names(dat_eth_l1w))
  dat_eth_l1w[col_miss] <- FALSE
  
  # Tidy ordering of columns and rows
  dat_out <- data.frame(
    dat_eth_l1w[order(dat_eth_l1w[, id_row]), c(id_col, eth_levels)])


  return(dat_out)

}
