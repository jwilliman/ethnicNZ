#' Check for errors in free text ethnicity field
#'
#' Check if the values entered in the free text ethnicity field match those
#' listed in the source dataset. By default the source dataset is taken from the
#' Statistics New Zealand Ethnicity Standard Classification 2005 version 2.
#'
#' @param data Data frame containing subject identifier and free text ethnicity field to be checked.
#' @param id_col Character giving name of the subject identifier column.
#' @param ethnicity Character giving name of the free text ethnicity column.
#' @param sep A character string used to separate different ethnicities listed in
#' the free text field. Comma by default.
#'
#' @return A dataframe with identifier, input ethnicity, and mapped ethnicity classifications.
#' @importFrom stats setNames
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
    data <- stats::setNames(
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
    
    dat_long <- stats::setNames(
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


#' Code ethnicity as per Statistics NZ guidelines.
#'
#' @param data Data collected as per the standard New Zealand census ethnicity
#'   question (used in 2001, 2006, 2013, and 2018).
#' @param cols Vector of numeric indices or column names of length 10,
#'   indicating positions or names of ethnicity variables. Columns should be in
#'   the following order (names aren't important); New Zealand European, Maori,
#'   Samoan, Cook Island Maori, Tongan, Niuean, Chinese, Indian, Other, Other
#'   specified. First nine columns should be logical or coercable as such (coded
#'   as 0 = No or 1 = Yes). Last column should be a text field with multiple
#'   ethnicities separated by a character string.
#' @param sep A character string used to separate different ethnicities listed in
#' the 'Other' field. Comma by default.
#' @param base_levels Names of input variables as recorded in the reference
#'   dataset although not necessary as recorded in the data. Defaults to New
#'   Zealand European, Maori, Samoan, Cook Island Maori, Tongan, Niuean,
#'   Chinese, Indian, Other
#' @param eth_levels Names of output columns. Defaults to European, Maori,
#' Pacific, Asian, MELAA, Other, and Unknown.
#' @param eth_prior A character string indicating the name to give a column
#'   containing prioritised ethnicity. Default is not to produce column.
#' @param prior_order Numeric vector giving prioritisation order of columns
#'   listed in eth_levels. Defaults to c(2:6,1,7), ie. Maori, Pacific, Asian,
#'   MELAA, Other, European, and Unknown.
#' @param add_cols Either a logical vector indicating whether to append the
#'   output to data collected, or a character vector containing the names of one
#'   or more variables to include in the output. Default is to return just the
#'   calculated ethnicity indicator variables.
#'
#' @return A data.frame with ethnicity formatted according to Statistics NZ
#'   levels.
#' @importFrom stats setNames reshape
#' @export
#'
tidy_ethnicity <- function(
  data, cols = 1:10, sep = ",", base_levels = NULL, eth_levels = NULL, eth_prior = NULL, prior_order = c(2:6,1,7), add_cols = FALSE) {
  
  if(is.numeric(cols))
    cols <- names(data)[cols]
  
  if(is.null(base_levels))
    base_levels <- c(
      "New Zealand European", "Maori", "Samoan", "Cook Islands Maori",
      "Tongan", "Niuean", "Chinese", "Indian", "Other")
  
  if(is.null(eth_levels))
    eth_levels <- c("European", "Maori", "Pacific", "Asian", "MELAA", "Other", "Unknown")
  
  # Check inputs
  assertthat::assert_that(length(cols) == 10)
  assertthat::assert_that(is.character(add_cols) | is.logical(add_cols))
  assertthat::assert_that(
    length(intersect(add_cols, cols)) == 0
    , msg = "cols and add_cols overlap")
  
  
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
  
  ## Correct 'other ethnicity' where other specified ethnicty is not empty
  dat$OtherSpec[dat$OtherSpec %in% ""] <- NA_character_
  dat[!is.na(dat$OtherSpec), rev(base_levels)[1]] <- TRUE
  
  ## Identify rows with no ethnicity recorded
  dat$`Not Stated` <- rowSums(dat[, base_levels]) == 0
  
  
  
  ## Code Standard ethnicities ----
  
  # Reshape data from wide to long, (excluding 'other' ethnicities for now)
  base_levels_ed <- c(base_levels[base_levels != "Other"], "Not Stated")
  
  dat_eth_core <- stats::reshape(
    data = dat[,c(id_row, base_levels_ed)],
    varying = base_levels_ed, # Dataset columns containing ethnicites.
    v.names = "value",
    timevar = "l4_label",
    times = base_levels_ed,
    idvar = id_row,
    direction = "long")
  
  
  # Only keep rows indicating presence of ethnicity
  dat_eth_core <- dat_eth_core[dat_eth_core$value,]
  
  # Recode 'Chinese' and 'Indian' as 'Chinese nfd', 'Indian nfd'
  dat_eth_core$l4_label[dat_eth_core$l4_label == "Chinese"] <- "Chinese nfd"
  dat_eth_core$l4_label[dat_eth_core$l4_label == "Indian"] <- "Indian nfd"
  
  # Combine with reference datasets
  dat_eth_core2 <- merge(dat_eth_core, ethnic05$v2, by = "l4_label")[, c(id_row, names(ethnic05$v2))]
  
  
  ## Code Other ethnicities and merge with core ethnicities --------------------
  # If other is yes but not specified - code as such
  dat$OtherSpec[dat$Other & (is.na(dat$OtherSpec) | dat$Other %in% "")] <- "Other Ethnicity nec"
  
  dat_eth_other <- check_ethnicity(dat, id_col = id_row, ethnicity = "OtherSpec", sep = sep)
  
  ## Code unidentifiable codes as such.
  dat_eth_other[is.na(dat_eth_other$l1_code),3:10] <- ethnic05$v2[
    ethnic05$v2$l4_label == "Response Unidentifiable",]
  
  dat_eth <- suppressWarnings(dplyr::bind_rows(dat_eth_core2, dat_eth_other))
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
  
  dat_eth_l1w <- stats::reshape(
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
    dat_eth_l1w[order(dat_eth_l1w[, id_row]), c(eth_levels)])
  
  # Add prioritised ethnicity
  if(!is.null(eth_prior))
    dat_out[, eth_prior] <- factor(eth_levels[prior_order][
      apply(dat_out[, prior_order], 1, FUN = function(x) 
        min(which(x == TRUE)))],
      levels = eth_levels[prior_order])
  
  
  # Add extra columns if specified
  if(is.character(add_cols))
    dat_out <- cbind(data[, unique(add_cols), drop = FALSE], dat_out)
  else if(add_cols == TRUE)
    dat_out <- cbind(data, dat_out)
  
  return(dat_out)
  
}

#' Code ethnicity recorded as Statistics NZ codes
#'
#' @param data Data collected as per the standard New Zealand census ethnicity
#'   question (used in 2001, 2006, 2013, and 2018). 
#'   Ethnicity columns should be named with a common prefix followed by valid StatsNZ ethnicity codes
#'   (e.g. eth___11, eth___21, ...).  
#' @param vars_prefix Common prefix used to denote ethnicity variable names.   
#' @param vars_binary Tidy-select argument to select ethnicity columns. 
#' Ethnicity columns should be named with a common prefix followed by valid StatsNZ ethnicity codes
#'   (e.g. eth___11, eth___21, ...). 
#'  Columns should be logical or coercable as such (coded as 0 = No or 1 = Yes).
#' @param vars_other Tidy-select argument to select columns containing 'other' ethnicities recording StatsNZ codes.
#' @param level_out Integer indicating StatsNZ total response output level (1, 2, 3 or 4).
#' @param col_names Names to give output columns. 
#' @param eth_prior A character string indicating the name to give a column
#'   containing prioritised ethnicity. Default is not to produce column.
#' @param eth_levels Names of output columns. Defaults to European, Maori,
#' Pacific, Asian, MELAA, Other, and Unknown.
#' @param prior_order Numeric vector giving prioritisation order of columns
#'   listed in eth_levels. Defaults to c(2:6,1,7), ie. Maori, Pacific, Asian,
#'   MELAA, Other, European, and Unknown.
#' @param add_cols Either a logical vector indicating whether to append the
#'   output to data collected, or a character vector containing the names of one
#'   or more variables to include in the output. Default is to return just the
#'   calculated ethnicity indicator variables.
#'
#' @return A data.frame with ethnicity formatted according to Statistics NZ
#'   levels.
#'   
#' @import dplyr
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom rlang quo enquo quo_is_missing
#'
#' @export
tidy_ethnicity_codes <- function(
    data, vars_prefix, vars_binary, vars_other, level_out = 1, col_names = paste0(
      "eth_", c("euro", "maori", "pacific", "asian", "melaa", "other", "unknown"
      )), eth_prior = NULL, eth_levels = NULL, prior_order = c(2:6,1,7), add_cols = TRUE) {
  
  
  assertthat::not_empty(vars_prefix)
  
  dat_eth_stand <- ethnic05$v2 |> 
    dplyr::select(matches(as.character(level_out))) |> 
    dplyr::distinct() |> 
    setNames(c("code", "label"))
  
  assertthat::are_equal(
    length(levels(dat_eth_stand$label)), length(col_names))  
  levels(dat_eth_stand$label) <- col_names
  
  
  if(rlang::quo_is_missing(enquo(vars_binary)))
    vars_binary = rlang::quo(starts_with( {{vars_prefix}} ))
  
  if(is.null(eth_levels))
    eth_levels <- c("European", "Maori", "Pacific", "Asian", "MELAA", "Other", "Unknown")
  
  dat_eth_logic <- data |> 
    dplyr::mutate(.id = row_number()) |> 
    dplyr::select(.data$.id, {{vars_binary}} ) |> 
    tidyr::pivot_longer(
      -.data$.id, names_to = "code", values_to = "value", 
      names_prefix = vars_prefix, names_transform = \(x) 
      substr(x, 1,level_out) |> as.integer()) 
  
  if(!rlang::quo_is_missing(enquo(vars_other))) {
    
    dat_eth_other <- data |> 
      dplyr::mutate(.id = row_number()) |> 
      dplyr::select(.data$.id, {{vars_other}}) |> 
      tidyr::pivot_longer(
        -.data$.id, names_to = "var", values_to = "code", values_transform = \(x) 
        substr(x, 1, level_out) |> as.integer()) |> 
      dplyr::filter(!is.na(.data$code)) |> 
      dplyr::mutate(value = TRUE) |> 
      dplyr::select(.data$var)
    
    dat_eth_all <- dat_eth_logic |> 
      dplyr::bind_rows(dat_eth_other) 
    
  } else {
    dat_eth_all <- dat_eth_logic
  }
  
  dat_out <- dat_eth_all |> 
    dplyr::summarise(value = any(.data$value), .by = c(.data$.id, .data$code)) |> 
    dplyr::left_join(dat_eth_stand, by = join_by(.data$code)) |>  
    dplyr::distinct(.data$.id, .data$label, .data$value) |> 
    dplyr::arrange(.data$label) |> 
    tidyr::pivot_wider(names_from = .data$label, values_from = .data$value, values_fill = FALSE) |> 
    dplyr::arrange(.data$.id) |> 
    dplyr::select(.data$.id)
  
  
  # Add prioritised ethnicity
  if(!is.null(eth_prior))
    dat_out[, eth_prior] <- factor(eth_levels[prior_order][
      apply(dat_out[, prior_order], 1, FUN = function(x) 
        min(which(x == TRUE)))],
      levels = eth_levels[prior_order])
  
  
  # Add extra columns if specified
  if(is.character(add_cols))
    dat_out <- cbind(data[, unique(add_cols), drop = FALSE], dat_out)
  else if(add_cols == TRUE)
    dat_out <- cbind(data, dat_out)
  
  return(dat_out)
  
}
