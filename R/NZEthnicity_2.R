census_2013_question_codes <- c(
  "New Zealand European" = 111,
  "Maori"                = 211,
  "Samoan"               = 311,
  "Cook Island Maori"    = 321,
  "Tongan"               = 331,
  "Niuean"               = 341,
  "Chinese"              = 421,
  "Indian"               = 431
)

ethnicity_level1_codes <- c(
  "european" = 1,
  "maori"    = 2,
  "pacific"  = 3,
  "asian"    = 4,
  "melaa"    = 5,
  "other"    = 6,
  "unknown"  = 9
)



#' Provides StatsNZ codes for ethnicity recorded in indicator (TRUE/FALSE, 1/0) columns
#'
#' @param data A data frame containing ethnicity questions to code.
#' @param cols <tidy-select> Columns containing ethnicity indicator columns
#' @param eth_codes A vector containing the StatsNZ codes for the ethnicity indicator columns. The length of codes should match the number of columns listed, and be in the same order. Default is as per the standard New Zealand census ethnicity question (used in 2001, 2006, 2013, and 2018). I.e. New Zealand European = 111, Māori = 211, ... .
#'
#' @export
#'
ethnic_code_indicators <- function(data, cols, eth_codes = census_2013_question_codes) {
  
  ## Add unique identifier for merging later.
  dat_eth_logic_wide <- data |> 
    dplyr::mutate(.rowid = row_number()) |> 
    dplyr::select(c(.rowid, {{ cols }} ))
  
  vct_eth_vars <- names(dat_eth_logic_wide)[-1]
  
  len_eth_vars  <- length(vct_eth_vars) 
  len_eth_codes <- length(eth_codes)
  
  if(len_eth_vars == 0) {
    warning("No ethnicity indicator columns selected")
    return(NULL)
  } else {
    
    assertthat::are_equal(len_eth_vars, len_eth_codes) |>
      assertthat::assert_that(msg = glue::glue(
        "Please check number of ethnicity columns ({len_eth_vars}) and ethnicity codes are the same ({len_eth_codes})."))
    
    dat_eth_logic_long <- dat_eth_logic_wide |>
      tidyr::pivot_longer(-c(.rowid), names_to = "code", values_to = "value") |>
      dplyr::mutate(
        code = factor(.data$code, levels = vct_eth_vars, labels = eth_codes) |>
          as.character() |> as.integer(),
        value = as.logical(.data$value)) |>
      dplyr::filter(.data$value %in% TRUE)
    
    return(dat_eth_logic_long)
    
  }
}

#' Provides StatsNZ codes for ethnicity recorded in text or numeric columns 
#'
#' @param data A data frame containing ethnicity questions to code.
#' @param cols <tidy-select> Columns containing ethnicity text or numeric columns
#' @param delim Provided deliminator if multiple ethnicities are recorded in a single column with deliminator separating them. 
#' @param code_level Provide the StatsNZ classification level (1,2,3 or 4) that ethnicity has been entered as. 
#' @param type Indicate whether ethnicity has been recorded as text (e.g. Taiwanese) or StatsNZ numeric code (e.g. 42116) 
#' @param check Logical to determine whether to output coded dataset (check = FALSE) or only return uncoded ethnicities (check = TRUE). 
#'
#' @export
#'
ethnic_code_text <- function(data, cols, delim = ",", code_level = 4, type = c("text", "code"), check = FALSE) {
  
  ## Add unique identifier for merging later.
  dat_eth_text_long <- data |> 
    dplyr::mutate(.rowid = row_number()) |> 
    dplyr::select(c(.rowid, {{ cols }} ))
  
  if(ncol(dat_eth_text_long) == 1) {
    warning("No ethnicity text columns selected")
    return(NULL)
  } else {
    
    dat_eth_stand <- ethnic05$v2 |> 
      dplyr::select(matches(as.character(code_level))) |> 
      dplyr::distinct() |> 
      setNames(c("code", "label"))
    
    dat_eth_text_long <- dat_eth_text_long |> 
      tidyr::pivot_longer(c(-.rowid), names_to = "var", values_to = "value") |> 
      dplyr::filter(!is.na(.data$value)) |> 
      tidyr::separate_longer_delim(c(value), delim = delim) |> 
      dplyr::mutate(value = trimws(.data$value)) 
    
    
    ## If ethnicity recorded as text
    if(type[1] == "text") {
      
      dat_eth_text_label <- dat_eth_text_long |> 
        select(c(.rowid, label = value)) |> 
        left_join(dat_eth_stand, by = "label")
      
      ## If ethnicity is already recorded as numeric code.
    } else if (type[1] == "code") {
      
      dat_eth_text_label <- dat_eth_text_long |>
        select(c(.rowid, code = value)) |>
        mutate(code = as.integer(.data$code)) |>
        left_join(dat_eth_stand, by = "code")
      
    }
    
    if(check == TRUE) #any(is.na(dat_eth_other_label$code)))
      return(
        dat_eth_text_label |>
          dplyr::filter(is.na(code))
      )
    else {
      
      length_uncoded <- sum(is.na(dat_eth_text_label$code))
      warning(
        assertthat::validate_that(
          length_uncoded == 0, 
          msg = glue::glue("{length_uncoded} ethnicities were not coded. Use function 'ethnic_code_text' with option 'check = TRUE' to identify which ones.")))
      
      return(
        dat_eth_text_label |> 
          dplyr::filter(!is.na(.data$code))  
      )
      
    }
  }
}


#' Code ethnicity as per Statistics NZ guidelines: Long
#'
#' @param data A data frame containing ethnicity questions to code.
#' @param indicator_cols Columns containing ethnicity indicator columns
#' @param indicator_codes A vector containing the StatsNZ codes for the ethnicity indicator columns. The length of codes should match the number of columns listed, and be in the same order. Default is as per the standard New Zealand census ethnicity question (used in 2001, 2006, 2013, and 2018). I.e. New Zealand European = 111, Māori = 211, ... .
#' @param text_cols <tidy-select> Columns containing ethnicity text or numeric columns
#' @param text_delim Provided deliminator if multiple ethnicities are recorded in a single column with deliminator separating them.
#' @param text_code_level Provide the StatsNZ classification level (1, 2, 3, or 4) that ethnicity has been entered as. 
#' @param level_out Provide the StatsNZ classification level (1, 2, 3, or 4) that ethnicity is to be outputted as. 
#'
#' @export
#'
ethnic_code_long <- function(
    data, indicator_cols, indicator_codes = census_2013_question_codes, 
    text_cols, text_delim = ",", text_code_level = 4,
    level_out = 3
) {
  
  list(
    indicators = ethnic_code_indicators(data, cols = {{ indicator_cols }}, eth_codes = indicator_codes),
    text       = ethnic_code_text(data, cols = {{ text_cols }}, delim = text_delim, code_level = text_code_level) 
  ) |> 
    bind_rows() |> 
    select(c(.rowid, code)) |> 
    arrange(.data$.rowid)
  
}

#' Code ethnicity as per Statistics NZ guidelines: Wide
#'
#' @param data data frame containing ethnicity questions to code.
#' @param level_out Provide the StatsNZ classification level (1, 2, 3, or 4) that ethnicity is to be outputted as.
#' @param col_names Names of output columns. 
#'
#' @export
#'
ethnic_code_wide <- function(data, level_out = 1, col_names = ethnicity_level1_codes) {
  
  dat_eth_stand <- ethnic05$v2 |> 
    dplyr::select(matches(as.character(level_out))) |> 
    dplyr::distinct() |> 
    setNames(c("code", "label")) |> 
    mutate(var_name = factor(.data$code, levels = col_names, labels = names(col_names)))
  
  dat_out <- data |> 
    select(.rowid, code) |> 
    dplyr::mutate(code = substr(.data$code, 1, level_out) |> as.integer()) |> 
    distinct() |> 
    left_join(dat_eth_stand, by = "code") |> 
    mutate(value = TRUE) |> 
    select(c(.rowid, var_name, value)) |> 
    arrange(.data$var_name) |> 
    pivot_wider(names_from = "var_name", values_from = "value", values_fill = FALSE) |>  
    arrange(.data$.rowid)
  
  return(dat_out)
  
}

#' Title
#'
#' @param data A data frame containing ethnicity questions to code.
#' @param indicator_cols Columns containing ethnicity indicator columns
#' @param indicator_codes A vector containing the StatsNZ codes for the ethnicity indicator columns. The length of codes should match the number of columns listed, and be in the same order. Default is as per the standard New Zealand census ethnicity question (used in 2001, 2006, 2013, and 2018). I.e. New Zealand European = 111, Māori = 211, ... .
#' @param text_cols <tidy-select> Columns containing ethnicity text or numeric columns
#' @param text_delim Provided deliminator if multiple ethnicities are recorded in a single column with deliminator separating them.
#' @param text_code_level Provide the StatsNZ classification level (1, 2, 3, or 4) that ethnicity has been entered as. 
#' @param level_out Provide the StatsNZ classification level (1, 2, 3, or 4) that ethnicity is to be outputted as.
#' @param col_names Names of output columns
#'
#' @export
#'
ethnic_code_all <- function(
    data, 
    indicator_cols, indicator_codes = census_2013_question_codes, 
    text_cols, text_delim = ",", text_code_level = 4,
    level_out = 1, col_names = ethnicity_level1_codes) {
  
  ethnic_code_long(
    data = data, 
    indicator_cols = {{ indicator_cols}}, 
    indicator_codes = indicator_codes,
    text_cols = {{ text_cols }}, 
    text_delim = text_delim,
    text_code_level = text_code_level
  ) |> 
    ethnic_code_wide(
      level_out = level_out, col_names = col_names
    )
  
}