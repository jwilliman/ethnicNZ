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

ethnicity_level1_labels <- c(
  "european" = "European",
  "maori"    = "Māori",
  "pacific"  = "Pacific",
  "asian"    = "Asian",
  "melaa"    = "MELAA",
  "other"    = "Other",
  "unknown"  = "Unknown"
)

ethnicity_level1_prior_order <- c(2:6, 1, 7)


return_id <- function(data, id_cols = NULL) {
  
  check_id_any <- assertthat::validate_that(
    !rlang::quo_is_null(rlang::enquo(id_cols)),
    msg = "No unique identifier column provided")
  
  if(check_id_any == TRUE) {
    
    return(rlang::ensym(id_cols))
    
  } else {
    
    message(check_id_any)
    
    check_id_first <- assertthat::validate_that(
      length(unique(data[[1]])) == nrow(data)
      , msg = "First column not unique")
    
    if(check_id_first == TRUE) {
      
      message("Using first column")
      return(rlang::sym(names(data)[1]))
      
    } else {
      
      message(check_id_first)
      message("Creating new identifier called .rowid")
      
      return(rlang::sym(".rowid"))
      
    }
  }
}


#' Provides StatsNZ codes for ethnicity recorded in indicator (TRUE/FALSE, 1/0) columns
#'
#' @param data A data frame containing ethnicity questions to code.
#' @param cols <tidy-select> Columns containing ethnicity indicator columns
#' @param eth_codes A vector containing the StatsNZ codes for the ethnicity indicator columns. The length of codes should match the number of columns listed, and be in the same order. Default is as per the standard New Zealand census ethnicity question (used in 2001, 2006, 2013, and 2018). I.e. New Zealand European = 111, Māori = 211, ... .
#'
#' @importFrom tibble add_column
#' @export
#'
ethnic_code_indicators <- function(data, cols, id_cols = NULL, eth_codes = census_2013_question_codes) {
  

  ## Add unique identifier for merging later.
  id_cols <- return_id(data, {{ id_cols }})
  
  if(id_cols == rlang::sym(".rowid"))
    data <- tibble::add_column(data, .rowid = 1:nrow(data), .before = 1)
  
  dat_eth_logic_wide <- data |> 
    dplyr::select(c({{ id_cols }}, {{ cols }} ))
  
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
      tidyr::pivot_longer(-c({{ id_cols }}), names_to = "code", values_to = "value") |>
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
#' @param type Indicate whether ethnicity has been recorded as `text` (e.g. Taiwanese) or StatsNZ numeric `code` (e.g. 42116) 
#' @param check Logical to determine whether to output coded dataset (check = FALSE) or only return uncoded ethnicities (check = TRUE). 
#'
#' @importFrom tibble add_column
#' @export
#'
ethnic_code_text <- function(data, cols, id_cols = NULL, delim = ",", code_level = 4, type = NULL, check = NULL) {
  
  ## Add unique identifier for merging later.
  id_cols <- return_id(data, {{ id_cols }})
  
  if(id_cols == rlang::sym(".rowid"))
    data <- tibble::add_column(data, .rowid = 1:nrow(data), .before = 1)
  
  ## Add unique identifier for merging later.
  dat_id <- data |> 
    dplyr::select(c({{ id_cols }}, {{ cols }} ))
  
  if(ncol(dat_id) == 1) {
    warning("No ethnicity text columns selected")
    return(NULL)
  } else {
    
    dat_eth_stand <- ethnic05$v2 |> 
      dplyr::select(matches(as.character(code_level))) |> 
      dplyr::distinct() |> 
      setNames(c("code", "label"))
    
    dat_eth_text_long <- dat_id |> 
      tidyr::pivot_longer(-c({{ id_cols }}), names_to = "var", values_to = "value") |> 
      dplyr::filter(!is.na(.data$value)) |> 
      tidyr::separate_longer_delim(c(value), delim = delim) |> 
      dplyr::mutate(value = trimws(.data$value)) 
    
    
    type_code <- all(grepl("[0-9]{5}", unique(na.omit(dat_eth_text_long$value))))
    
    if(is.null(type))
      if(type_code) type = "code" else type = "text"
        
    ## If ethnicity recorded as text
    if(type == "text") {
      
      dat_eth_text_label <- dat_eth_text_long |> 
        select(c({{ id_cols }}, label = value)) |> 
        left_join(dat_eth_stand, by = "label") |> 
        select(c({{ id_cols }}, code, label))
      
      ## If ethnicity is already recorded as numeric code.
    } else if (type == "code") {
      
      dat_eth_text_label <- dat_eth_text_long |>
        select(c({{ id_cols }}, code = value)) |>
        mutate(code = as.integer(.data$code)) |>
        left_join(dat_eth_stand, by = "code") |> 
        select(c({{ id_cols }}, code, label))
      
    }
    
    if(!is.null(check)) {
      
      if(check == TRUE) { #any(is.na(dat_eth_other_label$code))) 
        
        ## Return unmatched ethnicities only
        return(
          dat_eth_text_label |>
            dplyr::filter(is.na(code))
        )
        
      } else if(check == FALSE) {
        
        ## Return matched ethnicities only, without a warning.
        return(
          dat_eth_text_label |>
            dplyr::filter(!is.na(code))
        )
      } 
      
    } else {
      
      ## Return matched ethnicities only, with a warning.
      length_uncoded <- sum(is.na(dat_eth_text_label$code))
      
      warning(
        assertthat::validate_that(
          length_uncoded == 0, 
          msg = glue::glue("{length_uncoded} ethnicities were not coded. Use function 'ethnic_code_text' with option 'check = TRUE' to identify which ones, or 'check = FALSE' to suppress this warning.")))
      
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
#' @param check Logical to determine whether to output coded dataset (check = FALSE) or only return uncoded ethnicities (check = TRUE). 
#' @param level_out Provide the StatsNZ classification level (1, 2, 3, or 4) that ethnicity is to be outputted as. 
#'
#' @importFrom tibble add_column
#' @export
#'
#' @returns A dataframe with columns for identifier (or row id), ethnicity code, and 
#' 
ethnic_code_long <- function(
    data, indicator_cols, indicator_codes = census_2013_question_codes, 
    text_cols, text_delim = ",", text_code_level = 4, text_code_type = NULL, check = NULL,
    id_cols = NULL, 
    level_out = 3
) {
  
  ## Add unique identifier for merging later.
  id_cols <- return_id(data, {{ id_cols }})
  
  if(id_cols == rlang::sym(".rowid"))
    data <- tibble::add_column(data, .rowid = 1:nrow(data), .before = 1)
  
  list(
    
    indicators = ethnic_code_indicators(
      data, cols = {{ indicator_cols }}, id_cols = {{ id_cols }}, eth_codes = indicator_codes),
    text       = ethnic_code_text(      
      data, cols = {{ text_cols }}, id_cols = {{ id_cols }}, delim = text_delim, code_level = text_code_level, type = text_code_type, check = check) 
    
  ) |> 
    bind_rows() |> 
    select(c({{ id_cols }}, code)) |> 
    arrange({{ id_cols }})
  
}

#' Code ethnicity as per Statistics NZ guidelines: Wide
#'
#' @param data data frame containing ethnicity questions to code.
#' @param level_out Provide the StatsNZ classification level (1, 2, 3, or 4) that ethnicity is to be outputted as.
#' @param col_names Names of output columns. 
#'
#'
#' @importFrom tibble add_column
#' @export
#'
ethnic_code_wide <- function(data, id_cols = NULL, level_out = 1, col_names = ethnicity_level1_codes) {
  
  dat_eth_stand <- ethnic05$v2 |> 
    dplyr::select(matches(as.character(level_out))) |> 
    dplyr::distinct() |> 
    setNames(c("code", "label")) |> 
    mutate(var_name = factor(.data$code, levels = col_names, labels = names(col_names)))
  
  ## Add unique identifier for merging later.
  id_cols <- return_id(data, {{ id_cols }})
  
  if(id_cols == rlang::sym(".rowid"))
    data <- tibble::add_column(data, .rowid = 1:nrow(data), .before = 1)
  
  dat_out <- data |> 
    select({{ id_cols }}, code) |> 
    dplyr::mutate(code = substr(.data$code, 1, level_out) |> as.integer()) |> 
    distinct() |> 
    left_join(dat_eth_stand, by = "code") |> 
    mutate(value = TRUE) |> 
    select(c({{ id_cols }}, var_name, value)) |> 
    arrange(.data$var_name) |> 
    pivot_wider(names_from = "var_name", values_from = "value", values_fill = FALSE) |>  
    arrange({{ id_cols }})
  
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
#' @param add_cols Either a logical vector indicating whether to append the
#'   output to data collected, or a character vector containing the names of one
#'   or more variables to include in the output. Default is to return just the
#'   calculated ethnicity indicator variables.
#' @param eth_prior A character string indicating the name to give a column
#'   containing prioritised ethnicity. Default is not to produce column.
#' @param prior_order Numeric vector giving prioritisation order of columns
#'   listed in eth_levels. Defaults to c(2:6,1,7), ie. Maori, Pacific, Asian,
#'   MELAA, Other, European, and Unknown.
#' 
#' 
#' @importFrom tibble add_column
#' @export
#'
ethnic_code_all <- function(
    data, 
    indicator_cols, indicator_codes = census_2013_question_codes, 
    text_cols, text_delim = ",", text_code_level = 4, check = NULL,
    id_cols = NULL, 
    level_out = 1, col_names = ethnicity_level1_codes,
    add_cols = FALSE, eth_prior = NULL, prior_order = ethnicity_level1_prior_order,
    prior_labels = ethnicity_level1_labels) {
  
  ## Add unique identifier for merging later.
  id_cols <- return_id(data, {{ id_cols }})
  
  if(id_cols == rlang::sym(".rowid"))
    data <- tibble::add_column(data, .rowid = 1:nrow(data), .before = 1)
  
  
  dat_out <- ethnic_code_long(
    data = data, 
    indicator_cols = {{ indicator_cols }}, 
    indicator_codes = indicator_codes,
    text_cols = {{ text_cols }}, 
    text_delim = text_delim,
    text_code_level = text_code_level,
    check = check,
    id_cols = {{ id_cols }}
  ) |> 
    ethnic_code_wide(
      id_cols = {{ id_cols }}, level_out = level_out, col_names = col_names
    )
  
  # Add prioritised ethnicity
  if(!is.null(eth_prior)) {

    prior_names <- intersect(names(col_names)[prior_order], names(dat_out))
    
    dat_out[, eth_prior] <- factor(
      prior_names[max.col(dat_out[, prior_names], ties.method = "first")],
      levels = prior_names,
      labels = prior_labels[prior_names]
    )
  }

  
  # Add extra columns if specified
  if(add_cols == FALSE) {
    dat_out <- dat_out
  } else if(add_cols == TRUE) {
    dat_out <- data |> 
      dplyr::left_join(dat_out, by = join_by({{ id_cols }}))
  } else {
    dat_out <- data |> 
      dplyr::left_join(
        dat_out |> 
          select(c({{ id_cols }}, {{ add_cols }} ))
        , by = join_by({{ id_cols }}))
    
  }
    
  return(dat_out)
}

