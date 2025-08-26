#' Creates a string from a question's labels
#'
#' @param data tibble?, SPSS dataset
#' @param index int, index of the question
#' 
#' @return string
labels_to_string <- function(data, index){
  tryCatch(
    {
      obj <- get_labels(data[index])
      res = ""
      for (i in 1:length(row(obj[,"name"]))){
        temp <- paste(obj[i,"value"], obj[i, "name"], sep=":")
        if(i == 1){
          res <- temp
        }else{
          res <- paste(res, temp, sep = ", ")
        }
      }
      return(res)
    },
    error=function(e){
      return("")
    }
  )
}

#' Apply all zaps on data frame
#'
#' \code{zap_all} applies all \code{haven::zap_*} functions except zap_empty to the data frame
#' passed as argument.
#'
#' @param x Data frame
#'
#' @return Data frame
#'
zap_all <- function(x) {
  assertthat::assert_that(is.data.frame(x))

  x |>
    haven::zap_labels() |>
    haven::zap_formats() |>
    haven::zap_label() |>
    haven::zap_widths()
}

#' Extract the labels from labelled columns in a data frame
#'
#' \code{get_labels} creates a data frame with one row per question variable and labelled value.
#'
#' @param x Data frame
#' @param names_to String, specifies the name of the column, in the output data frame, storing the
#'  names of labelled columns of x
#' @param name String, specifies the name of the column \code{tibble::enframe} uses to store the
#'  name.
#' @param value String, specifies the name of the column \code{tibble::enframe} uses to store the
#'  value.
#'
#' @inheritParams rlang::args_dots_used
#'
#' @return Data frame
#'
get_labels <- function(
    x, ..., names_to = "variable_name", name = "name", value = "value") {
  assertthat::assert_that(is.data.frame(x))
  rlang::check_dots_empty()
  assertthat::assert_that(
    rlang::is_string(names_to), rlang::is_string(name), rlang::is_string(value)
  )

  x <- x |>
    dplyr::select(tidyselect::where(haven::is.labelled))

  if (ncol(x) == 0) {
    return(tibble::tibble(
      "{names_to}" := character(),
      "{name}" = character(), "{value}" = integer()
    ))
  }

  # Convert the labels to a list of data frames and bind the data frames by rows
  label_list <- purrr::imap(
    x,
    ~ tibble::enframe(attr(.x, "labels"), name = name, value = value) |>
      dplyr::mutate(
        !!name := stringr::str_squish(
          stringr::str_replace_all(.data[[name]], "(\n|\t)", " ")
          ),
        !!names_to := .y
      ) |>
      dplyr::select(!!names_to, !!name, !!value)
  )
  value_types <- purrr::map_chr(label_list, ~ typeof(.x[[value]]))
  unique_types <- unique(value_types)
  
  if (length(unique_types) > 1) {
    label_list <- purrr::map(label_list, ~ dplyr::mutate(.x, !!value := as.character(.data[[value]])))
  }
  
  dplyr::bind_rows(label_list)
}

#' Remove specific escape sequences
#'
#' \code{remove_escapeseqs} allows the user to remove escape sequences from the text. This is
#' particularly useful when \n or \t have been inserted, leading to issues when saving to .tsv.
#'
#' @param x Data frame
#' @param characters Character vector, specifies which escape characters should be removed from
#'  character columns of x.
#'
#' @inheritParams rlang::args_dots_used
#'
#' @return Data frame, with the specified escape characters removed from character columns
#'
remove_escapeseqs <- function(x, ..., characters = c("\n", "\t")) {
  assertthat::assert_that(is.data.frame(x))
  rlang::check_dots_empty()
  assertthat::assert_that(rlang::is_character(characters))
  # TODO: add check that all characters start with a backslash

  x |>
    dplyr::mutate(dplyr::across(
      tidyselect::where(is.character),
      ~ stringr::str_remove_all(.x, sprintf("(%s)", stringr::str_c(characters, collapse = "|")))
    ))
}


#' Pivot responses data frame to longer format
#'
#' \code{pivot_responses} pivots the responses data frame to a longer format, with one row per
#' sample x question.
#'
#' @param x Data frame
#' @param id_column String, specifies the column containing the sample id
#' @param selection_type String, specifies whether character or numeric columns should be
#'  pivoted.
#' @param remove_nas Boolean, specifies whether rows with NA should be removed
#' @param names_to String, specifies the name of the column in which the question_codes will be
#'  stored. Default is "name".
#' @param values_to String, specifies the name of the column in which the response text or value
#'  will be stored. Default is "value".
#'
#' @inheritParams rlang::args_dots_used
#'
#' @return Data frame, with the specified escape characters removed from character columns
#'
pivot_responses <- function(
    x,
    ...,
    id_column = "participant_code",
    selection_type = c("character", "numeric"),
    remove_nas = TRUE,
    names_to = "name",
    values_to = "value") {
  assertthat::assert_that(
    rlang::is_string(id_column), rlang::is_string(names_to), rlang::is_string(values_to)
  )
  rlang::arg_match(selection_type)
  assertthat::assert_that(rlang::is_bool(remove_nas))

  selection_function <- ifelse(
    selection_type == "character", rlang::is_character, is.numeric
  )

  y <- x |>
    dplyr::select(tidyselect::all_of(id_column), tidyselect::where(selection_function)) |>
    tidyr::pivot_longer(
      cols = -tidyselect::all_of(id_column), names_to = names_to, values_to = values_to
    )

  if (remove_nas) {
    y <- dplyr::filter(y, !is.na(.data[[values_to]]))
  }

  y
}

#' removes whitespaces at the begining of a string
#'
#' @param x string
remove_whitespace <- function(x){
  stringr::str_remove_all(x, "(^\\s+|\\s+$)")
}

# Extracts and formats geodata into lat,lng
extract_lat_lng <- function(json_string) {
  if (is.na(json_string) || json_string == "") return(json_string)
  tryCatch({
    parsed <- jsonlite::fromJSON(json_string)
    lat <- parsed[[1]]$lat
    lng <- parsed[[1]]$lng
    paste(lat, lng, sep = ",")
  }, error = function(e) {
    json_string
  })
}

# Detects if a string is coordinates
is_coordinates <- function(x) {
  grepl("^\\s*-?\\d+\\.?\\d*\\s*,\\s*-?\\d+\\.?\\d*\\s*$", x)
}

# Transforme les valeurs décimales en valeurs entière pour OPAL
double_to_integer <- function(data) {
  data[] <- lapply(data, function(x) {
    if (haven::is.labelled(x) && is.numeric(x)) {
      haven::labelled(as.integer(round(as.numeric(x))), labels = attr(x, "labels"), label = attr(x, "label"))
    } else if (is.numeric(x)) {
      haven::labelled(as.integer(round(x)), label = attr(x, "label"))
    } else x
  })
  data
}

