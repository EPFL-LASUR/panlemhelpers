library(dplyr)

# This script is used to preprocess the "raw" data from the second wave of the Lemanique Panel.
# Running it will result in a collection of .tsv files that can be ingested in the PostgreSQL
# database via the python ingestion script.
library(dplyr)

source(here::here("R/utils.R"))

# Columns to be included in the participants file. This is currently more liberal than the table
# definition
participants_colnames <- c(
  "participant_code", "pays", "group", "gp_age_source", "numero_insee", "numero_ofs", "weight",
  "titre_source", "cp_source", "localite_source", "titre_actuel", "cp_actuel", "localite_actuel", "code_raison_contact_1_v2",
  "code_raison_contact_2_v2", "code_raison_contact_3_v2", "particip_v2"
)

# Columns to be included in the survey_metadata (also named survey_completion) table
survey_metadata_colnames <- c(
  "count_miss1", "count_miss2", "progress", "start_date", "end_date", "temps_minute"
)

# These are additional column names that are currently simply discarded for the MVP
# TODO: figure out with Panel team what to do with these additional variables.
extra_colnames <- c(
  "particip_avant_changements", "flag_troll", "suppression_suite_v2", "flag_chgmt_pays", "mobile_ordi",
  "avant_chgmt_vet", "flag_chgmt_localite_v2"
)

#' Documentation de la vague 2
#' 
#' Cette fonction permet d'obtenir une documentation de toutes les variables de la vague 2
#' 
#' @param data Tibble avec les données de la vague 2
#' @return Tibble avec la description de chaque variable
#' @export
get_documentation_wave2 <- function(data){
  
  question_codes <- colnames(data)
  question_text <- character(length(question_codes))
  question_labels <- character(length(question_codes))
  for (i in seq_along(question_codes)) {
    question_text[i] <- gsub(
      "[\r\n]", 
      " ", 
      ifelse(is.null(attr(get(question_codes[i], data), "label")), 
             NA_character_, 
             attr(get(question_codes[i], data), "label"))
    )
    
    question_labels[i] <- labels_to_string(data, i)
  }
  
  result <- tibble::tibble(question_code = question_codes, question_text = question_text, question_labels = question_labels)
  
  result
}

#' Questions de la vague 2
#' 
#' Cette fonction permet d'obtenir l'ensemble des questions posées aux participant·es de 
#' la vague 2 ainsi que la section du questionnaire à laquelle
#' elles appartiennent
#' 
#' @param data Tibble avec les données de la vague 2
#' @return Tibble avec la description de chaque question
#' @export
get_questions_wave2 <- function(data){
  questions <- data |>
    dplyr::select(
      -tidyselect::all_of(participants_colnames),
      -tidyselect::all_of(survey_metadata_colnames),
      -tidyselect::all_of(extra_colnames)
    )
  
  question_labels <- get_labels(questions, names_to = "question_code") |>
    dplyr::rename(label = name)
  question_labels <- remove_escapeseqs(question_labels)
  
  questions <- questions |>
    purrr::map(~ attr(.x, "label")) |>
    unlist() |>
    tibble::enframe(name = "question_code", value = "question_text")
  
  questions <- questions |>
    dplyr::mutate(section_name = stringr::str_extract(question_code, "^[:alpha:]+(?=\\_)")) |>
    dplyr::mutate(section_name = dplyr::case_match(
      section_name,
      "ali" ~ "Alimentation",
      "con" ~ "Consommation",
      "end" ~ "Satisfaction",
      "ene" ~ "Energie",
      "equ" ~ "Equipement",
      "log" ~ "Logement",
      "rep" ~ "Rep",
      "temp" ~ "Temperature",
      "vot" ~ "Votation"
    ))
  
  questions <- remove_escapeseqs(questions)
  
  return(list(questions = questions, labels = question_labels))
}

#' Section de la vague 2
#' 
#' Cette fonction liste les sections du questionnaire de la vague 2
#' 
#' @param data Tibble avec les données de la vague 2
#' @return Liste avec le nom des sections
#' @export
get_section_wave2 <- function(data){
  tmp <- get_questions_wave2(data)
  questions <- tmp$questions
  
  sections <- questions |>
    dplyr::select(section_name) |>
    dplyr::group_by(section_name) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup()
  
  sections
}

#' Participant·es de la vague 2
#' 
#' Cette fonction retourne les caractèristiques des participant·es de la vague 2
#' 
#' @param data Tibble avec les données de la vague 2
#' @return Tibble avec les caractèristiques de chaque participant·e
#' @export
get_participants_wave2 <- function(data){
  participants <- data |>
    dplyr::select(tidyselect::all_of(participants_colnames))
  
  participant_labels <- get_labels(participants)
  
  participants <- participants |>
    zap_all() |>
    dplyr::mutate(numero_insee = strtoi(numero_insee, base = 10L))
  
  return(list(participants = participants, labels = participant_labels))
}

#' Métadonnées de la vague 2
#' 
#' Cette fonction retourne les métadonnées de la vague 2
#' 
#' @param data Tibble avec les données de la vague 2
#' @return Tibble avec les métadonnées
#' @export
get_survey_completion_wave2 <- function(data){
  survey_completion <- data |>
    dplyr::select(
      participant_code, count_miss1, count_miss2, progress, start_date, end_date, temps_minute,
      flag_troll
    )
  
  survey_completion_labels <- get_labels(survey_completion)
  
  survey_completion <- survey_completion |>
    zap_all()
  
  return(list(survey_completion = survey_completion, labels = survey_completion_labels))
}

#' Réponses à la vague 2
#' 
#' Cette fonction renvoie les réponses de tout·es les participant·es à la vague 2
#' 
#' @param data Tibble avec les données de la vague 2
#' @return Tibble avec une ligne par réponse et par participant·e
#' @export
get_answers_wave2 <- function(data){
  responses <- data |>
    dplyr::select(
      -all_of(c(participants_colnames[-1], survey_metadata_colnames, extra_colnames))
    ) |>
    zap_all()
  
  response_texts <- pivot_responses(
    responses,
    selection_type = "character", remove_NAs = TRUE, names_to = "question_code",
    values_to = "response_text"
  ) |>
    remove_escapeseqs()
  
  response_values <- pivot_responses(
    responses,
    selection_type = "numeric", remove_NAs = TRUE, names_to = "question_code",
    values_to = "response_value"
  )
  
  responses <- response_values |>
    dplyr::bind_rows(response_texts)
  
  responses
}