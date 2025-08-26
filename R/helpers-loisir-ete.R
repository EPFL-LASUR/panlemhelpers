library(tibble)
library(dplyr)

source(here::here("R/utils.R"))

get_section <- function (question_code){
  if (grepl("_END$", question_code)) {
    return("6. Fin")
  }
  
  if (grepl("_V1$", question_code)) {
    return("Question de la vague 1")
  }
  
  if (!grepl("^Q[0-9]+(\\.[0-9]+)?", question_code)) return(NA)
  num <- as.numeric(sub("^Q([0-9]+(\\.[0-9]+)?).*", "\\1", question_code))
  
  if (num >= 34 && num < 43) {
    decimal <- as.numeric(sub("^Q[0-9]+\\.(\\d+).*", "\\1", question_code))
    if (!is.na(decimal)) {
      return(switch(as.character(decimal),
                    "1" = "4.1 Aller chez des ami·es, la famille",
                    "2" = "4.2 Restaurants, bars, …",
                    "3" = "4.3 Activités culturelles",
                    "4" = "4.4 Activité de divertissement et de consommation",
                    "5" = "4.5 Activités sportives et de détente en extérieur",
                    "6" = "4.6 Sports sur terrain ou en salle",
                    "4. Les loisirs"
      ))
    }
    return("4. Les loisirs")
  }
  
  if (num >= 1 && num <= 4) return("1. L’emploi du temps estival")
  if (num >= 5 && num <= 21) return("2. Les courts séjours")
  if (num >= 22 && num <= 32) return("3. Les excursions")
  if (num >= 33 && num <= 42 || num == 131) return("4. Les loisirs")
  if (num >= 43 && num <= 45) return("5. Données personnelles")
  
  
  return(NA)
}


get_label_table <- function(data, cols, name = "participants"){
  
  cols_selected <- data |>
    dplyr::select(tidyselect::all_of(cols))
  
  result <- get_labels(cols_selected)
  
  if(name == "questions"){
    result <- result |>
      dplyr::rename(
        question_code = variable_name,
        label = name
      )
  }
  empty <- 1
  for(i in result){
    if(length(i) != 0){
      empty = 0
      break
    }
  }
  
  if(empty){
    result <- tibble::tibble()
  }
  result
}

#' Documentation de la vague loisirs estivaux
#' 
#' Cette fonction permet d'obtenir une documentation de toutes les variables de la vague sur les loisirs estivaux
#' 
#' @param data Tibble avec les données de la vague sur les loisirs estivaux
#' @return Tibble avec la description de chaque variable
#' @export
get_documentation_ete <- function(data){
  
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

#' Questions de la vague loisirs estivaux
#' 
#' Cette fonction permet d'obtenir l'ensemble des questions posées aux participant·es de 
#' la vague sur les loisirs estivaux ainsi que la section du questionnaire à laquelle
#' elles appartiennent
#' 
#' @param data Tibble avec les données de la vague sur les loisirs estivaux
#' @return Tibble avec la description de chaque question
#' @export
get_questions_ete <- function(data) {
  
  columns <- c()
  question_text <- c()
  section_name <- c()
  
  for (i in colnames(data)) {
    if (stringr::str_detect(i, "[Qq]\\d+")) {
      columns <- c(columns, i)
      
      label <- attr(data[[i]], "label")
      if (is.null(label)) {
        question_text <- c(question_text, NA_character_)
        message("No label found for column: ", i)
      } else {
        question_text <- c(question_text, gsub("[\n\t]", "", label))
      }
      
      section_name <- c(section_name, get_section(i))
    }
  }
  
  result <- tibble::tibble(
    question_code = columns,
    question_text = question_text,
    section_name = section_name
  )
  label <- get_label_table(data, columns, "questions")
  
  return(list(questions = result, labels = label))
}

#' Section de la vague loisirs estivaux
#' 
#' Cette fonction liste les sections du questionnaire de la vague loisirs estivaux
#' 
#' @return Liste avec le nom des sections
#' @export
get_section_ete <- function(){
  
  result <- tibble::tibble(
    section_name = c("1. L'emploi du temps estival", "2. Les courts séjours", "3. Les excursions", "4. Les loisirs", "4.1 Aller chez des ami·es, la famille",
                     "4.2 Restaurants, bars,...", "4.3 Activités culturelles", "4.4 Activités de divertissement et de consommation", "4.5 Activités sportives et de détente en extérieur",
                     "4.6 Sports sur terrain ou en salle", "5. Données personnelles", "6. Fin")
  )
  
  result
}

#' Participant·es de la vague loisirs estivaux
#' 
#' Cette fonction retourne les caractèristiques des participant·es de la vague loisirs estivaux
#' 
#' @param data Tibble avec les données de la vague sur les loisirs estivaux
#' @return Tibble avec les caractèristiques de chaque participant·e
#' @export
get_participants_ete <- function(data) {
  columns <- c(
    "participant_code", "pays", "group", "gp_age_source", "numero_ofs", "Q43", "Q107_V1", "Genre_actuel",
    "revenu", "age", "formation", "permis_auto", "titre", "GG_CP", "suppression_suite_W2", "suppression_suite_loisirs",
    "check_age", "filter_$", "mode_travail_unique", "dist_domtrav_cat", "dist_domform_cat", "dist_domachats_cat",
    "type_menage", "desserte_dom"
  )
  
  flag_colnames <- names(data)[startsWith(names(data), "Flag")]
  raison_contact_colnames <- names(data)[startsWith(names(data), "code_raison")]
  columns <- unique(c(columns, flag_colnames, raison_contact_colnames))
  
  result <- dplyr::select(data, dplyr::all_of(columns))
  
  names(result)[names(result) == "Q43"] <- "Q43 (date_naissance)"
  names(result)[names(result) == "Q107_V1"] <- "Q107_V1 (genre_W1)"
  
  label <- get_label_table(data, columns, "participant")

  return(list(participants = result, labels = label))  
}

#' Métadonnées de la vague loisirs estivaux
#' 
#' Cette fonction retourne les métadonnées de la vague sur les loisirs estivaux
#' 
#' @param data Tibble avec les données de la vague sur les loisirs estivaux
#' @return Tibble avec les métadonnées
#' @export
get_survey_completion_ete <- function(data){
  
  columns <- c("participant_code", "countmiss", "Progress", "StartDate", "EndDate", "Temps_minutes", "meta_Browser", "meta_Version",
               "meta_Operating_System", "meta_Resolution", "meta_end_Browser", "meta_end_Version", "meta_end_Operating_System", "meta_end_Resolution"
  )
  
  result <- data |>
    dplyr::select(dplyr::all_of(columns))
  
  label <- get_label_table(data, columns, "survey_completion")
  
  return(list(survey_completion = result, labels = label))
}

#' Réponses à la vague loisirs estivaux
#' 
#' Cette fonction renvoie les réponses de tout·es les participant·es à la vague loisirs estivaux
#' 
#' @param data Tibble avec les données de la vague sur les loisirs estivaux
#' @return Tibble avec une ligne par réponse et par participant·e
#' @export
get_answers_ete <- function(data){
  
  participants_colnames <- c(
    "pays", "group", "gp_age_source", "numero_ofs", "Q43", "Q107_V1", "Genre_actuel", "Weight",
    "revenu", "age", "formation", "permis_auto", "titre", "GG_CP", "suppression_suite_W2", "suppression_suite_loisirs",
    "check_age", "filter_$", "mode_travail_unique", "dist_domtrav_cat", "dist_domform_cat", "dist_domachats_cat",
    "type_menage", "desserte_dom"
  )
  
  survey_completion_colnames <- c(
    "countmiss", "Progress", "StartDate", "EndDate", "Temps_minutes", "meta_Browser", "meta_Version",
    "meta_Operating_System", "meta_Resolution", "meta_end_Browser", "meta_end_Version", "meta_end_Operating_System", "meta_end_Resolution"
  )
  
  flag_colnames <- names(data)[grep("^Flag", names(data))]
  contact_colnames <- names(data)[startsWith(names(data), "code_raison")]
  participation_colnames <- names(data)[startsWith(names(data), "Particip")]
  participation_colnames <- c(participation_colnames, "filter_$")
  
  responses <- data |>
    dplyr::select(
      -all_of(c(participants_colnames, contact_colnames, flag_colnames, participation_colnames, survey_completion_colnames))
    ) |>
    zap_all()
  
  # problem with : CP_actuel(all numbers but in text somehow), Q19CH_1_1 (sometimes string and sometimes numbers in dataset), Q19CH_2_1 (7050-011 ??)
  
  responses <- responses |> 
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~ remove_whitespace(.x))) |>
    dplyr::mutate(dplyr::across(tidyselect::everything(), ~ readr::parse_guess(as.character(.x))))
  
  response_texts <- pivot_responses(
    responses,
    id_column = "participant_code",
    selection_type = "character", remove_nas = TRUE, names_to = "question_code",
    values_to = "response_text"
  )  |>
    remove_escapeseqs()
  
  response_values <- pivot_responses(
    responses,
    id_column = "participant_code",
    selection_type = "numeric", remove_nas = TRUE, names_to = "question_code",
    values_to = "response_value"
  ) |>
    remove_escapeseqs()
  
  responses <- response_values |>
    dplyr::bind_rows(response_texts) |>
    dplyr::arrange(participant_code)
  
  responses
}