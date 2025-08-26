library(tibble)

source(here::here("R/utils.R"))

get_section <- function (question_code){
  
  num <- as.numeric(sub("^Q(\\d+).*", "\\1", question_code))
  
  if (num == 1) return("1. Qualité de vie et actions nécessaires")
  if (num == 2) return("2. Cadre de vie idéal")
  if (num == 3) return("3. Acceptabilité sociale de divers aménagements")
  if (num == 4) return("4. Questions sociodémographiques supplémentaires")
  if (num >= 26 && num <= 27) return("5. Fin")
  
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

#' Documentation de la vague PDCn
#' 
#' Cette fonction permet d'obtenir une documentation de toutes les variables de la vague PDCn
#' 
#' @param data Tibble avec les données de la vague PDCn
#' @return Tibble avec la description de chaque variable
#' @export
get_documentation_pdcn <- function(data){
  
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

#' Section de la vague PDCn
#' 
#' Cette fonction liste les sections du questionnaire de la vague PDCn
#' 
#' @return Liste avec le nom des sections
#' @export
get_section_pdcn <- function(){
  
  result <- tibble::tibble(
    section_name = c("1. Qualité de vie et actions nécessaires", "2, Cadre de vie idéal", "3. Acceptabilité sociale de divers aménagements",
                     "4. Questions sociodémographiques supplémentaires", "5. Fin")
  )
  result
}

#' Questions de la vague PDCn
#' 
#' Cette fonction permet d'obtenir l'ensemble des questions posées aux participant·es de 
#' la vague PDCn ainsi que la section du questionnaire à laquelle
#' elles appartiennent
#' 
#' @param data Tibble avec les données de la vague PDCn
#' @return Tibble avec la description de chaque question
#' @export
get_questions_pdcn <- function(data) {
  
  columns <- c()
  question_text <- c()
  section_name <- c()
  
  for (i in colnames(data)) {
    if (stringr::str_detect(i, "^Q\\d+")) {
      columns <- c(columns, i)
      
      label <- attr(data[[i]], "label")
      if (is.null(label)) {
        question_text <- c(question_text, NA_character_)
        message("No label found for column: ", i)
      } else {
        question_text <- c(question_text, gsub("[\n\t]", "", label))
      }
      
      num <- suppressWarnings(as.integer(sub("^Q(\\d+).*", "\\1", i)))
      section_name <- c(section_name, get_section(num))
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

#' Métadonnées de la vague PDCn
#' 
#' Cette fonction retourne les métadonnées de la vague PDCn
#' 
#' @param data Tibble avec les données de la vague PDCn
#' @return Tibble avec les métadonnées
#' @export
get_survey_completion_pdcn <- function(data){
  
  columns <- c("participant_code")
  NAs <- rep(NA_character_,nrow(data["participant_code"]))
  
  participant_code <- data$participant_code
  
  result <- data |>
    dplyr::select(
      participant_code, countmiss, Progress, StartDate, EndDate, Temps_en_minutes
    )
  result
}

#' Participant·es de la vague PDCn
#' 
#' Cette fonction retourne les caractèristiques des participant·es de la vague PDCn
#' 
#' @param data Tibble avec les données de la vague PDCn
#' @return Tibble avec les caractèristiques de chaque participant·e
#' @export
get_participants_pdcn <- function(data) {
  columns <- c(
    "participant_code", "pays_W1", "pays_vPDCN", "group", "gp_age_source", "numero_insee", "numero_ofs",
    "date_naissance", "genre", "titre_source", "cp_source", "localite_source", "titre_actuel", "cp_actuel",
    "localite_actuel", "Suppression_suite_vPDCN"
  )
  
  contact_colnames <- names(data)[startsWith(names(data), "code_raison")]
  participation_colnames <- names(data)[startsWith(names(data), "Particip")]
  flag_colnames <- names(data)[startsWith(names(data), "Flag")]
  ponderation_colnames <- names(data)[startsWith(names(data), "wgt")]
  columns <- unique(c(columns, flag_colnames, contact_colnames, participation_colnames, ponderation_colnames))
  
  result <- dplyr::select(data, dplyr::all_of(columns))
  label <- get_label_table(data, columns, "participants")
  
  return(list(participants = result, labels = label))
}

#' Réponses à la vague PDCn
#' 
#' Cette fonction renvoie les réponses de tout·es les participant·es à la vague PDCn
#' 
#' @param data Tibble avec les données de la vague PDCn
#' @return Tibble avec une ligne par réponse et par participant·e
#' @export
get_answers_pdcn <- function(data){
  
  participants_colnames <- c(
    "pays_W1", "pays_vPDCN", "group", "gp_age_source", "numero_insee", "numero_ofs", "weight", "Bloc",
    "date_naissance", "genre", "titre_source", "cp_source", "localite_source", "titre_actuel", "cp_actuel",
    "localite_actuel", "Suppression_suite_vPDCN"
  )
  
  survey_completion_colnames <- c("countmiss", "Progress", "StartDate", "EndDate", "Temps_en_minutes")
  
  flag_colnames <- names(data)[grep("^Flag", names(data))]
  contact_colnames <- names(data)[startsWith(names(data), "code_raison")]
  participation_colnames <- names(data)[startsWith(names(data), "Particip")]
  ponderation_colnames <- names(data)[startsWith(names(data), "wgt")]
  
  responses <- data |>
    dplyr::select(
      -all_of(c(participants_colnames, contact_colnames, flag_colnames, participation_colnames, survey_completion_colnames))
    ) |>
    zap_all()
  
  
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