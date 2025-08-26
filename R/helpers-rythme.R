library(dplyr)

source(here::here("R/utils.R"))

get_section <- function (section_number){
  section_name = c("1. Données personnelles", "2. Rythmes de mobilité et satisfaction lors des temps de déplacement", "3. Mobilité nocturne et sentiment d'insécurité",
                   "4. Recrutement", "5. Satisfatction et remarques")
  return(section_name[section_number+1])
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

#' Documentation de la vague rythme
#' 
#' Cette fonction permet d'obtenir une documentation de toutes les variables de la vague rythme
#' 
#' @param data Tibble avec les données de la vague rythme
#' @return Tibble avec la description de chaque variable
#' @export
get_documentation_rythme <- function(data){
  
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

#' Section de la vague rythme
#' 
#' Cette fonction liste les sections du questionnaire de la vague rythme
#' 
#' @return Liste avec le nom des sections
#' @export
get_section_rythme <- function(){
  
  result <- tibble::tibble(
    section_name = c("1. Données personnelles", "2. Rythmes de mobilité et satisfaction lors des temps de déplacement", "3. Mobilité nocturne et sentiment d'insécurité",
                     "4. Recrutement", "5. Satisfatction et remarques")
  )
  result
}

#' Questions de la vague rythme
#' 
#' Cette fonction permet d'obtenir l'ensemble des questions posées aux participant·es de 
#' la vague rythme ainsi que la section du questionnaire à laquelle
#' elles appartiennent
#' 
#' @param data Tibble avec les données de la vague rythme
#' @return Tibble avec la description de chaque question
#' @export
get_questions_rythme <- function(data) {
  
  columns <- c()
  question_text <- c()
  section_name <- c()
  
  for (i in colnames(data)) {
    if (stringr::str_detect(i, "^P\\d+_")) {
      columns <- c(columns, i)
      
      label <- attr(data[[i]], "label")
      if (is.null(label)) {
        question_text <- c(question_text, NA_character_)
        message("No label found for column: ", i)
      } else {
        question_text <- c(question_text, gsub("[\n\t]", "", label))
      }
      
      num <- suppressWarnings(as.integer(stringr::str_remove(stringr::str_extract(i, "^P\\d+"), "^P")))
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

#' Métadonnées de la vague rythme
#' 
#' Cette fonction retourne les métadonnées de la vague rythme
#' 
#' @param data Tibble avec les données de la vague rythme
#' @return Tibble avec les métadonnées
#' @export
get_survey_completion_rythme <- function(data){
  
  columns <- c("participant_code")
  NAs <- rep(NA_character_,nrow(data["participant_code"]))
  
  participant_code <- data$participant_code
  
  result <- data |>
    dplyr::select(
      participant_code, countmiss, Progress, Date_recode, StartDate, EndDate, Temps_en_minutes
    )
  result
}

#' Participant·es de la vague rythme
#' 
#' Cette fonction retourne les caractèristiques des participant·es de la vague rythme
#' 
#' @param data Tibble avec les données de la vague rythme
#' @return Tibble avec les caractèristiques de chaque participant·e
#' @export
get_participants_rythme <- function(data) {
  columns <- c(
    "participant_code", "pays_W1", "pays_c5", "group", "gp_age_source", "numero_insee", "numero_ofs", "weight",
    "P0_date_naissance", "date_naissance_W1", "check_age", "P0_genre", "genre_W1", "check_genre", "titre_source", 
    "cp_source", "localite_source", "titre_actuel", "cp_actuel", "localite_actuel"
  )
  
  contact_colnames <- names(data)[startsWith(names(data), "code_raison")]
  participation_colnames <- names(data)[startsWith(names(data), "Particip")]
  flag_colnames <- names(data)[startsWith(names(data), "Flag")]
  columns <- unique(c(columns, flag_colnames, contact_colnames, participation_colnames))
  
  result <- dplyr::select(data, dplyr::all_of(columns))
  
  result <- result %>%
    dplyr::rename(
      date_naissance = P0_date_naissance,
      genre = P0_genre
    )
  
  result$weight <- NA_character_
  
  label <- get_label_table(data, columns, "participant")
  
  return(list(participants = result, labels = label))
}

#' Réponses à la vague rythme
#' 
#' Cette fonction renvoie les réponses de tout·es les participant·es à la vague rythme
#' 
#' @param data Tibble avec les données de la vague rythme
#' @return Tibble avec une ligne par réponse et par participant·e
#' @export
get_answers_rythme <- function(data){
  
  participants_colnames <- c(
    "pays_W1", "pays_c5", "group", "Progress", "Temps_en_minutes", "countmiss", "gp_age_source", "numero_insee", "numero_ofs", "weight",
    "date_naissance_W1", "check_age", "genre_W1", "check_genre", "titre_source", 
    "cp_source", "localite_source", "titre_actuel", "cp_actuel", "localite_actuel"
  )
  flag_colnames <- names(data)[grep("^Flag", names(data))]
  contact_colnames <- names(data)[startsWith(names(data), "code_raison")]
  participation_colnames <- names(data)[startsWith(names(data), "Particip")]
  responses <- data |>
    dplyr::select(
      -all_of(c(participants_colnames, contact_colnames, flag_colnames, participation_colnames))
    ) |>
    zap_all()
  
  # problem with : CP_actuel(all numbers but in text somehow), Q19CH_1_1 (sometimes string and sometimes numbers in dataset), Q19CH_2_1 (7050-011 ??)
  
  responses <- responses |> 
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~ remove_whitespace(.x))) |>
    dplyr::mutate(dplyr::across(tidyselect::everything(), ~ readr::parse_guess(as.character(.x))))
  
  responses <- responses |>
    dplyr::mutate(dplyr::across(
      tidyselect::where(is.character),
      ~ stringr::str_remove_all(.x, "[\r\n\t]")
    ))
  
  
  response_texts <- pivot_responses(
    responses,
    id_column = "participant_code",
    selection_type = "character", remove_nas = TRUE, names_to = "question_code",
    values_to = "response_text"
  ) 
  
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