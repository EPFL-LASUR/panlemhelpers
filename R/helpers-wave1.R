library(dplyr)

source(here::here("R/utils.R"))

get_section <- function (question_number){
  sections_start <- c(12, 30, 37, 40, 61, 75, 77, 81, 85, 90, 107, 109, 115, 123, 131)
  sections <- c("Permis, véhicules et abbonements", "Multi-résidence", "Pratiques et moyens de déplacements", "Loop Leman Express",
                "Mobilités pour le travail et études", "Stationnement et financement", "Subventionnement abo", "Mobilités hors du travail", 
                "Excursions", "Séjours", "Opinions", "Variables sociodémographiques 1/2", "Variabes sociodémographiques 2/2",
                "Variables sociodémographiques_ménage", "Opt-out")
  for (i in 1:length(sections_start)){
    if(question_number < sections_start[i]) return(sections[i])
    else if (question_number == 218) return("Mobilités hors du travail")
  }
  return(NA_character_)
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

#' Documentation de la vague 1
#' 
#' Cette fonction permet d'obtenir une documentation de toutes les variables de la vague 1
#' 
#' @param data Tibble avec les données de la vague 1
#' @return Tibble avec la description de chaque variable
#' @export
get_documentation_wave1 <- function(data){
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

#' Section de la vague 1
#' 
#' Cette fonction liste les sections du questionnaire de la vague 1
#' 
#' @return Liste avec le nom des sections
#' @export
get_section_wave1 <- function(){
  result <- tibble::tibble(
    section_name = c("Permis, véhicules et abbonements", "Multi-résidence", "Pratiques et moyens de déplacements", "Loop Leman Express", "Mobilités pour le travail et études", "Stationnement et financement", "Subventionnement abo", "Mobilités hors du travail", 
                     "excursions", "Séjours", "Opinions", "Variables sociodémographiques 1/2", "Variabes sociodémographiques 2/2", "Variables sociodémographiques_ménage", "Opt-out")
  )
  result
}

#' Participant·es de la vague 1
#' 
#' Cette fonction retourne les caractèristiques des participant·es de la vague 1
#' 
#' @param data Tibble avec les données de la vague 1
#' @return Tibble avec les caractèristiques de chaque participant·e
#' @export
get_participants_wave1 <- function(data){
  columns <- c("participant_code","pays", "group", "date_naissance", "genre", "gp_age_source","numero_insee","numero_ofs", "GG_CP", "Titre_échantillon",
               "CP_source","Localité_source", "CP_actuel", "Localité_actuel", "suppression_suite"
  )
  
  contact_colnames <- names(data)[startsWith(names(data), "code_raison")]
  check_colnames <- names(data)[startsWith(names(data), "check")]
  flag_colnames <- names(data)[startsWith(names(data), "Flag")]
  columns <- unique(c(columns, flag_colnames, contact_colnames, check_colnames))
  
  result <- dplyr::select(data, dplyr::all_of(columns))
  label <- get_label_table(data, columns, "participants")
  
  return(list(participants = result, labels = label))
}

#' Questions de la vague 1
#' 
#' Cette fonction permet d'obtenir l'ensemble des questions posées aux participant·es de 
#' la vague 1 ainsi que la section du questionnaire à laquelle
#' elles appartiennent
#' 
#' @param data Tibble avec les données de la vague 1
#' @return Tibble avec la description de chaque question
#' @export
get_questions_wave1 <- function(data){
  columns <- c()
  question_text <- c()
  section_name <- c()
  
  for(i in colnames(data)){
    if(stringr::str_detect(i, "^Q\\d*.*$")){
      columns <- c(columns, i)
      question_text <- c(question_text, gsub("\n","",attr(get(i, data), "label")))
      section_name <- c(section_name, get_section(as.integer(stringr::str_extract(i,"\\d+"))))
    }
  }
  result <- tibble::tibble(
    question_code = columns,
    question_text = question_text,
    section_name =  section_name
  )
  label <- get_label_table(data, columns, "questions")
  
  return(list(questions = result, labels = label))
}

#' Métadonnées de la vague 1
#' 
#' Cette fonction retourne les métadonnées de la vague 1
#' 
#' @param data Tibble avec les données de la vague 1
#' @return Tibble avec les métadonnées
#' @export
get_survey_completion_wave1 <- function(data){
  columns <- c("participant_code", "countmiss", "Progress", "StartDate", "EndDate", "Temps_en_Minutes",
               "meta_Browser", "meta_Version", "meta_Operating_System", "meta_Resolution",
               "meta_end_Browser", "meta_end_Version", "meta_end_Operating_System", "meta_end_Resolution"
  )
  
  result <- data |>
    dplyr::select(all_of(columns))
  label <- get_label_table(data, columns, "survey_completion")
  
  return(list(survey_completion = result, labels = label))
}

#' Réponses à la vague 1
#' 
#' Cette fonction renvoie les réponses de tout·es les participant·es à la vague 1
#' 
#' @param data Tibble avec les données de la vague 1
#' @return Tibble avec une ligne par réponse et par participant·e
#' @export
get_answers_wave1 <- function(data){
  columns <- c("pays", "group", "date_naissance", "genre", "gp_age_source","numero_insee","numero_ofs", "GG_CP", "Titre_échantillon",
               "CP_source","Localité_source", "CP_actuel", "Localité_actuel", "suppression_suite"
  )
  survey_completion <- c("countmiss", "Progress", "StartDate", "EndDate", "Temps_en_Minutes",
                         "meta_Browser", "meta_Version", "meta_Operating_System", "meta_Resolution",
                         "meta_end_Browser", "meta_end_Version", "meta_end_Operating_System", "meta_end_Resolution"
  )
  contact_colnames <- names(data)[startsWith(names(data), "code_raison")]
  check_colnames <- names(data)[startsWith(names(data), "check")]
  flag_colnames <- names(data)[startsWith(names(data), "Flag")]
  
  columns <- unique(c(columns, flag_colnames, contact_colnames, check_colnames, survey_completion))
  
  responses <- data |>
    dplyr::select(
      -all_of(c(columns))
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
  ) |>
    remove_escapeseqs()
  
  response_values <- pivot_responses(
    responses,
    id_column = "participant_code",
    selection_type = "numeric", remove_nas = TRUE, names_to = "question_code",
    values_to = "response_value"
  )
  
  responses <- response_values |>
    dplyr::bind_rows(response_texts) |> 
    dplyr::arrange(participant_code)
  
  responses
}