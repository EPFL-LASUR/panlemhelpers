library(tibble)

source(here::here("R/utils.R"))

get_section_W1 <- function (question_number){
  sections_start <- c(12, 30, 37, 40, 61, 75, 77, 81, 85, 90, 107, 109, 115, 123, 129)
  sections <- c("W1- Permis, vehicules et abbonements", "W1- Multi-residence", "W1- Pratiques et moyens de déplacements", "W1- Loop Leman Express",
                "W1- Mobilites pour le travail et etudes", "W1- Stationnement et financement", "W1- Subventionnement abo", "W1- Mobilités hors du travail", 
                "W1- Excursions", "W1- Séjours", "W1- Opinions", "W1- Variables sociodémographiques 1/2", 
                "W1- Variabes sociodémographiques 2/2", "W1- Variables sociodémographiques_ménage", "W1- Opt-out")
  for (i in 1:length(sections_start)){
    if(question_number < sections_start[i]) return(sections[i])
  }
}

get_section_W3 <- function (question_number){
  sections <- c("W3- Permis et véhicules", "W3- Abonnements", "W3- Logement", "W3- Habitudes de déplacement", "W3- Travail/Formation", "W3- Stationnement et financement",
                "W3- Activités", "W3- Excursions", "W3- Séjours", "W3- Opinions", "W3- Données personnelles", "W3- Caractéristiques du ménage", "W3- Satisfaction")
  sections_start <- c(10, 13, 33, 40, 62, 78, 82, 86, 101, 108, 116, 336, 337)
  
  for (i in 1:length(sections_start)){
    if(question_number == 288) return("W3 - Moyens d'orientation")
    else if(question_number < sections_start[i]) return(sections[i])
  }
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

#' Documentation de la vague 3
#' 
#' Cette fonction permet d'obtenir une documentation de toutes les variables de la vague 3
#' 
#' @param data Tibble avec les données de la vague 3
#' @return Tibble avec la description de chaque variable
#' @export
get_documentation_wave3 <- function(data){
  
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

#' Questions de la vague 3
#' 
#' Cette fonction permet d'obtenir l'ensemble des questions posées aux participant·es de 
#' la vague 3 ainsi que la section du questionnaire à laquelle
#' elles appartiennent
#' 
#' @param data Tibble avec les données de la vague 3
#' @return Tibble avec la description de chaque question
#' @export
get_questions_wave3 <- function(data) {
  columns <- c()
  question_text <- c()
  section_name <- c()
  lien_avec_W1 <- c()
  
  for (i in colnames(data)) {
    if (stringr::str_detect(i, "^[Qq]\\d+|^W1_q\\d+")) {
      columns <- c(columns, i)
      
      label <- attr(data[[i]], "label")
      if (is.null(label)) {
        question_text <- c(question_text, NA_character_)
        message("No label found for column: ", i)
      } else {
        question_text <- c(question_text, gsub("[\n\t]", "", label))
      }
      
      num <- suppressWarnings(as.integer(stringr::str_extract(i, "(?i)(?<=q)\\d+")))
      
      if (stringr::str_detect(i, "Q320|Q285|Q291")) {
        section_name <- c(section_name, "W3- Travail/Formation")
      } else if (stringr::str_detect(i, "Q218")) {
        section_name <- c(section_name, "W3- Activités")
      } else if (stringr::str_detect(i, "^Q303(_\\d+)?$")) {
        section_name <- c(section_name, "W3- Séjours")
      } else if (stringr::str_detect(i, "Q129_R")) {
        section_name <- c(section_name, "W3- Satisfaction")
      } else if (stringr::str_detect(i, "W1")) {
        section_name <- c(section_name, get_section_W1(num))
      } else {
        section_name <- c(section_name, get_section_W3(num))
      }
      
      if (stringr::str_detect(i, "^Q325(_\\d+)?$")) {
        lien_avec_W1 <- c(lien_avec_W1, "W1_q117")
      } else if (stringr::str_detect(i, "^Q326(_\\d+)?$")) {
        lien_avec_W1 <- c(lien_avec_W1, "W1_q118")
      } else if (stringr::str_detect(i, "^Q327(_\\d+)?$")) {
        lien_avec_W1 <- c(lien_avec_W1, "W1_q119")
      } else if (stringr::str_detect(i, "^Q\\d+")) {
        prefix <- paste0("w1_q", num)  # lower
        pattern <- paste0("^", prefix, "(_|$)")
        
        matches <- tolower(names(data))[stringr::str_detect(tolower(names(data)), pattern)]
        
        if (length(matches) > 0) {
          original_match <- names(data)[tolower(names(data)) == matches[1]]
          lien_avec_W1 <- c(lien_avec_W1, original_match)
        } else {
          lien_avec_W1 <- c(lien_avec_W1, NA_character_)
        }
      } else {
        # Si ce n’est pas une Q de W3, alors pas de lien
        lien_avec_W1 <- c(lien_avec_W1, NA_character_)
      }
    }
  }
  
  result <- tibble::tibble(
    question_code = columns,
    question_text = question_text,
    section_name = section_name,
    lien_avec_W1 = lien_avec_W1
  )
  label <- get_label_table(data, columns, "questions")
  
  return(list(questions = result, labels = label))
}

#' Section de la vague 3
#' 
#' Cette fonction liste les sections du questionnaire de la vague 3
#' 
#' @return Liste avec le nom des sections
#' @export
get_section_wave3 <- function(){
  
  result <- tibble::tibble(
    section_name = c("Permis et véhicules", "Abonnements", "Logement", "Habitudes de déplacement", "Travail/Formation", "Stationnement et financement",
                     "Activités", "Excursions", "Séjours", "Opinions", "Données personnelles", "Caractéristiques du ménage", "Moyens d'orientation", "Satisfaction")
  )
  result
}

#' Métadonnées de la vague 3
#' 
#' Cette fonction retourne les métadonnées de la vague 3
#' 
#' @param data Tibble avec les données de la vague 3
#' @return Tibble avec les métadonnées
#' @export
get_survey_completion_wave3 <- function(data){
  
  columns <- c("participant_code")
  NAs <- rep(NA_character_,nrow(data["participant_code"]))
  
  participant_code <- data$participant_code
  
  result <- data |>
    dplyr::select(
      participant_code, countmiss, Progress, StartDate, EndDate, Temps_en_minutes
    )
  
 result
}

#' Participant·es de la vague 3
#' 
#' Cette fonction retourne les caractèristiques des participant·es de la vague 3
#' 
#' @param data Tibble avec les données de la vague 3
#' @return Tibble avec les caractèristiques de chaque participant·e
#' @export
get_participants_wave3 <- function(data) {
  columns <- c(
    "participant_code", "pays", "pays_w3", "group", "gp_age_source",
    "numero_insee", "numero_ofs", "date_naissance", "genre",
    "Titre_source", "CP_source", "Localité_source",
    "Titre_actuel", "CP_actuel", "Localité_actuel", "canton_dep",
    "AGGLO_CH_dom", "Suppression_suite_W3", "Suppression_Q129_W3C5",
    "wgt_agg_trim_95", "wgt_cant_trim_95", "wgt_agg_trim_98", "wgt_cant_trim_98",
    "wgt_agg_trim_99", "wgt_cant_trim_99"
  )
  
  contact_colnames <- names(data)[startsWith(names(data), "code_raison")]
  flag_colnames <- names(data)[startsWith(names(data), "Flag")]
  columns <- unique(c(columns, flag_colnames, contact_colnames))
  
  result <- dplyr::select(data, dplyr::all_of(columns))
  
  result$Weight <- NA_character_
  
  label <- get_label_table(data, columns, "participant")
  
  return(list(participants = result, labels = label))
}

#' Réponses à la vague 3
#' 
#' Cette fonction renvoie les réponses de tout·es les participant·es à la vague 3
#' 
#' @param data Tibble avec les données de la vague 3
#' @return Tibble avec une ligne par réponse et par participant·e
#' @export
get_answers_wave3 <- function(data){
  
  participants_colnames <- c("pays","pays_w3", "Weight", "Particip_v3Mob", "countmiss", "Progress", "Temps_en_minutes", "group","gp_age_source","numero_insee","numero_ofs", "date_naissance", 
                             "genre","Titre_source", "CP_source","Localité_source", "Titre_actuel", "CP_actuel", "Localité_actuel", "canton_dep", "AGGLO_CH_dom", "Suppression_suite_W3")
  flag_colnames <- names(data)[grep("^Flag", names(data))]
  embedded_colnames <- names(data)[grep("^embedded", names(data), ignore.case = TRUE)]
  ponderation <- c("wgt_agg_trim_95", "wgt_cant_trim_95", "wgt_agg_trim_98", "wgt_cant_trim_98", "wgt_agg_trim_99", "wgt_cant_trim_99")
  responses <- data |>
    dplyr::select(
      -all_of(c(participants_colnames, ponderation, flag_colnames, embedded_colnames))
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