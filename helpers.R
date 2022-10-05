# Qualtrics Visualizer - Helper functions # 

## function to convert from one tab to another ## 
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

# function to load survey from Qualtrics API ------------------------------
# this is putting data into the global environment upon download from the API
# design; colmap; survey questions; survey choices; and survey response data
load_survey <- function(sid) {
  pacman::p_load(tidyverse, here, janitor, glue, qsurvey, qualtRics)
  # pull survey design (design object)
  d <<- qsurvey::design(id = sid)
  # load survey 
  svy <<- qualtRics::fetch_survey(sid,
                                  include_display_order = FALSE,
                                  convert = FALSE,
                                  force_request = TRUE)
  # pull the mapping 
  colmap <<- qualtRics::extract_colmap(svy)
  
  # helper to strip html tags
  cleanFun <- function(string) {
    return(gsub("<.*?>", "", string))
  }
  # questions  
  svy_q <<- qsurvey::questions(design_object = d) |>  
    dplyr::mutate(question_text = cleanFun(question_text), # strip html tags
                  question_text = gsub("[\r\n\t]", " ", question_text), # strip line breaks
                  question_text = trimws(gsub("\\s+", " ", question_text))) |>  
    dplyr::filter(question_type != "DB") |> 
    dplyr::as_tibble()
  # get survey choices for each question
  svy_choice <<- qsurvey::choices(design_object = d) %>%
    dplyr::mutate(choice_text = cleanFun(choice_text), # strip html tags
                  choice_text = gsub("[\r\n\t]", " ", choice_text), # strip line breaks
                  choice_text = trimws(gsub("\\s+", " ", choice_text))) %>%
    dplyr::as_tibble()
  
  # keep a tidy TOC of distinct questions 
  toc <<- svy_q |> 
    dplyr::distinct(question_id, .keep_all = TRUE)
}