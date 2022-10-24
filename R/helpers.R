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
  # pacman::p_load(tidyverse, here, janitor, glue, qsurvey, qualtRics)
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
                  question_text = trimws(gsub("\\s+", " ", question_text)),
                  question_text = stringr::str_replace_all(question_text, "&quot;", "'")) |>  
    dplyr::filter(question_type != "DB") |> 
    dplyr::as_tibble()
  # get survey choices for each question
  svy_choice <<- qsurvey::choices(design_object = d) |> 
    dplyr::mutate(choice_text = cleanFun(choice_text), # strip html tags
                  choice_text = gsub("[\r\n\t]", " ", choice_text), # strip line breaks
                  choice_text = trimws(gsub("\\s+", " ", choice_text))) |> 
    dplyr::as_tibble()
  
  # pull blocks and questions within each
  get_blocks <- function(bl) {
    blockname <- d$blocks[[bl]]$description
    
    qids <- data.table::rbindlist(d$blocks[[bl]]$elements, fill = TRUE) |>  
      data.frame() |>  
      dplyr::filter(type == "Question") |>  
      dplyr::mutate(block = blockname) |>  
      dplyr::select(-type, question_id = questionId)
  }
  blockbuild <- purrr::map_dfr(names(d$blocks), get_blocks)
  
  # function to extract selector type (Likert, Bipolar, etc)
  get_selector <- function(q) {
    tmp <- d$questions[[q]]$questionType$selector
  }
  
  # keep a tidy TOC of distinct questions 
  toc <<- svy_q |> 
    dplyr::distinct(question_id, .keep_all = TRUE) |> 
    dplyr::left_join(blockbuild) |>  
    dplyr::relocate(block, .before = "question_order") |> 
    dplyr::mutate(selector_type = purrr::map(question_id, get_selector)) |> 
    tidyr::unnest(cols = selector_type)
  
  # add age group and cohort options to the toc
  cohort_generation <- function(tbl) {
    if (any(toc$question_text == "How old are you? Please enter your current age below.")) {
      v1 <- tbl |> 
        dplyr::filter(question_text == "How old are you? Please enter your current age below.") |>
        dplyr::mutate(question_id = paste0(question_id, "a"),
                      question_type = "TE_AGE",
                      question_text = "Respondent breakdown by age cohort")
      
      v2 <- tbl |> 
        dplyr::filter(question_text == "How old are you? Please enter your current age below.") |>
        dplyr::mutate(question_id = paste0(question_id, "b"),
                      question_type = "TE_AGE",
                      question_text = "Respondent breakdown by generation")
      
      age_add <- rbind(v1, v2)
    } 
                                  
  }
  toc <<- rbind(toc, cohort_generation(toc)) |> 
    dplyr::arrange(question_order)
}

# filter down the TOC questions by Block; and allow for RESET
toc_filter <- function(tbl, blck) {
  
  if (any(tbl$block == blck)) {
    tmp <- tbl |>  
      dplyr::filter(block %in% blck)  |>  
      dplyr::mutate(newtoc = paste0("Q", question_order, ": ", question_text))
    # now grab newtoc and set names
    tmptoc <- tmp$question_id |> 
      purrr::set_names(nm = tmp$newtoc)

  } else
    tmp <- tbl |>  
      dplyr::mutate(newtoc = paste0("Q", question_order, ": ", question_text))  
  # now grab newtoc and set names
  tmptoc <- tmp$question_id |> 
    purrr::set_names(nm = tmp$newtoc)
}







