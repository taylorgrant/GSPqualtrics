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
                  question_text = str_replace_all(question_text, "&quot;", "'")) |>  
    dplyr::filter(question_type != "DB") |> 
    dplyr::as_tibble()
  # get survey choices for each question
  svy_choice <<- qsurvey::choices(design_object = d) %>%
    dplyr::mutate(choice_text = cleanFun(choice_text), # strip html tags
                  choice_text = gsub("[\r\n\t]", " ", choice_text), # strip line breaks
                  choice_text = trimws(gsub("\\s+", " ", choice_text))) %>%
    dplyr::as_tibble()
  
  # pull blocks and questions within each
  blockbuild <- data.table::rbindlist(d$blocks) |>  
    unnest(elements) |>  
    unnest(elements) |> 
    filter(!elements %in% c("Question", "PageBreak")) |>
    mutate(description = trimws(description)) |>
    rename(block = description, question_id = elements)
  
  # keep a tidy TOC of distinct questions 
  toc <- svy_q |> 
    dplyr::distinct(question_id, .keep_all = TRUE) |> 
    left_join(blockbuild) |>  
    relocate(block, .before = "question_order")
  
  # add in age group and cohort to the toc
  cohort_generation <- function(tbl) {
    if (any(toc$question_text == "How old are you? Please enter your current age below.")) {
      v1 <- tbl |> 
        filter(question_text == "How old are you? Please enter your current age below.") |>
        mutate(question_id = paste0(question_id, "a"),
               question_type = "TE_AGE",
               question_text = "Respondent breakdown by age cohort")
      
      v2 <- tbl |> 
        filter(question_text == "How old are you? Please enter your current age below.") |>
        mutate(question_id = paste0(question_id, "b"),
               question_type = "TE_AGE",
               question_text = "Respondent breakdown by generation")
      
      age_add <- bind_rows(v1, v2)
    } 
                                  
  }
  toc <<- rbind(toc, cohort_generation(toc)) |> 
    arrange(question_order)
  
  

}

# filter down the TOC questions by Block; and allow for RESET
toc_filter <- function(tbl, blck) {
  
  if (any(tbl$block == blck)) {
    tmp <- tbl |>  
      filter(block %in% blck)  |>  
      mutate(newtoc = paste0("Q", question_order, ": ", question_text))
    # now grab newtoc and set names
    tmptoc <- tmp$question_id |> 
      set_names(nm = tmp$newtoc)

  } else
    tmp <- tbl |>  
      mutate(newtoc = paste0("Q", question_order, ": ", question_text))  
  # now grab newtoc and set names
  tmptoc <- tmp$question_id |> 
    set_names(nm = tmp$newtoc)
}


# SINGLE QUESTION SUMMARY  ------------------------------------------------

question_summary <- function(qid) {
  meta <- toc %>% filter(question_id == qid)
  
  # based on question_type, run through summary function # 
  if (meta$question_type == "MC") {
    out <- multichoice(qid)
  } else if ((meta$question_type == "TE") & str_detect(meta$question_text, "current age")) {
    out <- textage(qid)
  } else if (meta$question_type == "RO") {
    out <- rankorder(qid)
  } else if (meta$question_type == "Slider") {
    out <- slider(qid)
  } else if (meta$question_type == "Matrix") {
    out <- matrix_q(qid)
  } else if (meta$question_type == "PGR") {
    out <- pickgrouprank(qid)
  } else if (meta$question_type == "DD") {
    out <- drilldown(qid)
  } 
  return(out)
}



