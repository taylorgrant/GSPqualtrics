# working through crosstabbing 

# load packages
pacman::p_load(tidyverse, here, janitor, glue, qsurvey, qualtRics, gt)

sids <- readRDS(here("data", "qualtrics_sids.rds"))

source(here("R", "helpers.R"))

# load Sep Omnibus to play with # 
load_survey(sid = sids$id[1])
# change to Aug Omnibus for RO question
load_survey(sid = sids$id[7])
# for PGR (multi group)
load_survey(sid = "SV_88kbX2XMoe4hPWS")
# PGR (no groups; default to matrix)
load_survey(sid = "SV_0eny0VJXqdF0biZ")
# DD 
load_survey(sid = "SV_dj3bL70Bhql9FAy")

# crosstabbing  -----------------------------------------------------------

# pull in response data for each q and summarise
get_responses <- function(q) {

  meta <- toc %>% filter(question_id == q)
  add_break <- function(x) gsub("(.{28,}?)\\s", "\\1\n", x)
  
  ### --- MULTIPLE CHOICE --- ###
  
  if (meta$question_type == "MC") {
    # get question names to pull columns from survey
    qpull <- svy_q %>%
      filter(question_id == q) %>% 
      filter(!str_detect(export_name, "TEXT"))
    # get the possible options of the MC question
    qchoice <- svy_choice %>% 
      filter(question_id == q) %>%
      arrange(as.numeric(choice_recode)) %>%
      mutate(choice_text = add_break(choice_text))
    # pull columns from survey data
    tmp <- svy %>% 
      dplyr::select(ResponseId, all_of(qpull$export_name))
    # summarize (order by factor)
    tmp <- tmp %>%
      pivot_longer(-ResponseId) %>% 
      dplyr::filter(!str_detect(name, "TEXT"),
                    !is.na(value)) %>%
      dplyr::mutate(value = add_break(value)) %>%
      mutate(value = factor(value, levels = dput(qchoice$choice_text)))

    ### --- TEXT AGE --- ###
    
  } else if (meta$question_type == "TE_AGE") {
    
    qpull <- svy_q %>%
      filter(question_id == str_replace_all(q, "a|b", ""))
    
    # pull columns from survey data
    tmp <- svy %>%
      dplyr::select(ResponseId, all_of(qpull$export_name))
    # summarize (order by factor)
    tmp <- tmp %>%
      pivot_longer(-ResponseId) %>%
      dplyr::filter(!str_detect(name, "TEXT"),
                    !is.na(value)) 
    
    if (meta$question_text == "Respondent breakdown by age cohort") {
      tmp <- tmp %>%
        mutate(group = case_when(value <= 24 ~ "18-24",
                                 value > 24 & value <= 34 ~ "25-34",
                                 value > 34 & value <= 44 ~ "35-44",
                                 value > 44 & value <= 54 ~ "45-54",
                                 value > 54 & value <= 64 ~ "55-64",
                                 value > 64 ~ "65+")) %>%
        mutate(group = factor(group, levels = c("18-24", "25-34", "35-44",
                                                "45-54", "55-64", "65+"))) %>%
        select(-value) %>% 
        rename(value = group)

    } else {
      tmp <- tmp %>%
        mutate(yob = 2022 - value) %>%
        mutate (gen = case_when (yob < 2013 & yob > 1996 ~ 'Gen Z',
                                 yob < 1997 & yob > 1980 ~ 'Millennial',
                                 yob < 1981 & yob > 1964 ~ 'Gen X',
                                 yob < 1965 & yob > 1945 ~ 'Boomers',
                                 yob < 1946 & yob > 1927 ~ 'Silent',
                                 yob < 1928 ~ 'Greatest',
                                 yob > 2012 ~ 'Post-Z')) %>%
        mutate(gen = factor(gen, levels = c("Post-Z","Gen Z", "Millennial", "Gen X", "Boomers",
                                            "Silent", "Greatest"))) %>%
        select(-c(value, yob)) %>%
        rename(value = gen)
    }
    
    ### --- MATRIX --- ###
    
  } else if (meta$question_type == "Matrix") {
      
    add_break <- function(x) gsub("(.{28,}?)\\s", "\\1\n", x)
    
    # pull colmap
    matrix_map <- colmap %>%
      filter(str_detect(ImportId, glue::glue("{q}_"))) %>%
      select(name = qname, ImportId, choice_text = sub) %>%
      mutate(choice_text = trimws(gsub("\\ - .*", "", choice_text)))
    
    # pull matrix options for factor order
    qchoice <- svy_choice %>%
      filter(question_id == q) %>%
      mutate(choice_text = add_break(choice_text))
    
    tmp <- svy %>%
      dplyr::select(ResponseId, all_of(matrix_map$name))
    
    ### --- MATRIX & BIPOLAR--- ###
    
    if (meta$selector_type == "Bipolar") {
      add_statement_break <- function(x) gsub("(.{20,}?)\\s", "\\1\n", x)
      
      tmp <- tmp %>%
        pivot_longer(-ResponseId) %>%
        dplyr::filter(!str_detect(name, "TEXT"),
                      !is.na(value)) %>%
        left_join(select(matrix_map, c(name, choice_text))) %>%
        mutate(choice_text = factor(choice_text, levels = dput(unique(matrix_map$choice_text))),
               value = factor(value, levels = dput(qchoice$choice_text)))
      
      
      ### --- MATRIX & LIKERT--- ###
      
    } else {
      tmp <- tmp %>%
        pivot_longer(-ResponseId) %>%
        dplyr::filter(!str_detect(name, "TEXT"),
                      !is.na(value)) %>%
        left_join(select(matrix_map, c(name, choice_text))) %>%
        mutate(choice_text = add_break(choice_text),
               choice_text = factor(choice_text, levels = dput(add_break(unique(matrix_map$choice_text)))),
               value = add_break(value),
               value = factor(value, levels = dput(qchoice$choice_text)))
    }
    
    ### --- RANK ORDER --- ###
    
  } else if (meta$question_type == "RO") {
    
    # get question names to pull columns from survey
    qpull <- svy_q %>%
      filter(question_id == q) %>%
      filter(!str_detect(export_name, "TEXT"))
    # get the possible options of the MC question
    qchoice <- svy_choice %>%
      filter(question_id == q) %>%
      mutate(choice_text = add_break(choice_text))
    
    # pull columns from survey data
    tmp <- svy %>%
      dplyr::select(ResponseId, all_of(qpull$export_name))

    # summarize
    tmp <- tmp %>%
      pivot_longer(-ResponseId) %>%
      dplyr::filter(!str_detect(name, "TEXT"),
                    !is.na(value)) %>%
      mutate(choice_id = as.numeric(gsub(".*\\_", "", name))) %>%
      left_join(select(qchoice, c(choice_id, choice_text))) %>%
      dplyr::mutate(value = add_break(value)) %>%
      mutate(choice_text = factor(choice_text, levels = dput(qchoice$choice_text)))
    
    ### --- SLIDER --- ###
    
  } else if (meta$question_type == "Slider") {
    
    # get question names to pull columns from survey
    qpull <- svy_q %>%
      filter(question_id == q) %>%
      filter(!str_detect(export_name, "TEXT"))
    
    # get the possible options of the MC question
    qchoice <- svy_choice %>%
      filter(question_id == q)
    
    # pull columns from survey data
    tmp <- svy %>%
      dplyr::select(ResponseId, all_of(qpull$export_name))

    # summarize (slider value is numeric, not factor)
    tmp <- tmp %>%
      pivot_longer(-ResponseId) %>%
      dplyr::filter(!str_detect(name, "TEXT"),
                    !is.na(value))
    
    ### --- PICK, GROUP, RANK --- ###
    
  } else if (meta$question_type == "PGR") {
    
    qpull <- colmap %>%
      filter(str_detect(ImportId, glue::glue("{q}_")))
    
    tmp <- svy %>%
      dplyr::select(ResponseId, all_of(qpull$qname))
    
    # extract groupings from the question
    grps <- data.table::rbindlist(d$questions[[q]]$groups) %>%
      as_tibble() %>%
      rename(name = recode, choice_text = description)
    
    if (nrow(grps) <= 1) {
      merge_and_group <- function(dat) {
        grp <- dat %>%
          select(ResponseId, contains("GROUP")) %>%
          pivot_longer(-ResponseId) %>%
          set_names(nm = c("ResponseId" ,"group", 'response'))
        rank <- dat %>%
          select(ResponseId, contains("RANK")) %>%
          pivot_longer(-ResponseId) %>%
          set_names(nm = c("ResponseId2" ,"ranking", 'rank'))
        out <- bind_cols(grp, rank)
      }
      tmp <- merge_and_group(tmp) %>%
        filter(!is.na(response)) %>%
        select(ResponseId, choice_text = response, value = rank) %>%
        mutate(choice_text = add_break(choice_text),
               value = factor(value))
      
    } else {
      
      tmp <- tmp %>%
        pivot_longer(-ResponseId,
                     values_transform = list(value = as.character)) %>%
        dplyr::filter(!str_detect(name, "TEXT"),
                      !is.na(value)) %>%
        filter(str_detect(name, "GROUP")) %>%
        mutate(name = sub(".*?_", "", name),
               name = gsub("\\_.*", "", name)) %>%
        left_join(grps) %>%
        mutate(choice_text = factor(choice_text, levels = dput(grps$choice_text))) %>% 
        select(-name)

    }
    
    ### --- DRILLDOWN --- ###
    
  } else if (meta$question_type == "DD") {
    
    # pull colmap
    drill_map <- colmap %>%
      filter(str_detect(ImportId, glue::glue("{q}_"))) %>%
      select(key = qname, ImportId, choice_text = sub) %>%
      mutate(choice_text = trimws(gsub("\\ - .*", "", choice_text)))
    
    drill_name <- drill_map %>%
      summarise(nm = paste(choice_text, collapse = " - ")) %>%
      pull()
    
    # pull matrix options for factor order
    qchoice <- svy_choice %>%
      filter(question_id == q)
    
    tmp <- svy %>%
      dplyr::select(ResponseId, all_of(drill_map$key))
    
    # summarize
    tmp <- tmp %>%
      unite(choice_text, sep = " - ", !ResponseId) %>% 
      mutate(value = ifelse(choice_text == "NA - NA", NA, "tmp_letter"))
    
  }
  
  # set attributes for later usage 
  attr(tmp, "question_type") <- meta$question_type
  attr(tmp, "selector_type") <- meta$selector_type
  attr(tmp, "question_text") <- meta$question_text
  tmp
}

# function to pull responses, merge on responseID, and crosstab
build_crosstab <- function(qs) {
  
  # pull responses
  tmp_responses <- map(qs, get_responses) %>% 
    set_names(qs)
  
  merged <- tmp_responses[[1]] %>% 
    left_join(tmp_responses[[2]], by = "ResponseId") %>% 
    rename(group = value.x, target = value.y)
    
  # pulling question and selector type of target variable 
  target_qt <- attributes(tmp_responses[[2]])$question_type
  
  # this is the proper total for the group variable
  group_totals <- merged %>% 
    filter(!is.na(target)) %>%
    group_by(group) %>% 
    summarise(total = n_distinct(ResponseId))
    
  if (target_qt %in% c("MC", "TE_AGE", "Slider")) {
      
    out <- merged %>% 
      group_by(group) %>%
      count(group, target) %>% 
      filter(!is.na(target)) %>% 
      left_join(group_totals) %>% 
      mutate(frac = n/total) %>%
      ungroup()
      
    } else if (target_qt %in% c("Matrix", "RO", "PGR")) {
    
      out <- merged %>% 
        count(choice_text, target, group) %>% 
        filter(!is.na(target)) %>% 
        left_join(group_totals) %>% 
        mutate(frac = n/total) %>%
        ungroup()
      
      # when passing Matrix/Bipolar need to separate 
    
      } else if (target_qt == "DD") {
          
        out <- merged %>% 
          count(choice_text, group) %>%
          left_join(group_totals) %>% 
          mutate(frac = n/total) %>%
          ungroup()
      }
  
  if (target_qt == "PGR") {
    grps <- nrow(data.table::rbindlist(d$questions[[qs[2]]]$groups) %>%
           as_tibble() %>% 
           rename(name = recode, choice_text = description))
    if (grps <= 1) {
      attr(out, "target_qt") <- "RO"
      attr(out, "target_st") <- attributes(tmp_responses[[2]])$selector_type
      attr(out, "target_qtext") <- attributes(tmp_responses[[2]])$question_text
      attr(out, "group_qtext") <- attributes(tmp_responses[[1]])$question_text
      attr(out, "group_qt") <- attributes(tmp_responses[[1]])$question_type
      attr(out, "group_st") <- attributes(tmp_responses[[1]])$selector_type
      attr(out, "nsize") <- length(unique(merged[!is.na(merged$target),]$ResponseId))
    } else {
      attr(out, "target_qt") <- attributes(tmp_responses[[2]])$question_type
      attr(out, "target_st") <- attributes(tmp_responses[[2]])$selector_type
      attr(out, "target_qtext") <- attributes(tmp_responses[[2]])$question_text
      attr(out, "group_qtext") <- attributes(tmp_responses[[1]])$question_text
      attr(out, "group_qt") <- attributes(tmp_responses[[1]])$question_type
      attr(out, "group_st") <- attributes(tmp_responses[[1]])$selector_type
      attr(out, "nsize") <- length(unique(merged[!is.na(merged$target),]$ResponseId))
    }
  } else {
    attr(out, "target_qt") <- attributes(tmp_responses[[2]])$question_type
    attr(out, "target_st") <- attributes(tmp_responses[[2]])$selector_type
    attr(out, "target_qtext") <- attributes(tmp_responses[[2]])$question_text
    attr(out, "group_qtext") <- attributes(tmp_responses[[1]])$question_text
    attr(out, "group_qt") <- attributes(tmp_responses[[1]])$question_type
    attr(out, "group_st") <- attributes(tmp_responses[[1]])$selector_type
    attr(out, "nsize") <- length(unique(merged[!is.na(merged$target),]$ResponseId))
  }
  out
  
  multiQ_table(out)
}

qs <- c("QID2438", "QID165")
build_crosstab(qs)




