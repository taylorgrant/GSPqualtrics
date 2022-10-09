# dataViz Single Question Summary #

# Multiple Choice (MC)
multichoice <- function(q, color) {
  meta <- toc %>% filter(question_id == q)
  add_break <- function(x) gsub("(.{28,}?)\\s", "\\1\n", x)
  
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
  # get the respondent count answering the question
  resp_count <- sum((rowSums(is.na(tmp[2:ncol(tmp)])) == ncol(tmp[2:ncol(tmp)])) == "FALSE")
  
  # summarize (order by factor)
  tmp <- tmp %>%
    pivot_longer(-ResponseId) %>% 
    dplyr::filter(!str_detect(name, "TEXT"),
                  !is.na(value)) %>%
    dplyr::mutate(value = add_break(value)) %>%
    mutate(value = factor(value, levels = dput(qchoice$choice_text))) %>%
    count(value) %>%
    mutate(frac = n/resp_count)
  # graph and output
  out <- singleQ_barplot(tmp, meta$question_text, resp_count, color)
}

# Text (only for Age question - TE)
textage <- function(q, color) {
  
  meta <- toc %>% filter(question_id == q)
  qpull <- svy_q %>%
    filter(question_id == str_replace_all(q, "a|b", ""))
  
  # pull columns from survey data
  tmp <- svy %>% 
    dplyr::select(ResponseId, all_of(qpull$export_name))
  # get the respondent count answering the question
  resp_count <- sum((rowSums(is.na(tmp[2:ncol(tmp)])) == ncol(tmp[2:ncol(tmp)])) == "FALSE")
  # summarize (order by factor)
  tmp <- tmp %>%
    pivot_longer(-ResponseId) %>% 
    dplyr::filter(!str_detect(name, "TEXT"),
                  !is.na(value)) %>%
    count(value) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(frac = n/resp_count)
  
  if (meta$question_text == "Respondent breakdown by age cohort") {
    tmp <- tmp %>% 
      mutate(group = case_when(value <= 24 ~ "18-24",
                               value > 24 & value <= 34 ~ "25-34",
                               value > 34 & value <= 44 ~ "35-44",
                               value > 44 & value <= 54 ~ "45-54",
                               value > 54 & value <= 64 ~ "55-64",
                               value > 64 ~ "65+")) %>% 
      group_by(group) %>% 
      summarise(n = sum(n)) %>% 
      mutate(frac = n/resp_count,
             group = factor(group, levels = c("18-24", "25-34", "35-44", 
                                              "45-54", "55-64", "65+"))) %>% 
      rename(value = group) %>%
      arrange(value)
    
    out <- singleQ_barplot(tmp, meta$question_text, resp_count, color)
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
      group_by(gen) %>% 
      summarise(n = sum(n)) %>% 
      mutate(frac = n/resp_count,
             gen = factor(gen, levels = c("Post-Z","Gen Z", "Millennial", "Gen X", "Boomers",
                                          "Silent", "Greatest"))) %>% 
      rename(value = gen) %>% 
      arrange(value)
    
    out <- singleQ_barplot(tmp, meta$question_text, resp_count, color)
  }
}

# Rank Order (RO)
rankorder <- function(q, color) {
  meta <- toc %>% filter(question_id == q)
  add_break <- function(x) gsub("(.{21,}?)\\s", "\\1\n", x)
  
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
  # get the respondent count answering the question
  resp_count <- sum((rowSums(is.na(tmp[2:ncol(tmp)])) == ncol(tmp[2:ncol(tmp)])) == "FALSE")
  # summarize 
  tmp <- tmp %>%
    pivot_longer(-ResponseId) %>% 
    dplyr::filter(!str_detect(name, "TEXT"),
                  !is.na(value)) %>% 
    mutate(choice_id = as.numeric(gsub(".*\\_", "", name))) %>% 
    left_join(select(qchoice, c(choice_id, choice_text))) %>% 
    dplyr::mutate(value = add_break(value)) %>%
    mutate(choice_text = factor(choice_text, levels = dput(qchoice$choice_text))) %>%
    count(choice_text, value) %>% 
    group_by(choice_text) %>% 
    mutate(frac = n/resp_count)
  
  out <- rankorderQ_barplot(tmp, meta$question_text, resp_count, color) 
}

# Slider (Slider)
slider <- function(q, color) {
  meta <- toc %>% filter(question_id == q)
  add_break <- function(x) gsub("(.{21,}?)\\s", "\\1\n", x)
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
  # get the respondent count answering the question
  resp_count <- sum((rowSums(is.na(tmp[2:ncol(tmp)])) == ncol(tmp[2:ncol(tmp)])) == "FALSE")
  # summarize (order by slider value; no factor)
  tmp <- tmp %>%
    pivot_longer(-ResponseId) %>%  
    dplyr::filter(!str_detect(name, "TEXT"),
                  !is.na(value)) %>%
    count(value) %>%
    mutate(frac = n/resp_count)
  # graph and output
  out <- singleQ_barplot(tmp, meta$question_text, resp_count, color)
}

# Matrix (Matrix)
matrix_q <- function(q, color) {
  
  add_break <- function(x) gsub("(.{28,}?)\\s", "\\1\n", x)
  meta <- toc %>% filter(question_id == q)
  # pull colmap 
  matrix_map <- colmap %>% 
    filter(str_detect(ImportId, glue::glue("{q}_"))) %>% 
    select(name = qname, ImportId, choice_text = sub) %>% 
    mutate(choice_text = trimws(gsub("\\ - .*", "", choice_text)),
           choice_text = add_break(choice_text))
  # pull matrix options for factor order
  qchoice <- svy_choice %>% 
    filter(question_id == q) %>% 
    mutate(choice_text = add_break(choice_text))
  
  tmp <- svy %>% 
    dplyr::select(ResponseId, all_of(matrix_map$name))
  # get the respondent count answering the question
  resp_count <- sum((rowSums(is.na(tmp[2:ncol(tmp)])) == ncol(tmp[2:ncol(tmp)])) == "FALSE")
  
  # summarize 
  tmp <- tmp %>%
    pivot_longer(-ResponseId) %>%  
    dplyr::filter(!str_detect(name, "TEXT"),
                  !is.na(value)) %>% 
    left_join(select(matrix_map, c(name, choice_text))) %>%
    mutate(choice_text = add_break(choice_text),
           choice_text = factor(choice_text, levels = dput(unique(matrix_map$choice_text))),
           value = add_break(value),
           value = factor(value, levels = dput(qchoice$choice_text))) %>%
    count(choice_text, value) %>% 
    group_by(choice_text) %>% 
    mutate(frac = n/resp_count)
  
  out <- matrixQ_barplot(tmp, meta$question_text, resp_count)
}

# Pick Group Rank (PGR)
pickgrouprank <- function(q) {
  qpull <- colmap %>% 
    filter(str_detect(ImportId, glue::glue("{q}_")))
  
  tmp <- svy %>% 
    dplyr::select(ResponseId, all_of(qpull$qname))
  # get the respondent count answering the question
  resp_count <- sum((rowSums(is.na(tmp[2:ncol(tmp)])) == ncol(tmp[2:ncol(tmp)])) == "FALSE")
  
  # extract groupings from the question
  grps <- data.table::rbindlist(d$questions[[q]]$groups) %>% 
    as_tibble() %>% 
    rename(name = recode, choice_text = description)
  
  if (nrow(grps) <= 1) {
    merge_and_group <- function(dat) {
      grp <- dat %>% 
        select(ResponseId, contains("GROUP")) %>% 
        pivot_longer(-ResponseId) %>% 
        set_names(nm = c("id" ,"group", 'response'))
      rank <- dat %>% 
        select(ResponseId, contains("RANK")) %>%
        pivot_longer(-ResponseId) %>%
        set_names(nm = c("id" ,"ranking", 'rank'))
      out <- bind_cols(grp, rank)
    }
    tmp <- merge_and_group(tmp) %>%
      filter(!is.na(response)) %>% 
      select(value = response, choice_text = rank) %>% 
      count(value, choice_text) %>% 
      mutate(frac = n/resp_count)
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
      count(choice_text, value) %>% 
      mutate(frac = n/resp_count) %>% 
      group_by(choice_text) %>% 
      arrange(desc(frac), .by_group = TRUE)
  }
}

# DrillDown (DD)
drilldown <- function(q) {
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
  
  # get the respondent count answering the question
  resp_count <- sum((rowSums(is.na(tmp[2:ncol(tmp)])) == ncol(tmp[2:ncol(tmp)])) == "FALSE")
  
  # summarize
  tmp <- tmp %>%
    unite(choice_text, sep = " - ", !ResponseId) %>% 
    count(choice_text) %>% 
    mutate(frac = n/resp_count) %>%
    arrange(desc(frac))
}