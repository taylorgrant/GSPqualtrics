# working through crosstabbing

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
        mutate(yob = 2023 - value) %>%
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

# pull groups for possible filter
group_filter <- function(group, target) {

  qs <- c(group, target)
  # pull responses
  tmp_responses <- map(qs, get_responses) %>%
    set_names(qs)

  groups <- tmp_responses[[1]] %>%
    left_join(tmp_responses[[2]], by = "ResponseId") %>%
    rename(group = value.x, target = value.y) %>%
    filter(!is.na(target)) %>%
    group_by(group) %>%
    summarise(total = n_distinct(ResponseId)) %>%
    pull(group)
}


# significance testing by question type -----------------------------------

# multiple choice
sig_test_mc <- function(tbl, totals, conf_level, grp_filter) {

  # pull the grouping variable
  grp1 <- tbl %>%
    distinct(group) %>%
    filter(!group %in% grp_filter) %>%
    pull()

  # cross together so we get every pairing
  groups <- crossing(grp1 = grp1, grp2 = grp1, target = tbl$target) %>%
    arrange(target) %>%
    distinct(grp1, grp2, target, .keep_all = TRUE)

  # now load in
  df <- groups %>%
    left_join(select(tbl, c(group, target, n)), by = c("grp1" = "group",
                                                       "target" = "target")) %>%
    left_join(select(tbl, c(group, target, n)), by = c("grp2" = "group",
                                                       "target" = "target")) %>%
    left_join(totals, by = c("grp1" = "group")) %>%
    left_join(totals, by = c("grp2" = "group")) %>%
    filter(grp1 %in% tbl[!tbl$group %in% grp_filter,]$group) %>%
    filter(grp2 %in% tbl[!tbl$group %in% grp_filter,]$group) %>%
    filter(grp1 != grp2) %>%
    mutate(across(n.x:n.y, ~replace_na(., 0)))

  df <- df %>%
    mutate(
      signif = list(n.x, n.y, total.x, total.y) %>% pmap(~ {
        prop.test(
          x = c(..1, ..2),
          n = c(..3, ..4),
          conf.level = as.numeric(conf_level)
        ) %>% broom::tidy()
      })) %>%
    unnest(cols = signif) %>%
    mutate(test = as.numeric(grp1) - (min(as.numeric(grp1) - 1)),
           test = LETTERS[test]) %>%
    mutate(test = ifelse(((p.value < (1 - as.numeric(conf_level))) & (estimate2 > estimate1)), test, NA)) %>%
    filter(!is.na(test)) %>%
    group_by(grp2, target) %>%
    summarise(sig.test = paste(test, collapse = ", "))

  tmp_out <- tbl %>%
    left_join(df, by = c("group" = 'grp2',
                         "target" = "target")) %>%
    mutate(frac = scales::percent(frac, accuracy = 1)) %>%
    unite("frac", c(frac, sig.test), sep = "-") %>%
    mutate(frac = str_replace_all(frac, "-NA", "")) %>%
    filter(!group %in% grp_filter)

  total_row <- tmp_out %>%
    ungroup() %>%
    distinct(group, total) %>%
    janitor::adorn_totals() %>%
    mutate(test = row_number(),
           test = ifelse(group != "Total",
                         LETTERS[test], "-")) %>%
    t() %>%
    data.frame() %>%
    janitor::row_to_names(row_number = 1) %>%
    tibble() %>%
    mutate(across(everything(), str_trim)) %>%
    relocate(Total, .before = everything()) %>%
    mutate(target = "Total Count (Answering)") %>%
    relocate(target, .before = everything())

  total_frac <- tmp_out %>%
    ungroup() %>%
    group_by(target) %>%
    tally(n) %>%
    mutate(Total = n/as.numeric(total_row$Total[1]),
           Total = scales::percent(Total, accuracy = 1))

  tbl_data <- bind_rows(total_row, tmp_out %>%
                          pivot_wider(id_cols = c(target),
                                      names_from = group,
                                      values_from = frac) %>%
                          mutate(target = as.character(target)) %>%
                          left_join(select(total_frac, -n)) %>%
                          relocate(Total, .after = "target")) %>%
    mutate(id = row_number(), # used to set rule for % formatting
           target_group = ifelse(str_detect(target, "Total Count"), "", "Response"),
           target = ifelse(Total == "-", "", target))

}

# matrix, PGR
sig_test_matrix <- function(tbl, totals, conf_level, grp_filter) {

  # pull the grouping variable
  grp1 <- tbl %>%
    distinct(group) %>%
    filter(!group %in% grp_filter) %>%
    pull()

  # cross together so we get every pairing
  groups <- crossing(grp1 = grp1, grp2 = grp1, choice_text = tbl$choice_text, target = tbl$target) %>%
    arrange(target) %>%
    distinct(grp1, grp2, choice_text, target, .keep_all = TRUE)

  df <- groups %>%
    left_join(select(tbl, c(group, choice_text, target, n)),
              by = c("grp1" = "group",
                     "choice_text" = "choice_text",
                     "target" = "target")) %>%
    left_join(select(tbl, c(group, choice_text, target, n)),
              by = c("grp2" = "group",
                     "choice_text" = "choice_text",
                     "target" = "target")) %>%
    left_join(totals, by = c("grp1" = "group")) %>%
    left_join(totals, by = c("grp2" = "group")) %>%
    filter(grp1 %in% tbl[!tbl$group %in% grp_filter,]$group) %>%
    filter(grp2 %in% tbl[!tbl$group %in% grp_filter,]$group) %>%
    filter(grp1 != grp2) %>%
    mutate(across(n.x:n.y, ~replace_na(., 0)))

  df <- df %>%
    mutate(
      signif = list(n.x, n.y, total.x, total.y) %>% pmap(~ {
        prop.test(
          x = c(..1, ..2),
          n = c(..3, ..4),
          conf.level = as.numeric(conf_level)
        ) %>% broom::tidy()
      })) %>%
    unnest(cols = signif) %>%
    mutate(test = as.numeric(grp1) - (min(as.numeric(grp1) - 1)),
           test = LETTERS[test]) %>%
    mutate(test = ifelse(((p.value < (1 - as.numeric(conf_level))) & (estimate2 > estimate1)), test, NA)) %>%
    filter(!is.na(test)) %>%
    group_by(grp2, choice_text, target) %>%
    summarise(sig.test = paste(test, collapse = ", "))

  tmp_out <- tbl %>%
    left_join(df, by = c("group" = 'grp2',
                         "choice_text" = "choice_text",
                         "target" = "target")) %>%
    mutate(frac = scales::percent(frac, accuracy = 1)) %>%
    unite("frac", c(frac, sig.test), sep = "-") %>%
    mutate(frac = str_replace_all(frac, "-NA", "")) %>%
    filter(!group %in% grp_filter)

  total_row <- tmp_out %>%
    ungroup() %>%
    distinct(group, total) %>%
    janitor::adorn_totals() %>%
    mutate(test = row_number(),
           test = ifelse(group != "Total",
                         LETTERS[test], "-")) %>%
    t() %>%
    data.frame() %>%
    janitor::row_to_names(row_number = 1) %>%
    tibble() %>%
    mutate(across(everything(), str_trim)) %>%
    relocate(Total, .before = everything()) %>%
    mutate(target = "Total Count (Answering)",
           choice_text = "") %>%
    relocate(target, .before = everything())

  total_frac <- tmp_out %>%
    ungroup() %>%
    group_by(choice_text, target) %>%
    tally(n) %>%
    mutate(Total = n/as.numeric(total_row$Total[1]),
           Total = scales::percent(Total, accuracy = 1))

  tbl_data <- bind_rows(total_row, tmp_out %>%
                          pivot_wider(id_cols = c(choice_text, target),
                                      names_from = group,
                                      values_from = frac) %>%
                          mutate(target = as.character(target)) %>%
                          left_join(select(total_frac, -n)) %>%
                          relocate(Total, .after = "target")) %>%
    mutate(id = row_number(), # used to set rule for % formatting
           target = ifelse(Total == "-", "", target)) %>%
    rename(target_group = choice_text)

}

# Bipolar matrix question
sig_test_bipolar <- function(tbl, totals, conf_level, grp_filter) {

  # pull the grouping variable
  grp1 <- tbl %>%
    distinct(group) %>%
    filter(!group %in% grp_filter) %>%
    pull()

  # cross together so we get every pairing
  groups <- crossing(grp1 = grp1, grp2 = grp1, choice_text = tbl$choice_text, target = tbl$target) %>%
    arrange(target) %>%
    distinct(grp1, grp2, choice_text, target, .keep_all = TRUE)

  df <- groups %>%
    left_join(select(tbl, c(group, choice_text, target, n)),
              by = c("grp1" = "group",
                     "choice_text" = "choice_text",
                     "target" = "target")) %>%
    left_join(select(tbl, c(group, choice_text, target, n)),
              by = c("grp2" = "group",
                     "choice_text" = "choice_text",
                     "target" = "target")) %>%
    left_join(totals, by = c("grp1" = "group")) %>%
    left_join(totals, by = c("grp2" = "group")) %>%
    filter(grp1 %in% tbl[!tbl$group %in% grp_filter,]$group) %>%
    filter(grp2 %in% tbl[!tbl$group %in% grp_filter,]$group) %>%
    filter(grp1 != grp2) %>%
    mutate(across(n.x:n.y, ~replace_na(., 0)))

  df <- df %>%
    mutate(
      signif = list(n.x, n.y, total.x, total.y) %>% pmap(~ {
        prop.test(
          x = c(..1, ..2),
          n = c(..3, ..4),
          conf.level = as.numeric(conf_level)
        ) %>% broom::tidy()
      })) %>%
    unnest(cols = signif) %>%
    mutate(test = as.numeric(grp1) - (min(as.numeric(grp1) - 1)),
           test = LETTERS[test]) %>%
    mutate(test = ifelse(((p.value < (1 - as.numeric(conf_level))) & (estimate2 > estimate1)), test, NA)) %>%
    filter(!is.na(test)) %>%
    group_by(grp2, choice_text, target) %>%
    summarise(sig.test = paste(test, collapse = ", "))

  tmp_out <- tbl %>%
    left_join(df, by = c("group" = 'grp2',
                         "choice_text" = "choice_text",
                         "target" = "target")) %>%
    mutate(frac = scales::percent(frac, accuracy = 1)) %>%
    unite("frac", c(frac, sig.test), sep = "-") %>%
    mutate(frac = str_replace_all(frac, "-NA", "")) %>%
    filter(!group %in% grp_filter)

  total_row <- tmp_out %>%
    ungroup() %>%
    distinct(group, total) %>%
    janitor::adorn_totals() %>%
    mutate(test = row_number(),
           test = ifelse(group != "Total",
                         LETTERS[test], "-")) %>%
    t() %>%
    data.frame() %>%
    janitor::row_to_names(row_number = 1) %>%
    tibble() %>%
    mutate(across(everything(), str_trim)) %>%
    relocate(Total, .before = everything()) %>%
    mutate(choice_text = "Total Count (Answering)",
           group = "") %>%
    relocate(choice_text, .before = everything())

  total_frac <- tmp_out %>%
    ungroup() %>%
    group_by(choice_text, target) %>%
    tally(n) %>%
    mutate(Total = n/as.numeric(total_row$Total[1]),
           Total = scales::percent(Total, accuracy = 1)) %>%
    group_by(choice_text) %>%
    mutate(choice = row_number()) %>%
    group_by(choice) %>%
    mutate(id = cumsum(!duplicated(choice_text)),
           group = LETTERS[id]) %>%
    mutate(choice_text = ifelse(choice == min(.$choice), gsub("\\:.*", "", choice_text),
                                ifelse(choice == max(.$choice), gsub(".*\\:", "", choice_text),
                                       "---"))) %>%
    ungroup %>%
    select(choice, Total, group)

  tbl_data <- bind_rows(total_row, tmp_out %>%
                          pivot_wider(id_cols = c(choice_text, target),
                                      names_from = group,
                                      values_from = frac) %>%
                          group_by(choice_text) %>%
                          mutate(choice = row_number()) %>%
                          group_by(choice) %>%
                          mutate(id = cumsum(!duplicated(choice_text)),
                                 group = LETTERS[id]) %>%
                          mutate(choice_text = ifelse(choice == min(.$choice), gsub("\\:.*", "", choice_text),
                                                      ifelse(choice == max(.$choice), gsub(".*\\:", "", choice_text),
                                                             "---"))) %>%
                          ungroup %>%
                          select(-c(target, id)) %>%
                          left_join(total_frac, by = c("choice" = "choice",
                                                       "group" = "group")) %>%
                          relocate(Total, .after = "choice_text")) %>%
    select(-choice) %>%
    mutate(id = row_number(), # used to set rule for % formatting
           choice_text = ifelse(Total == "-", "", choice_text)) %>%
    rename(target = choice_text,
           target_group = group)

}

# rank order
sig_test_ro <- function(tbl, totals, conf_level, grp_filter) {

  # pull the grouping variable
  grp1 <- tbl %>%
    distinct(group) %>%
    filter(!group %in% grp_filter) %>%
    pull()

  # cross together so we get every pairing
  groups <- crossing(grp1 = grp1, grp2 = grp1, choice_text = tbl$choice_text, target = tbl$target) %>%
    arrange(target) %>%
    distinct(grp1, grp2, choice_text, target, .keep_all = TRUE)

  df <- groups %>%
    left_join(select(tbl, c(group, choice_text, target, n)),
              by = c("grp1" = "group",
                     "choice_text" = "choice_text",
                     "target" = "target")) %>%
    left_join(select(tbl, c(group, choice_text, target, n)),
              by = c("grp2" = "group",
                     "choice_text" = "choice_text",
                     "target" = "target")) %>%
    left_join(totals, by = c("grp1" = "group")) %>%
    left_join(totals, by = c("grp2" = "group")) %>%
    filter(grp1 %in% tbl[!tbl$group %in% grp_filter,]$group) %>%
    filter(grp2 %in% tbl[!tbl$group %in% grp_filter,]$group) %>%
    filter(grp1 != grp2) %>%
    mutate(across(n.x:n.y, ~replace_na(., 0)))

  df <- df %>%
    mutate(
      signif = list(n.x, n.y, total.x, total.y) %>% pmap(~ {
        prop.test(
          x = c(..1, ..2),
          n = c(..3, ..4),
          conf.level = as.numeric(conf_level)
        ) %>% broom::tidy()
      })) %>%
    unnest(cols = signif) %>%
    mutate(test = as.numeric(grp1) - (min(as.numeric(grp1) - 1)),
           test = LETTERS[test]) %>%
    mutate(test = ifelse(((p.value < (1 - as.numeric(conf_level))) & (estimate2 > estimate1)), test, NA)) %>%
    filter(!is.na(test)) %>%
    group_by(grp2, choice_text, target) %>%
    summarise(sig.test = paste(test, collapse = ", "))

  tmp_out <- tbl %>%
    left_join(df, by = c("group" = 'grp2',
                         "choice_text" = "choice_text",
                         "target" = "target")) %>%
    mutate(frac = scales::percent(frac, accuracy = 1)) %>%
    unite("frac", c(frac, sig.test), sep = "-") %>%
    mutate(frac = str_replace_all(frac, "-NA", "")) %>%
    filter(!group %in% grp_filter)

  total_row <- tmp_out %>%
    ungroup() %>%
    distinct(group, total) %>%
    janitor::adorn_totals() %>%
    mutate(test = row_number(),
           test = ifelse(group != "Total",
                         LETTERS[test], "-")) %>%
    t() %>%
    data.frame() %>%
    janitor::row_to_names(row_number = 1) %>%
    tibble() %>%
    mutate(across(everything(), str_trim)) %>%
    relocate(Total, .before = everything()) %>%
    mutate(choice_text = "Total Count (Answering)",
           target = "") %>%
    relocate(target, .before = everything()) %>%
    relocate(choice_text, .before = everything())

  total_frac <- tmp_out %>%
    ungroup() %>%
    group_by(choice_text, target) %>%
    tally(n) %>%
    mutate(Total = n/as.numeric(total_row$Total[1]),
           Total = scales::percent(Total, accuracy = 1))

  tbl_data <- bind_rows(total_row, tmp_out %>%
                          pivot_wider(id_cols = c(choice_text, target),
                                      names_from = group,
                                      values_from = frac) %>%
                          mutate(target = as.character(target)) %>%
                          left_join(select(total_frac, -n)) %>%
                          relocate(Total, .after = "target")) %>%
    mutate(id = row_number(), # used to set rule for % formatting
           choice_text = ifelse(Total == "-", "", choice_text)) %>%
    rename(target = choice_text,
           target_group = target)
}

# function to pull responses, merge on responseID, and crosstab
build_crosstab <- function(group, target, conf_level, group_filter) {

  qs <- c(group, target)
  # pull responses
  tmp_responses <- map(qs, get_responses) %>%
    set_names(qs)

  # merge together on ResponseId
  merged <- tmp_responses[[1]] %>%
    left_join(tmp_responses[[2]], by = "ResponseId") %>%
    rename(group = value.x, target = value.y)

  # pull question and selector type of target variable
  target_qt <- attributes(tmp_responses[[2]])$question_type
  target_st <- attributes(tmp_responses[[2]])$selector_type

  # run check on PGR groupings
  if (target_qt == "PGR") {
    grps <- nrow(data.table::rbindlist(d$questions[[qs[2]]]$groups) %>%
                   as_tibble() %>%
                   rename(name = recode, choice_text = description))
    if (grps <= 1) {
      target_qt <- "RO"
    }
  }

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

    tbl_data <- sig_test_mc(out, group_totals, conf_level, group_filter)

  } else if (target_qt %in% c("Matrix", "RO", "PGR")) {

    out <- merged %>%
      count(choice_text, target, group) %>%
      filter(!is.na(target)) %>%
      left_join(group_totals) %>%
      mutate(frac = n/total) %>%
      ungroup()

    if (target_st != "Bipolar") {

      tbl_data <- sig_test_matrix(out, group_totals, conf_level, group_filter)


    } else if (target_st == "Bipolar") {

      tbl_data <- sig_test_bipolar(out, group_totals, conf_level, group_filter)

    }
  } else if (target_qt == "RO") {

    tbl_data <- sig_test_ro(out, group_totals, conf_level, group_filter)

  } else if (target_qt == "DD") {

    # THE DD HASN'T BEEN FINISHED YET
    out <- merged %>%
      count(choice_text, group) %>%
      left_join(group_totals) %>%
      mutate(frac = n/total) %>%
      ungroup()
  }

  # set attributes for further use
  attr(tbl_data, "target_qt") <- attributes(tmp_responses[[2]])$question_type
  attr(tbl_data, "target_st") <- attributes(tmp_responses[[2]])$selector_type
  attr(tbl_data, "target_qtext") <- attributes(tmp_responses[[2]])$question_text
  attr(tbl_data, "group_qtext") <- attributes(tmp_responses[[1]])$question_text
  attr(tbl_data, "group_qt") <- attributes(tmp_responses[[1]])$question_type
  attr(tbl_data, "group_st") <- attributes(tmp_responses[[1]])$selector_type
  attr(tbl_data, "nsize") <- length(unique(merged[!is.na(merged$target),]$ResponseId))
  attr(tbl_data, 'conf_level') <- scales::percent(as.numeric(conf_level), accuracy = 1)
  tbl_data

}
