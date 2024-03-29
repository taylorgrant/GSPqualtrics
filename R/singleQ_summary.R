# dataViz Single Question Summary #

singleQ_summary <- function(q, color, ordered) {

  add_break <- function(x) gsub("(.{28,}?)\\s", "\\1\n", x)
  meta <- toc %>% filter(question_id == q)

  if (meta$question_type == "MC") {

    # get question names to pull columns from survey
    qpull <- svy_q %>%
      filter(question_id == q) %>%
      filter(!str_detect(export_name, "TEXT"))
    # get the possible options of the MC question
    qchoice <- svy_choice %>%
      filter(question_id == q) %>%
      mutate(choice_text = add_break(choice_text)) %>%
      arrange(as.numeric(choice_recode))

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

  } else if (meta$question_type == "TE_AGE") {

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
      print("line 56")
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

      out <- singleQ_plot(tmp, meta, resp_count, color)
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
        group_by(gen) %>%
        summarise(n = sum(n)) %>%
        mutate(frac = n/resp_count,
               gen = factor(gen, levels = c("Post-Z","Gen Z", "Millennial", "Gen X", "Boomers",
                                            "Silent", "Greatest"))) %>%
        rename(value = gen) %>%
        arrange(value)
    }

    } else if (meta$question_type == "RO") {

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

  } else if (meta$question_type == "Slider") {

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
      mutate(choice_text = add_break(choice_text)) %>%
      arrange(as.numeric(choice_recode))

    tmp <- svy %>%
      dplyr::select(ResponseId, all_of(matrix_map$name))

    # get the respondent count answering the question
    resp_count <- sum((rowSums(is.na(tmp[2:ncol(tmp)])) == ncol(tmp[2:ncol(tmp)])) == "FALSE")

    # for bipolar comparing two statements
    if (meta$selector_type == "Bipolar") {

      add_statement_break <- function(x) gsub("(.{20,}?)\\s", "\\1\n", x)

      tmp <- tmp %>%
        pivot_longer(-ResponseId) %>%
        dplyr::filter(!str_detect(name, "TEXT"),
                      !is.na(value)) %>%
        left_join(select(matrix_map, c(name, choice_text))) %>%
        mutate(choice_text = factor(choice_text, levels = dput(unique(matrix_map$choice_text))),
               value = factor(value, levels = dput(qchoice$choice_text))) %>%
        count(choice_text, value) %>%
        group_by(choice_text) %>%
        mutate(frac = n/resp_count) %>%
        separate(choice_text,
                 into = c("statement_a", "statement_b"), sep = ":") %>%
        mutate(statement_a = add_statement_break(statement_a),
               statement_b = add_statement_break(statement_b))

    } else {

      # summarize
      tmp <- tmp %>%
        pivot_longer(-ResponseId) %>%
        dplyr::filter(!str_detect(name, "TEXT"),
                      !is.na(value)) %>%
        left_join(select(matrix_map, c(name, choice_text))) %>%
        mutate(choice_text = add_break(choice_text),
               choice_text = factor(choice_text, levels = dput(add_break(unique(matrix_map$choice_text)))),
               value = add_break(value),
               value = factor(value, levels = dput(qchoice$choice_text))) %>%
        count(choice_text, value) %>%
        group_by(choice_text) %>%
        mutate(frac = n/resp_count) %>%
        group_by(choice_text)
    }

  } else if (meta$question_type == "PGR") {

    qpull <- colmap %>%
      filter(str_detect(ImportId, glue::glue("{q}_")))

    add_break <- function(x) gsub("(.{15,}?)\\s", "\\1\n", x)

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
        select(choice_text = response, value = rank) %>%
        count(choice_text, value) %>%
        mutate(frac = n/resp_count,
               choice_text = add_break(choice_text),
               value = factor(value))

      # convert question type to send to matrix plot
      meta$question_type <- "Matrix"

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
        mutate(frac = n/resp_count,
               value = add_break(value)) %>%
        group_by(choice_text) %>%
        arrange(desc(frac), .by_group = TRUE)
    }

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

      # get the respondent count answering the question
      resp_count <- sum((rowSums(is.na(tmp[2:ncol(tmp)])) == ncol(tmp[2:ncol(tmp)])) == "FALSE")

      # summarize
      tmp <- tmp %>%
        unite(choice_text, sep = " - ", !ResponseId) %>%
        count(choice_text) %>%
        mutate(frac = n/resp_count) %>%
        arrange(desc(frac))
    }

  if (ordered == "No") {
    tmp
  } else if (ordered == "Yes" & meta$question_type %in% c("MC", "TE_AGE")) {
    print("line 298")
    tmp <- tmp %>%
      mutate(value = forcats::fct_reorder(value, frac, max)) %>%
      arrange(desc(frac))
  } else if (ordered == "Yes" & meta$question_type == "Matrix") {
    # don't allow the Likert scales to reorder
    if (!meta$selector_type %in% c("Bipolar", "Likert")) {

      tmp <- tmp %>%
        group_by(choice_text) %>%
        mutate(value = forcats::fct_reorder(value, frac, max)) %>%
        arrange(desc(frac), .by_group = TRUE)
    }
  }
  out <- singleQ_plot(tmp, meta, resp_count, color, ordered)

}

