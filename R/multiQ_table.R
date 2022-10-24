# GT Table - Crosstab Data # 

multiQ_table <- function(dat) {
  
  if (attributes(dat)$target_qt %in% c("MC", "TE_AGE", "Slider")) {
    
    # create totals
    total_row <- dat %>% 
      ungroup() %>% 
      distinct(group, total) %>% 
      janitor::adorn_totals() %>%
      pivot_wider(names_from = group, 
                  values_from = total) %>% 
      relocate(Total, .before = everything()) %>%
      mutate(target = "Total Count (Answering)") %>%
      relocate(target, .before = everything())
    
    # create total percentages
    total_frac <- dat %>% 
      ungroup() %>% 
      group_by(target) %>% 
      tally(n) %>% 
      mutate(Total = n/total_row$Total,
             target = as.character(target))
    
    # build/format GT table
    tbl_data <- bind_rows(total_row, dat %>% 
                            pivot_wider(id_cols = c(target), 
                                        names_from = group, 
                                        values_from = frac) %>%
                            mutate(target = as.character(target)) %>%
                            left_join(select(total_frac, -n)) %>% 
                            relocate(Total, .after = "target")) %>% 
      mutate(id = row_number(), # used to set rule for % formatting
             target_group = ifelse(str_detect(target, "Total Count"), "", "Response"))
    
    g <- tbl_data %>% 
      gt(rowname_col = "target",
         groupname_col = "target_group")
    
  } else if (attributes(dat)$target_qt %in% c("Matrix", "PGR")) {
    
    # if not Bipolar
    if (attributes(dat)$target_st != "Bipolar") {
      
      # create totals
      total_row <- dat %>% 
        ungroup() %>% 
        distinct(group, total) %>% 
        janitor::adorn_totals() %>%
        pivot_wider(names_from = group, 
                    values_from = total) %>% 
        relocate(Total, .before = everything()) %>%
        mutate(target = "Total Count (Answering)",
               choice_text = "") %>%
        relocate(target, .before = everything()) %>% 
        relocate(choice_text, .before = everything())
      
      # create total percentages
      total_frac <- dat %>% 
        ungroup() %>% 
        group_by(choice_text, target) %>% 
        tally(n) %>% 
        mutate(Total = n/total_row$Total)
      
      # build/format GT table
      tbl_data <- bind_rows(total_row, dat %>% 
                              pivot_wider(id_cols = c(choice_text, target),
                                          names_from = group,
                                          values_from = frac) %>% 
                              left_join(select(total_frac, -n)) %>%
                              relocate(Total, .after = "target")) %>% 
        mutate(id = row_number())  %>% # used to set rule for % formatting
        rename(target_group = choice_text) # rename for table template
      
      g <- tbl_data %>% 
        gt(rowname_col = "target",
           groupname_col = "target_group")
      
    } else if (attributes(dat)$target_st == "Bipolar") {
      # create totals
      total_row <- dat %>% 
        ungroup() %>% 
        distinct(group, total) %>% 
        janitor::adorn_totals() %>%
        pivot_wider(names_from = group, 
                    values_from = total) %>% 
        relocate(Total, .before = everything()) %>%
        mutate(choice_text = "Total Count (Answering)",
               group = "") %>%
        relocate(choice_text, .before = everything())
      
      # create total percentages
      total_frac <- dat %>% 
        ungroup() %>% 
        group_by(choice_text, target) %>% 
        tally(n) %>% 
        mutate(Total = n/total_row$Total) %>% 
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
      
      
      # build/format GT table
      tbl_data <- bind_rows(total_row, dat %>%
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
        mutate(id = row_number()) %>% 
        rename(target = choice_text, 
               target_group = group)
      
      g <- tbl_data %>% 
        gt(rowname_col = "target",
           groupname_col = "target_group")
    }
    } else if (attributes(dat)$target_qt == "RO") {
      
      # create totals
      total_row <- dat %>% 
        ungroup() %>% 
        distinct(group, total) %>% 
        janitor::adorn_totals() %>%
        pivot_wider(names_from = group, 
                    values_from = total) %>% 
        relocate(Total, .before = everything()) %>%
        mutate(choice_text = "Total Count (Answering)",
               target = "") %>%
        relocate(target, .before = everything()) %>% 
        relocate(choice_text, .before = everything())
      
      # create total percentages
      total_frac <- dat %>% 
        ungroup() %>% 
        group_by(choice_text, target) %>% 
        tally(n) %>% 
        mutate(Total = n/total_row$Total)
      
      # build/format GT table
      tbl_data <- bind_rows(total_row, dat %>% 
                              pivot_wider(id_cols = c(choice_text, target),
                                          names_from = group,
                                          values_from = frac) %>% 
                              left_join(select(total_frac, -n)) %>%
                              relocate(Total, .after = "target")) %>% 
        mutate(id = row_number()) %>% # used to set rule for % formatting
        rename(target = choice_text, 
               target_group = target)
      
      g <- tbl_data %>% 
        gt(rowname_col = "target",
           groupname_col = "target_group")
    }
  
  # style the table
  g %>% 
    # title 
    tab_header( 
      title = attributes(dat)$target_qtext
    ) %>%
    # caption 
    tab_source_note( # adding source note
      source_note = html(glue::glue("<em>Source: GS&P {d$name} <br> N-size: {attributes(dat)$nsize} respondents<br>Column percentages won't always sum to 100% due to rounding or multi-select options</em>")) 
    ) %>%
    # add spanner 
    tab_spanner(
      label = attributes(dat)$group_qtext,
      columns = everything()
    ) %>%
    # format percentages (hold out first row)
    fmt_percent(
      rows = id > 1,
      columns = everything(),
      decimals = 0
    ) %>%
    # styling the table --------------------------- 
  opt_table_font(
    font = list(
      google_font("Open Sans")
    )
  ) %>%
    # aligning the title and subtitle left
    tab_style(
      style = cell_text(align = 'left',
                        weight = 'bold',
                        size = px(18)),
      locations = cells_title(c("title"))
    ) %>%
    # alignment of spanner
    tab_style(
      style = cell_text(weight = "bold",
                        size = px(13),
                        align = "center"),
      locations = list(
        cells_column_spanners(everything())
      )
    ) %>%
    # bolding groups and columns
    tab_style(
      style = cell_text(weight = 'bold',
                        size = px(13)),
      locations = list(
        cells_column_labels(
          columns = everything()),
        cells_row_groups(groups = TRUE))
    ) %>%
    # aligning columnn labels
    tab_style(
      style = cell_text(align = "center"),
      locations = list(
        cells_column_labels(
          columns = everything()
        )
      )
    ) %>%
    # indenting the stub (more room for the identifiers)
    tab_style( 
      style = cell_text(size = px(12)),
      locations = cells_stub()
    ) %>%
    # font size in table
    tab_style(
      style = cell_text(size = px(12),
                        align = 'center'),
      locations = cells_body(
        columns = everything())
    ) %>%
    # hide id column
    cols_hide(
      columns = "id"
    ) %>%
    # format missing data
    sub_missing(
      columns = everything(),
      missing_text = "---"
    ) %>%
    # final options
    tab_options(
      data_row.padding = px(6),
      row_group.padding = px(6),
      source_notes.font.size = px(10),
      footnotes.font.size = px(10),
      footnotes.marks = "LETTERS",
      table.font.names = "Open Sans"
    )
}
