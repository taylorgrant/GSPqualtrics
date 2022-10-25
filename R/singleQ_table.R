# GT Table - Single Question Data # 

singleQ_table <- function(dat, meta, nsize) {
 
  if (meta$question_type %in% c('MC', "TE_AGE", "Slider")) {
  
    gt_tbl <- dat %>%
      gt() %>% 
      # title and subtitle
      tab_header( 
        title = meta$question_text
      ) %>%
      # caption 
      tab_source_note( # adding source note
        source_note = html(glue::glue("<em>Source: GS&P {d$name} <br> N-size: {nsize} respondents</em>")) 
      ) %>%
      fmt_percent(
        columns = matches("frac"),
        decimals = 0
      )  %>%
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
      # bolding groups and columns
      tab_style(
        style = cell_text(weight = 'bold',
                          size = px(13)),
        locations = list(
          cells_column_labels(
            columns = everything()),
          cells_row_groups(groups = TRUE))
      ) %>%
      # align column headers
      tab_style(
        style = cell_text(align = "left"),
        locations = list(
          cells_column_labels(
            columns = "value"
          )
        )
      ) %>%
      tab_style(
        style = cell_text(align = "center"),
        locations = list(
          cells_column_labels(
            columns = matches("n|frac")
          )
        )
      ) %>%
      # font size in table
      tab_style(
        style = cell_text(size = px(12)),
        locations = cells_body(
          columns = everything())
      ) %>%
      # font alignment
      tab_style(
        style = cell_text(align = "left"),
        locations = cells_body(
          columns = matches("value"))
      ) %>%
      # font alignment
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_body(
          columns = matches("n|frac"))
      ) %>%
      # rename columns if necessary
      cols_label(
        value = "Response", n = "Count",
        frac = "Percent"
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
    
  } else if (meta$question_type == "Matrix") {
    
    if (meta$selector_type == "Bipolar") {
      
      dat %>% 
        group_by(statement_a) %>% 
        mutate(choice = row_number()) %>% 
        select(-c(value, n)) %>% 
        pivot_wider(id_cols = c(statement_a, statement_b), 
                    names_from = choice,
                    values_from = frac) %>% 
        relocate(statement_b, .after = everything()) %>%
        ungroup() %>% 
        gt() %>%
        # title and subtitle
        tab_header( 
          title = meta$question_text
        ) %>%
        # caption 
        tab_source_note( # adding source note
          source_note = html(glue::glue("<em>Source: GS&P {d$name} <br> N-size: {nsize} respondents</em>")) 
        ) %>%
        # format our percentage options
        fmt_percent(
          columns = !contains("statement"),
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
          # bolding groups and columns
        tab_style(
          style = cell_text(weight = 'bold',
                            size = px(13)),
          locations = list(
            cells_column_labels(
            columns = everything()),
            cells_row_groups(groups = TRUE))
          ) %>%
          # align column headers
        tab_style(
          style = cell_text(align = "left"),
          locations = list(
            cells_column_labels(
            columns = "statement_a"
              )
            )
          ) %>%
        tab_style(
          style = cell_text(align = "right"),
          locations = list(
            cells_column_labels(
              columns = "statement_b"
            )
          )
        ) %>%
        tab_style(
          style = cell_text(align = "center"),
          locations = list(
            cells_column_labels(
              columns = !contains("statement")
              )
            )
          ) %>%
          # font size in table
        tab_style(
          style = cell_text(size = px(12)),
          locations = cells_body(
            columns = everything())
          ) %>%
        # font alignment in table
        tab_style(
          style = cell_text(align = "left"),
          locations = cells_body(
            columns = "statement_a")
          ) %>%
        tab_style(
          style = cell_text(align = "right"),
          locations = cells_body(
            columns = "statement_b")
        ) %>%
        # font alignment
        tab_style(
          style = cell_text(align = "center"),
          locations = cells_body(
            columns = !contains('statement'))
          ) %>%
        # rename columns if necessary
        cols_label(
          statement_a = "Statement A",
          statement_b = "Statement B"
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
      
    } else {
      
      if (length(unique(dat$value)) <= 5) {
        gt_tbl <- dat %>% 
          select(choice_text, value, frac) %>% 
          pivot_wider(id_cols = choice_text, 
                      names_from = value, 
                      values_from = frac) %>%
          ungroup %>%
          gt(rowname_col = "choice_text") %>% 
          # title and subtitle
          tab_header( 
            title = meta$question_text
          ) %>%
          # caption 
          tab_source_note( # adding source note
            source_note = html(glue::glue("<em>Source: GS&P {d$name} <br> N-size: {nsize} respondents</em>")) 
          ) %>% 
          # format our rankings into percentages
          fmt_percent(
            columns = everything(),
            decimals = 0
          )  %>%
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
          # align column headers
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
            style = cell_text(size = px(12)),
            locations = cells_body(
              columns = everything())
          ) %>%
          # font alignment
          tab_style(
            style = cell_text(align = "center"),
            locations = cells_body(
              columns = everything()
            )
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
      } else {
        gt_tbl <- dat %>% 
          select(choice_text, value, frac) %>% 
          pivot_wider(id_cols = value, 
                      names_from = choice_text, 
                      values_from = frac) %>%
          ungroup %>%
          gt(rowname_col = "value") %>% 
          # title and subtitle
          tab_header( 
            title = meta$question_text
          ) %>%
          # caption 
          tab_source_note( # adding source note
            source_note = html(glue::glue("<em>Source: GS&P {d$name} <br> N-size: {nsize} respondents</em>")) 
          ) %>% 
          # format our rankings into percentages
          fmt_percent(
            columns = everything(),
            decimals = 0
          )  %>%
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
          # align column headers
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
            style = cell_text(size = px(12)),
            locations = cells_body(
              columns = everything())
          ) %>%
          # font alignment
          tab_style(
            style = cell_text(align = "center"),
            locations = cells_body(
              columns = everything()
            )
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
    }
    } else if (meta$question_type == "RO") {
    
    gt_table <- dat %>% 
      select(choice_text, value, frac) %>% 
      pivot_wider(id_cols = choice_text, 
                  names_from = value, 
                  values_from = frac) %>%
      ungroup %>%
      gt(rowname_col = "choice_text") %>% 
      # title and subtitle
      tab_header( 
        title = meta$question_text
      ) %>%
      # caption 
      tab_source_note( # adding source note
        source_note = html(glue::glue("<em>Source: GS&P {d$name} <br> N-size: {nsize} respondents</em>")) 
      ) %>% 
      # add spanner 
      tab_spanner(
        label = "Rank",
        columns = everything()
      ) %>%
      # format our rankings into percentages
      fmt_percent(
        columns = everything(),
        decimals = 0
      )  %>%
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
      # align column headers
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
        style = cell_text(size = px(12)),
        locations = cells_body(
          columns = everything())
      ) %>%
      # font alignment
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_body(
          columns = everything()
          )
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
    
    } else if (meta$question_type == "PGR") {
      gt_tbl <- dat %>% 
        select(choice_text, value, frac) %>% 
        pivot_wider(id_cols = value, 
                    names_from = choice_text, 
                    values_from = frac) %>%
        ungroup %>%
        gt(rowname_col = "value") %>%
        # title and subtitle
        tab_header( 
          title = meta$question_text
        ) %>%
        # caption 
        tab_source_note( # adding source note
          source_note = html(glue::glue("<em>Source: GS&P {d$name} <br> N-size: {nsize} respondents</em>")) 
        ) %>% 
        # add spanner over WAR
        tab_spanner(
          label = "Rank",
          columns = everything()
        ) %>%
        # format our rankings into percentages
        fmt_percent(
          columns = everything(),
          decimals = 0
        )  %>%
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
        # align column headers
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
          style = cell_text(size = px(12)),
          locations = cells_body(
            columns = everything())
        ) %>%
        # font alignment
        tab_style(
          style = cell_text(align = "center"),
          locations = cells_body(
            columns = everything()
          )
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
}


