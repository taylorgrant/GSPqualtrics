# GT Table - Crosstab Data # 

multiQ_table <- function(dat, target_filter = NULL) {
  
  if (is.null(target_filter) == FALSE) {
    dat <- dat |> 
      filter(!target_group %in% target_filter)
  } else {
    dat
  }
  
  gt_crosstab <- dat |> 
    gt(rowname_col = "target",
       groupname_col = "target_group") |> 
  # style the table
    # title 
    tab_header( 
      title = attributes(dat)$target_qtext
    ) |>
    # caption 
    tab_source_note( # adding source note
      source_note = html(glue::glue("<em>Source: GS&P {d$name} <br> N-size: {attributes(dat)$nsize} respondents<br>Letters indicate statistical significance at {attributes(dat)$conf_level}<br>Column percentages won't always sum to 100% due to rounding or multi-select options</em>")) 
    ) |>
    # add spanner 
    tab_spanner(
      label = attributes(dat)$group_qtext,
      columns = everything()
    ) |>
    # format percentages (hold out first row)
    text_transform(
      locations = cells_body(
        rows = id > 1
      ),
      fn = function(x) {
        str_replace_all(x,
                        pattern = "-",
                        replacement = "<SUP STYLE='font-size:xx-small'>") %>% 
          str_replace_all("~",
                          "</sup>") }
    ) |>
    # styling the table --------------------------- 
  opt_table_font(
    font = list(
      google_font("Open Sans")
    )
  ) |>
    # aligning the title and subtitle left
    tab_style(
      style = cell_text(align = 'left',
                        weight = 'bold',
                        size = px(18)),
      locations = cells_title(c("title"))
    ) |>
    # alignment of spanner
    tab_style(
      style = cell_text(weight = "bold",
                        size = px(13),
                        align = "center"),
      locations = list(
        cells_column_spanners(everything())
      )
    ) |>
    # bolding groups and columns
    tab_style(
      style = cell_text(weight = 'bold',
                        size = px(13)),
      locations = list(
        cells_column_labels(
          columns = everything()),
        cells_row_groups(groups = TRUE))
    ) |>
    # aligning columnn labels
    tab_style(
      style = cell_text(align = "center"),
      locations = list(
        cells_column_labels(
          columns = everything()
        )
      )
    ) |>
    # indenting the stub (more room for the identifiers)
    tab_style( 
      style = cell_text(size = px(12)),
      locations = cells_stub()
    ) |>
    # font size in table
    tab_style(
      style = cell_text(size = px(12),
                        align = 'center'),
      locations = cells_body(
        columns = everything())
    ) |>
    # hide id column
    cols_hide(
      columns = c("id")
    ) |>
    # format missing data
    sub_missing(
      columns = everything(),
      missing_text = "0%"
    ) |>
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
