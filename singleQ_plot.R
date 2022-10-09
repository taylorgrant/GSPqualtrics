# Dataviz Single Question Data # 

singleQ_barplot <- function(dat, title, nsize, color) {
  add_title_break <- function(x) gsub("(.{55,}?)\\s", "\\1\n", x)
  
  # graph -------------------------------------------------------------------
  # function to add a line break to title in front of a word (based on count of spaces)
  
  pdat <- dat %>%
    mutate(txtcol = ifelse(frac < .075, "black", "white"),
           v_just = ifelse(frac < .075, -0.2, 1.4),
           h_just = ifelse(frac < .075, -.1, 1.2),
           p1size = ifelse(nrow(dat) > 6, 3, 4))
  
  # add angle if value is too long
  if (any(nchar(as.character(pdat$value)) > 10) && nrow(pdat) > 3) {
    pdat$txtangle <- 45
    pdat$txtjust <- 1
    pdat$expansion <- 1.5
  } else {
    pdat$txtangle <- 0
    pdat$txtjust <- 0.5
    pdat$expansion <- 0
  }
  
  ptitle <- ifelse(str_count(title, fixed(' ')) > 7, 
                   add_title_break(title), title)
  
  # graph (vertical bar)
  p1 <- ggplot(pdat, aes(x = value, y = frac)) +
    geom_bar(stat = "identity", fill = color) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(expand = expansion(add = unique(pdat$expansion))) +
    geom_text(aes(x = value, y = frac, 
                  label = scales::percent(frac, accuracy = .1)),
              vjust = pdat$v_just, col = pdat$txtcol,
              size = pdat$p1size) +
    labs(x = NULL, y = "Percent",
         title = ptitle,
         caption = glue::glue("Source: GS&P {d$name}\nN-size: {nsize} respondents")) + 
    theme_xf() +
    theme(plot.title.position = "plot",
          panel.background = element_rect(fill = "white",
                                          colour = "white"),
          plot.background = element_rect(fill = "white",
                                         color = 'white'),
          axis.text.x = element_text(angle = unique(pdat$txtangle), 
                                     hjust = unique(pdat$txtjust)))
  
  # graph (horizontal bar)
  p1_flip <- ggplot(pdat, aes(x = value, y = frac)) +
    geom_bar(stat = "identity", fill = color) +
    scale_y_continuous(labels = scales::percent) +
    geom_text(aes(x =value, y = frac, 
                  label = scales::percent(frac, accuracy = .1)),
              hjust = pdat$h_just, col = pdat$txtcol) +
    labs(x = NULL, y = "Percent",
         title = ptitle,
         caption = glue::glue("Source: GS&P {d$name}\nN-size: {nsize} respondents")) + 
    theme_xf() +
    coord_flip() +
    theme(plot.title.position = "plot",
          panel.background = element_rect(fill = "white",
                                          colour = "white"),
          plot.background = element_rect(fill = "white",
                                         color = 'white'))
  data_plot <- list(data = pdat, p1 = p1, p1_flip = p1_flip)
}


rankorderQ_barplot <- function(dat, title, nsize) {
  # add a palette
  b2y <- c("#115f9a", "#1984c5", "#22a7f0", "#48b5c4", "#76c68f", "#a6d75b", "#c9e52f", "#d0ee11", "#d0f400")
  
  add_title_break <- function(x) gsub("(.{55,}?)\\s", "\\1\n", x)
  
  # graph -------------------------------------------------------------------
  # function to add a line break to title in front of a word (based on count of spaces)
  
  pdat <- dat %>%
    mutate(txtcol = ifelse(value <= 4, "white", "#5A6366"))
  
  ptitle <- ifelse(str_count(title, fixed(' ')) > 7, 
                   add_title_break(title), title)
  
  p1 <- ggplot(pdat, aes(x = choice_text, y = frac, group = factor(value), fill = factor(value))) + 
    geom_bar(stat = "identity", position = ggplot2::position_stack(reverse = TRUE)) + 
    scale_fill_manual(values = b2y, name = "Rank") +
    scale_y_continuous(labels = scales::percent) +
    geom_text(aes(x = choice_text, y = frac, label = scales::percent(frac, accuracy = 1)),
              position = position_stack(vjust = .5, reverse = TRUE),
              color = pdat$txtcol) +
    theme_xf() +
    theme(plot.title.position = "plot",
          panel.background = element_rect(fill = "white",
                                          colour = "white"),
          plot.background = element_rect(fill = "white",
                                         color = 'white')) +
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(x = NULL, y = "Percent",
         title = ptitle,
         caption = glue::glue("Source: GS&P {d$name}\nN-size: {nsize} respondents"))
  
  p1_flip <- ggplot(pdat, aes(x = choice_text, y = frac, group = factor(value), fill = factor(value))) + 
    geom_bar(stat = "identity", position = ggplot2::position_stack(reverse = TRUE)) +
    scale_fill_manual(values = b2y, name = "Rank") + 
    scale_y_continuous(labels = scales::percent) +
    geom_text(aes(x = choice_text, y = frac, label = scales::percent(frac, accuracy = 1)),
              position = position_stack(vjust = .5, reverse = TRUE),
              color = pdat$txtcol) + 
    theme_xf() +
    theme(plot.title.position = "plot",
          panel.background = element_rect(fill = "white",
                                          colour = "white"),
          plot.background = element_rect(fill = "white",
                                         color = 'white')) +
    guides(fill = guide_legend(reverse = TRUE)) + 
    labs(x = NULL, y = "Percent",
         title = ptitle,
         caption = glue::glue("Source: GS&P {d$name}\nN-size: {nsize} respondents")) + 
    coord_flip()
  
  data_plot <- list(data = pdat, p1 = p1, p1_flip = p1_flip)
}

matrixQ_barplot <- function(dat, title, nsize) {
  
  add_title_break <- function(x) gsub("(.{55,}?)\\s", "\\1\n", x)
  
  # check to see if matrix question is statement comparison
  if (any(str_detect(dat$choice_text, "[A-Za-z]:[A-Za-z]"))) {
    # add a palette
    pal_choice <- function(tbl) {
      if (length(unique(tbl$value)) == 2) {
        c("#fafa6e", "#2a4858")
      } else if (length(unique(tbl$value)) == 3) {
        c("#fafa6e","#23aa8f","#2a4858")
      } else if (length(unique(tbl$value)) == 4) {
        c("#fafa6e","#64c987","#00898a","#2a4858")
      } else {
        c("#fafa6e","#86d780","#23aa8f","#007882", "#2a4858")
      }
    }
    pal <- pal_choice(dat)
    add_statement_break <- function(x) gsub("(.{20,}?)\\s", "\\1\n", x)
    
    # add line breaks to statement and created grouped id for plot
    pdat <- separate(dat, choice_text, 
                    into = c("statement_a", "statement_b"), sep = ":") %>% 
      mutate(statement_a = gsub("[\r\n\t]", " ", statement_a),
             statement_b = gsub("[\r\n\t]", " ", statement_b),
             statement_a = add_statement_break(statement_a),
             statement_b = add_statement_break(statement_b)) %>%
      group_by(value) %>%
      mutate(id = cumsum(!duplicated(statement_a))) %>% 
      group_by(id) %>% 
      mutate(choice_count = row_number(),
             txtcol = ifelse(choice_count > 2, "white", "black"))
    
    # line break for the title
    ptitle <- ifelse(str_count(title, fixed(' ')) > 7, 
                     add_title_break(title), title)
    
    p1 <- ggplot(pdat, aes(x = frac, y = reorder(statement_a, id),
                    fill = value)) +
      geom_bar(stat = "identity", 
               position = ggplot2::position_stack(reverse = TRUE)) +
      scale_x_continuous(labels = scales::percent) +
      scale_fill_manual(values = pal) +
    guides(y.sec = ggh4x::guide_axis_manual(
      breaks = pdat$id,
      labels = pdat$statement_b)) +
      geom_text(aes(x = frac, y = statement_a,
                    label = scales::percent(frac, accuracy = 1)),
                col = pdat$txtcol,
                position = position_stack(vjust = .5, reverse = TRUE)) +
      labs(x = NULL, y= NULL,
           title = ptitle,
           caption = glue::glue("Source: GS&P {d$name}\nN-size: {nsize} respondents")) +
      theme(legend.position = "none",
            plot.title.position = "plot")
  } else {
    pdat <- dat %>%
      mutate(txtcol = ifelse(frac < .075, "black", "black"),
             v_just = ifelse(frac < .075, -0.2, -0.2),
             h_just = ifelse(frac < .075, -.1, 1.2),
             p1size = ifelse(nrow(dat) > 6, 2, 4))
    
    # add angle if value is too long
    if (any(nchar(as.character(pdat$value)) > 10) && nrow(pdat) > 3) {
      pdat$txtangle <- 45
      pdat$txtjust <- 1
      pdat$expansion <- 1.5
    } else {
      pdat$txtangle <- 0
      pdat$txtjust <- 0.5
      pdat$expansion <- 0
    }
    
    ptitle <- ifelse(str_count(title, fixed(' ')) > 7, 
                     add_title_break(title), title)
    
    # graph (vertical bar)
    p1 <- ggplot(pdat, aes(x = choice_text, y = frac, group = value, fill = value)) +
      geom_bar(stat = "identity", position = "dodge") + 
      scale_y_continuous(labels = scales::percent) +
      scale_x_discrete(expand = expansion(add = unique(pdat$expansion))) +
      geom_text(aes(x = choice_text, y = frac, 
                    label = scales::percent(frac, accuracy = .1)),
                position = position_dodge(width = .85),
                vjust = pdat$v_just,
                col = pdat$txtcol,
                size = pdat$p1size
                ) +
      labs(x = NULL, y = "Percent",
           title = ptitle,
           caption = glue::glue("Source: GS&P {d$name}\nN-size: {nsize} respondents")) + 
      theme_xf() +
      theme(plot.title.position = "plot",
            panel.background = element_rect(fill = "white",
                                            colour = "white"),
            plot.background = element_rect(fill = "white",
                                           color = 'white'),
            axis.text.x = element_text(angle = unique(pdat$txtangle), 
                                       hjust = unique(pdat$txtjust)))
  }
  
  data_plot <- list(data = pdat, p1 = p1)
}
