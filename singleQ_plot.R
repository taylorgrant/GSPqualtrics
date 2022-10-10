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
  
  add_title_break <- function(x) gsub("(.{50,}?)\\s", "\\1\n", x)
  
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

matrixQ_barplot <- function(dat, meta, nsize) {
  
  add_title_break <- function(x) gsub("(.{50,}?)\\s", "\\1\n", x)
  # line break for the title
  ptitle <- ifelse(str_count(meta$question_text, fixed(' ')) > 7, 
                   add_title_break(meta$question_text), meta$question_text)
  
  # check to see if matrix question is statement comparison
  if (meta$selector_type == "Bipolar") {
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
    
    # add line breaks to statement and created grouped id for plot
    pdat <- dat %>%
      group_by(value) %>%
      mutate(id = cumsum(!duplicated(statement_a))) %>% 
      group_by(id) %>% 
      mutate(choice_count = row_number(),
             txtcol = ifelse(choice_count > 2, "white", "black"))
    
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
    
    data_plot <- list(data = pdat, p1 = p1)
    
  } else { # if not bipolar
    pdat <- dat %>%
      mutate(txtcol = "black",
             v_just = -0.2,
             h_just = -.05,
             p1size = ifelse(nrow(dat) > 6, 2, 3))
    
    pal <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "#7AA6DCFF", 
             "#003C67FF", "#8F7700FF", "#3B3B3BFF", "#A73030FF", "#4A6990FF",
             "#FAFD7CFF", "#82491EFF", "#24325FFF", "#B7E4F9FF", "#FB6467FF", 
             "#526E2DFF", "#E762D7FF", "#E89242FF", "#FAE48BFF", "#A6EEE6FF", 
             "#917C5DFF", "#69C8ECFF") 
    
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
    
    # graph (vertical bar)
    p1 <- ggplot(pdat, aes(x = choice_text, y = frac, group = value, fill = value)) +
      geom_bar(stat = "identity", position = "dodge") + 
      scale_y_continuous(labels = scales::percent) +
      scale_x_discrete(expand = expansion(add = unique(pdat$expansion))) +
      scale_fill_manual(values = pal, name = NULL) +
      geom_text(aes(x = choice_text, y = frac, 
                    label = scales::percent(frac, accuracy = .1)),
                position = position_dodge(width = 1),
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
                                       hjust = unique(pdat$txtjust)),
            legend.text=element_text(size= 7),
            legend.key.size = unit(.2, 'cm'))
    
    p1_flip <- ggplot(pdat, aes(x = choice_text, y = frac, group = value, fill = value)) +
      geom_bar(stat = "identity", position = "dodge") + 
      scale_y_continuous(labels = scales::percent,
                         expand = expansion(mult = .1)) +
      geom_text(aes(x = choice_text, y = frac,
                    label = scales::percent(frac, accuracy = .1)),
                position = position_dodge(width = .85),
                hjust = pdat$h_just, col = pdat$txtcol,
                size = pdat$p1size) +
      scale_fill_manual(values = pal, name = NULL) +
      labs(x = NULL, y = "Percent",
           title = ptitle,
           caption = glue::glue("Source: GS&P {d$name}\nN-size: {nsize} respondents")) + 
      theme_xf() +
      coord_flip() +
      theme(plot.title.position = "plot",
            legend.position = "top",
            panel.background = element_rect(fill = "white",
                                            colour = "white"),
            plot.background = element_rect(fill = "white",
                                           color = 'white'),
            legend.text=element_text(size= 7),
            legend.key.size = unit(.2, 'cm')) +
      guides(fill=guide_legend(nrow=2,byrow=TRUE))
    
    data_plot <- list(data = pdat, p1 = p1, p1_flip = p1_flip)
  }
}

pgrQ_barplot <- function(dat, meta, nsize, color) {
  
  add_title_break <- function(x) gsub("(.{50,}?)\\s", "\\1\n", x)
  # line break for the title
  ptitle <- ifelse(str_count(meta$question_text, fixed(' ')) > 7, 
                   add_title_break(meta$question_text), meta$question_text)
  
  pdat <- dat %>%
    mutate(txtcol = ifelse(frac < .075, "black", "white"),
           v_just = ifelse(frac < .075, -0.2, 1.4),
           h_just = ifelse(frac < .075, -.1, 1.2),
           p1size = ifelse(nrow(dat) > 6, 3, 4),
           cols = ifelse(length(unique(choice_text)) > 3, 2, 1))
  
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
  
  # graph (vertical bar)
  p1 <- ggplot(pdat, aes(x = value, y = frac)) +
    geom_bar(stat = "identity", fill = color) +
    facet_wrap(~choice_text, ncol = unique(pdat$cols),
               scales = "free_y") +
    scale_y_continuous(labels = scales::percent,
                       expand = expansion(mult = .1)) +
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
                                     hjust = unique(pdat$txtjust)),
          strip.background =element_rect(fill="lightgray", color = NA))
  
  p1_flip <- ggplot(pdat, aes(x = value, y = frac)) + 
    geom_bar(stat = "identity", position = 'dodge', fill = color) +
    facet_grid(choice_text~., switch = "y") +
    scale_x_discrete(position = "top") + 
    coord_flip() + 
    theme_xf() + 
    labs(x = NULL, y = "Percent",
         title = ptitle,
         caption = glue::glue("Source: GS&P {d$name}\nN-size: {nsize} respondents")) + 
    scale_y_continuous(labels = scales::percent, 
                       position = "right") +
    geom_text(aes(x =value, y = frac, 
                  label = scales::percent(frac, accuracy = .1)),
              hjust = pdat$h_just, col = pdat$txtcol,
              size = pdat$p1size) +
    theme(panel.background = element_rect(fill = "white",
                                          colour = "white"),
          plot.background = element_rect(fill = "white",
                                         color = 'white'),
          plot.title.position = "plot",
          strip.text.y.left = element_text(angle = 0, size = 8, hjust = .5),
          strip.background = element_rect(fill = "#E8E8E8", color = "#E8E8E8"))
  
  data_plot <- list(data = pdat, p1 = p1, p1_flip = p1_flip)
}


