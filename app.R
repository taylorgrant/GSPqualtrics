## Qualtrics Data Visualizer ## 


# load packages -----------------------------------------------------------
pacman::p_load(tidyverse, janitor, here, glue, qsurvey, shiny,
               qualtRics, shinydashboard, shinyWidgets, gt)

# load Qualtrics Survey IDs
sids <- readRDS("/srv/shiny-server/qualtrics-viz/data/qualtrics_sids.rds")

# colors for summary plots ------------------------------------------------ 
library(scales)
col2Hex <- function(col) {
  mat <- grDevices::col2rgb(col, alpha = TRUE)
  grDevices::rgb(mat[1, ]/255, mat[2, ]/255, mat[3,]/255)
}

choices_brewer <- list(
  "Xfinity" = c('#E6004D', '#E64F00', '#FFAA00', '#FFFFFF', '#ECECF3',
                '#008558', "#1F69FF", "#6138F5", '#8B8B97', "#0D0D0F"),
  "Blue-Red" = c("#001219", "#005f73", "#0a9396", "#94d2bd", 
                 "#e9d8a6", "#ee9b00", "#ca6702", "#bb3e03", "#ae2012", 
                 "#9b2226"),
  "Set1" = c("#000000", "#E69F00", "#56B4E9", "#009E73", 
             "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
  "Solar" = c("#003f5c", "#2f4b7c", "#665191", "#a05195", 
              "#d45087", "#f95d6a", "#ff7c43", "#ffa600"),
  "Viridis" = rev(col2Hex(viridis_pal(option = "viridis")(8))),
  "Magma" = rev(col2Hex(viridis_pal(option = "magma")(8))),
  "Inferno" = rev(col2Hex(viridis_pal(option = "inferno")(8))),
  "Plasma" = rev(col2Hex(viridis_pal(option = "plasma")(8))),
  "Cividis" = rev(col2Hex(viridis_pal(option = "cividis")(8)))
)


# SHINY APP  --------------------------------------------------------------

# dashboard contains 3 parts - header, sidebar, and body # 

# header ------------------------------------------------------------------
header <- dashboardHeader(title = "Qualtrics Visualizer")


# sidebar -----------------------------------------------------------------
sidebar <- dashboardSidebar(
  width = 325,
  sidebarMenu(
    id = "tabs",
    convertMenuItem(
      menuItem("Single Question Summary", 
               tabName = "qualsurvey", 
               icon = icon("square-poll-vertical"), 
               selected=T,
               menuItem("Available Surveys", uiOutput("surveySelect")),
               uiOutput("importSurvey", 
                        align = "center"),
               br(),
               menuItem("Block list:", uiOutput("blockContents")),
               menuItem("Question List:", uiOutput("tableContents")),
               menuItem("Reorder Data:", 
                        radioButtons("reorder", 
                                     label = "Reorder data by %?",
                                     inline = TRUE,
                                     choices = list("Yes", "No"), 
                                     selected = "No")),
               menuItem("Color",
                        colorSelectorInput(
                          inputId = "my_color",
                          label = "Pick a color :",
                          selected = choices_brewer$Set1[6],
                          choices = choices_brewer,
                          mode = "radio",
                          display_label = TRUE
                        ))), "qualsurvey"
    ),
    convertMenuItem(
      menuItem("Crosstab Summary", tabName = "crosstab",
               icon = icon("square-poll-vertical"),
               selected=F,
               menuItem("Grouping Variables:",
                        uiOutput("groupBlock"),
                        uiOutput("groupVariable")),
               menuItem("Target Variables:",
                        uiOutput("targetBlock"),
                        uiOutput("targetVariable")),
               menuItem("Group Filter", 
                        uiOutput("groupFilter"),
                        uiOutput("filterSubmit")),
               menuItem("Target Filter", uiOutput("targetFilter")),
               menuItem("Confidence Level:", 
                        radioButtons("conf_level", 
                                     label = "Select confidence:",
                                     inline = TRUE,
                                     choices = c("90%" = ".90", 
                                                 "95%" = ".95"), 
                                     selected = c("90%" = ".90")))
               ),"crosstab")
  )
)


# body --------------------------------------------------------------------
body <- dashboardBody(
  ## CSS styling for the validation error message on Monthly Sales ## 
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: blue;
        font-size: 130%;
      }
    "))
  ),
  # tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
  tabItems(
    # conditionally render the output using inputs (server side)
    tabItem("qualsurvey", uiOutput("tab1"),
            shinysky::busyIndicator(text = 'Please wait...', wait = 1500)),
    tabItem("crosstab", uiOutput("tab2"),
            shinysky::busyIndicator(text = 'Please wait...', wait = 1500))
  )
)

# build the UI ------------------------------------------------------------
ui = dashboardPage(header, sidebar, body, skin = "black")


# server side -------------------------------------------------------------
server = function(input, output, session) { 
  
  # validation message to give people idea of where to go
  output$tab1 <- renderUI({
    validate(
      need(input$surveySelect != "", "To begin, please select a survey from the dropdown menu at left")
    )
  })
  
  output$surveySelect <- renderUI({
    selectInput("surveySelect", "Select a survey:",
                c("", sids$name)
    )
  })
  
  # import button once survey is selected
  observeEvent(input$surveySelect, {
    output$importSurvey <- renderUI({
      if (input$surveySelect == '') return()
      actionButton("importSurvey", 
                   label = "Import Survey",
                   onclick = "var $btn=$(this); setTimeout(function(){$btn.remove();},0);",
                   icon = icon("upload"),
                   width = "200px",
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    })
  })
  
  # import survey on butotn push and load topline metadata
  observeEvent(input$importSurvey, {
    
    # load data here
    load_survey(sids[sids$name == input$surveySelect,]$id) # survey load on button push
    #
    
    output$tab1 <- renderUI({
      tabItem("qualsurvey", h4("Imported Survey"), 
              fluidRow(
                box(width = 4,
                    renderTable(data.frame(Metadata = c("Name", "ID", "Created", "Responses", "Questions", "Blocks"),
                                           Response = c(d$name, d$id, d$creationDate, d$responseCounts$auditable,
                                                        length(d$questions), length(d$blocks))))
                )
              ))
    })
    
    # On load, pull in the survey blocks
    output$blockContents <- renderUI({
      selectInput("block_contents", "Survey Block",
                  choices = c(unique(toc$block), "RESET / ALL BLOCKS"))
    })
    
    # On load, pull in the survey questionnaire
    output$tableContents <- renderUI({
      toc_list <- c("", toc$question_id)
      names(toc_list) <- c("", paste0("Q", toc$question_order, ": ", toc$question_text))
      # names are seen by user, id is on the backend
      selectInput("table_contents", "Question",
                  choices = toc_list)
    })
  })
  
  # Allow the Survey Block selection to filter the questionnaire
  observeEvent(input$block_contents, {
    # call helper to filter questions as necessary
    updated_toc <- toc_filter(toc, input$block_contents)
    # update
    updateSelectInput(
      session = session,
      inputId = "table_contents",
      choices = c("", updated_toc)
    )
  })
  
  # run summary and plot function 
  survey_data <- reactive({
    # question_summary(input$table_contents, input$my_color)
    singleQ_summary(input$table_contents, input$my_color, input$reorder)
  })
  
  # take reactive GT and render it for use
  output$gt_table <- render_gt(
    survey_data()$gt_tbl
  )
  
  # render main dash for use upon selection of question from TOC
  observeEvent(input$table_contents, {
    output$tab1 <- renderUI({
      tab1_ui <- tabItem("qualsurvey", h4("Survey Results"), value="test1",
                         fluidRow(
                           box(
                             width = 6,
                             validate(
                               need(input$table_contents != "", "Please select a question from the dropdown...")
                             ),
                             validate(
                               need(toc[toc$question_id == input$table_contents,]$question_type != "TE", "This app doesn't visualize text open-ends yet...")
                             ),
                             renderPlot(survey_data()$p1)
                           ),
                           box(
                             width = 3,
                             title_side = "left",
                             title = "Download",
                             splitLayout(
                               numericInput("width1", label = "Width (in)", value = 6.8),
                               numericInput("height1", label = "Height (in)", value = 6.3),
                             ),
                             downloadButton("downloadPlot1", "Download Plot")
                           )),
                         fluidRow(
                           box(
                             width = 6,
                             renderPlot(survey_data()$p1_flip)
                           ),
                           box(
                             width = 3,
                             title_side = "left",
                             title = "Download",
                             splitLayout(
                               numericInput("width2", label = "Width (in)", value = 6.8),
                               numericInput("height2", label = "Height (in)", value = 6.3),
                             ),
                             downloadButton("downloadPlot2", "Download Plot")
                           )
                         ),
                         fluidRow(
                           box(
                             width = 6,
                             validate(
                               need(input$table_contents != "", "Please select a question from the dropdown...")
                             ),
                             validate(
                               need(toc[toc$question_id == input$table_contents,]$question_type != "TE", "This app doesn't visualize text open-ends yet...")
                             ),
                             gt_output("gt_table")
                           ),
                           box(
                             width = 3,
                             title_side = "left",
                             title = "Download",
                             capture::capture(
                               selector = "#gt_table",
                               filename = paste0(d$name, "-Q", toc[toc$question_id == input$table_contents,]$question_order, "-TBL.png"),
                               icon("download"), "Download Table"
                             )
                           )
                         )
      )
    })
  })
  
  # download handler (plot1)
  output$downloadPlot1 <- downloadHandler(
    filename = function(){
      paste0(d$name, "-Q", toc[toc$question_id == input$table_contents,]$question_order, ".png")
    },
    content = function(file){
      ggsave(file, 
             plot = survey_data()$p1,
             device = 'png', 
             width = input$width1, 
             height = input$height1, 
             unit = "in")
    }
  )
  
  # plot2
  output$downloadPlot2 <- downloadHandler(
    filename = function(){
      paste0(d$name, "-Q", toc[toc$question_id == input$table_contents,]$question_order, "-flip.png")
    },
    content = function(file){
      ggsave(file, 
             plot = survey_data()$p1_flip,
             device = 'png', 
             width = input$width2, 
             height = input$height2, 
             unit = "in")
    }
  ) 
  
  
  # Tab 2 -------------------------------------------------------------------
  
  # validation requiring both Group and Target selections
  output$tab2 <- renderUI({
    validate(
      need(input$target_variable != "", "Please select your Group and Target variables to crosstab...")
    )
    validate(
      need(input$group_variable != "", "Please select your Group and Target variables to crosstab...")
    )
  })


# grouping variables ------------------------------------------------------
  output$groupBlock <- renderUI({
    selectInput("group_block", "Group Block",
                choices = c(unique(toc$block), "RESET / ALL BLOCKS"))
  })

  output$groupVariable <- renderUI({
    toc_list <- c("", toc[toc$question_type %in% c("MC", "TE_AGE"),]$question_id)
    names(toc_list) <- c("", paste0("Q", toc[toc$question_type %in% c("MC", "TE_AGE"),]$question_order, ": ", toc[toc$question_type %in% c("MC", "TE_AGE"),]$question_text))
    # names are seen by user, id is on the backend
    selectInput("group_variable", "Grouping Question:",
                choices = toc_list)
  })


  # not sure if this is doing anything 
  observeEvent(input$group_block, {
    # call helper to filter questions as necessary
    updated_toc <- group_toc_filter(toc, input$group_block)
    # update
    updateSelectInput(
      session = session,
      inputId = "group_variable",
      choices = c("", updated_toc)
    )
  })


# target variables --------------------------------------------------------
  output$targetBlock <- renderUI({
    selectInput("target_block", "Target Block",
                choices = c(unique(toc$block), "RESET / ALL BLOCKS"))
  })

  output$targetVariable <- renderUI({
    toc_list <- c("", toc$question_id)
    names(toc_list) <- c("", paste0("Q", toc$question_order, ": ", toc$question_text))
    # names are seen by user, id is on the backend
    selectInput("target_variable", "Target Question:",
                choices = toc_list)
  })

  # group selection updates target variables available
  observeEvent(input$target_block, {
    # call helper to filter questions as necessary
    updated_toc <- toc_filter(toc, input$target_block)
    # update
    updateSelectInput(
      session = session,
      inputId = "target_variable",
      choices = c("", updated_toc)
    )
  })
  
  # Groups that can be filtered out of the data
  filter_groups <- reactive({
    group_filter(input$group_variable, input$target_variable)
  })
  
  # main crosstab function; includes the significance testing within it
  crosstab_data <- reactive({
    build_crosstab(input$group_variable, input$target_variable, input$conf_level, input$group_filter)
  })
  
  # to drop group variables from the table
  output$groupFilter <- renderUI({
    checkboxGroupInput("group_filter", "Select any variables you want to drop:",
                choices = filter_groups()
    )
  })
  
  # to filter out specific questions/options in matrix questions (filtered via the GT table function)
  output$targetFilter <- 
      renderUI({
        if (attributes(crosstab_data())$target_qt %in% c("Matrix", "RO", "PGR")) {
          checkboxGroupInput("target_filter", "Select any variables you want to drop:",
                             choices = crosstab_data() %>% distinct(target_group) %>% filter(target_group != "") %>% pull())
        }
  })

  # take reactive GT and render it for use (target filter drops vars)
  output$gt_crosstab <- render_gt(
    multiQ_table(crosstab_data(), input$target_filter)
  )
  
  # render dash; made page a little wider for some tables
  output$tab2 <- renderUI({
    tab2_ui <- tabItem("crosstab", h4("Crosstab Results"), value="test2",
                       fluidRow(
                         box(
                           width = 7,
                           validate(
                             need(input$target_variable != "", "Please select your Group and Target variables to crosstab...")
                           ),
                           validate(
                             need(input$group_variable != "", "Please select your Group and Target variables to crosstab...")
                           ),
                           
                           gt_output("gt_crosstab")
                         ),
                         box(
                           width = 3,
                           title_side = "left",
                           title = "Download",
                           capture::capture(
                             selector = "#gt_crosstab",
                             filename = paste0(d$name, "-Q", toc[toc$question_id == input$target_variable,]$question_order, "-CROSSTAB.png"),
                             icon("download"), "Download Table"
                           )
                         )))
  })
}

shinyApp(ui, server)




