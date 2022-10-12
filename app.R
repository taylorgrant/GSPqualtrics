pacman::p_load(tidyverse, janitor, here, glue, qsurvey, 
               qualtRics, shinydashboard, shinyWidgets, gt)

source('/srv/shiny-server/qualtrics-viz/helpers.R')
source('/srv/shiny-server/qualtrics-viz/singleQ_summary.R')
source('/srv/shiny-server/qualtrics-viz/singleQ_plot.R')
source('/srv/shiny-server/qualtrics-viz/singleQ_table.R')
source("/srv/shiny-server/qualtrics-viz/theme_xf.R")

# load Qualtrics Survey IDs
sids <- readRDS("/srv/shiny-server/qualtrics-viz/data/qualtrics_sids.rds")

# SHINY APP  --------------------------------------------------------------

# dashboard contains 3 parts - header, sidebar, and body # 
header <- dashboardHeader(title = "Qualtrics Visualizer")

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
               menuItem("Color", spectrumInput(
                 inputId = "my_color",
                 label = "Pick a color:",
                 selected = "#0072B2",
                 flat = TRUE,
                 choices = list(
                   list('black','dodgerblue', 'forestgreen', "#666666"),
                   list("#E69F00", "#0072B2", "#000000", "#56B4E9",
                        "#009E73", "#F0E442", "#D55E00", "#CC79A7"),
                   list("#003f5c", "#2f4b7c", "#665191", "#a05195",
                        "#d45087", "#f95d6a", "#ff7c43", "#ffa600"),
                   list("#001219", "#005f73", "#0a9396", "#94d2bd",
                        "#e9d8a6", "#ee9b00", "#ca6702", "#bb3e03", "#ae2012",
                        "#9b2226"),
                   list("#115f9a", "#1984c5", "#22a7f0", "#48b5c4",
                        "#76c68f", "#a6d75b", "#c9e52f", "#d0ee11", "#d0f400"),
                   list('#E6004D', '#E64F00', '#FFAA00', '#FFFFFF', '#ECECF3',
                        '#008558', "#1F69FF", "#6138F5", '#8B8B97', "#0D0D0F")
                 ),
                 options = list(`toggle-palette-more-text` = "Show more")
               ))
               ), 
      "qualsurvey"
    )
  )
)

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
    # conditionally render the output using inputs (see renderUI below)
    tabItem("qualsurvey", uiOutput("tab1"),
            shinysky::busyIndicator(text = 'Please wait...', wait = 1500))
  )
)

ui = dashboardPage(header, sidebar, body, skin = "black")

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
  
  observeEvent(input$importSurvey, {
    # 
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
    
    # 4a. On load, pull in the survey blocks
    output$blockContents <- renderUI({
      selectInput("block_contents", "Survey Block",
                  choices = c(unique(toc$block), "RESET / ALL BLOCKS"))
    })
    
    #   # 4b. On load, pull in the survey questionnaire
    output$tableContents <- renderUI({
      toc_list <- c("", toc$question_id)
      names(toc_list) <- c("", paste0("Q", toc$question_order, ": ", toc$question_text))
      # names are seen by user, id is on the backend
      selectInput("table_contents", "Question",
                  choices = toc_list)
    })
  })
  
  # 5. Allow the Survey Block selection to filter the questionnaire
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
    question_summary(input$table_contents, input$my_color)
  })
  
  # take reactive GT and render it for use
  output$gt_table <- render_gt(
    survey_data()$gt_tbl
  )
  
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
   
  # download handler 
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
    
  }


shinyApp(ui, server)


