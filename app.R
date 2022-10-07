# Shiny App - Qualtrics Visual and Table #

pacman::p_load(tidyverse, janitor, here, glue, qsurvey, qualtRics, shinydashboard)

# temporary - will have this on a cron job in the rstudio side # 
# sids <- qualtRics::all_surveys() %>%
#   dplyr::mutate(creationDate = as.Date(creationDate)) %>%
#   dplyr::arrange(desc(creationDate))
# saveRDS(sids, "/srv/shiny-server/qualtrics-viz/data/qualtrics_sids.rds")
source("theme_xf.R")
# load Qualtrics Survey IDs
sids <- readRDS("/srv/shiny-server/qualtrics-viz/data/qualtrics_sids.rds")

# load helpers
source('/srv/shiny-server/qualtrics-viz/helpers.R')
source('/srv/shiny-server/qualtrics-viz/singleQ_summary.R')
source('/srv/shiny-server/qualtrics-viz/singleQ_plot.R')

# SHINY APP  --------------------------------------------------------------

# dashboard contains 3 parts - header, sidebar, and body # 
header <- dashboardHeader(title = "Qualtrics Visualizer")

sidebar <- dashboardSidebar(
  width = 325,
  sidebarMenu(
    id = "tabs",
    convertMenuItem(menuItem("Qualtrics Data", tabName = "gfk", icon = icon("square-poll-vertical"), selected=T,
                             menuItem("Available Surveys", uiOutput("surveySelect")), 
                             br(),
                             menuItem("Block list:", uiOutput("blockContents")),
                             menuItem("Question List:", uiOutput("tableContents")))
                    , "gfk"
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
    tabItem("gfk", uiOutput("tab1"),
            shinysky::busyIndicator(text = 'Please wait...', wait = 1500))
  )
)

ui = dashboardPage(header, sidebar, body, skin = "black")


# SERVER SIDE -------------------------------------------------------------
server = function(input, output, session) { 
  
  # validation message to give people idea of where to go
  output$tab1 <- renderUI({
    validate(
      need(input$surveySelect != "", "To begin, please select a survey from the dropdown menu at left")
    )
  })
  
  # Menu Options for UI -----------------------------------------------------
  
  # 1. Select a survey to possibly load into environment
  output$surveySelect <- renderUI({
    selectInput("surveySelect", "Select a survey:",
                c("", sids$name)
    )
  })
  
  # 2. Import Survey button to trigger the import from API
  observeEvent(input$surveySelect, {
    output$tab1 <- renderUI({
      tab1_ui <- tabItem("gfk")
      if (input$surveySelect == '') return()
      actionButton("importSurvey", "Import Survey", icon = icon("upload"),
                   width = "200px")
    })
  })
  
  # 3. Importing survey brings summary data to make sure proper survey loaded 
  observeEvent(input$importSurvey, {
    # load the survey on button push
    # load_survey(sids[sids$name == input$surveySelect,]$id)
    output$tab1 <- renderUI({
      tabItem("gfk", h4("Imported Survey"), #value = "test1",
              fluidRow(
                box(width = 5,
                    renderTable(data.frame(Metadata = c("Name", "ID", "Created", "Responses", "Questions", "Blocks"),
                                           Response = c(d$name, d$id, d$creationDate, d$responseCounts$auditable,
                                                   length(d$questions), length(d$blocks))))
                    )
              ))
    })
    
    # 4a. On load, pull in the survey blocks
    output$blockContents <- renderUI({
      selectInput("block_contents", "Survey Block",
                  choices = c("", unique(toc$block), "RESET / ALL BLOCKS"))
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
  
  # Render in the main dashboard
  observeEvent(input$table_contents, {
    
    output$tab1 <- renderUI({
      tab1_ui <- tabItem("gfk", h4("Survey Results"), value="test1",
                         fluidRow(
                           box(
                             width = 5,
                             
                             validate(
                               need(input$table_contents != "", "Plesae select a question from the dropdown...")
                             ),
                             validate(
                               need(toc[toc$question_id == input$table_contents,]$question_type != "TE", "This app doesn't visualize text open-ends yet...")
                             ),
                             renderPlot(question_summary(input$table_contents)$p1)
                           ),
                           box(
                             width = 3,
                             title_side = "left",
                             title = "Download",
                             downloadButton("downloadPlot1", "Download Plot")
                           )
                         ),
                         fluidRow(
                           box(
                             width = 5,
                             renderPlot(question_summary(input$table_contents)$p1_flip)
                             # renderTable(question_summary(input$table_contents)$data)
                           ),
                           box(
                             width = 3,
                             title_side = "left",
                             title = "Download",
                             downloadButton("downloadPlot2", "Download Plot")
                           )
                         )
      )

    })
})
  
  # download handler with size and resolution set
  output$downloadPlot1 <- downloadHandler(
    filename = function(){
      paste0(d$name, "-Q", toc[toc$question_id == input$table_contents,]$question_order, ".png")
      # paste0(d$name, "-Q", toc[toc$input$table_contents,]$question_order, ".png")
    },
    content = function(file){
      req(question_summary(input$table_contents)$p1)
      ggsave(file, plot = question_summary(input$table_contents)$p1, device = 'png', width = 6.8, height = 6.3, unit = "in")
    }
  )
  
  output$downloadPlot2 <- downloadHandler(
    filename = function(){
      paste0(d$name, "-Q", toc[toc$question_id == input$table_contents,]$question_order, "-flip.png")
    },
    content = function(file){
      # req(out$p1)
      ggsave(file, plot = question_summary(input$table_contents)$p1_flip, device = 'png', width = 6.8, height = 6.3, unit = "in")
    }
  )
  
  
}


shinyApp(ui, server)


