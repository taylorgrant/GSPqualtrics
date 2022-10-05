# Shiny App - Qualtrics Visual and Table #

pacman::p_load(tidyverse, janitor, here, glue, qsurvey, qualtRics, shinydashboard)

# temporary - will have this on a cron job in the rstudio side # 
# sids <- qualtRics::all_surveys() %>%
#   dplyr::mutate(creationDate = as.Date(creationDate)) %>%
#   dplyr::arrange(desc(creationDate))
# saveRDS(sids, "/srv/shiny-server/qualtrics-viz/data/qualtrics_sids.rds")

# load Qualtrics Survey IDs
sids <- readRDS("/srv/shiny-server/qualtrics-viz/data/qualtrics_sids.rds")

# load helpers
source('/srv/shiny-server/qualtrics-viz/helpers.R')

# SHINY APP  --------------------------------------------------------------

# dashboard contains 3 parts - header, sidebar, and body # 
header <- dashboardHeader(title = "Qualtrics Visualizer")

sidebar <- dashboardSidebar(
  width = 350,
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
    tabItem("gfk", uiOutput("tab1"))
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
    # load the survey here
    # load_survey(sids[sids$name == input$surveySelect,]$id)
    output$tab1 <- renderUI({
      tabItem("gfk", h4("Imported Survey"), #value = "test1",
              fluidRow(
                box(width = 5,
                    renderTable(data.frame(Metadata = c("Name", "ID", "Created", "Responses", "Questions", "Blocks"),
                                           Response = c(d$name, d$id, d$creationDate, d$responseCounts$auditable, 
                                                   length(d$questions), length(d$blocks)))))
              ))
    })
    
    # 4a. On load, pull in the survey blocks
    output$blockContents <- renderUI({
      selectInput("block_contents", "Survey Block",
                  choices = c(unique(toc$block), "RESET / ALL BLOCKS"))
    })
    
    # 4b. On load, pull in the survey questionnaire
    output$tableContents <- renderUI({
      selectInput("table_contents", "Question",
                  c("", paste0("Q", toc$question_order, ": ", toc$question_text)))
    })
  })
  
  # 5. Allow the Survey Block selection to filter the questionnaire 
  observe({
    # call helper to filter questions as necessary
    updated_toc <- toc_filter(toc, input$block_contents)

    updateSelectInput(
      session = session, 
      inputId = "table_contents",
      choices = c("", updated_toc)
    )
  })
  
  observeEvent(input$table_contents, {   
    output$tab1 <- renderUI({
      tab1_ui <- tabItem("gfk", h4("Quarterly GFK Survey Response"), value="test1",
                         fluidRow(
                           box(
                             width = 8,
                             renderTable(toc %>% 
                                  filter(question_text == trimws(gsub(".*\\:", "", input$table_contents))))
                             # highchartOutput("hcontainer", height = "650px")
                           ),
                           box(
                             width = 3,
                             title_side = "left",
                             title = "About the GfK Data:"
                             # htmlOutput("about")
                           )
                         ),
                         fluidRow(
                           box(
                             width = 3,
                             title_side = "left",
                             title = "Download"
                             # downloadButton("downloadPlot", "Download Plot")
                           )
                         )
      )

    })
})
  
  
  
  
}


shinyApp(ui, server)


