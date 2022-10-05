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
                             uiOutput("importSurvey"),
                             br(),
                             menuItem("Question List:", uiOutput("tableContents"))
    ),
    "gfk"
    ),
    convertMenuItem(menuItem("Sales Data by Make", tabName = "sales", icon = icon("dashboard"),selected=F
    ), "sales"),
    convertMenuItem(menuItem("Monthly Sales by Make & Model", tabName = "monthlysales", icon = icon("dashboard"),
                             selected=F
    ), "monthlysales")
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
    tabItem("gfk", uiOutput("tab1")),
    tabItem("sales", uiOutput("tab2")),
    tabItem("monthlysales", uiOutput("tab3"))
  )
)

ui = dashboardPage(header, sidebar, body, skin = "black")


# SERVER SIDE -------------------------------------------------------------
server = function(input, output, session) { 
  

  # Menu Options for UI -----------------------------------------------------
  output$surveySelect <- renderUI({
    selectInput("surveySelect", "Select a survey:",
                c("", sids$name)
    )
  })
  
  output$importSurvey <- renderUI({
    if (input$surveySelect == '') return() 
    actionButton("importSurvey", "Import Survey", icon = icon("upload"),
                 width = "150px")
  })
  
  observe({
    exists("toc")
  output$tableContents <- renderUI({
    selectInput("tableContents", "Question",
                c("", paste0(toc$export_name, " ", toc$question_text)))
    })
  })
  output$tab1 <- renderUI({
    # validation message rather than the standard error
    validate(
      need(input$surveySelect != "", "To begin, please select a survey from the dropdown menu at left")
    )
  })
  
  observeEvent(input$importSurvey, {
    # load_survey(sids[sids$name == input$surveySelect,]$id) # hold off on DL for now
    output$tab1 <- renderUI({
      # validation message rather than the standard error
      validate(need(input$surveySelect != "", "Please select a survey"))
      tab1_ui <- tabItem("gfk", h4("Quarterly GFK Survey Response"), value="test1",
                         fluidRow(
                           box(
                             width = 8,
                             renderTable(head(svy[25:30, 48:56]))
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


