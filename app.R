# Shiny App - Qualtrics Visual and Table #

pacman::p_load(tidyverse, janitor, here, glue, qsurvey, 
               qualtRics, shinydashboard, shinyWidgets)

# temporary - will have this on a cron job in the rstudio side # 
# sids <- qualtRics::all_surveys() %>%
#   dplyr::mutate(creationDate = as.Date(creationDate)) %>%
#   dplyr::arrange(desc(creationDate))
# saveRDS(sids, "/srv/shiny-server/qualtrics-viz/data/qualtrics_sids.rds")


# load Qualtrics Survey IDs
sids <- readRDS("/srv/shiny-server/qualtrics-viz/data/qualtrics_sids.rds")


# SHINY APP  --------------------------------------------------------------

# dashboard contains 3 parts - header, sidebar, and body # 
header <- dashboardHeader(title = "Qualtrics Visualizer")

# spectrumInput can't be called via renderUI b/c the selected color 
# doesn't pass through until the menu is selected; 
sidebar <- dashboardSidebar(
  width = 325,
  sidebarMenu(
    id = "tabs",
    convertMenuItem(
      menuItem("Qualtrics Data", 
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
                   list('black', 'white', 'blanchedalmond', 'steelblue', 'forestgreen'),
                   as.list(scales::brewer_pal(palette = "Blues")(9)),
                   as.list(scales::brewer_pal(palette = "Greens")(9)),
                   as.list(scales::brewer_pal(palette = "Spectral")(11)),
                   as.list(scales::brewer_pal(palette = "Dark2")(8)),
                   as.list(scales::brewer_pal(palette = "Set1")(8))
                 ),
                 options = list(`toggle-palette-more-text` = "Show more")
               ))), 
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
  
  
  # 3. Importing survey brings summary data to make sure proper survey loaded 
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
  
  # Render in the main dashboard
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
                             renderPlot(question_summary(input$table_contents, input$my_color)$p1)
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
                           )
                         ),
                         fluidRow(
                           box(
                             width = 6,
                             renderPlot(question_summary(input$table_contents, input$my_color)$p1_flip)
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
      # req(question_summary(input$table_contents)$p1)
      ggsave(file, 
             plot = question_summary(input$table_contents, input$my_color)$p1, 
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
      # req(question_summary(input$table_contents)$p1_flip)
      ggsave(file, 
             plot = question_summary(input$table_contents, input$my_color)$p1_flip, 
             device = 'png', 
             width = input$width2, 
             height = input$height2, 
             unit = "in")
    }
  )
  
  
}


shinyApp(ui, server)


