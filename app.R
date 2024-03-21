library(shiny)
library(ggplot2)
library(readr) # For reading CSV files

# Define UI
ui <- fluidPage(
  titlePanel("Trial Characterization Results"),
  sidebarLayout(
    sidebarPanel(
      fileInput("fileUpload", "Upload Input File", accept = c(".csv")),
      actionButton("runAnalysis", "Run Analysis", icon = icon("play")),
      uiOutput("varSelectUI"), # Dropdown UI for selecting variable
      actionButton("plotButton", "Generate Boxplot"),
      downloadButton("downloadData", "Download Results")
    ),
    mainPanel(
      plotOutput("boxplot"),
      dataTableOutput("viewData") # Display data tables
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Path to the scripts and results
  codesPath <- "C:/Users/sam/Documents/GitHub/APSIMX_SeasonalCharacterization-main"
  resultFolderPath <- "C:/Users/sam/Documents/GitHub/APSIMX_SeasonalCharacterization-main/apsimx_output/output"
  
  # Reactive values for storing the analysis state and the selected variable
  analysisDone <- reactiveVal(TRUE)
  selectedVariable <- reactiveVal()
  
  # Observe the file upload and save the file to the server
  observeEvent(input$fileUpload, {
    file.copy(input$fileUpload$datapath, paste0(resultFolderPath, "/input.csv"), overwrite = TRUE)
  })
  
  # Run analysis when the button is clicked and indicate when done
  observeEvent(input$runAnalysis, {
    req(input$fileUpload)
    setwd(codesPath)
    source(paste0(codesPath, "/apsimxinproved.R"))
    setwd(resultFolderPath) # Change working directory to results
    analysisDone(TRUE) # Update the analysis state
  })
  
  # Read the results file and render the data table when analysis is done
  output$viewData <- renderDataTable({
    req(analysisDone())
    resultsFilePath <- paste0(resultFolderPath, "/charact_x.csv")
    if(file.exists(resultsFilePath)) {
      read.csv(resultsFilePath)
    } else {
      showModal(modalDialog(
        title = "Error",
        "Results file not found. Please check if the analysis generated the file.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
  })
  
  # Generate the UI for variable selection once the analysis is done
  output$varSelectUI <- renderUI({
    req(analysisDone())
    resultsFilePath <- paste0(resultFolderPath, "/charact_x.csv")
    if(file.exists(resultsFilePath)) {
      data <- read.csv(resultsFilePath)
      selectInput("varSelect", "Select Variable", choices = names(data)[-1])
    }
  })
  
  # Update selected variable when the dropdown value changes
  observeEvent(input$varSelect, {
    selectedVariable(input$varSelect)
  }, ignoreInit = TRUE)
  
  # Generate the boxplot when the plot button is clicked
  observeEvent(input$plotButton, {
    req(analysisDone(), selectedVariable())
    resultsFilePath <- paste0(resultFolderPath, "/charact_x.csv")
    if(file.exists(resultsFilePath)) {
      data <- read.csv(resultsFilePath)
      output$boxplot <- renderPlot({
        ggplot(data, aes_string(x = names(data)[1], y = selectedVariable())) +
          geom_boxplot() +
          labs(x = "Site", y = selectedVariable()) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
    }
  })
  
  # Implement the download handler for downloading results
  output$downloadData <- downloadHandler(
    filename = function() { "results.csv" },
    content = function(file) {
      resultsFilePath <- paste0(resultFolderPath, "/charact_x.csv")
      if(file.exists(resultsFilePath)) {
        file.copy(resultsFilePath, file)
      }
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
