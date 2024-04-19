library(shiny)
library(shinydashboard)
library(DT)
library(readr)
library(dplyr)
library(pheatmap)


# Define UI
ui <- dashboardPage(
dashboardHeader(title = "Trial Characterization Results"),
dashboardSidebar(
  sidebarMenu(
    menuItem("Upload and Analyze", tabName = "analysis", icon = icon("upload")),
    menuItem("View Results", tabName = "results", icon = icon("image")),
    menuItem("View Heatmap", tabName = "heatmap", icon = icon("fire")),
    menuItem("Daily Between Sites", tabName = "daily_between_sites", icon = icon("chart-line")),
    menuItem("Within Sites", tabName = "within_sites", icon = icon("chart-area")),
    menuItem("Between Sites", tabName = "between_sites", icon = icon("chart-bar")),
    selectInput("cropType", "Select Crop Type", choices = c("Maize" = "Maize", "Soy" = "Soy"))
  )
),
dashboardBody(
  tags$head(
    tags$style(HTML("
        .content-wrapper {
          overflow-x: auto;
        }
        .main-sidebar {
          overflow-x: hidden;
        }
        .box-body {
          overflow-x: auto;
        }
      ")),
    tags$script(HTML("
        $(document).on('shiny:sessioninitialized', function(event) {
          $('.sidebar-toggle').on('click', function() {
            setTimeout(function() {
              $(window).trigger('resize');
            }, 250); // Adjust timing if necessary
          });
        });
      "))
  ),
  tabItems(
    tabItem(tabName = "analysis",
            fluidPage(
              fileInput("fileUpload", "Upload Input File", accept = c(".csv")),
              actionButton("runAnalysis", "Run Analysis", icon = icon("play")),
              downloadButton("downloadData", "Download Results")
            )
    ),
    tabItem(tabName = "results",
            fluidPage(
              uiOutput("varSelectUI"),
              actionButton("plotButton", "Generate Boxplot"),
              plotOutput("boxplot"),
              DTOutput("viewData")
            )
    ),
    tabItem(tabName = "heatmap",
            fluidPage(
              uiOutput("varHeatmapUI"),
              uiOutput("heatmapPlotUI")  # Use uiOutput to render the heatmap plot
            )
    ),
    tabItem(tabName = "daily_between_sites",
            fluidPage(
              plotOutput("plotDailyBetweenSites")
            )),
    tabItem(tabName = "within_sites",
            fluidPage(
              selectInput("selectSite", "Select Site", choices = NULL),
              plotOutput("plotWithinSites")
            )),
    tabItem(tabName = "between_sites",
            fluidPage(
              plotOutput("plotBetweenSites")
            ))
  )
)
)

# Define server logic
server <- function(input, output, session) {
  # Path to the scripts and results
  codesPath <- "C:/Users/sam/Documents/GitHub/APSIMX_SeasonalCharacterization-main"
  resultFolderPath <- "C:/Users/sam/Documents/GitHub/APSIMX_SeasonalCharacterization-main/apsimx_output/output"
  
  # Reactive values for storing the analysis state and the selected variable
  #analysisDone <- reactiveVal(FALSE)
  analysisDone <- reactiveVal(TRUE)
  selectedVariable <- reactiveVal()
  
  observeEvent(input$fileUpload, {
    if (!dir.exists(resultFolderPath)) {
      dir.create(resultFolderPath, recursive = TRUE)
    }
    
    tryCatch({
      file.copy(input$fileUpload$datapath, paste0(resultFolderPath, "/input.csv"), overwrite = TRUE)
      if (file.exists(paste0(resultFolderPath, "/input.csv"))) {
        cat("File copy successful\n")
      } else {
        cat("File copy failed\n")
      }
    }, error = function(e) {
      cat("An error occurred during file copy: ", e$message, "\n")
    })
  })
  
  observeEvent(input$runAnalysis, {
    req(input$fileUpload)
    setwd(codesPath)
    codes_dir <- "C:/Users/sam/Documents/GitHub/APSIMX_SeasonalCharacterization-main" #where the folder with the codes is
    #codes_dir <- "~/GitHub/APSIMX_SeasonalCharacterization"
    setwd("C:/Users/sam/Documents/GitHub/APSIMX_SeasonalCharacterization-main/apsimx_output")
    #setwd("C:/Users/cmg3/Box/Gilbert/apsimx_output")
    
    crop <- input$cropType #  !!! ask Sam if this can be set via a button 
    trials_df <- read_csv(paste0(codes_dir,"/small_charact_dt.csv")) %>% distinct() %>% mutate(id_trial = row_number()) %>%
      rename(X = Longitude, Y = Latitude)
    locs_df <- select(trials_df, X, Y) %>% distinct() %>% mutate(id_loc = row_number())
    trials_df <- left_join(trials_df, locs_df)
    trials_df <- trials_df %>% mutate(Planting = as.Date(Planting), Year = format(Planting,"%Y")) %>% 
      mutate(sim_start = Planting %m-% months(2), sim_end = Planting %m+% months(8))
    
    # Get what maturities of cultivar we'll use
    if (crop == "Soy"){
      trials_df <- trials_df %>%
        mutate(Mat = case_when(
          Genetics > 10 ~ "10",
          Genetics <= -2 ~ "000",
          Genetics == -1 ~ "00",
          Genetics == 0 ~ "0",
          Genetics >= 1 & Genetics <= 9 ~ as.character(Genetics)
        )) %>% mutate(Mat = paste0("Generic_MG",Mat))
    }
    
    if (crop == "Maize"){
      corn_mats <- c(80,90,95,100,103,105,108,110,112,115,120,130)
      trials_df <- trials_df %>%
        mutate(Mat = corn_mats[which.min(abs(corn_mats - Genetics))[1]]) %>%
        mutate(Mat = paste0("B_",as.character(Mat)))
    }
    
    
    
    
    
    
    
    
    
    source(paste0(codesPath, "/apsimximproved.R"))
    setwd(resultFolderPath)
    analysisDone(TRUE)
  })
  
  # Reactive for handling the processed data
  bigmet <- reactive({
    req(input$fileUpload)  # Ensure there's a file uploaded
    # Process data similar to the way you described
    current_year <- as.numeric(substr(Sys.time(), 1, 4)) - 1
    trials_df <- read_csv(input$fileUpload$datapath) %>% distinct() %>% mutate(id_trial = row_number())
    bigmet <- data.frame()
    
    for(s in 1:max(trials_df$id_loc)){
      lil_met <- read_apsim_met(paste0("met/loc_", s, ".met"), verbose = F) %>% 
        as_tibble() %>%
        filter(year >= current_year - 9, year <= current_year) %>% 
        mutate(id_loc = s)
      bigmet <- rbind(bigmet, lil_met)
    }
    bigmet <- trials_df %>% 
      select(Site, id_loc) %>% 
      distinct() %>% 
      left_join(bigmet) %>% 
      group_by(Site, id_loc, year, day) %>%
      mutate(tt = max((min(maxt, 34) + max(mint, 0)) / 2 - 0, 0)) %>%
      ungroup()
    filter(bigmet, day >= 200, day <= 300)
  })
  
  
  # Daily Between Sites Plot
  output$plotDailyBetweenSites <- renderPlot({
    req(bigmet())  # Ensure data is available
    dbtw_sites <- bigmet() %>% 
      group_by(Site, day) %>% 
      summarize(acc_precip = mean(rain), acc_tt = mean(tt)) %>%
      mutate(acc_precip = cumsum(acc_precip), acc_tt = cumsum(acc_tt))
    
    ggplot(dbtw_sites, aes(x = day, y = acc_precip, colour = Site)) +
      geom_line() +
      scale_color_hue(direction = 1) +
      theme_minimal()
  })
  
  
  
  
  output$viewData <- renderDT({
    req(analysisDone())
    resultsFilePath <- paste0(resultFolderPath, "/charact_x.csv")
    if(file.exists(resultsFilePath)) {
      datatable(read.csv(resultsFilePath), extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ), escape = FALSE)
    } else {
      return()
    }
  }, options = list(scrollX = TRUE))
  
  output$varSelectUI <- renderUI({
    req(analysisDone())
    resultsFilePath <- paste0(resultFolderPath, "/charact_x.csv")
    if(file.exists(resultsFilePath)) {
      data <- read.csv(resultsFilePath)
      selectInput("varSelect", "Select Variable", choices = names(data)[-1])
    }
  })
  
  observeEvent(input$varSelect, {
    selectedVariable(input$varSelect)
  }, ignoreInit = TRUE)
  
  output$boxplot <- renderPlot({
    req(analysisDone(), selectedVariable())
    resultsFilePath <- paste0(resultFolderPath, "/charact_x.csv")
    if(file.exists(resultsFilePath)) {
      data <- read.csv(resultsFilePath)
      ggplot(data, aes_string(x = names(data)[1], y = selectedVariable())) +
        geom_boxplot() +
        labs(x = "Site", y = selectedVariable()) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { "results.csv" },
    content = function(file) {
      resultsFilePath <- paste0(resultFolderPath, "/charact_x.csv")
      if(file.exists(resultsFilePath)) {
        file.copy(resultsFilePath, file)
      }
    }
  )
  output$heatmapPlotUI <- renderUI({
    req(input$heatmapSelect)  # Ensure there's a selected value
    plotOutput("heatmapPlot", width = "70%", height = "600px")
  })
  
  output$varHeatmapUI <- renderUI({
    req(analysisDone())
    resultsFilePath <- paste0(resultFolderPath, "/daily_charact_x.csv")
    if(file.exists(resultsFilePath)) {
      data <- read.csv(resultsFilePath)
      varchoice <- names(data)[sapply(data, is.numeric) & !names(data) %in% c("DOY", "Stage", "id_trial", "Yieldkgha", "Period")]
      selectInput("heatmapSelect", "Select Variable for Heatmap", choices = varchoice)
    }
  })
  
  output$heatmapPlot <- renderPlot({
    req(input$heatmapSelect)  # Ensure there's a selected value
    updateHeatmap()  # Call a function to update the heatmap
  })
  
  updateHeatmap <- function() {
    resultsFilePath <- paste0(resultFolderPath, "/charact_x.csv")
    if (file.exists(resultsFilePath)) {
      data <- read.csv(resultsFilePath)
      var <- input$heatmapSelect
      var_mat <- data %>%
        filter(Genetics == 2) %>%
        select(Site, starts_with(var)) %>%
        group_by(Site) %>%
        summarize(across(starts_with(var), mean, na.rm = TRUE)) %>%
        column_to_rownames('Site') %>%
        as.matrix()
      
      pheatmap(var_mat, angle_col = 45,
               color = brewer.pal(11, "RdBu"),
               fontsize = 10,
               display_numbers = round(var_mat, 2),
               number_color = "white",
               number_format = "%.2f",
               legend = FALSE,
               cluster_cols = FALSE,
               cluster_rows = FALSE,
               main = paste0("Means of ", var, " by Site"))
    } else {
      plot(NULL, main = "Data not available")
    }
  }
  
  output$plotDailyBetweenSites <- renderPlot({
    req(bigmet())
    dbtw_sites <- bigmet() %>% 
      group_by(Site, day) %>% 
      summarize(acc_precip = mean(rain), acc_tt = mean(tt)) %>%
      mutate(acc_precip = cumsum(acc_precip), acc_tt = cumsum(acc_tt))
    
    ggplot(dbtw_sites, aes(x = day, y = acc_precip, colour = Site)) +
      geom_line() +
      scale_color_hue(direction = 1) +
      theme_minimal()
  })
  
  output$selectSite <- renderUI({
    req(bigmet())
    selectInput("selectSite", "Select Site", choices = unique(bigmet()$Site))
  })
  
  output$plotWithinSites <- renderPlot({
    req(input$selectSite, bigmet())
    plot_data <- bigmet() %>% 
      filter(Site == input$selectSite) %>% 
      summarize(acc_precip = sum(rain), acc_tt = sum(tt))
    
    ggplot(plot_data, aes(x = acc_precip, y = acc_tt)) +
      geom_point() +
      geom_vline(aes(xintercept = mean(acc_precip)), color = "black", linetype = "dashed") +
      geom_hline(aes(yintercept = mean(acc_tt)), color = "black", linetype = "dashed") +
      labs(x = "Total Precipitation", y = "Total Thermal Time", title = input$selectSite) +
      theme_minimal()
  })
  
  output$plotBetweenSites <- renderPlot({
    req(bigmet())
    site_summary <- bigmet() %>% 
      group_by(Site) %>%
      summarize(acc_precip = mean(sum(rain)), acc_tt = mean(sum(tt)))
    
    ggplot(site_summary, aes(x = acc_precip, y = acc_tt)) +
      geom_point() +
      geom_vline(aes(xintercept = mean(acc_precip)), color = "black", linetype = "dashed") +
      geom_hline(aes(yintercept = mean(acc_tt)), color = "black", linetype = "dashed") +
      geom_label(aes(label = Site), nudge_y = 0.05) +
      theme_minimal()
  })
  
  
  # 
  # # Adjust this part within your existing server function
  # data_for_esquisse <- reactive({
  #   req(analysisDone())  # Ensure analysis is done
  #   resultsFilePath <- paste0(resultFolderPath, "/charact_x.csv")
  #   if(file.exists(resultsFilePath)) {
  #     read.csv(resultsFilePath)
  #   } else {
  #     NULL  # Provide a fallback or error message if the file isn't found
  #   }
  # })
  # 
  # # Implement the Esquisse module
  # output$esquisse_ui <- renderUI({
  #   req(data_for_esquisse())  # Ensure data is loaded
  #   esquisserUI(id = "esquisse", data = data_for_esquisse(), header = FALSE)
  # })
  # 
  # # observeEvent(input$esquisse_ui, {
  # #   esquisse_server(input, output, session, data_rv = data_for_esquisse)
  # # })
}

# Run the app
shinyApp(ui = ui, server = server)
