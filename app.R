library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(DT)
library(readr)
library(dplyr)
library(pheatmap)
library(apsimx)
library(tidyverse)
library(daymetr)
library(data.table)
library(RColorBrewer)
library(pheatmap)
library(janitor)
library(tidyverse)
library(esquisse)
library(tidyr)
library(zip)
library(bslib)


# Define UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),  # Apply a modern theme
  navbarPage(
    title = tags$div(
      style = "display: flex; align-items: center;",
      tags$img(src = "aces.png", height = "40px", style = "margin-right: 10px;"),
      tags$span("Trial Characterization Results", style = "font-size: 24px; font-weight: bold;")
    ),
    
    tabPanel(
      title = "Description",
      fluidPage(
        h2("Seasonal Characterization Tool"),
        p("Built in R and Shiny using the apsimr package."),
        p("Purpose: To characterize the growing season at one or more sites according to the crop's response to environmental conditions at those sites."),
        h3("Seasonal Characterization Tool Can be used to:"),
        tags$ul(
          tags$li("Understand the environment in terms of the conditions/stressors present at specific stages of the crop's development."),
          tags$li("Compare seasonal conditions between sites and how those conditions have changed over time."),
          tags$li("Predict crop phenology and performance from a cultivar's maturity, planting date, and location.")
        ),
        h4("Instructions"),
        p("Use the 'Upload and Analyze' page to upload your input data and run the analysis."),
        p("View results in the 'Visualizations and Results' page."),
        h5("Contact"),
        p("For questions, suggestions, or contributions, reach out to:"),
        p("Sam Shi, mshi17@illinois.edu"),
        p("Catherine Gilbert, cmg3@illinois.edu"),
        p("GitHub: https://github.com/CatherineGilbert/APSIMX_SeasonalCharacterization")
      )
    ),
    
    tabPanel(
      title = "Upload and Analyze",
      fluidPage(
        selectInput("cropType", "Select Crop Type", choices = c("Maize" = "Maize", "Soy" = "Soy")),
        fileInput("fileUpload", "Upload Input File", accept = c(".csv")),
        actionButton("runAnalysis", "Run Analysis", icon = icon("play")),
        downloadButton("downloadData", "Download Results"),
        br(),
        h3("Dataset Descriptions"),
        p(strong("charact_x:"), " Contains the environmental and developmental variables summarized for each stage of each simulation."),
        p(strong("trials_x:"), " Contains simulation parameters, identifying information, and values which aren’t summarized by stage."),
        p(strong("daily_charact_x:"), " Contains the recorded values of the reporting variables at each day of each simulation."),
        uiOutput("progressBar")  # Display the progress bar here
      )
    ),
    
    tabPanel(
      title = "Visualizations and Results",
      fluidPage(
        tabsetPanel(
          tabPanel(
            title = "Boxplot",
            div(
              class = "large-text-label control-label",
              tags$label("Boxplot")
            ),
            uiOutput("fileSelectPlotUI"),
            uiOutput("varSelectUI"),
            plotOutput("boxplot"),
            downloadButton("downloadBoxplot", "Download Boxplot")
          ),
          tabPanel(
            title = "Heatmap",
            p("This heatmap visualizes the means of the selected variable by site and genetic group."),
            uiOutput("varHeatmapUI"),
            uiOutput("genSelectUI"),
            plotOutput("heatmapPlot"),
            downloadButton("downloadHeatmap", "Download Heatmap")
          ),
          tabPanel(
            title = "Daily Comparison",
            p("Compare daily accumulated precipitation and thermal time between different sites."),
            selectInput("comparisonType", "Select Comparison Type", choices = c(
              "Accumulated Precipitation (Day of Year)" = "precip_doy",
              "Accumulated Thermal Time (Day of Year)" = "tt_doy",
              "Accumulated Precipitation (Days after Sowing)" = "precip_das",
              "Accumulated Thermal Time (Days after Sowing)" = "tt_das"
            )),
            plotOutput("comparisonPlot"),
            downloadButton("downloadComparisonPlot", "Download Plot")
          ),
          tabPanel(
            title = "10-Year Averages",
            p("Visualize the 10-year site averages for a typical growing season."),
            plotOutput("plotBetweenSites"),
            downloadButton("downloadBetweenSitesPlot", "Download Plot")
          )
        )
      )
    )
  )
)
# Define server logic
server <- function(input, output, session) {
  
  gen <- 1 # should be selectable for heatmap
  setwd("C:/Users/sam/Documents/GitHub/APSIMX_SeasonalCharacterization/apsimx_output")
  # Path to the scripts and results
  codesPath <- "C:/Users/sam/Documents/GitHub/APSIMX_SeasonalCharacterization"
  resultFolderPath <- "C:/Users/sam/Documents/GitHub/APSIMX_SeasonalCharacterization/apsimx_output/output"
  heatmap_plot <- reactiveVal(NULL)
  # Reactive values for storing the analysis state and the selected variable
  #analysisDone <- reactiveVal(FALSE)
  analysisDone <- reactiveVal(TRUE)
  analysisInProgress <- reactiveVal(FALSE)
  observe({
    req(analysisDone())
    source("C:/Users/sam/Documents/GitHub/APSIMX_SeasonalCharacterization/visualization.R")
  })
  
  
  
  
  observe({
    if (analysisInProgress()) {
      shinyjs::disable("runAnalysis")
    } else {
      shinyjs::enable("runAnalysis")
    }
  })
  
  # for read log file for analysis progress
  progress <- reactiveVal(0)
  
  observe({
    
    invalidateLater(5000, session)
    setwd("C:/Users/sam/Documents/GitHub/APSIMX_SeasonalCharacterization/apsimx_output")
    if (file.exists("progress.log")) {
      log_contents <- readLines("progress.log")
      # Update progress based on the log contents
      total_steps <- 16  # Define the total number of steps in the log
      current_step <- length(log_contents)  # Update based on the number of log entries
      progress_value <- round(current_step / total_steps * 100)
      progress(progress_value)
      updateProgressBar(session, id = "progressBar", value = progress_value)
    }
  })
  
  output$progressBar <- renderUI({
    progressBar(id = "progressBar", value = progress(), display_pct = TRUE)
  })
  
  
  #----------------#
  selectedVariable <- reactiveVal()
  trials_df <- reactiveVal()
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
    
    updateSiteSelectionUI()
  })
  
  observeEvent(input$runAnalysis, {
    
    req(input$fileUpload)
    analysisInProgress(TRUE)
    
    
    setwd("C:/Users/sam/Documents/GitHub/APSIMX_SeasonalCharacterization/apsimx_output")
    #setwd("C:/Users/cmg3/Box/Gilbert/apsimx_output")
    
    file.create("progress.log")
    
    crop <- input$cropType #  !!! ask Sam if this can be set via a button 
    writeLines(crop, paste0(codesPath, "/selected_crop.txt"))
    
    source("C:/Users/sam/Documents/GitHub/APSIMX_SeasonalCharacterization/apsimximproved.R")
    
    
    #update outputs and visaluzations
    #think about future labs and also company opportunities after i graduate next year
    
    #get feedback from breeders on what they think is valuable and what kind of outputs they value
    #for over-performance / under-performance can use maturity checks as yield checks 
    #check that the actual maturity (DtM) and simulated maturities (stage DOYs) are accurate
    #investigate structural equation modeling
    
    #build a machine learning model directly off the seasonal parameters instead of just using the apsim yield output
    
    #which of the seasonal variables are affecting the performance of the varieties
    
    # Start, set up trials_df -----
    assign("trials_df", trials_df, envir = .GlobalEnv)
    assign("locs_df", locs_df, envir = .GlobalEnv)
    assign("soil_profile_list", soil_profile_list, envir = .GlobalEnv)
    assign("daily_output", daily_output, envir = .GlobalEnv)
    assign("yields", yields, envir = .GlobalEnv)
    assign("res", res, envir = .GlobalEnv)
    assign("trials_x", trials_x, envir = .GlobalEnv)
    assign("charact_x", charact_x, envir = .GlobalEnv)
    assign("daily_charact_x", daily_charact_x, envir = .GlobalEnv)
    assign("bigmet", bigmet, envir = .GlobalEnv)
    analysisDone(TRUE)
    analysisInProgress(FALSE)
    updateSiteSelectionUI()
    updateSiteSelectionFacetdUI()
    updateSiteSelectionBetweenUI()
  })
  
  
  
  updateSiteSelectionUI <- function() {
    req(analysisDone())
    trials_df <- read_csv(paste0(resultFolderPath, "/trials_x.csv"))
    sites <- sort(unique(trials_df$Site))  # Sort site names alphabetically
    output$siteSelectionUI <- renderUI({
      fluidRow(
        column(width = 12,
               actionButton("selectAllSites", "Select All"),
               actionButton("unselectAllSites", "Unselect All")
        ),
        column(width = 12,
               checkboxGroupInput("selectedSites", "Select Sites", choices = sites, selected = sites[1:2])
        )
      )
    })
  }
  
  
  
  updateSiteSelectionFacetdUI <- function() {
    req(analysisDone())
    trials_df <- read_csv(paste0(resultFolderPath, "/trials_x.csv"))
    sites <- sort(unique(trials_df$Site))
    output$siteSelectionUI_faceted <- renderUI({
      fluidRow(
        column(width = 12,
               actionButton("selectAllSites_faceted", "Select All"),
               actionButton("unselectAllSites_faceted", "Unselect All")
        ),
        column(width = 12,
               checkboxGroupInput("selectedSites_faceted", "Select Sites", choices = sites, selected = sites[1:2])
        )
      )
    })
  }
  
  observeEvent(input$selectAllSites, {
    trials_df <- read_csv(paste0(resultFolderPath, "/trials_x.csv"))
    sites <- sort(unique(trials_df$Site))
    updateCheckboxGroupInput(session, "selectedSites", selected = sites)
  })
  
  observeEvent(input$unselectAllSites, {
    updateCheckboxGroupInput(session, "selectedSites", selected = character(0))
  })
  
  observeEvent(input$selectAllSites_faceted, {
    trials_df <- read_csv(paste0(resultFolderPath, "/trials_x.csv"))
    sites <- sort(unique(trials_df$Site))
    updateCheckboxGroupInput(session, "selectedSites_faceted", selected = sites)
  })
  
  observeEvent(input$unselectAllSites_faceted, {
    updateCheckboxGroupInput(session, "selectedSites_faceted", selected = character(0))
  })
  
  
  # Reactive for handling the processed data
  bigmet <- reactive({
    req(input$fileUpload)  # Ensure there's a file uploaded
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
  
  
  # Reactive expression to filter the meteorological data based on selected sites
  filteredMetData <- reactive({
    req(input$selectedSites)
    trials_x <- read_csv(paste0(resultFolderPath, "/trials_x.csv"))
    current_year <- as.numeric(substr(Sys.time(), 1, 4)) - 1
    bigmet <- data.frame()
    for(s in 1:max(trials_x$id_loc)){
      lil_met <- read_apsim_met(paste0("met/loc_", s, ".met"), verbose = F) %>% 
        as_tibble() %>%
        filter(year >= current_year - 9, year <= current_year) %>% 
        mutate(id_loc = s)
      bigmet <- rbind(bigmet, lil_met)
    }
    bigmet <- trials_x %>% 
      select(Site, id_loc) %>% 
      distinct() %>% 
      left_join(bigmet) %>% 
      group_by(Site, id_loc, year, day) %>%
      mutate(tt = max((min(maxt, 34) + max(mint, 0)) / 2 - 0, 0)) %>%
      ungroup()
    bigmet <- filter(bigmet, Site %in% input$selectedSites)
    bigmet
  })
  
  # Reactive expression to generate the filtered and accumulated data
  accumulatedData <- reactive({
    req(filteredMetData())
    trials_x <- read_csv(paste0(resultFolderPath, "/trials_x.csv"))
    startend <- select(daily_charact_x, id_trial, DOY, Stage) %>% filter(Stage != 1) %>% 
      group_by(id_trial) %>% filter(Stage == max(Stage) | Stage == min(Stage)) %>%
      summarize(first_doy = DOY[1], final_doy = DOY[2]) %>% 
      mutate(final_doy = ifelse(final_doy < first_doy, final_doy + 365, final_doy)) %>%
      left_join(select(trials_x, Site, Year, id_trial, Genetics)) %>% ungroup()
    mean_startend <- group_by(startend, Site) %>% 
      summarize(first_doy = mean(first_doy, na.rm = T), final_doy = mean(final_doy, na.rm = T)) %>%
      mutate(final_doy = ifelse(final_doy > 365, final_doy - 365, final_doy))
    filtmet <- filteredMetData() %>% left_join(mean_startend) %>% filter(day >= first_doy & day <= final_doy)
    dbtw_sites <- filtmet %>% group_by(Site, year) %>% 
      mutate(acc_precip = cumsum(rain), acc_tt = cumsum(tt)) %>%
      ungroup() %>% group_by(Site, day) %>% 
      summarize(acc_precip = mean(acc_precip, na.rm = T), acc_tt = mean(acc_tt, na.rm = T))
    dbtw_sites
  })
  
  
  # Store the generated plot for download
  comparison_plot_data <- reactiveVal()
  
  output$comparisonPlot <- renderPlot({
    req(accumulatedData(), input$comparisonType)
    data <- accumulatedData()
    
    p <- ggplot(data) 
    
    if (input$comparisonType == "precip_doy") {
      p <- p + 
        aes(x = day, y = acc_precip, colour = Site) +
        geom_line() +
        scale_color_hue(direction = 1) +
        labs(x = "Day of Year", y = "Accumulated Precipitation (mm)") +
        theme_minimal()
    } else if (input$comparisonType == "tt_doy") {
      p <- p + 
        aes(x = day, y = acc_tt, colour = Site) +
        geom_line() +
        scale_color_hue(direction = 1) +
        labs(x = "Day of Year", y = "Accumulated Thermal Time") +
        theme_minimal()
    } else if (input$comparisonType == "precip_das") {
      sdbtw_sites <- data %>% mutate(day = day - min(day) + 1)
      p <- p + 
        aes(x = day, y = acc_precip, colour = Site) +
        geom_line() +
        scale_color_hue(direction = 1) +
        labs(x = "Days after Sowing", y = "Acc. Precipitation (mm)") +
        theme_minimal()
    } else if (input$comparisonType == "tt_das") {
      sdbtw_sites <- data %>% mutate(day = day - min(day) + 1)
      p <- p + 
        aes(x = day, y = acc_tt, colour = Site) +
        geom_line() +
        scale_color_hue(direction = 1) +
        labs(x = "Days after Sowing", y = "Acc. Thermal Time") +
        theme_minimal()
    }
    
    comparison_plot_data(p)  # Store the plot in a reactive value
    print(p)  # Render the plot
  })
  
  # Download handler for the daily between sites plot
  output$downloadComparisonPlot <- downloadHandler(
    filename = function() {
      paste0("comparison_plot-", input$comparisonType, "-", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      print(comparison_plot_data())  # Print the stored plot
      dev.off()
    }
  )
  
  
  # Store the generated plot for download
  faceted_comparison_plot_data <- reactiveVal()
  
  output$facetedComparisonPlot <- renderPlot({
    req(input$selectedSites_faceted)
    selected_sites <- input$selectedSites_faceted
    print(selected_sites)
    
    plot_dt <- wthn_sites %>% filter(Site %in% selected_sites)
    means <- plot_dt %>% group_by(Site) %>%
      summarise(mean_acc_precip = mean(acc_precip, na.rm = TRUE),
                mean_acc_tt = mean(acc_tt, na.rm = TRUE))
    
    p <- ggplot(plot_dt, aes(x = acc_precip, y = acc_tt)) +
      facet_wrap(vars(Site), scales = "free") +
      geom_vline(data = means, aes(xintercept = mean_acc_precip), color = "black", linetype = "dashed") +
      geom_hline(data = means, aes(yintercept = mean_acc_tt), color = "black", linetype = "dashed") +
      geom_label(aes(label = plot_dt$year), size = 3) +
      labs(x = "Acc. Precipitation (mm)", y = "Acc. Thermal Time") +
      theme_minimal() +
      theme(legend.position = "none")
    
    faceted_comparison_plot_data(p)  # Store the plot in a reactive value
    print(p)  # Render the plot
  })
  
  # Download handler for the faceted comparison plot
  output$downloadFacetedComparisonPlot <- downloadHandler(
    filename = function() {
      paste0("faceted_comparison_plot-", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1200, height = 800)
      print(faceted_comparison_plot_data())  # Print the stored plot
      dev.off()
    }
  )  
  
  
  
  output$viewData <- renderDT({
    req(analysisDone())
    updateSiteSelectionUI()
    updateSiteSelectionBetweenUI()
    
    file_to_view <- input$fileToView
    file_path <- paste0(resultFolderPath, "/", file_to_view)
    
    if (file.exists(file_path)) {
      data <- read.csv(file_path)
      # Round all numeric columns to 2 decimal places
      data <- data %>% mutate(across(where(is.numeric), round, 2))
      # Debugging print statements
      print(dim(data))  # Print the dimensions of the data
      datatable(data, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        scrollX = TRUE
      ), escape = FALSE)
    } else {
      print("File not found: ", file_to_view)
      return(NULL)
    }
  })
  
  #first select file for boxplot
  output$fileSelectPlotUI <- renderUI({
    req(analysisDone())
    files <- c("trials_x.csv", "charact_x.csv", "daily_charact_x.csv")
    selectInput("fileSelectPlot", "Select File to Plot", choices = files, selected = "trials_x.csv")
  })
  
  #second select var for UI
  output$varSelectUI <- renderUI({
    req(analysisDone())
    selected_file <- input$fileSelectPlot  # Use the selected file
    file_path <- paste0(resultFolderPath, "/", selected_file)
    
    if (file.exists(file_path)) {
      data <- read.csv(file_path)
      selectInput("varSelect_boxplot", "Select Variable", choices = names(data)[-1])
    }
  })
  
  observeEvent(input$varSelect_boxplot, {
    selectedVariable(input$varSelect_boxplot)
  }, ignoreInit = TRUE)
  
  # Store the generated plot for download
  boxplot_data <- reactiveVal()
  
  output$boxplot <- renderPlot({
    req(analysisDone(), selectedVariable())
    selected_file <- input$fileSelectPlot  # Use the selected file
    file_path <- paste0(resultFolderPath, "/", selected_file)
    
    if (file.exists(file_path)) {
      data <- read.csv(file_path)
      
      if (selected_file != "trials_x.csv") {
        trials_x <- read.csv(paste0(resultFolderPath, "/trials_x.csv"))
        data <- left_join(data, trials_x[, c("id_trial", "Site")], by = "id_trial")
      }
      
      data$Site <- as.factor(data$Site)  # Ensure Site is treated as a factor
      
      selected_var <- selectedVariable()
      
      # Check if selected_var is in the column names of data
      if(selected_var %in% names(data)) {
        # Create the box plot
        p <- ggplot(data, aes(x = Site, y = .data[[selected_var]], fill = Site)) +
          geom_boxplot() +  # Use geom_boxplot to create a box plot
          labs(x = "Site", y = selected_var) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        boxplot_data(p)  # Store the plot in a reactive value
        print(p)  # Render the plot
      } else {
        print(paste("Error: Variable", selected_var, "not found in data frame"))
      }
    } else {
      print(paste("Error: File", selected_file, "does not exist"))
    }
  })
  
  # Download handler for the boxplot
  output$downloadBoxplot <- downloadHandler(
    filename = function() {
      paste0("boxplot-", input$varSelect_boxplot, "-", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      print(boxplot_data())  # Print the stored plot
      dev.off()
    }
  )
  
  #disable download button if no analysis. 
  
  observe({
    if (analysisDone()) {
      shinyjs::enable("downloadData")
    } else {
      shinyjs::disable("downloadData")
    }
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("results_", Sys.Date(), ".zip")  # Name the zip file
    },
    content = function(file) {
      # Create a temporary directory to store the files
      temp_dir <- tempdir()
      files <- list.files(resultFolderPath, full.names = TRUE)
      
      # Copy the selected files to the temporary directory
      file_paths <- file.path(temp_dir, basename(files))
      file.copy(files, file_paths)
      
      # Create a zip file from the files in the temporary directory
      zip::zipr(file, files = file_paths)
    }
  )
  
  output$fileSelectUI <- renderUI({
    req(analysisDone())
    files <- list.files(resultFolderPath, full.names = FALSE)
    selectInput("fileSelect", "Select File to Download", choices = files)
  })
  
  
  output$genSelectUI <- renderUI({
    req(analysisDone())
    input_path <- paste0(resultFolderPath, "/input.csv")
    if(file.exists(input_path)) {
      input<- read.csv(input_path)
      gen_choices <- unique(input$Genetics)
      selectInput("genSelect", "Select Gen for Heatmap", choices = gen_choices, selected = gen_choices[1])
    }
  })
  
  output$varHeatmapUI <- renderUI({
    req(analysisDone())
    charact_x_path <- paste0(resultFolderPath, "/daily_charact_x.csv")
    if(file.exists(charact_x_path)) {
      charact_x <- read.csv(charact_x_path)
      varchoice <- charact_x %>% ungroup() %>% select(where(is.numeric) & !c(id_trial, Period)) %>%select(-c(DOY, Stage)) %>%  names()
      print(varchoice)
      selectInput("heatmapSelect", "Select Variable for Heatmap", choices = varchoice)
      
    }
  })
  
  observe({
    output$heatmapPlotUI <- renderUI({
      updateSiteSelectionFacetdUI()
      #graphics.off()
      req(input$heatmapSelect)  # Ensure there's a selected value
      plotOutput("heatmapPlot", height = "600px", width = "90%")
      
    })
  })
  
  
  
  
  
  
  output$heatmapPlot <- renderPlot(
    
    {
      req(input$heatmapSelect)  # Ensure a variable is selected
      req(analysisDone())
      gen <- input$genSelect 
      var <- input$heatmapSelect
      # Logic to prepare the heatmap matrix
      charact_x_path <- paste0(resultFolderPath, "/charact_x.csv")
      trials_x_path <-paste0(resultFolderPath,"/trials_x.csv")
      daily_charact_x_path <- paste0(resultFolderPath,"/daily_charact_x.csv")
      
      
      trials_x <- read_csv(trials_x_path)
      charact_x <- read_csv(charact_x_path)
      daily_charact_x <- read_csv(daily_charact_x_path)
      
      j_dt <- filter(trials_x, Genetics == gen) %>% select(id_trial,Genetics, Site) %>% left_join(charact_x)
      
      if (file.exists(charact_x_path)) {
        var <- input$heatmapSelect
        var_mat <- j_dt %>% select(id_trial, Site, Period, starts_with(var)) %>%
          pivot_wider(names_from = Period, values_from = var) %>% select(-id_trial) %>%
          group_by(Site) %>% summarize(across(where(is.numeric), function(x){mean(x,na.rm=T)})) %>%
          column_to_rownames("Site") %>%
          remove_empty(which = "rows") %>%
          as.matrix()
        sorted_colnames <- as.character(sort(as.numeric(colnames(var_mat))))
        var_mat <- var_mat[, sorted_colnames]
        print(head(var_mat))
        print(dim(var_mat))
        heatmap <- pheatmap(var_mat, angle_col = 0,
                            
                            fontsize = 16, 
                            display_numbers = round(var_mat, 2), 
                            number_color = "black", 
                            #scale = "column",
                            number_format = "%.2f", 
                            legend = F,
                            cluster_cols = F,
                            cluster_rows = T,
                            main = paste0("Means of ",var," by Site (MG ",gen,")"))
        heatmap_plot(heatmap)
        
        
      } else {
        plot(NULL, main = "Data not available")
      }
      list(var_mat = var_mat, var = var)
      
    })
  
  
  # Add the download handler here
  output$downloadHeatmap <- downloadHandler(
    filename = function() {
      paste0("heatmap-", input$heatmapSelect, "-", Sys.Date(), ".png")
    },
    content = function(file) {
      # Use the stored heatmap for the download
      png(file, width = 1400, height = 1000)
      grid::grid.draw(heatmap_plot()$gtable)  # Draw the stored heatmap
      dev.off()
    }
  )
  
  
  
  updateSiteSelectionBetweenUI <- function() {
    req(analysisDone())
    trials_df <- read_csv(paste0(resultFolderPath, "/trials_x.csv"))
    sites <- sort(unique(trials_df$Site))
    output$siteSelectionUI_between <- renderUI({
      fluidRow(
        column(width = 12,
               actionButton("selectAllSites_between", "Select All"),
               actionButton("unselectAllSites_between", "Unselect All")
        ),
        column(width = 12,
               checkboxGroupInput("selectedSites_between", "Select Sites", choices = sites, selected = sites[1:2])
        )
      )
    })
  }
  
  observeEvent(input$selectAllSites_between, {
    trials_df <- read_csv(paste0(resultFolderPath, "/trials_x.csv"))
    trials_x_path <-paste0(resultFolderPath,"/trials_x.csv")
    trials_x <- read_csv(trials_x_path)
    sites <- sort(unique(trials_df$Site))
    updateCheckboxGroupInput(session, "selectedSites_between", selected = sites)
  })
  
  observeEvent(input$unselectAllSites_between, {
    updateCheckboxGroupInput(session, "selectedSites_between", selected = character(0))
  })
  
  
  
  
  # Store the generated plot for download
  between_sites_plot_data <- reactiveVal()
  
  output$plotBetweenSites <- renderPlot({
    req(analysisDone())
    selected_sites <- input$selectedSites_between
    
    plot_dtt <- wthn_sites %>% 
      filter(Site %in% selected_sites) %>% 
      group_by(Site) %>%
      summarize(acc_precip = mean(acc_precip, na.rm = TRUE),
                acc_tt = mean(acc_tt, na.rm = TRUE))
    
    p <- ggplot(plot_dtt) +
      aes(x = acc_precip, y = acc_tt) +
      geom_vline(aes(xintercept = mean(acc_precip)), color = "black", linetype = "dashed") + 
      geom_hline(aes(yintercept = mean(acc_tt)), color = "black", linetype = "dashed") +
      geom_label(aes(label = Site), size = 3) +
      theme_minimal() +
      labs(x = "Acc. Precipitation (mm)", y = "Acc. Thermal Time", 
           title = "10 Year Site Averages for a Typical Growing Season") +
      theme(legend.position = "none")
    
    between_sites_plot_data(p)  # Store the plot in a reactive value
    print(p)  # Render the plot
  })
  
  # Download handler for the between sites plot
  output$downloadBetweenSitesPlot <- downloadHandler(
    filename = function() {
      paste0("between_sites_plot-", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      print(between_sites_plot_data())  # Print the stored plot
      dev.off()
    }
  )
  
  
  
  output$selectSite <- renderUI({
    req(bigmet())
    selectInput("selectSite", "Select Site", choices = unique(bigmet()$Site))
  })
  
  
  
  
  
  # 
  # # Adjust this part within your existing server function
  # data_for_esquisse <- reactive({
  #   req(analysisDone())  # Ensure analysis is done
  #   charact_x_path <- paste0(resultFolderPath, "/charact_x.csv")
  #   if(file.exists(charact_x_path)) {
  #     read.csv(charact_x_path)
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