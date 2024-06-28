library(shiny)
library(shinydashboard)
library(shinyBS)
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


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Trial Characterization Results"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Description", tabName = "description", icon = icon("info")),
      menuItem("Upload and Analyze", tabName = "analysis", icon = icon("upload")),
      menuItem("View Results", tabName = "results", icon = icon("image")),
      menuItem("View Heatmap", tabName = "heatmap", icon = icon("fire")),
      menuItem("Daily Between Sites", tabName = "daily_between_sites", icon = icon("chart-line")),
      menuItem("Faceted Comparison", tabName = "faceted_comparison", icon = icon("chart-area")),
      menuItem("Between Sites", tabName = "between_sites", icon = icon("chart-bar"))
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

      // Additional script to ensure plots resize correctly after rendering
      $(document).on('shiny:value', function(event) {
        if (event.name === 'heatmapPlot') {
          setTimeout(function() {
            $(window).trigger('resize');
          }, 500);
        }
      });
    "))
    ),
    tabItems(
      tabItem(tabName = "description",
              fluidPage(
                h2("Seasonal Characterization Tool"),
                p("Built in R and Shiny using the apsimr package."),
                p("Purpose: To characterize the growing season at one or more sites according to the crop's response to environmental conditions at those sites."),
                h3("Seasonal Characterization Tool Can be used to:"),
                tags$ul(
                  tags$li("Understand environment in terms of the conditions / stressors present at specific stages of the crop's development."),
                  tags$li("Compare seasonal conditions between sites and how those conditions have changed over time."),
                  tags$li("Predict crop phenology and performance from a cultivar's maturity, planting date, and location.")
                )
              )
      ),
      tabItem(tabName = "analysis",
              fluidPage(
                selectInput("cropType", "Select Crop Type", choices = c("Maize" = "Maize", "Soy" = "Soy")),
                fileInput("fileUpload", "Upload Input File", accept = c(".csv")),
                actionButton("runAnalysis", "Run Analysis", icon = icon("play")),
                uiOutput("fileSelectUI"),  # Add this line
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
                p("This heatmap visualizes the means of the selected variable by site and genetic group. 
            Use the dropdown menus to select the variable and genetic group for analysis."),
                uiOutput("varHeatmapUI"),
                uiOutput("genSelectUI"),
                uiOutput("heatmapPlotUI")  # Use uiOutput to render the heatmap plot
              )
      ),
      tabItem(tabName = "daily_between_sites",
              fluidPage(p("This section allows you to compare daily accumulated precipitation and thermal time between different sites. Select the comparison type and sites for analysis."),
              fluidRow(
                column(width = 9,  # Adjust the width as needed
                       selectInput("comparisonType", "Select Comparison Type", choices = c(
                         "Accumulated Precipitation (Day of Year)" = "precip_doy",
                         "Accumulated Thermal Time (Day of Year)" = "tt_doy",
                         "Accumulated Precipitation (Days after Sowing)" = "precip_das",
                         "Accumulated Thermal Time (Days after Sowing)" = "tt_das"
                       )),
                       plotOutput("comparisonPlot")
                ),
                column(width = 3,  # Adjust the width as needed
                       uiOutput("siteSelectionUI")
                )
              )
              )
      ),
      tabItem(tabName = "between_sites",
              fluidPage(
                p("This section visualizes the 10-year site averages for a typical growing season, comparing accumulated precipitation and thermal time between selected sites."),
              fluidRow(
                column(width = 3,
                       uiOutput("siteSelectionUI_between")
                ),
                column(width = 9,
                       plotOutput("plotBetweenSites")
                )
              )
              )
      ),
      tabItem(tabName = "faceted_comparison",
              fluidPage(
                p("This section provides a faceted comparison of accumulated precipitation and thermal time for different sites. Select the sites to visualize the comparison."),
              fluidRow(
                column(width = 3,
                       uiOutput("siteSelectionUI_faceted")
                ),
                column(width = 9,
                       plotOutput("facetedComparisonPlot")
                )
              )
      )
      )
    )
  )
)
# Define server logic
server <- function(input, output, session) {

  gen <- 1 # should be selectable for heatmap
  setwd("C:/Users/cmg3/Documents/GitHub/APSIMX_SeasonalCharacterization/apsimx_output")
  # Path to the scripts and results
  codesPath <- "C:/Users/cmg3/Documents/GitHub/APSIMX_SeasonalCharacterization"
  resultFolderPath <- "C:/Users/cmg3/Documents/GitHub/APSIMX_SeasonalCharacterization/apsimx_output/output"

  # Reactive values for storing the analysis state and the selected variable
  analysisDone <- reactiveVal(FALSE)
  #analysisDone <- reactiveVal(TRUE)
  analysisInProgress <- reactiveVal(FALSE)
  
  observe({
    if (analysisInProgress()) {
      shinyjs::disable("runAnalysis")
    } else {
      shinyjs::enable("runAnalysis")
    }
  })
  
  
  
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
    
    
    setwd("C:/Users/cmg3/Documents/GitHub/APSIMX_SeasonalCharacterization/apsimx_output")
    #setwd("C:/Users/cmg3/Box/Gilbert/apsimx_output")
    
    crop <- input$cropType #  !!! ask cmg3 if this can be set via a button 
    writeLines(crop, paste0(codesPath, "/selected_crop.txt"))
    source("C:/Users/cmg3/Documents/GitHub/APSIMX_SeasonalCharacterization/apsimx_helper.R")
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
  
  
  # Render the comparison plot based on the selected comparison type
  output$comparisonPlot <- renderPlot({
    req(accumulatedData(), input$comparisonType)
    data <- accumulatedData()
    if (input$comparisonType == "precip_doy") {
      ggplot(data) + 
        aes(x = day, y = acc_precip, colour = Site) +
        geom_line() +
        scale_color_hue(direction = 1) +
        labs(x = "Day of Year", y = "Accumulated Precipitation (mm)") +
        theme_minimal()
    } else if (input$comparisonType == "tt_doy") {
      ggplot(data) + 
        aes(x = day, y = acc_tt, colour = Site) +
        geom_line() +
        scale_color_hue(direction = 1) +
        labs(x = "Day of Year", y = "Accumulated Thermal Time") +
        theme_minimal()
    } else if (input$comparisonType == "precip_das") {
      sdbtw_sites <- data %>% mutate(day = day - min(day) + 1)
      ggplot(sdbtw_sites) + 
        aes(x = day, y = acc_precip, colour = Site) +
        geom_line() +
        scale_color_hue(direction = 1) +
        labs(x = "Days after Sowing", y = "Acc. Precipitation (mm)") +
        theme_minimal()
    } else if (input$comparisonType == "tt_das") {
      sdbtw_sites <- data %>% mutate(day = day - min(day) + 1)
      ggplot(sdbtw_sites) + 
        aes(x = day, y = acc_tt, colour = Site) +
        geom_line() +
        scale_color_hue(direction = 1) +
        labs(x = "Days after Sowing", y = "Acc. Thermal Time") +
        theme_minimal()
    }
  })
  
  output$facetedComparisonPlot <- renderPlot({
    req(input$selectedSites_faceted)
    selected_sites <- input$selectedSites_faceted
    req(length(selected_sites) > 0)
    filtmet <- bigmet() %>% left_join(mean_startend) %>% filter(day >= first_doy & day <= final_doy)
    plot_data <- wthn_sites %>% filter(Site %in% selected_sites)
    means <- plot_data %>% group_by(Site) %>%
      summarise(mean_acc_precip = mean(acc_precip),
                mean_acc_tt = mean(acc_tt))
    
    ggplot(plot_data) +
      aes(x = acc_precip, y = acc_tt) +
      facet_wrap(vars(Site), scales = "free") +
      geom_vline(data = means, aes(xintercept = mean_acc_precip), color = "black", linetype = "dashed") + 
      geom_hline(data = means, aes(yintercept = mean_acc_tt), color = "black", linetype = "dashed") +
      geom_label(label = plot_data$year, size = 3) +
      labs(x = "Acc. Precipitation (mm)", y = "Acc. Thermal Time") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  
  
  output$viewData <- renderDT({
    req(analysisDone())
    updateSiteSelectionUI()
    updateSiteSelectionBetweenUI()
    charact_x_path <- paste0(resultFolderPath, "/charact_x.csv")
    if (file.exists(charact_x_path)) {
      data <- read.csv(charact_x_path)
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
      print("awdawdawdawd")
      return(NULL)
    }
  })
  
  output$varSelectUI <- renderUI({
    req(analysisDone())
    charact_x_path <- paste0(resultFolderPath, "/charact_x.csv")
    if (file.exists(charact_x_path)) {
      data <- read.csv(charact_x_path)
      selectInput("varSelect_boxplot", "Select Variable", choices = names(data)[-1])
    }
  })
  
  observeEvent(input$varSelect_boxplot, {
    selectedVariable(input$varSelect_boxplot)
  }, ignoreInit = TRUE)
  
  output$boxplot <- renderPlot({
    req(analysisDone(), selectedVariable())
    charact_x_path <- paste0(resultFolderPath, "/charact_x.csv")
    
    if (file.exists(charact_x_path)) {
      data <- read.csv(charact_x_path)
      data$Site <- as.factor(data$Site)  # Ensure Site is treated as a factor
      
      selected_var <- selectedVariable()
      
      # Debugging print statements
      print("Selected variable is:")
      print(selected_var)
      print("Column names of data are:")
      print(names(data))
      print("Dimensions of data are:")
      print(dim(data))
      
      # Check if selected_var is in the column names of data
      if(selected_var %in% names(data)) {
        print("Summary of selected variable:")
        print(summary(data[[selected_var]]))
        
        # Additional debugging to see if any NA values or other issues
        print("Checking for NA values in selected variable:")
        print(sum(is.na(data[[selected_var]])))
        
        # Ensure that we have non-zero length for the selected variable column
        if (length(data[[selected_var]]) != nrow(data)) {
          print(paste("Error: Length of selected variable column", length(data[[selected_var]]), "does not match number of rows", nrow(data)))
        }
        
        # Create the bar plot
        ggplot(data, aes(x = Site, y = .data[[selected_var]], fill = Site)) +
          geom_bar(stat = "identity") +
          labs(x = "Site", y = selected_var) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        print(paste("Error: Variable", selected_var, "not found in data frame"))
      }
    } else {
      print(paste("Error: File", charact_x_path, "does not exist"))
    }
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$fileSelect, ".csv")
    },
    content = function(file) {
      selected_file_path <- file.path(resultFolderPath, input$fileSelect)
      if (file.exists(selected_file_path)) {
        file.copy(selected_file_path, file)
      }
    }
  )
  
  output$fileSelectUI <- renderUI({
    req(analysisDone())
    files <- list.files(resultFolderPath, pattern = "\\.csv$", full.names = FALSE)
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
  
  
  output$heatmapPlotUI <- renderUI({
    updateSiteSelectionFacetdUI()
    req(input$heatmapSelect)  # Ensure there's a selected value
    plotOutput("heatmapPlot", width = "70%", height = "600px")
  })
  
  
  
  
  
  heatmap_data <- reactive({
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
      
      
      pheatmap(var_mat, angle_col = 45,
               color=brewer.pal(11,"RdBu"),
               fontsize = 10, 
               display_numbers = round(var_mat, 2), 
               number_color = "white", 
               #scale = "column",
               number_format = "%.2f", 
               legend = F,
               cluster_cols = F,
               cluster_rows = T,
               main = paste0("Means of ",var," by Site (MG ",gen,")"))
    } else {
      plot(NULL, main = "Data not available")
    }
    list(var_mat = var_mat, var = var)
  })
  
  output$heatmapPlot <- renderPlot({
    data <- req(heatmap_data())  # Make sure data is available
    pheatmap(data$var_mat, main = paste("Heatmap for", data$var))
  })
  
  output$varHeatmapUI <- renderUI({
    req(analysisDone())
    charact_x_path <- paste0(resultFolderPath, "/daily_charact_x.csv")
    if(file.exists(charact_x_path)) {
      charact_x <- read.csv(charact_x_path)
      varchoice <- charact_x %>% ungroup() %>% select(where(is.numeric) & !c(id_trial, Period)) %>% names()
      selectInput("heatmapSelect", "Select Variable for Heatmap", choices = varchoice)
    }
  })
  
  
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
  
  filteredBetweenData <- reactive({
    req(input$selectedSites_between)
    req(analysisDone())
    cat("Selected Sites for Between Data:", input$selectedSites_between, "\n")
    
    # Ensure that bigmet data is ready and reactive to selected sites
    data <- bigmet()
    cat("Bigmet data head:\n")
    print(head(data))
    
    # Filter the bigmet data based on selected sites
    filtered_data <- data %>% filter(Site %in% input$selectedSites_between)
    cat("Filtered Data head:\n")
    print(head(filtered_data))
    
    filtered_data
  })
  
  output$plotBetweenSites <- renderPlot({
    #req(filteredBetweenData())
    req(analysisDone())
    selected_sites <- input$selectedSites_between
    
    plot_dtt <- wthn_sites %>% summarize(acc_precip = mean(acc_precip), acc_tt = mean(acc_tt))%>% filter(Site %in% selected_sites)
    plot_data <- filteredBetweenData() %>% filter(Site %in% selected_sites) %>%
      group_by(Site) %>%
      summarize(acc_precip = mean(rain, na.rm = TRUE),
                acc_tt = mean(tt, na.rm = TRUE))
    
    ggplot(plot_dtt) +
      aes(x = acc_precip, y = acc_tt) +
      geom_vline(aes(xintercept = mean(acc_precip)), color = "black", linetype = "dashed") + 
      geom_hline(aes(yintercept = mean(acc_tt)), color = "black", linetype = "dashed") +
      geom_label(aes(label = Site), size = 3) +
      theme_minimal() +
      labs(x = "Acc. Precipitation (mm)", y = "Acc. Thermal Time", 
           title = "10 Year Site Averages for a Typical Growing Season") +
      theme(legend.position = "none")
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
