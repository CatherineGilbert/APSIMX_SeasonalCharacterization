library(shiny)
library(shinydashboard)
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
library(RColorBrewer)
library(tidyverse)
library(esquisse)
library(tidyr)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Trial Characterization Results"),
  dashboardSidebar(
    sidebarMenu(
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
      tabItem(tabName = "analysis",
              fluidPage(
                selectInput("cropType", "Select Crop Type", choices = c("Maize" = "Maize", "Soy" = "Soy")),
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
      ),
      tabItem(tabName = "between_sites",
              fluidRow(
                column(width = 3,
                       uiOutput("siteSelectionUI_between")
                ),
                column(width = 9,
                       plotOutput("plotBetweenSites")
                )
              )
      ),
      tabItem(tabName = "faceted_comparison",
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

# Define server logic
server <- function(input, output, session) {
  setwd("C:/Users/sam/Documents/GitHub/APSIMX_SeasonalCharacterization/apsimx_output")
  # Path to the scripts and results
  codesPath <- "C:/Users/sam/Documents/GitHub/APSIMX_SeasonalCharacterization"
  resultFolderPath <- "C:/Users/sam/Documents/GitHub/APSIMX_SeasonalCharacterization/apsimx_output/output"
  
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
  
    updateSiteSelectionUI()
    })
  
  observeEvent(input$runAnalysis, {
    
    req(input$fileUpload)
    crop <- input$cropType #  !!! ask Sam if this can be set via a button 
    
    #update outputs and visaluzations
    #think about future labs and also company opportunities after i graduate next year
    
    #get feedback from breeders on what they think is valuable and what kind of outputs they value
    #for over-performance / under-performance can use maturity checks as yield checks 
    #check that the actual maturity (DtM) and simulated maturities (stage DOYs) are accurate
    #investigate structural equation modeling
    
    #for location comparison, standardize axes for acc tt and acc tt instead 
    #  of using real values so that you can plot all the years and locations
    
    #build a machine learning model directly off the seasonal parameters instead of just using the apsim yield output
    #maturity / flowering validation > yield validation for describing the growing season
    
    #which of the seasonal variables are affecting the performance of the varieties
    #rate stress during the different periods as high med low? and use to describe environments 
    #get feedback from breeders on what they think is valuable and what kind of outputs they value
    #for over-performance / under-performance can use maturity checks as yield checks 
    #check that the actual maturity (DtM) and simulated maturities (stage DOYs) are accurate
    #investigate structural equation modeling
    
    #build a machine learning model directly off the seasonal parameters instead of just using the apsim yield output
    
    #which of the seasonal variables are affecting the performance of the varieties
    
    # Start, set up trials_df -----
    #get feedback from breeders on what they think is valuable and what kind of outputs they value
    #for over-performance / under-performance can use maturity checks as yield checks 
    #check that the actual maturity (DtM) and simulated maturities (stage DOYs) are accurate
    #investigate structural equation modeling
    
    #build a machine learning model directly off the seasonal parameters instead of just using the apsim yield output
    
    #which of the seasonal variables are affecting the performance of the varieties
    
    # Start, set up trials_df -----
    
    library(apsimx)
    library(tidyverse)
    library(daymetr)
    library(data.table)
    library(soilDB)
    library(spData)
    library(parallel)  # For parallel computing
    Sys.setlocale("LC_ALL", "English_United States")
    start_time <- Sys.time() # track running time
    
    
    codes_dir <- "C:/Users/sam/Documents/GitHub/APSIMX_SeasonalCharacterization" #where the folder with the codes is
    #codes_dir <- "~/GitHub/APSIMX_SeasonalCharacterization"
    #codes_dir <- "/Users/cmg3/Documents/GitHub/APSIMX_SeasonalCharacterization"
    setwd("C:/Users/sam/Documents/GitHub/APSIMX_SeasonalCharacterization/apsimx_output")
    #setwd("C:/Users/cmg3/Box/Gilbert/apsimx_output")
    #setwd("~/Library/CloudStorage/Box-Box/apsimx_output")
    
    trials_df <- read_csv(paste0(codes_dir,"/apsimx_output/output/input.csv")) %>% distinct() %>% mutate(id_trial = row_number()) %>%
      rename(X = Longitude, Y = Latitude)
    locs_df <- select(trials_df, X, Y) %>% distinct() %>% mutate(id_loc = row_number())
    trials_df <- left_join(trials_df, locs_df)
    trials_df <- trials_df %>% mutate(Planting = as.Date(Planting), Year = format(Planting,"%Y")) %>% 
      mutate(sim_start = Planting %m-% months(1), sim_end = Planting %m+% months(10))
    
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
    
    # Get weather, make met files -----
    prev_year <- as.numeric(substr(Sys.time(),1,4)) - 1
    
    locyear_df <- trials_df %>% select(X,Y,id_loc, sim_start) %>% 
      mutate(first_year = year(sim_start)) %>% 
      select(-sim_start) %>% unique() %>% group_by(id_loc,X,Y) %>%
      summarize(first_year = min(first_year)) %>%
      mutate(first_year = min(first_year, prev_year - 10), last_year = prev_year) 
    
    unlink("met",recursive = T) ; dir.create("met")
    
    # Setup for parallel processing
    no_cores <- detectCores() - 2  # Reserve 2 cores for the system
    cl <- makeCluster(no_cores)
    clusterExport(cl, varlist = c("locyear_df","get_daymet2_apsim_met", "napad_apsim_met", "impute_apsim_met", "write_apsim_met"), envir = environment())
    
    
    # Ensure the directory exists for weather data
    dir.create("met", recursive = TRUE, showWarnings = FALSE)
    
    parLapply(cl, seq_len(nrow(locyear_df)), function(idx) {
      Sys.setlocale("LC_ALL", "English_United States")
      locyear_tmp <- locyear_df[idx, ]
      try({
        met_tmp <- get_daymet2_apsim_met(lonlat = c(locyear_tmp$X, locyear_tmp$Y), 
                                         years = c(as.integer(locyear_tmp$first_year), as.integer(locyear_tmp$last_year)), 
                                         silent = TRUE)
        na_met_tmp <- tryCatch(napad_apsim_met(met_tmp), error = function(e) met_tmp)
        imp_met_tmp <- tryCatch(impute_apsim_met(na_met_tmp), warning = function(w) na_met_tmp)
        attr(imp_met_tmp, "site") <- attr(met_tmp, "site")
        attr(imp_met_tmp, "latitude") <- attr(met_tmp, "latitude")
        attr(imp_met_tmp, "longitude") <- attr(met_tmp, "longitude")
        write_apsim_met(imp_met_tmp, wrt.dir = "met", paste0("loc_", locyear_tmp$id_loc, ".met"))
      })
    })
    
    
    # Get soil, make soil file -----
    soil_profile_list = list()
    unlink("soils",recursive = T) ; dir.create("soils")
    locs_df$got_soil <- NA
    
    ids_needs_soil <- locs_df[locs_df$got_soil == F | is.na(locs_df$got_soil),]$id_loc
    for (id in ids_needs_soil){
      locs_tmp <- locs_df[locs_df$id_loc == id,]
      tryCatch({
        soil_profile_tmp <- tryCatch(get_ssurgo_soil_profile(lonlat = c(locs_tmp$X,locs_tmp$Y), fix = T),
                                     error = function(e){soil_profile_tmp <- list(get_isric_soil_profile(lonlat = c(locs_tmp$X,locs_tmp$Y), fix = T))})
        
        horizon <- soil_profile_tmp[[1]]$soil 
        
        soilwat <- soilwat_parms() #creating SWCON in SoilWater parameters
        PO <- 1-horizon$BD/2.65
        soilwat$SWCON <- (PO-horizon$DUL)/PO
        soilwat$SWCON <- ifelse(soilwat$SWCON < 0, 0, soilwat$SWCON)
        soilwat$Thickness <- horizon$Thickness 
        soil_profile_tmp[[1]]$soilwat <- soilwat
        
        initwat <- initialwater_parms() #set initial water to reasonable values
        initwat$InitialValues <- horizon$DUL
        initwat$Thickness <- horizon$Thickness
        soil_profile_tmp[[1]]$initialwater <- initwat
        
        oc_min <- 0.001 #set minimum carbon content in soils
        given_oc <- soil_profile_tmp[[1]][["soil"]]$Carbon
        soil_profile_tmp[[1]][["soil"]]$Carbon <- ifelse(given_oc < oc_min, oc_min, given_oc) 
        
        write_rds(soil_profile_tmp, file = paste0("soils/soil_profile_",id))
        soil_profile_list[[as.character(id)]] <- soil_profile_tmp[[1]]
        locs_df[locs_df$id_loc == id,"got_soil"] <- T
        print(paste0("loc: ",id,"   ",round(which(ids_needs_soil == id)/length(ids_needs_soil),4)))
      }, error = function(e){
        locs_df[locs_df$id_loc == id,"got_soil"] <<- F
        print(paste0("loc: ",id,"   ",round(which(ids_needs_soil == id)/length(ids_needs_soil),4),"  FAIL"))
      })
    }
    write_rds(soil_profile_list, "soils/soil_profile_list.rds")
    
    
    # Create APSIM files -----
    unlink("apsim", recursive = TRUE)
    dir.create("apsim")
    file.copy(from = paste0(codes_dir, "/template_models/", crop, "_Template.apsimx"), 
              to = paste0(crop, "_.apsimx"), overwrite = TRUE)
    
    # Prepare for parallel processing
    
    clusterExport(cl, c("trials_df", "codes_dir", "crop", "edit_apsimx", "edit_apsimx_replace_soil_profile", 
                        "paste0", "dir.create", "file.copy", "tryCatch", "print"))
    
    #edit the dates so the simulations runs from a month before sowing to a year afterward (max the end of the met file)
    
    # Parallel APSIM files creation
    apsimxfilecreate <- parLapply(cl, 1:nrow(trials_df), function(trial_n) {
      Sys.setlocale("LC_ALL", "English_United States")
      
      trial_tmp <- trials_df[trial_n,]
      if(!dir.exists(paste0("apsim/trial_",trial_n))) {dir.create(paste0("apsim/trial_",trial_n))}
      source_dir <- paste0("apsim/trial_",trial_n)
      write_dir <-  paste0("apsim/trial_",trial_n)
      filename <- paste0(crop, "_", trial_n,".apsimx")
      edit_apsimx(file = paste0(crop,"_.apsimx"), wrt.dir = write_dir, edit.tag = trial_n,
                  node = "Clock", parm = "Start", value = paste0(trial_tmp$sim_start,"T00:00:00"), verbose = F)
      edit_apsimx(file = filename,  src.dir = source_dir, wrt.dir = write_dir, overwrite = T,
                  node = "Clock", parm = "End", value = paste0(trial_tmp$sim_end,"T00:00:00"), verbose = F)
      edit_apsimx(file = filename, src.dir = source_dir, wrt.dir = write_dir, overwrite = T,
                  node = "Weather", value = paste0(getwd(),"/met/loc_",trial_tmp$id_loc,".met"), verbose = F)
      edit_apsimx(filename, src.dir = source_dir,  wrt.dir = write_dir, overwrite = T,
                  node = "Manager", manager.child = "Sow on a fixed date",
                  parm = "SowDate", value = as.character(format(trial_tmp$Planting, "%d-%b")), verbose = F)
      edit_apsimx(filename, src.dir = source_dir, wrt.dir = write_dir, overwrite = T, node = "Crop", parm = "SowDate", 
                  value = as.character(format(trial_tmp$Planting, "%d-%b")), verbose = F)
      edit_apsimx(filename, src.dir = source_dir,  wrt.dir = write_dir, overwrite = T,
                  node = "Crop", parm = "CultivarName", value = trial_tmp$Mat, verbose = F)
      tryCatch({
        edit_apsimx_replace_soil_profile(file = filename, src.dir = source_dir, wrt.dir = write_dir, overwrite = T,
                                         soil.profile = soil_profile_list[[as.character(trial_tmp$id_loc)]], verbose = F)
      }, error = function(e){})
      invisible()
    })
    
    # Run APSIM files -----
    
    # Define the number of batches
    num_batches <- 10  # You can change this to run different percentages at a time
    
    # Calculate the number of trials per batch
    batch_size <- ceiling(nrow(trials_df) / num_batches)
    
    
    clusterExport(cl, c("trials_df", "codes_dir", "crop", "edit_apsimx", "edit_apsimx_replace_soil_profile", "paste0", "dir.create", "file.copy", "tryCatch", "print", "apsimx", "mutate", "write_csv", "soil_profile_list"))
    
    
    # Initialize a list to hold results from all batches
    all_results <- list()
    
    
    # Process each batch
    for (batch in 1:num_batches) {
      # Determine the rows for the current batch
      Sys.setlocale("LC_ALL", "English_United States")
      batch_rows <- ((batch - 1) * batch_size + 1):min(nrow(trials_df), batch * batch_size)
      batch_trials <- trials_df[batch_rows, ]
      
      # Split trials for parallel execution
      trial_list <- split(batch_trials, seq(nrow(batch_trials)))
      
      # Run APSIM simulations in parallel for the current batch
      # Run APSIM simulations in parallel
      results <- parLapply(cl, trial_list, function(trial) {
        
        Sys.setlocale("LC_ALL", "English_United States")
        trial_n <- trial$id_trial  # Assuming 'id_trial' is the identifier
        source_dir <- paste0("apsim/trial_", trial_n)
        filename <- paste0(crop, "_", trial_n, ".apsimx")
        output <- data.frame()  # Initialize an empty data frame for the results
        
        log_file <- paste0(source_dir, "/", crop, "_", trial_n, "_log.txt")
        sink(log_file, append = TRUE)
        # Wrap APSIM simulation and result handling in tryCatch to handle any errors
        tryCatch({
          output_tmp <- apsimx(filename, src.dir = source_dir)
          output_tmp <- mutate(output_tmp, "id_trial" = trial_n) 
          # Append the output of this trial to the overall results
          output <- rbind(output, output_tmp)
          # Save individual trial results
          write_csv(output_tmp, file = paste0(source_dir, "/", crop, "_", trial_n, "_out.csv"))
          message(sprintf("Successfully written file for trial %d", trial_n))
          sink()  # Close the connection to the log file
          return(output)  # Return the output for this trial
        }, error = function(e){
          cat(paste0("Simulation for trial ", trial_n, " failed with error: ", e$message, "\n"))
          sink()
          return(NULL)  # Return NULL if there was an error
        })
      })                                                                                                                                                                                                                                                                                                                                    
      
      # Combine the results from this batch and add to the all_results list
      batch_results <- do.call(rbind, results)
      all_results[[batch]] <- batch_results
      
      # Print out the progress
      cat(sprintf("Completed batch %d out of %d (%.2f%%)\n", batch, num_batches, 100 * batch / num_batches))
    }
    
    
    # Stop the cluster
    stopCluster(cl)
    
    analysisDone(TRUE)
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
    current_year <- as.numeric(substr(Sys.time(), 1, 4)) - 1
    bigmet <- data.frame()
    for(s in 1:max(trials_x$id_loc)){
      lil_met <- read_apsim_met(paste0("met/loc_",s,".met"), verbose = F) %>% as_tibble() %>%
        filter(year >= current_year - 9, year <= current_year) %>% mutate(id_loc = s)
      bigmet <- rbind(bigmet, lil_met)
    }
    bigmet <- trials_x %>% select(Site, id_loc) %>% distinct() %>% left_join(bigmet) %>% group_by(Site, id_loc, year, day)
    max_temp = 34
    base_temp = 0
    bigmet <- mutate(bigmet, tt = max((min(maxt, max_temp) + max(mint, base_temp)) / 2 - base_temp, 0)) %>% ungroup()
    bigmet <- filter(bigmet, Site %in% input$selectedSites)
    bigmet
  })
  
  # Reactive expression to generate the filtered and accumulated data
  accumulatedData <- reactive({
    req(filteredMetData())
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
    if(file.exists(charact_x_path)) {
      datatable(read.csv(charact_x_path), extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ), escape = FALSE)
    } else {
      return()
    }
  }, options = list(scrollX = TRUE))
  
  output$varSelectUI <- renderUI({
    req(analysisDone())
    charact_x_path <- paste0(resultFolderPath, "/charact_x.csv")
    if(file.exists(charact_x_path)) {
      data <- read.csv(charact_x_path)
      selectInput("varSelect", "Select Variable", choices = names(data)[-1])
    }
  })
  
  observeEvent(input$varSelect, {
    selectedVariable(input$varSelect)
  }, ignoreInit = TRUE)
  
  output$boxplot <- renderPlot({
    req(analysisDone(), selectedVariable())
    charact_x_path <- paste0(resultFolderPath, "/charact_x.csv")
    if(file.exists(charact_x_path)) {
      data <- read.csv(charact_x_path)
      ggplot(data, aes_string(x = names(data)[1], y = selectedVariable())) +
        geom_boxplot() +
        labs(x = "Site", y = selectedVariable()) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { "results.csv" },
    content = function(file) {
      charact_x_path <- paste0(resultFolderPath, "/charact_x.csv")
      if(file.exists(charact_x_path)) {
        file.copy(charact_x_path, file)
      }
    }
  )
  output$heatmapPlotUI <- renderUI({
    updateSiteSelectionFacetdUI()
    req(input$heatmapSelect)  # Ensure there's a selected value
    plotOutput("heatmapPlot", width = "70%", height = "600px")
  })
  
  
  
  
  
  heatmap_data <- reactive({
    gen <- 1
    req(input$heatmapSelect)  # Ensure a variable is selected
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
    sites <- sort(unique(trials_df$Site))
    updateCheckboxGroupInput(session, "selectedSites_between", selected = sites)
  })
  
  observeEvent(input$unselectAllSites_between, {
    updateCheckboxGroupInput(session, "selectedSites_between", selected = character(0))
  })
  
  filteredBetweenData <- reactive({
    req(input$selectedSites_between)
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
