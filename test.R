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


codes_dir <- "/srv/shiny-server/shiny-app" #where the folder with the codes is
#codes_dir <- "~/GitHub/APSIMX_SeasonalCharacterization"
#codes_dir <- "/Users/cmg3/Documents/GitHub/APSIMX_SeasonalCharacterization"
setwd("/srv/shiny-server/shiny-app/apsimx_output")
#setwd("C:/Users/cmg3/Box/Gilbert/apsimx_output")
#setwd("~/Library/CloudStorage/Box-Box/apsimx_output")
clear_progress_log <- function() {
  fileConn <- file("progress.log", "w")
  close(fileConn)
}

log_progress <- function(message) {
  write(message, file = "progress.log", append = TRUE)
}

log_progress("Starting analysis")
crop <- readLines(paste0(codes_dir, "/apsimx_output/output/selected_crop.txt"))
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
# Get soil, make soil file -----
soil_profile_list = list()
unlink("soils",recursive = T) ; dir.create("soils")
locs_df$got_soil <- NA

ids_needs_soil <- locs_df[locs_df$got_soil == F | is.na(locs_df$got_soil),]$id_loc
for (id in ids_needs_soil){
  locs_tmp <- locs_df[locs_df$id_loc == id,]
  tryCatch({
  soil_profile_tmp <- tryCatch(get_ssurgo_soil_profile(lonlat = c(locs_tmp$X, locs_tmp$Y), fix = TRUE),
                               error = function(e){
                                 message <- paste0("Error in get_ssurgo_soil_profile: ", e$message)
                                 print(message)
                                 log_progress(message)
                                 return(list(get_isric_soil_profile(lonlat = c(locs_tmp$X, locs_tmp$Y), fix = TRUE)))
                               })
  
  horizon <- soil_profile_tmp[[1]]$soil 
  
  soilwat <- soilwat_parms() # creating SWCON in SoilWater parameters
  PO <- 1 - horizon$BD / 2.65
  soilwat$SWCON <- (PO - horizon$DUL) / PO
  soilwat$SWCON <- ifelse(soilwat$SWCON < 0, 0, soilwat$SWCON)
  soilwat$Thickness <- horizon$Thickness 
  soil_profile_tmp[[1]]$soilwat <- soilwat
  
  initwat <- initialwater_parms() # set initial water to reasonable values
  initwat$InitialValues <- horizon$DUL
  initwat$Thickness <- horizon$Thickness
  soil_profile_tmp[[1]]$initialwater <- initwat
  
  oc_min <- 0.001 # set minimum carbon content in soils
  given_oc <- soil_profile_tmp[[1]][["soil"]]$Carbon
  soil_profile_tmp[[1]][["soil"]]$Carbon <- ifelse(given_oc < oc_min, oc_min, given_oc) 
  
  write_rds(soil_profile_tmp, file = paste0("soils/soil_profile_", id))
  soil_profile_list[[as.character(id)]] <- soil_profile_tmp[[1]]
  locs_df[locs_df$id_loc == id, "got_soil"] <- TRUE
  print(paste0("loc: ", id, "   ", round(which(ids_needs_soil == id) / length(ids_needs_soil), 4)))
}, error = function(e){
  locs_df[locs_df$id_loc == id, "got_soil"] <<- FALSE
  error_message <- paste0("Error for loc: ", id, ": ", e$message)
  print(error_message)
  log_progress(error_message)
})
}
write_rds(soil_profile_list, "soils/soil_profile_list.rds")
