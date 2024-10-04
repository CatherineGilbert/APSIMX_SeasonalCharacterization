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

crop <- "Soy" #  !!! ask Sam if this can be set via a button 
trials_df <- read_csv(paste0(codes_dir,"/tiny_charact_dt.csv")) %>% distinct() %>% mutate(id_trial = row_number()) %>%
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


# Ensure the directory exists for weather data
dir.create("met", recursive = TRUE, showWarnings = FALSE)

parLapply(cl, seq_len(nrow(locyear_df)), function(idx) {
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

