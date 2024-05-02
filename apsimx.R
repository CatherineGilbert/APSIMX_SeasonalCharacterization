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

# Start, set up trials_df -----

library(apsimx)
library(tidyverse)
library(daymetr)
library(data.table)
library(soilDB)
library(spData)
library(parallel)  # For parallel computing
#Sys.setlocale("LC_ALL", "English_United States")
start_time <- Sys.time() # track running time


#codes_dir <- "C:/Users/sam/Documents/GitHub/APSIMX_SeasonalCharacterization-main" #where the folder with the codes is
codes_dir <- "~/GitHub/APSIMX_SeasonalCharacterization"
codes_dir <- "/Users/cmg3/Documents/GitHub/APSIMX_SeasonalCharacterization"
#setwd("C:/Users/sam/Documents/GitHub/APSIMX_SeasonalCharacterization-main/apsimx_output")
setwd("C:/Users/cmg3/Box/Gilbert/apsimx_output")
setwd("~/Library/CloudStorage/Box-Box/nust_apsimx_output")

crop <- "Soy" #  !!! ask Sam if this can be set via a button 
trials_df <- read_csv(paste0(codes_dir,"/nust_charact_dt.csv")) %>% distinct() %>% mutate(id_trial = row_number()) %>%
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

clusterExport(cl, c("trials_df", "codes_dir", "crop", "edit_apsimx", "edit_apsimx_replace_soil_profile", "paste0", "dir.create", "file.copy", "tryCatch", "print"))

#edit the dates so the simulations runs from a month before sowing to a year afterward (max the end of the met file)

# Parallel APSIM files creation
apsimxfilecreate <- parLapply(cl, 1:nrow(trials_df), function(trial_n) {
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
  batch_rows <- ((batch - 1) * batch_size + 1):min(nrow(trials_df), batch * batch_size)
  batch_trials <- trials_df[batch_rows, ]
  
  # Split trials for parallel execution
  trial_list <- split(batch_trials, seq(nrow(batch_trials)))
  
  # Run APSIM simulations in parallel for the current batch
  # Run APSIM simulations in parallel
  results <- parLapply(cl, trial_list, function(trial) {
    trial_n <- trial$id_trial  # Assuming 'id_trial' is the identifier
    source_dir <- paste0("apsim/trial_", trial_n)
    filename <- paste0(crop, "_", trial_n, ".apsimx")
    output <- data.frame()  # Initialize an empty data frame for the results
    
    # Wrap APSIM simulation and result handling in tryCatch to handle any errors
    tryCatch({
      output_tmp <- apsimx(filename, src.dir = source_dir)
      output_tmp <- mutate(output_tmp, "id_trial" = trial_n) 
      # Append the output of this trial to the overall results
      output <- rbind(output, output_tmp)
      # Save individual trial results
      write_csv(output_tmp, file = paste0(source_dir, "/", crop, "_", trial_n, "_out.csv"))
      return(output)  # Return the output for this trial
    }, error = function(e){
      cat(paste0("Simulation for trial ", trial_n, " failed with error: ", e$message, "\n"))
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


# Summarize Results -----

# Merge Outputs
outfiles <- list.files("apsim/", pattern = "_out", recursive = T)
daily_output <- data.table::rbindlist(lapply(outfiles, function(x){read_csv(paste0("apsim/",x),show_col_types = FALSE)}))
daily_output <- select(daily_output, -CheckpointID,-SimulationID,-Zone,-Year) %>% arrange(id_trial)

# Add cumulative precipitation and thermal time
daily_output <- daily_output %>% group_by(id_trial) %>% mutate(AccPrecip = cumsum(Rain), AccTT = cumsum(ThermalTime)) %>% 
  relocate(AccPrecip, .after = Rain) %>% relocate(AccTT, .after = ThermalTime) 

# Format Outputs into the Characterization
yields <- group_by(daily_output, id_trial) %>% summarize(Yield_Sim = max(Yieldkgha))
mats <- group_by(daily_output, id_trial) %>% select(StageName, Date, id_trial) %>%
  filter(StageName == "ReadyForHarvesting") %>% mutate(MatDate_Sim = date(Date), .keep = "none")
trials_df <- left_join(trials_df, yields) %>% left_join(mats) 
trials_df <- rename(trials_df, Latitude = Y, Longitude = X) %>%
  mutate(DTM_Sim = as.numeric(MatDate_Sim - Planting)) %>%
  relocate(id_trial, id_loc, Site, Latitude, Longitude, Planting, MatDate_Sim, 
           DTM_Sim, sim_start, sim_end, Year, Genetics, Mat, Yield_Sim)

#change stage seperation rule to before or after harvest date, not just 180
# Periods
daily_output %>% group_by(id_trial) %>% mutate(StageName = case_when(
  Stage <= 2 & (Date <= where(daily_output[StageName == "ReadyForHarvesting","Date"])) ~ 1,
  .default = StageName
)) 

daily_output <- mutate(daily_output, Period = case_when(
  Stage < 2 & DOY < 180 ~ "0", 
  Stage == 1 & DOY >= 180 ~ "10", 
  .default = as.character(floor(Stage) - 1)
)) %>% mutate(Period = factor(Period, ordered = T, levels = as.character(0:10)))



charact_x <- daily_output %>% 
  group_by(Period, id_trial) %>% select(-Yieldkgha, -Stage) %>% 
  mutate(AccPrecip = cumsum(Rain), AccTT = cumsum(ThermalTime)) %>%
  summarize(across(where(is.numeric) & !c(DOY,AccPrecip,AccTT), function(x){mean(x,na.omit=T)}), 
            AccPrecip = max(AccPrecip), AccTT = max(AccTT),
            Start_DOY = min(DOY)) %>%
  relocate(id_trial, Period, Rain) %>% 
  relocate(AccPrecip, .after = Rain) %>% relocate(AccTT, .after = ThermalTime) %>%
  relocate(Start_DOY, .after = last_col()) %>%
  arrange(id_trial)

daily_charact_x <- daily_output

unlink("output",recursive = T) ; dir.create("output")
write_csv(trials_df, "output/trials_x.csv")
write_csv(charact_x, "output/charact_x.csv")
write_csv(daily_charact_x, "output/daily_charact_x.csv")


#calculate time duration for running the code:
end_time <- Sys.time()
duration <- end_time - start_time
print(duration)
