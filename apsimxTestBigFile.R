library(apsimx)
library(tidyverse)
library(daymetr)
library(data.table)
library(parallel)  # For parallel computing
Sys.setlocale("LC_ALL", "English_United States")
start_time <- Sys.time() # track running time


codes_dir <- "C:/Users/sam/Documents/GitHub/APSIMX_SeasonalCharacterization-main" #where the folder with the codes is
setwd("C:/Users/sam/Documents/GitHub/APSIMX_SeasonalCharacterization-main/apsimx_output")


crop <- "Soy"
trials_df <- read_csv("small_charact_dt.csv") %>%
  distinct() %>%
  rename(X = Longitude, Y = Latitude) %>%
  mutate(id_trial = row_number())
locs_df <- select(trials_df, X, Y) %>%
  distinct() %>%
  mutate(id_loc = row_number())
trials_df <- left_join(trials_df, locs_df)
trials_df <- mutate(trials_df, id_trial = row_number())
trials_df <- trials_df %>%
  mutate(Planting = as.Date(Planting, format = "%m/%d/%Y"),
         Day = format(Planting,"%d"),
         Month = format(Planting,"%b"),
         Year = format(Planting,"%Y"))


# Prepare locyear_df for parallel processing

# Get weather, make met files
locyear_df <- trials_df %>% select(X,Y,id_loc,Year) %>% unique() %>% group_by(id_loc) %>%
  mutate(first_year = min(Year), last_year = max(Year)) %>% select(-Year) %>% distinct() %>% arrange(id_loc)
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


# Get soil, make soil file
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
  trials_df <- trials_df %>%
    mutate(Mat = case_when(
      Genetics <= 1 ~ "80",
      Genetics == 2 ~ "90",
      Genetics == 3 ~ "95",
      Genetics == 4 ~ "103",
      Genetics == 5 ~ "108",
      Genetics == 6 ~ "112",
      Genetics == 7 ~ "115",
      Genetics == 8 ~ "120",
      Genetics >= 9 ~ "130",
    )) %>% mutate(Mat = paste0("B_",Mat))
}
# Create APSIM files preparation
unlink("apsim", recursive = TRUE)
dir.create("apsim")
file.copy(from = paste0(codes_dir, "/template_models/", crop, "_Template.apsimx"), 
          to = paste0(crop, "_.apsimx"), overwrite = TRUE)

# Prepare for parallel processing

clusterExport(cl, c("trials_df", "codes_dir", "crop", "edit_apsimx", "edit_apsimx_replace_soil_profile", "paste0", "dir.create", "file.copy", "tryCatch", "print"))

# Parallel APSIM files creation
apsimxfilecreate <- parLapply(cl, 1:nrow(trials_df), function(trial_n) {
  trial_tmp <- trials_df[trial_n,]
  if(!dir.exists(paste0("apsim/trial_",trial_n))) {dir.create(paste0("apsim/trial_",trial_n))}
  source_dir <- paste0("apsim/trial_",trial_n)
  write_dir <-  paste0("apsim/trial_",trial_n)
  filename <- paste0(crop, "_", trial_n,".apsimx")
  edit_apsimx(file = paste0(crop,"_.apsimx"), wrt.dir = write_dir, edit.tag = trial_n,
              node = "Clock", parm = "Start", value = paste0(trial_tmp$Year,"-01-01T00:00:00"), verbose = F)
  edit_apsimx(file = filename,  src.dir = source_dir, wrt.dir = write_dir, overwrite = T,
              node = "Clock", parm = "End", value = paste0(trial_tmp$Year,"-12-31T00:00:00"), verbose = F)
  edit_apsimx(file = filename, src.dir = source_dir, wrt.dir = write_dir, overwrite = T,
              node = "Weather", value = paste0(getwd(),"/met/loc_",trial_tmp$id_loc,".met"), verbose = F)
  edit_apsimx(filename, src.dir = source_dir,  wrt.dir = write_dir, overwrite = T,
              node = "Manager", manager.child = "Sow on a fixed date",
              parm = "SowDate", value = paste0(trial_tmp$Day,"-",trial_tmp$Month), verbose = F)
  edit_apsimx(filename, src.dir = source_dir, wrt.dir = write_dir, overwrite = T, node = "Crop", parm = "SowDate", 
              value = paste0(trial_tmp$Day,"-",trial_tmp$Month), verbose = F)
  edit_apsimx(filename, src.dir = source_dir,  wrt.dir = write_dir, overwrite = T,
              node = "Crop", parm = "CultivarName", value = trial_tmp$Mat, verbose = F)
  tryCatch({
    edit_apsimx_replace_soil_profile(file = filename, src.dir = source_dir, wrt.dir = write_dir, overwrite = T,
                                     soil.profile = soil_profile_list[[as.character(trial_tmp$id_loc)]], verbose = F)
  }, error = function(e){})
  invisible()
})


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

# Combine all the individual trial results
combined_output <- do.call(rbind, results)

# Merge Outputs
outfiles <- list.files("apsim/", pattern = "_out", recursive = T)
daily_output <- data.table::rbindlist(lapply(outfiles, function(x){read_csv(paste0("apsim/",x),show_col_types = FALSE)}))
daily_output <- select(daily_output, -CheckpointID,-SimulationID,-Zone,-Year)

#keep before planting, substitute after harvest 
#report field capacity, soil temperature before harvest
#validate models using developmental checkpoints (day of start of period)

# Soybean Periods
if (crop == "Soy"){
  daily_output <- daily_output %>%
    mutate(Period = case_when(
      Stage == 1 & DOY < 180 ~ "0", #beginning to sowing  
      Stage == 1 & DOY >= 180 ~ "6", #harvest to end
      Stage >= 1 & Stage < 4 ~ "1", #sowing to flowering  
      Stage >= 4 & Stage < 6 ~ "2", #flowering to start grain fill   
      Stage >= 6 & Stage < 7.5 ~ "3", #start grain fill to mid grain fill   
      Stage >= 7.5 & Stage <= 9 ~ "4", #mid grain fill to end grain fill  
      Stage > 9 & Stage <= 11 ~ "5", #end grain fill to harvest
    )) 
}

# Maize Periods
if (crop == "Maize"){
  daily_output <- daily_output %>%
    mutate(Period = case_when(
      Stage == 1 & DOY <= 180 ~ "0", #beginning to sowing   
      Stage == 1 & DOY > 180 ~ "6", #harvest to end
      Stage >= 1 & Stage < 4 ~ "1", #sowing to endjuvenile
      Stage >= 4 & Stage < 6 ~ "2", #endjuvenile to floral initiation
      Stage >= 6 & Stage < 8 ~ "3", #floral initiation to start grain fill 
      Stage >= 8 & Stage <= 9 ~ "4", #start grain fill to end grain fill  
      Stage > 9 & Stage <= 11 ~ "5", #end grain fill to harvest
    )) 
}

# Format Outputs into the Characterization
trials_df <- select(trials_df, -Day, -Month)
yields <- group_by(daily_output, id_trial) %>% summarize(Yield_Sim = max(Yieldkgha))
merge_output <- group_by(daily_output, Period, id_trial) %>% select(-Yieldkgha, -Stage) %>% 
  summarize(across(where(is.numeric) & !c(DOY), function(x){mean(x,na.omit=T)}), Start_DOY = min(DOY)) %>%
  relocate(Period, id_trial, Rain) %>% relocate(Start_DOY, .after = last_col())
wide_output <- pivot_wider(merge_output, names_from = Period, values_from = Rain:Start_DOY)
wide_output <- left_join(yields, wide_output)
charact_x <- left_join(trials_df, wide_output)

unlink("output",recursive = T) ; dir.create("output")
write_csv(charact_x, "output/charact_x.csv")
write_csv(daily_output, "output/daily_charact_x.csv")


#calculate time duration for running the code:
end_time <- Sys.time()
duration <- end_time - start_time
print( duration)