library(apsimx)
library(tidyverse)
library(daymetr)
library(data.table)

#maize is not in (reg parameters against a trial_corn_100 file)
crop <- "Soy"

codes_dir <- "C:/Users/cmg3/Documents/GitHub/APSIMX_SeasonalCharacterization"
setwd("C:/Users/cmg3/Box/Gilbert/apsimx_output")

trials_df <- read_csv("small_charact_dt.csv") 
trials_df <- trials_df %>% distinct() %>% rename(X = Longitude, Y = Latitude)
# Create and add location IDs and trial ids
locs_df <- dplyr::select(trials_df, X, Y) %>% distinct() %>% mutate(id_loc = row_number())
trials_df <- left_join(trials_df, locs_df)
trials_df <- mutate(trials_df, id_trial = row_number())

# Convert dates
trials_df <- trials_df %>% dplyr::mutate(Planting = as.Date(Planting, format = "%m/%d/%Y"),
                                         Day = format(Planting,"%d"),
                                         Month = format(Planting,"%b"),
                                         Year = format(Planting,"%Y")) 

# Get weather, make met files
locyear_df <- trials_df %>% select(X,Y,id_loc,Year) %>% unique() %>% group_by(id_loc) %>%
  mutate(first_year = min(Year), last_year = max(Year)) %>% select(-Year) %>% distinct() %>% arrange(id_loc)
unlink("met",recursive = T) ; dir.create("met")
for (loc in 1:nrow(locyear_df)){
  locyear_tmp <- locyear_df[loc,]
  met_tmp <- get_daymet2_apsim_met(lonlat = c(locyear_tmp$X,locyear_tmp$Y), 
                        years = c(locyear_tmp$first_year,locyear_tmp$last_year), 
                        silent = FALSE)
  na_met_tmp <- tryCatch(napad_apsim_met(met_tmp), error = function(e){na_met_tmp <- met_tmp})
  imp_met_tmp <- tryCatch(impute_apsim_met(na_met_tmp), warning = function(w){imp_met_tmp <- na_met_tmp})
  attr(imp_met_tmp,"site") <- attr(met_tmp, "site") #currently seems to be a bug where impute_apsim_met messes with the formatting of the attributes
  attr(imp_met_tmp,"latitude") <- attr(met_tmp, "latitude") #so this is just correcting for that
  attr(imp_met_tmp,"longitude") <- attr(met_tmp, "longitude")
  write_apsim_met(imp_met_tmp, wrt.dir = "met", paste0("loc_",loc,".met"))
  print(loc/nrow(locyear_df))
}

# Get soil, make soil file
soil_profile_list = list()
unlink("soils",recursive = T) ; dir.create("soils")
for (loc in 1:nrow(locs_df)){
  locs_tmp <- locs_df[loc,]
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
  
  write_rds(soil_profile_tmp, file = paste0("soils/soil_profile_",loc))
  soil_profile_list <- append(soil_profile_list, list(soil_profile_tmp))
  print(paste0("loc: ",loc,"   ",round(loc/nrow(locs_df),4)))
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

# Create APSIM files
unlink("apsim",recursive = T) ; dir.create("apsim")
file.copy(from = paste0(codes_dir,"/template_models/",crop,"_Template.apsimx"), to = paste0(crop,"_.apsimx"), overwrite = T)

for (trial_n in 1:nrow(trials_df)){ 
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
  edit_apsimx_replace_soil_profile(file = filename, src.dir = source_dir, wrt.dir = write_dir, overwrite = T,
                                  soil.profile = soil_profile_list[[trial_tmp$id_loc]][[1]], verbose = F)
  print(trial_n / nrow(trials_df))
}

# Run APSIM files, create outputs
output <- data.frame()
for (trial_n in 1:nrow(trials_df)){ #
  source_dir <- paste0("apsim/trial_",trial_n)
  filename <- paste0(crop, "_", trial_n,".apsimx")
  output_tmp <- apsimx(filename, src.dir = source_dir)
  output_tmp <- mutate(output_tmp, "id_trial" = trial_n) 
  output <- rbind(output, output_tmp)
  write_csv(output_tmp, file = paste0(source_dir,"/",crop,"_",trial_n,"_out.csv"))
  print(trial_n / nrow(trials_df))
}

# Merge Outputs
outfiles <- list.files("apsim/", pattern = "_out", recursive = T)
daily_output <- data.table::rbindlist(lapply(outfiles, function(x){read_csv(paste0("apsim/",x))}))
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
