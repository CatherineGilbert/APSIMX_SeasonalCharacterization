library(apsimx)
library(tidyverse)
library(daymetr)
library(sf)

setwd("C:/Users/cmg3/Documents/GitHub/APSIMX_SeasonalCharacterization")
trials_df <- read_csv("small_charact_dt.csv") %>% distinct() %>% rename(X = Longitude, Y = Latitude)
# Create and add location IDs
locs_df <- dplyr::select(trials_df, X, Y) %>% distinct() %>% mutate(id_loc = row_number())
trials_df <- left_join(trials_df, locs_df)
# Convert dates
trials_df <- trials_df %>% dplyr::mutate(Planting_date = as.Date(Planting, format = "%m/%d/%Y"),
                                         Day = format(Planting_date,"%d"),
                                         Month = tolower(format(Planting_date,"%b")),
                                         Year = format(Planting_date,"%Y")) 

# Get weather, make met files
locyear_df <- trials_df %>% select(X,Y,id_loc,Year) %>% unique() %>% group_by(id_loc) %>%
  mutate(first_year = min(Year), last_year = max(Year)) %>% select(-Year) %>% distinct() %>% arrange(id_loc)
unlink("met",recursive = T) ; dir.create("met")
for (loc in 1:nrow(locyear_df)){
  locyear_tmp <- locyear_df[loc,]
  met_tmp <- get_daymet2_apsim_met(lonlat = c(locyear_tmp$X,locyear_tmp$Y), 
                        years = c(locyear_tmp$first_year,locyear_tmp$last_year), 
                        silent = FALSE)
  na_met_tmp <- napad_apsim_met(met_tmp)
  imp_met_tmp <- impute_apsim_met(na_met_tmp)
  write_apsim_met(imp_met_tmp, wrt.dir = "met", paste0("loc_",loc,".met"))
}

# Get soil, make soil file
#soil_table_list = list()
soil_profile_list = list()
for (loc in 1:nrow(locs_df)){
  locs_tmp <- locs_df[loc,]
  #soil_table_tmp <- get_ssurgo_tables(lonlat = c(locs_tmp$X,locs_tmp$Y))
  #soil_table_list <- append(soil_table_list, list(soil_table_tmp))
  soil_profile_tmp <- get_ssurgo_soil_profile(lonlat = c(locs_tmp$X,locs_tmp$Y))
  write_rds(soil_profile_tmp, file = paste0("soils/soil_profile_",loc))
  soil_profile_list <- append(soil_profile_list, list(soil_profile_tmp))
  print(loc/nrow(locs_df))
}
#write_rds(soil_table_list, "./soil_table_list.rds")
write_rds(soil_profile_list, "soil_profile_list.rds")

#maturity is not in
#sow on date is not in
#maize is not in
#the soil profiles collected don't have a water component
#all files are not running outside of the program itself

unlink("apsim",recursive = T) ; dir.create("apsim")
for (trial_n in 1){ #1:nrow(trials_df)){
  trial_tmp <- trials_df[trial_n,]
  if(!dir.exists(paste0("apsim/trial_",trial_n))) {dir.create(paste0("apsim/trial_",trial_n))}
  source_dir <- paste0("apsim/trial_",trial_n)
  write_dir <-  paste0("apsim/trial_",trial_n)
  filename <- paste0("trial_crct_edit_",trial_n,".apsimx")
  edit_apsimx(file = "trial_crct_edit.apsimx", wrt.dir = write_dir, edit.tag = paste0("_",trial_n),
              node = "Clock", parm = "Start", value = paste0(trial_tmp$Year,"-01-01T00:00:00"))
  edit_apsimx(file = filename,  src.dir = source_dir, wrt.dir = write_dir, overwrite = T,
              node = "Clock", parm = "End", value = paste0(as.character(as.numeric(trial_tmp$Year)+1),"-01-01T00:00:00"))
  #edit_apsimx(file = filename,  src.dir = source_dir, wrt.dir = write_dir, overwrite = T,
  #            node = "Manager", manager.child = "SowingRule", parm = "StartDate", value = paste0(trial_tmp$day,"-",trial_tmp$month))
  edit_apsimx(file = filename, src.dir = source_dir, wrt.dir = write_dir, overwrite = T,
              node = "Weather", value = paste0(getwd(),"/met/loc_",trial_tmp$id_loc,".met"))
  edit_apsimx_replace_soil_profile(file = filename, src.dir = source_dir, wrt.dir = write_dir, overwrite = T,
                                   soil.profile = soil_profile_list[[trial_tmp$id_loc]][[1]])
}

".Simulations.WT.paddock.Manager folder.Tillage on fixed date"

apsimx(file = "Soybean.apsimx")
apsimx(file = "trial_crct_edit_1.apsimx", src.dir = "apsim/trial_1")

inspect_apsimx(file = filename,  src.dir = source_dir)

inspect_apsimx_replacement(file = "trial_crct.apsimx", 
                           root = list("Manager"), node = "Clock", parm)

apsimx("Soybean_1.apsimx", src.dir = "apsim/trial_1")
