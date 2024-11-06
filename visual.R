library(pheatmap)
library(janitor)
library(RColorBrewer)
library(tidyverse)
library(esquisse)
library(lubridate)
library(apsimx)
#fernando miguez for information on why APSIM soybean model may not be progressing correctly

setwd("/srv/shiny-server/shiny-app/apsimx_output")
trials_x <- read_csv("output/trials_x.csv")
charact_x <- read_csv("output/charact_x.csv")
daily_charact_x <- read_csv("output/daily_charact_x.csv")

var <- "Rain"
gen <- 1
site_tag <- "watertown_sd"

varchoice <- charact_x %>% ungroup() %>% select(where(is.numeric) & !c(id_trial, Period)) %>% names()
j_dt <- filter(trials_x, Genetics == gen) %>% select(id_trial,Genetics, Site) %>% left_join(charact_x)

for(var in varchoice){
  var_mat <- j_dt %>% select(id_trial, Site, Period, starts_with(var)) %>%
    pivot_wider(names_from = Period, values_from = var) %>% select(-id_trial) %>%
    group_by(Site) %>% summarize(across(where(is.numeric), function(x){mean(x,na.rm=T)})) %>%
    column_to_rownames("Site") %>%
    remove_empty(which = "rows") %>%
    as.matrix()
}


#get thermal time and precip for the last ten years of records
current_year <- as.numeric(substr(Sys.time(),1,4)) - 1
bigmet <- data.frame()
for(s in 1:max(trials_x$id_loc)){
  lil_met <- read_apsim_met(paste0("met/loc_",s,".met"), verbose = F) %>% as_tibble() %>%
    filter(year >= current_year - 9, year <= current_year) %>% mutate(id_loc = s)
  bigmet <- rbind(bigmet, lil_met)
}
bigmet <- trials_x %>% select(Site, id_loc) %>% distinct() %>% left_join(bigmet) %>% group_by(Site, id_loc, year, day)
max_temp = 34 #thermal time max temp
base_temp = 0 #thermal time base temp
bigmet <- mutate(bigmet, tt = max((min(maxt,max_temp)+max(mint,base_temp))/2 - base_temp,0)) %>% ungroup() 

#start and end of simulation as doy, going over 365 if wrapping over the new year
startend <- select(daily_charact_x, id_trial, DOY, Stage) %>% filter(Stage != 1) %>% 
  group_by(id_trial) %>% filter(Stage == max(Stage) | Stage == min(Stage)) %>%
  summarize(first_doy = DOY[1], final_doy = DOY[2]) %>% 
  mutate(final_doy = ifelse(final_doy < first_doy, final_doy + 365, final_doy)) %>%
  left_join(select(trials_x, Site, Year,id_trial,Genetics)) %>% ungroup()
#mean start doy and end doy for each site
mean_startend <- group_by(startend, Site) %>% 
  summarize(first_doy = mean(first_doy, na.rm = T), final_doy = mean(final_doy, na.rm = T)) %>%
  mutate(final_doy = ifelse(final_doy > 365, final_doy - 365, final_doy))
#season limited to average start and end of simulations
filtmet <- bigmet %>% left_join(mean_startend) %>% filter(day >= first_doy & day <= final_doy)
#filtmet <- filter(filtmet, Site %in% c("ames_ia","urbana_il","lubbock_tx","colombia_mo","centerville_sd"))

#accumulation of thermal time / precip for an average season at each site
#doy of sowing/harvest set on average dates based on trials that were input
dbtw_sites <- filtmet %>% group_by(Site, year) %>% 
  mutate(acc_precip = cumsum(rain), acc_tt = cumsum(tt)) %>%
  ungroup() %>% group_by(Site, day) %>% 
  
#days after sowing
sdbtw_sites <- dbtw_sites %>% mutate(day = day-min(day)+1)


#cross charts comparing accumulated precip/thermal time

wthn_sites <- filtmet %>% ungroup() %>% group_by(Site, year) %>% 
  summarize(acc_precip = sum(rain), acc_tt = sum(tt)) 

#comparing conditions over the last ten years at the same site	

#comparing conditions over the last ten years, faceted for several sites

#summarizing conditions over the last ten years, for several sites



#accumulated precipitation and thermal time from time of sowing to time of harvest 
#(or end of development for unharvested trials) for each trial/genetics/site
# trial_comp <- select(daily_charact_x, Stage, id_trial, Rain, ThermalTime) %>% filter(Stage != 1) %>% 
#   group_by(id_trial) %>% summarize(acc_precip = sum(Rain), acc_tt = sum(ThermalTime)) %>% 
#   left_join(.,select(trials_x, Site, Genetics, id_trial, Year))
