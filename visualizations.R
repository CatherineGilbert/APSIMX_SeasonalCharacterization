library(pheatmap)
library(janitor)
library(RColorBrewer)
library(tidyverse)
library(esquisse)

var <- "Rain"
gen <- 3

varchoice <- daily_charact_x %>% ungroup() %>% 
  select(where(is.numeric) & !c(DOY,Stage,id_trial,Yieldkgha,Period)) %>% names()

for(var in varchoice){

  var_mat <- filter(charact_x, Genetics == gen) %>% select(Site, starts_with(var)) %>% 
    group_by(Site) %>% summarize(across(where(is.numeric), function(x){mean(x,na.rm=T)})) %>% column_to_rownames("Site") %>%
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
      cluster_rows = F,
      main = paste0("Means of ",var," by Site"))

}

#site, year, sowing, harvest doys for trials 
#for trials that reached harvest
sysh_success <- charact_x %>% filter(!is.na(Start_DOY_7)) %>% mutate(sow_doy = Start_DOY_1, harvest_doy = Start_DOY_7 - 1) %>%
  select(id_trial, Site, Year, Genetics, sow_doy, harvest_doy) %>% mutate(Stage = 11)
#get fake harvest date for failed trials, which will be the last day the plant was developing
sysh_failed <- charact_x %>% filter(is.na(Start_DOY_7)) %>% mutate(sow_doy = Start_DOY_1) %>% select(id_trial, Site, Year, Genetics, sow_doy)
fake_harvest <- daily_charact_x %>% select(DOY, id_trial, Stage) %>% filter(id_trial %in% sysh_failed$id_trial) %>% 
  group_by(id_trial) %>% filter(Stage == max(Stage)) %>% filter(DOY == min(DOY)) %>% #get date of highest stage of development
  rename(harvest_doy = DOY)
sysh <- left_join(sysh_failed, fake_harvest) %>% rbind(sysh_success) %>% arrange(id_trial)

#accumulated precipitation and thermal time from time of sowing to time of harvest (or end of development for unharvested trials)
fullseason <- daily_charact_x %>% select(id_trial, DOY, AccPrecip, AccTT) %>% group_by(id_trial) %>% left_join(sysh) %>% 
  filter(DOY >= sow_doy & DOY <= harvest_doy) %>% summarize(AccPrecip = max(AccPrecip) - min(AccPrecip), AccTT = max(AccTT) - min(AccTT)) %>% 
  left_join(sysh)

#scaled accumulated precip and thermal time, by maturity and site
fullseason_comp <- fullseason %>% group_by(Site) %>% 
  mutate(scale_AccPrecip = as.numeric(scale(AccPrecip)), 
         scale_AccTT = as.numeric(scale(AccTT)))
esquisser(fullseason_comp)


current_year <- as.numeric(substr(Sys.time(),1,4)) - 1
bigmet <- data.frame()
for(s in 1:max(trials_df$id_loc)){
  lil_met <- read_apsim_met(paste0("met/loc_",s,".met"), verbose = F) %>% as_tibble() %>%
    filter(year >= current_year - 9, year <= current_year) %>% mutate(id_loc = s)
  bigmet <- rbind(bigmet, lil_met)
}
bigmet <- trials_df %>% select(Site, id_loc) %>% distinct() %>% left_join(bigmet) %>% group_by(Site, id_loc, year, day)
max_temp = 34
base_temp = 0 
bigmet <- mutate(bigmet, tt = max((min(maxt,max_temp)+max(mint,base_temp))/2 - base_temp,0))
bigmet <- ungroup(bigmet, day)
filtmet <- filter(bigmet, day >= 200, day <= 300) 

#daily between sites
dbtw_sites <- filtmet %>% group_by(Site, day) %>% summarize(acc_precip = mean(rain), acc_tt = mean(tt)) %>%
   mutate(acc_precip = cumsum(acc_precip), acc_tt = cumsum(acc_tt))
ggplot(dbtw_sites) +
  aes(x = day, y = acc_precip, colour = Site) +
  geom_line() +
  scale_color_hue(direction = 1) +
  theme_minimal()


#within sites
wthn_sites <- filtmet %>% summarize(acc_precip = sum(rain), acc_tt = sum(tt))

site_n = "ames_ia"

plot_dt <- filter(wthn_sites, Site == site_n) 
ggplot(plot_dt) +
  aes(x = acc_precip, y = acc_tt) +
  geom_vline(aes(xintercept = mean(acc_precip)), color = "black", linetype = "dashed") + 
  geom_hline(aes(yintercept = mean(acc_tt)), color = "black", linetype = "dashed") +
  geom_label(label = plot_dt$year, size = 3L) +
  labs(x = "Total Precipitation",y = "Total Thermal Time", title = site_n) +
  theme_minimal()

means <- wthn_sites %>%
  group_by(Site) %>%
  summarise(mean_acc_precip = mean(acc_precip),
            mean_acc_tt = mean(acc_tt))
ggplot(wthn_sites) +
  aes(x = acc_precip, y = acc_tt, fill = year) +
  facet_wrap(vars(Site), scales = "free") +
  scale_fill_gradient(low = "#F3F3F3", high = "#8AC2ED") +
  geom_vline(data = means, aes(xintercept = mean_acc_precip), color = "black", linetype = "dashed") + 
  geom_hline(data = means, aes(yintercept = mean_acc_tt), color = "black", linetype = "dashed") +
  geom_label(label = wthn_sites$year, size = 3) +
  theme_minimal() +
  theme(legend.position = "none") 


#between sites
btwn_sites <- wthn_sites %>% summarize(acc_precip = mean(acc_precip), acc_tt = mean(acc_tt))
ggplot(btwn_sites) +
  aes(x = acc_precip, y = acc_tt) +
  geom_vline(aes(xintercept = mean(acc_precip)), color = "black", linetype = "dashed") + 
  geom_hline(aes(yintercept = mean(acc_tt)), color = "black", linetype = "dashed") +
  geom_label(label = btwn_sites$Site, size = 3) +
  theme_minimal() +
  theme(legend.position = "none") 

