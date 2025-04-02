#soil water balance variables for trend analysis
library(tidyverse)
library(lubridate)
library(ggpubr)
#bring in files
wb<-read.csv("_Data\\Climate\\GLCA-WaterBalance-Monthly-Historical-GRIDMET-015-results.csv", stringsAsFactors =FALSE)
#from thoma paper
#The monthly water balance variables at the center of each map unit were aggregated to 
#annual values by summing (P, AET, PET, RAIN, SNOW, CWD), or averaging (SM, GDD, TAVG) 
#for each water year Oct 1 - Sep 30.

#park ecosite annual monthly waterbalance variables
formatted_wb<-wb%>%
  separate(ID, c("Park", "Plot"), 4, remove = TRUE)%>%
  unite(EcoSite, c("Park", "Plot"), sep = "_", remove = FALSE)%>%
  separate(EcoSite, c("EcoSite", NA), 6, remove = TRUE)%>%
  separate(Date, c("Year", "Month", NA), sep = "-", remove = TRUE)%>%
  select(-Latitude, -Longitude)%>%
  rename_with(~str_remove(., 'WaterBalance_Monthly_Historical_GRIDMET_015_'))

formatted_wb$Year <- as.integer(formatted_wb$Year)


#check for autocorrelation in these covariates
#get annual values--these aren't water years
annual_wb<-formatted_wb%>%
  group_by(Park, EcoSite, Plot, Year)%>%
  summarize(ann_deficit = sum(Deficit), ann_soil_water = sum(Soilwater), ann_rain = sum(Rain), ann_aet = sum(AET))

#how stongly are they correlated? (Just glca)
correlation<-annual_wb%>%
  filter(Park %in% c("GLCA"))

aetdef<-ggplot(correlation, aes(x = ann_deficit, y = ann_aet))+
  geom_point()+
  facet_wrap(~EcoSite)+
  stat_cor(method = "pearson")
aetdef
#>.8
swdef<-ggplot(correlation, aes(x = ann_soil_water, y = ann_deficit))+
  geom_point()+
  facet_wrap(~EcoSite)+
  stat_cor(method = "pearson")
swdef
#<.8 in all but Page
raindef<-ggplot(correlation, aes(x = ann_rain, y = ann_deficit))+
  geom_point()+
  facet_wrap(~EcoSite)+
  stat_cor(method = "pearson")
raindef

swaet<-ggplot(correlation, aes(x = ann_soil_water, y = ann_aet))+
  geom_point()+
  facet_wrap(~EcoSite)+
  stat_cor(method = "pearson")
swaet

aetrain<-ggplot(correlation, aes(x = ann_rain, y = ann_aet))+
  geom_point()+
  facet_wrap(~EcoSite)+
  stat_cor(method = "pearson")
aetrain

swrain<-ggplot(correlation, aes(x = ann_soil_water, y = ann_rain))+
  geom_point()+
  facet_wrap(~EcoSite)+
  stat_cor(method = "pearson")
swrain

allcorr<-ggarrange(swrain, aetrain, raindef, swdef, swaet, aetdef)
ggsave("Figures/WB_correlations.png", width = 20, height=12, units="in")

#need to change year to an integer
#annual_wb$Year <- as.numeric(annual_wb$Year)
#universally change mo/yr to water year.
water_year<-formatted_wb%>%
  mutate(WaterYear = ifelse(Month %in% c("10", "11", "12"), Year+1, Year))%>%
  select(EcoSite, Plot, WaterYear, Month, Deficit, AET, Soilwater)

deficit<-water_year%>%
  select(EcoSite, Plot, WaterYear, Month, Deficit)%>%
  pivot_wider(names_from = Month, values_from = Deficit)%>%
  rename(Dec = "12",Jan = "01", Feb = "02", Mar = "03", Apr = "04", May = "05", Jun = "06", Jul = "07", Aug = "08", Sep = "09", Oct = "10", Nov = "11")

###lag year summaries
lag_deficit<-deficit%>%
  mutate(LagYear = WaterYear +1)
  
colnames(lag_deficit) <- paste("L", colnames(lag_deficit), sep = "_")

lag_deficit<-lag_deficit%>%
  rename(EcoSite = L_EcoSite, Plot = L_Plot, WaterYear = L_LagYear)%>%
  select(-L_WaterYear)

#join the lagged to the regular deficit data
#this is the monthly same year and lagged deficits
monthly_deficit<-deficit%>%
  left_join(lag_deficit, by = c("EcoSite", "Plot", "WaterYear"))%>%
  filter(WaterYear>2007)

#seasonal aggregates of deficit
#deficit in mm park ecosite aggregated seasonal
deficit_aggregates<-monthly_deficit%>%
  rowwise()%>%
  mutate(p_monsoon_D = sum(c(L_Jul, L_Aug, L_Sep, Oct)))%>%
  mutate(winter_D = sum(c(Nov, Dec, Jan)))%>%
  mutate(p_winter_D = sum(c(L_Nov, L_Dec, L_Jan)))%>%
  mutate(spring_D = sum(c(Feb, Mar, Apr)))%>%
  mutate(p_spring_D = sum(c(L_Feb, L_Mar, L_Apr, L_May)))%>%
  mutate(p_annual_D = sum(c(L_Oct, L_Nov, L_Jan, L_Feb, L_Mar, L_Apr, L_May, L_Jun, L_Jul, L_Aug, L_Sep)))%>%
  select(EcoSite, Plot, WaterYear, p_monsoon_D, p_winter_D, p_spring_D, p_annual_D, winter_D, spring_D)

write.csv(deficit_aggregates, "Output/DeficitAggregates04022025.csv", row.names = F)

##now do the same for any others we want. Most are highly correlated. 
##Left this placeholder with xx

xx<-water_year%>%
  select(EcoSite, Plot, WaterYear, Month, xx)%>%
  pivot_wider(names_from = Month, values_from = xx)%>%
  rename(Dec = "12",Jan = "01", Feb = "02", Mar = "03", Apr = "04", May = "05", Jun = "06", Jul = "07", Aug = "08", Sep = "09", Oct = "10", Nov = "11")

###lag year summaries
lag_xx<-xx%>%
  mutate(LagYear = WaterYear +1)

colnames(lag_xx) <- paste("L", colnames(lag_xx), sep = "_")

lag_xx<-lag_xx%>%
  rename(EcoSite = L_EcoSite, Plot = L_Plot, WaterYear = L_LagYear)%>%
  select(-L_WaterYear)

#join the lagged to the regular xx data
#this is the monthly same year and lagged xxs
monthly_xx<-xx%>%
  left_join(lag_xx, by = c("EcoSite", "Plot", "WaterYear"))%>%
  filter(WaterYear>2007)

#seasonal aggregates of xx
#xx in mm park ecosite aggregated seasonal
xx_aggregates<-monthly_xx%>%
  rowwise()%>%
  mutate(p_monsoon_D = sum(c(L_Jul, L_Aug, L_Sep, Oct)))%>%
  mutate(winter_D = sum(c(Nov, Dec, Jan)))%>%
  mutate(p_winter_D = sum(c(L_Nov, L_Dec, L_Jan)))%>%
  mutate(spring_D = sum(c(Feb, Mar, Apr)))%>%
  mutate(p_spring_D = sum(c(L_Feb, L_Mar, L_Apr, L_May)))%>%
  mutate(p_annual_D = sum(c(L_Oct, L_Nov, L_Jan, L_Feb, L_Mar, L_Apr, L_May, L_Jun, L_Jul, L_Aug, L_Sep)))%>%
  select(EcoSite, Plot, WaterYear, p_monsoon_D, p_winter_D, p_spring_D, p_annual_D, winter_D, spring_D)

write.csv(xx_aggregates, "Output/xxAggregates04022025.csv", row.names = F)



##join to aridity index
#pull in file
AI<-read.csv("C:\\Users\\mswan\\OneDrive - DOI\\Documents\\R\\Projects\\SCPN_Upland\\AridityIndex\\SCPN_UplVeg_plots_AridityIndex_InterpolatedValues.csv", header = T)
#match formatting
AI_format<-AI%>%
  separate(Plot_ID, into=c('Park', 'Eco', 'PlotNo'), c(4, 5))%>%
  unite(col = EcoSite, Park, Eco, sep = "_", remove = F)%>%
  unite(col = Plot, Eco, PlotNo, sep="")%>%
  select(Park, EcoSite, Plot, AI = RASTERVALU)%>%
  filter(Park %in% "GLCA")

write.csv(AI_format, "Output/AI.csv", row.names = F)



#join to monthly_deficit
deficitAI<-deficit_aggregates%>%
  left_join(AI_format, by = c("EcoSite", "Plot"))

write.csv(deficitAI, "C:/m4md/assets/uplands-data/SCPN/GLCA/deficitAI_cov.csv", row.names = FALSE)

write.csv(deficitAI, "C:/m4md/assets/uplands-data/SCPN/GLCA/deficitAI_cov.csv", row.names = FALSE)



#comparison of ppt from radar and water balance
#seasonal aggregates
#aet park ecosite aggregated seasonal
pcp_wide<-formatted_wb%>%
  mutate(Rain = rain/10)%>%
  select(Park, EcoSite, Plot, EventYear = Year, Month, Rain)%>%
  pivot_wider(names_from = Month, values_from = Rain)%>%
  rename(Dec = "12",Jan = "1", Feb = "2", Mar = "3", Apr = "4", May = "5", Jun = "6", Jul = "7", Aug = "8", Sep = "9", Oct = "10", Nov = "11")%>%
  group_by(Park, EcoSite, Plot, EventYear)%>%
  mutate(amj_pcp = sum(c(Apr, May, Jun)), ndjfm_pcp = sum(c(Nov, Dec, Jan, Feb, Mar)), jas_pcp = sum(c(Jul, Aug, Sep)), ann_pcp = sum(ndjfm_pcp, amj_pcp, jas_pcp))%>%
  select(Park, EcoSite, Plot, EventYear, Jul_pcp = Jul, amj_pcp, ndjfm_pcp, jas_pcp, ann_pcp)%>%
  filter(Park %in% c("PEFO", "CHCU", "WUPA") & EventYear > 2006)

covar$EventYear<-as.character(covar$EventYear)

ppt<-covar%>%
  select(Park, EcoSite, Plot, EventYear, jas, ndjfm, amj)

bothprecip<-pcp_wide%>%
  left_join(ppt)

#join to covariate file
allpcp<-covar%>%
  left_join(pcp_wide)

write.csv(allpcp, "C:/m4md/assets/uplands-data/SCPN/raintest.csv")

ggplot(bothprecip)+
  geom_point(aes(y = jas, x = EventYear, group = Plot), color = "turquoise")+
  geom_point(aes(y = jas_pcp, x = EventYear, group = Plot), color = "indianred")+
  facet_wrap(~EcoSite)

summary(allwbupdate)

covfig<-allwbupdate%>%
  group_by(Park, EcoSite, EventYear)%>%
  summarize(mean_amj_D= mean(amj_D), 
            mean_ndjfm_D = mean(ndjfm_D), mean_jas_D = mean(jas_D),
            mean_janjun = mean(janjun), mean_dlagjas = mean(D_lag_jas))
covfiglong<-covfig%>%
  pivot_longer(4:8)


##plot covariates
ggplot(data= covfiglong)+
  geom_point(aes(x = EventYear, y = value, color = name))+
  geom_line(aes(x = EventYear, y = value, color = name))+
  facet_wrap(~EcoSite)
