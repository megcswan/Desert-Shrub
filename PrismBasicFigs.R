#Climate status data and figures
### Load the library packages

```{r SetPackagesProtocolEcos, echo = TRUE, results = "hide", eval = TRUE, message=FALSE}
library(tidyverse)
library(lubridate)
library(ggplot2)
library(stringr)
library(ggrepel)
library(ggpubr)
library(viridis)

ecosite_colors <- c(GLCA_B = "#996666", GLCA_P = "#C19D32", GLCA_H = "#99CCCC")

#labels for sites
ecosite.labs<-c("Bullfrog", "Page", "Escalante")
names(ecosite.labs) <- c("GLCA_B", "GLCA_P", "GLCA_H")

#labels for prism data
eco.labs<-c("Bullfrog", "Page", "Escalante")
names(eco.labs) <- c("GLCAB", "GLCAP", "GLCAH")
climate.labs<-c("Spring CWD", "Monsoon CWD", "Winter CWD", "AGDD")
names(climate.labs) <- c("amj_D", "jas_D", "ndjfm_D", "gdd")

#plot themes
#facetted dot plot theme
dotplot.theme = function(){
  theme(panel.background = element_rect(fill = "white", color = "black"),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12, face = "bold"),
        strip.background = element_rect(color="black", fill="white", size=1, linetype="solid"), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour = "gray88"),
        panel.grid.minor.x = element_line(colour = "gray88"),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())  
}
#boxplot theme
boxplot.theme = function(){
  theme(panel.background = element_rect(fill = "white"),
        axis.text.x = element_text (size = 11, face = "bold"),
        axis.text.y = element_text (size = 11, vjust = .04, face = "bold"),
        strip.text.x = element_text(size = 13, face = "bold"),
        strip.background = element_rect(
          color="black", fill="white", size=1, linetype="solid"), 
        panel.grid.major.y = element_line(colour = "gray88"),
        panel.grid.major.x = element_line(colour = "gray77"),
        panel.grid.minor.x = element_line(colour = "gray77"),
        legend.position = "bottom")
}

#Load data
#Updated to data I manually downloaded from Prism.
#monthly norms
prismNormMonL<-read.csv("_Data/Climate/MonthlyNorms_Prism.csv", stringsAsFactors = F)

#annual norms
prismNormAnnL<-prismNormMonL%>%
  filter(Mo%in% "Annual")

#monthly monitoring period
prismMonthL<-read.csv("_Data/Climate/prismMonthL2009-2024.csv", stringsAsFactors = F)

#stageIVprecip no longer available
#pcpStageIV<-read.csv("pcpStageIV.csv", stringsAsFactors = F)

```

### format prism normals (1991-2021)

glca_prism_norms<-prismNormMonL%>%
  filter(grepl("GLCA", Name))%>%
  separate(Name, into = c('Park', 'Eco', 'PlotNo'), c(4, 5))%>%
  unite("EcoSite",Park, Eco,  remove = F)%>%
  unite("Plot", Eco, PlotNo, sep="")%>%
  select(-Latitude, -Longitude)%>%
  pivot_longer(6:11)



normalsAnnMedians <- glca_prism_norms%>%
  filter(!Mo %in% 'Annual')%>%
  group_by(EcoSite, Mo, name) %>%
  summarise(MedVal = median(value, na.rm = TRUE),MaxVal = max(value, na.rm = TRUE),MinVal = min(value, na.rm = TRUE)) %>%
  ungroup()
normalsAnnMedians$Mo<-factor(normalsAnnMedians$Mo, levels = month.name)

#lets look at this
ggplot()+
  geom_line(data = normalsAnnMedians, aes(x=Mo, y = MedVal, color = name, group = name))+
  facet_grid(rows = vars(EcoSite))

### Format the PRISM monthly data 
glca_prism<-prismMonthL%>%
  filter(grepl("GLCA", Name))%>%
  separate(Name, into = c('Park', 'Eco', 'PlotNo'), c(4, 5))%>%
  unite("EcoSite",Park, Eco,  remove = F)%>%
  unite("Plot", Eco, PlotNo, sep="")%>%
  select(-Latitude, -Longitude)%>%
  pivot_longer(6:11)

## Get monthly prism summary values 

#median Prism values across EcoSitesite
glca_prism_monthlyMediansEcoSite <- glca_prism%>%
  group_by(EcoSite, name, Date ) %>%
  summarise(MedVal = median(value, na.rm = TRUE),
            MaxVal = max(value, na.rm = TRUE),
            MinVal = min(value, na.rm = TRUE)) %>%
  ungroup()
#lets look at this
ggplot()+
  geom_line(data = glca_prism_monthlyMediansEcoSite, aes(x=Date, y = MedVal, color = name, group = name))+
  facet_grid(rows = vars(EcoSite))
#a couple big precip spikes
#Calculate and format plot-scale derived variables for use as covariates & plotting

#what are plot-scale ppt normals?
norm_ann_precip<-prismNormAnnL%>%
  filter(grepl("GLCA", Name))%>%
  separate(Name, into = c('Park', 'Eco', 'PlotNo'), c(4, 5))%>%
  unite("EcoSite",Park, Eco,  remove = F)%>%
  unite("Plot", Eco, PlotNo, sep="")%>%
  select(-Latitude, -Longitude, -Elevation..m.)%>%
  pivot_longer(5:10)%>%
  filter(name %in% "ppt_mm")%>%
  rename(AnnNorm=value)%>%
  select(Park, EcoSite, Plot, AnnNorm)


#calculate percent of normal ppt (mm) using PRISM normals
percofnormals<-glca_prism%>%
  separate(Date, into=c("Year", "Mo"), sep = "-")%>%
  filter(name %in%'ppt_mm')%>%
  group_by(Park, EcoSite, Plot, Year)%>%
  summarize(Annppt = sum(value))%>%
  ungroup()%>%
  left_join(norm_ann_precip, by = c("Park", "EcoSite", "Plot"))%>%
  mutate(PercofNorm = (Annppt/AnnNorm) *100)%>%
  mutate(Anomaly_mm = (Annppt-AnnNorm))

str(percofnormals)

#summary by ecosite
percnormsummary<-percofnormals%>%
  group_by(EcoSite, Year)%>%
  summarize(medianPercofNorm=median(PercofNorm), medianAnomaly_mm = median(Anomaly_mm))%>%
  mutate(Color = ifelse(medianAnomaly_mm>0, 'pos', 'neg'))

#get running mean
library(zoo)
runningmean<-percnormsummary%>%
  group_by(EcoSite)%>%
  mutate(id = row_number())%>%
  mutate(running_ave=rollmean(medianAnomaly_mm, id, align = 'right', k=3))%>%
  ungroup()

#what does it look like
ggplot(data = percnormsummary)+
  geom_col(data = percnormsummary, aes(x = Year, y=medianAnomaly_mm, fill = EcoSite),position = 'dodge2', color = "black")+
  geom_line(data = runningmean, aes(x = Year, y = running_ave, group = EcoSite, color=EcoSite))+
  scale_fill_manual(values = ecosite_colors)+
  scale_color_manual(values = ecosite_colors)+
  scale_y_continuous(limits = c(-150, 150))+
  boxplot.theme()

ggsave("Figures\\ppt_3yr_running_ave.jpg", width = 8, height = 4, units = "in")

#Climate figures for desert shrub report
#Figure 4 bimodal precipitation pattern (prism normals)

glca_prism_norms<-prismNormMonL%>%
  filter(grepl("GLCA", Name))%>%
  separate(Name, into = c('Park', 'Eco', 'PlotNo'), c(4, 5))%>%
  unite("EcoSite",Park, Eco,  remove = F)%>%
  unite("Plot", Eco, PlotNo, sep="")%>%
  select(-Latitude, -Longitude)%>%
  pivot_longer(6:11)

#remove annual totals
glca_prism_norms_monthly<-glca_prism_norms%>%
  filter(!Mo %in% 'Annual')
glca_prism_norms_monthly$Mo<-factor(glca_prism_norms_monthly$Mo, levels = month.name)
#ecosite monthly ppt
fig4<-glca_prism_norms_monthly%>% 
  filter(name %in% "ppt_mm")%>%
  group_by(EcoSite, Mo)%>%
  summarize(medianppt = median(value))
#just checking
str(fig4)
#plot monthly ppt
#fig4b<-glca_prism_norms_monthly%>%
#  filter(name %in% "ppt_mm")
#labels for line fig
fig4c<-fig4%>%
  group_by(EcoSite, Mo)%>%
  summarize(median = median(medianppt))%>%
  filter(Mo %in% "December")
#monthly tmax
fig4d<-glca_prism_norms_monthly%>%
  filter(name %in% "TmeanC")%>%
  group_by(EcoSite, Mo)%>%
  summarize(median = median(value))


ggplot()+
  #  geom_point(data = fig4b, aes(x = Mo, y = value, color = EcoSite, group = EcoSite,shape = EcoSite), alpha = 0.2, size = 4)+
  geom_col(data = fig4, aes(x = Mo, y = medianppt, fill = EcoSite, group = EcoSite), position = 'dodge', color = 'black')+
  geom_line(data = fig4d, aes(x = Mo, y = median, group = EcoSite, color = EcoSite), size = 2)+
  # geom_label_repel(data = fig4c, aes(x= Mo, y = median, label = ecosite.labs, color = EcoSite), nudge_x = 1,nudge_y = .5, label.size=NA, family=("sans"),
  #     size = 4,
  #   direction = "y",
  #   hjust = 0,
  #   segment.size = .7,
  #   segment.alpha = .5,
  #   segment.linetype = "dotted",
  #   box.padding = .2,
  #   segment.curvature = -0.1,
  #   segment.ncp = 3,
  #   segment.angle = 20)+
  scale_y_continuous(sec.axis = sec_axis(trans = ~.*1, name="Degrees (C)")) +
  labs (x = "Month of year", y = "Monthly accumulated precipitation (mm)")+
  scale_fill_manual(values = ecosite_colors, labels = ecosite.labs)+
  scale_color_manual(values = ecosite_colors, labels = ecosite.labs)+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "gray88"),
        panel.grid.minor.y = element_line(colour = "gray88"),
        panel.grid.major.x = element_line(colour = "gray88"),
        panel.grid.minor.x = element_line(colour = "gray88"),
        axis.ticks.x = element_line(color = "gray88"),
        axis.ticks.y = element_line(color = "gray88"),
        legend.position = "none",
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8))

ggsave("Figures\\Fig4_bimodalprecip.jpg", width = 8, height = 4, units = "in")

#Figure 5 ecosite comparison of precip and temperature based on prism normals

norm_ann_precip<-prismNormAnnL%>%
  filter(grepl("GLCA", Name))%>%
  separate(Name, into = c('Park', 'Eco', 'PlotNo'), c(4, 5))%>%
  unite("EcoSite",Park, Eco,  remove = F)%>%
  unite("Plot", Eco, PlotNo, sep="")%>%
  select(-Latitude, -Longitude, -Elevation..m.)%>%
  pivot_longer(5:10)%>%
  filter(name %in% "ppt_mm")%>%
  rename(AnnNorm=value)%>%
  select(Park, EcoSite, Plot, AnnNorm)


fig5a<-norm_ann_precip%>%
  group_by(EcoSite)%>%
  summarize(MedVal=median(AnnNorm))


norm_ann_tmax<-prismNormAnnL%>%
  filter(grepl("GLCA", Name))%>%
  separate(Name, into = c('Park', 'Eco', 'PlotNo'), c(4, 5))%>%
  unite("EcoSite",Park, Eco,  remove = F)%>%
  unite("Plot", Eco, PlotNo, sep="")%>%
  select(-Latitude, -Longitude, -Elevation..m.)%>%
  pivot_longer(5:10)%>%
  filter(name %in% "TmaxC")%>%
  rename(AnnNorm=value)%>%
  select(Park, EcoSite, Plot, AnnNorm)


fig5b<-norm_ann_tmax%>%
  group_by(EcoSite)%>%
  summarize(MedVal=median(AnnNorm))

#make figures
#need to order both figs by desc tmax-Matthew wants them the same--
#removed this line x = reorder(Eco, -MedVal) that may be useful later
ppt<-ggplot()+
  geom_point(data = fig5a, aes(x = EcoSite, y = MedVal), fill = "black",  shape = 21, size = 6)+ 
  geom_point(data = norm_ann_precip, aes(x = EcoSite, y = AnnNorm), color = "black", shape = 3, size = 2, alpha = .7)+
  labs(x = element_blank(), y = "Median annual precipitation (mm)")+
  scale_x_discrete(labels = ecosite.labs)+
  theme(
    panel.background = element_rect(fill = "#99CCCF",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    axis.text.x = element_text(size = 10, family = "sans", face = "bold", angle = 45, vjust = .9, hjust = .9),
    axis.text.y = element_text(size = 10, family = "sans", face = "bold"),
    axis.title = element_text(size = 10),
    legend.position = "none")

ppt

tmax<-ggplot()+
  geom_point(data = fig5b, aes(x = EcoSite, y = MedVal), fill = "black",  shape = 21, size = 6)+ 
  geom_point(data = norm_ann_tmax, aes(x = EcoSite, y = AnnNorm), color = "black", shape = 3, size = 2, alpha = .7)+
  labs(x = element_blank(), y = "Median Tmax (degrees C)")+
  scale_x_discrete(labels = ecosite.labs)+
  theme(
    panel.background = element_rect(fill = "#FFCCCC",
                                    colour = "#D27661",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    axis.text.x = element_text(size = 10, family = "sans", face = "bold", angle = 45, vjust = .9, hjust = .9),
    axis.text.y = element_text(size = 10, family = "sans", face = "bold"),
    axis.title = element_text(size = 10),
    legend.position = "none")
tmax
library(ggpubr)
#arrange together
ggarrange(ppt, tmax, 
          #labels = c("A", "B"),
          ncol = 2, nrow = 1, common.legend = TRUE, legend="bottom")

ggsave("/Figures/desertshrub_t&p.png", width = 9, height = 3, units = "in")
```


#Figure 11 annual precipitation normals
ggplot(percofnormals)+
  geom_tile(aes(y = factor(Year), x = Plot, fill = PercofNorm))+
  scale_fill_viridis()+
  labs(y= "Year", x = "Plot number", fill = "Percent of normal precipitation")+
  scale_x_discrete()+
  scale_y_discrete(limits = rev)+
  facet_wrap(~EcoSite, scales = "free_x", nrow = 1, 
             labeller=labeller(EcoSite = ecosite.labs))+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(face="bold", color="black", 
                                   size=6, angle=90, hjust = .2, vjust = .5),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=8), panel.background = element_rect(fill = "white", ),
        strip.text.x = element_text(size = 12, face = "bold"), strip.background = element_rect(
          color="black", fill="white", size=1, linetype="solid"),
        panel.border=element_rect(colour="black",size=1, fill = NA))


ggsave("Figures/DS_PrecipHeatMap.jpg", width = 10, height = 6, units = "in")




















```{r figure 12 }
#seasonal precip norms vs period of monitoring
seasonaggnorm<-prismNormMonL%>%
  filter(VarName %in% "ppt")%>%
  pivot_wider(id_cols = "SiteID", names_from = "MonthV",  values_from = "Value")%>%
  group_by(SiteID)%>%
  mutate(Spring = sum(apr, may, jun), Summer = sum(jul, aug, sep), Winter = sum(oct, nov, dec, jan, feb, mar), Annual = sum(oct, nov, dec, jan, feb, mar, apr, may, jun, jul, aug, sep))%>%
  ungroup()%>%
  separate(SiteID, into = c ("Eco", NA), 5, remove = FALSE)%>%
  separate(SiteID, into = c (NA, "Plot"), 4, remove = FALSE)%>%
  select(Eco, Spring, Summer, Winter, Annual)%>%
  group_by(Eco)%>%
  pivot_longer(cols = 2:5, names_to = "Season", values_to = "Norm")%>%
  group_by(Eco, Season)%>%
  summarize(MedianNorm = median(Norm))

#medians of annual
seasonaggtime<-seasonal_agg_pcp%>%
  select(Eco, SiteID, WaterYear, jas, ndjfm, amj, janjun, annual)%>%
  group_by(Eco, WaterYear)%>%
  summarise(Summer = median(jas), Winter= median(ndjfm), Spring = median (amj), Annual = median(annual))%>%
  ungroup()%>%
  filter(WaterYear > 2006 & WaterYear < 2021)%>%
  pivot_longer(cols = 3:6, names_to = "Season", values_to = "Median")

seasonal<-seasonaggtime%>%
  left_join(seasonaggnorm)%>%
  mutate(Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Annual")))

seasonal$WaterYear<-as.factor(seasonal$WaterYear)


#facetted bar chart with norms
ggplot(seasonal)+
  geom_bar(aes(x = WaterYear, y  = Median), stat = "identity", fill = "#408279", size = 1.5)+
  #geom_line(aes(x = EventYear, y = EcoTmax, color = Eco, size = 2))+
  geom_hline(aes(yintercept =  MedianNorm), color = "#D27661", size = 1, linetype = 2)+
  scale_x_discrete(breaks=c("2006", "2008", "2010","2012", "2014", "2016", "2018", "2020"),
                   labels=c("2006", "2008", "2010","2012", "2014", "2016", "2018", "2020")) +
  facet_grid(Season~Eco, labeller = labeller(Eco = eco.labs))+
  dotplot.theme()+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 10, face = "bold"),
        axis.text.y = element_text(vjust=0.5, size = 10, face = "bold"),
        axis.title=element_text(size=12,face="bold"), 
        
        axis.line = element_line(colour="black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.background = element_blank(), legend.position = "none")+
  labs(
    x="Year",
    y="Accumulated seasonal precipitation (mm)")

ggsave("C:/Users/mswan/Documents/R/Projects/SCPN_Upland/Grassland/Figures/Final/Figure12_SeasonalAggComparison.jpg", height = 4.1, width = 6.5, units = "in" )
```

```{r figure 13}
#t max and Tmin over time
#figure 13

tmaxplot2<-yearlyMediansPR%>%
  filter(VarName %in% "tmax")%>%
  group_by(Eco, EventYear = YearV)%>%
  summarise(EcoTmax = median(MedVal))%>%
  ungroup()

normaltmax<-tmaxplot2%>%
  left_join(normalsAnnMedianTmax)%>%
  rename(medTmax = MedVal)

#tmin over time
tminplot2<-yearlyMediansPR%>%
  filter(VarName %in% "tmin")%>%
  group_by(Eco, EventYear = YearV)%>%
  summarise(EcoTmin = mean(MedVal))%>%
  ungroup()

Tmin_normal<-prismNormAnnL%>%
  filter(VarName %in% "tmin")%>%
  group_by(Eco = EcoSite)%>%
  summarise(MedVal = median(Value))

normaltmin<-tminplot2%>%
  left_join(Tmin_normal)

fig13<-normaltmax%>%
  full_join(normaltmin)%>%
  rename(medTmin = MedVal)%>%
  filter(EventYear > 2006)

fig13$EventYear<-as.factor(fig13$EventYear)

ggplot(fig13)+
  geom_point(aes(x = EventYear, y = EcoTmax), size = 4, fill = "#D27661", shape = 24, alpha = .8)+
  geom_point(aes(x = EventYear, y = EcoTmin), fill = "#408279", size = 4, shape = 25, alpha = .8)+
  geom_hline(aes(yintercept = medTmin), color = "black", size = 1, linetype = 2)+
  geom_hline(aes(yintercept = medTmax), color = "black", size = 1, linetype = 2)+
  facet_wrap(~Eco, labeller = labeller(Eco = eco.labs), nrow = 1) +
  dotplot.theme()+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 12, face = "bold"), 
        axis.title=element_text(size=14,face="bold"), 
        axis.text.y=element_text(size=12, ),
        axis.line = element_line(colour="black"), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_x_discrete(breaks=c("2008", "2010","2012", "2014", "2016", "2018", "2020"),
                   labels=c("2008", "2010","2012", "2014", "2016", "2018", "2020"))+
  labs(
    #title="Annual max and min temperature 2007-2020",
    x="Year",
    y="Temperature (degrees C)")+
  scale_color_manual(values = ecosite_colors)+
  theme(legend.position = "none")


ggsave("C:/Users/mswan/Documents/R/Projects/SCPN_Upland/Grassland/Figures/Final/Figure13_tempcomparison.jpg", height = 3.5, width = 6.5, units = "in" )

#how many years exceeded long term t max and t min for each ecosite?
tmaxexceeds<-fig12%>%
  group_by(Eco)%>%
  filter(EcoTmax > medTmax)%>%
  summarize(maxExceed = n())

tminexceeds<-fig12%>%
  group_by(Eco)%>%
  filter(EcoTmin > medTmin)%>%
  summarize(minExceed = n())

tempexceedencecounts<-tmaxexceeds%>%
  full_join(tminexceeds)

write.csv(tempexceedencecounts, "Temperatureexceedence_countsbyEcosite.csv")
library(ggpubr)

```
