#Sampling Visualization Figures
#Create dot plots of plots by sampling date to show current and past sampling events.
#First load libraries and get data.

library(RODBC)
library(tidyverse)
library(ggplot2)
library(lubridate)
#get labels
ecosite.labs<-c("GLCA P", "GLCA H", "GLCA B")
names(ecosite.labs) <- c("GLCA_P", "GLCA_H", "GLCA_B")

#Sampling dotplot
#Connect to sql and pull relevant data from our database using diversity view
event <- read.csv("Data//view_Event_Sampled_All_2007-2024.csv")

#remove decommisioned plots
eventplot<-event%>%
  filter(EventPanel %in% c("A", "B", "C"))

#Thin data frame and complete it for status figure
sampled <- eventplot%>%
  select(EventYear, Plot, EventPanel)

sampled_complete<-sampled%>%
  complete(Plot, EventYear)%>%
  replace_na(list(EventDate = 0, EventPanel = "X"))

sampled_forfig<-sampled_complete%>%
  mutate(SampleStatus = if_else(EventPanel != 'X', 1, 0))%>%
  separate(Plot, c("EcoSite", "PlotNumber"), sep = -2, remove = FALSE)
#Create status figure

sampling_event_plot <- ggplot(sampled_forfig,  aes(x= factor(EventYear), y=factor(PlotNumber), shape=factor(SampleStatus),                                          color = factor(SampleStatus))) +
  scale_shape_manual(values = c(13, 16))+
  scale_color_manual(values=c("gray65", "black"))+
  facet_wrap(~EcoSite, labeller = labeller(EcoSite = ecosite.labs), scales = "free_y")+
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_line(color = "black", size = 0.2), 
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.background = element_rect(color="black", fill="white", linewidth=1, linetype="solid"),
        panel.background = element_blank(),
        axis.text.y  = element_text(size=11), 
        axis.text.x = element_text(size = 11, angle = 90, vjust = 0.5))  +  
  geom_point(alpha=1, size=3) + 
  ylab("Plot number") + xlab("Sampling Year")


sampling_event_plot

ggsave("Figures\\SummaryFigs\\SamplingHistory.png", width = 8, height = 10.5, units = "in")





#lollipop view of sampling intervals
sampled_date <- eventplot%>%
  select(EventYear, EcoSite, Plot, EventDate)%>%
  separate(Plot, into = c("Park", "Plot"), sep = "_")

#convert date to julian date
sampled_date$EventDate <- as.Date(sampled_date$EventDate)
sampled_date$DOY <- format(sampled_date$EventDate, "%j")
#dates$EventDate<-as.Date(ymd(dates$EventDate))
#Need to remove the visits post fire as not including this data
#temp removing plot we sampled once in Nov.
doy.fig<-sampled_date%>%
  group_by(Park, EcoSite, EventYear)%>%
  #separate(EventDate, c(NA, "Month"), 5, remove = FALSE)%>%
  summarize(start = min(DOY), finish = max(DOY))%>%
  ungroup()%>%
  filter(!finish>300)

library(scales)
str(doy.fig)

doy.fig$start<-as.numeric(doy.fig$start)
doy.fig$finish<-as.numeric(doy.fig$finish)

ggplot(doy.fig)+
  geom_segment( aes(x=start, xend=finish, y=EventYear, yend=EventYear), color="gray20", size = 3)+
  #geom_point( aes(x=startdate, y=EventYear), color="black", shape = 1, size=3 ) +
  #geom_point( aes(x=finishdate, y=EventYear), color="black", shape = 1, size=3 ) 
  labs(y = "Year", x = "DOY sampled")+
  scale_y_reverse( lim=c(2025,2007), breaks = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021, 2023), labels = c("2007", "2009", "2011", "2013", "2015", "2017", "2019","2021", "2023"))+
  scale_x_continuous(lim = c(0, 365), expand = c(0,0))+
  facet_wrap(~EcoSite, labeller = labeller(EcoSite = ecosite.labs), ncol = 1)+
  theme(legend.position = "none", 
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.background = element_rect(color="black", fill="white", size=1, linetype="solid"),
        panel.background = element_rect(color = "black", fill = "white"),
        panel.grid = element_line(color = "gray80", size = 0.3),
        axis.text.y  = element_text(size=11), 
        axis.text.x = element_text(size = 11, vjust = 0.5))


ggsave("Figures\\SummaryFigs\\DatesofSample.png", width = 8, height = 5.5, units = "in")


#how close did we start sampling
doy.dates<-doy.fig%>%
  group_by(EcoSite)%>%
  summarise(min = min(start), max=max(start))


write.csv(doy.dates,"Output\\DatesofSampling.csv", row.names = F)


#get sampling data for use in pipeline
sampled_date <- eventplot%>%
  select(EventYear, Plot, EventDate)%>%
  separate(Plot, into = c("Park", "Plot"), sep = "_")
#convert date to julian date
sampled_date$EventDate <- as.Date(sampled_date$EventDate)
sampled_date$DOY <- format(sampled_date$EventDate, "%j")

sampled_doy<-sampled_date%>%
  select(Park, Plot, EventYear, DOY)

write.csv(sampled_doy,"Output\\PlotSamplingDates.csv", row.names = F)
