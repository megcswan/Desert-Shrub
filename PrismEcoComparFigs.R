#prism eco comparison
#load packages
library(tidyverse)
library(ggplot2) 
library(ggpubr)
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

ecosite_colors <- c(GLCA_B = "#1C313E", GLCA_P = "#C19D32", GLCA_H = "#D27661")

#labels for sites
ecosite.labs<-c("Bullfrog", "Page", "Escalante")

names(ecosite.labs) <- c("GLCA_B", "GLCA_P", "GLCA_H")




#get data
prism_norms<-read.csv("Data/Climate/MonthlyNorms_Prism.csv")

glca_prism_norms<-prism_norms%>%
  filter(grepl("GLCA", Name))%>%
  separate(Name, into = c('Park', 'Eco', 'PlotNo'), c(4, 5))%>%
  unite("EcoSite",Park, Eco,  remove = F)%>%
  unite("Plot", Eco, PlotNo, sep="")
#how does annual precip vary?
annualnormsprecip<-glca_prism_norms%>%
  filter(Mo%in% "Annual")

ggplot(data=glca_prism_norms)+
  geom_boxplot(aes(y=ppt_mm, x=EcoSite, fill=EcoSite))+
  scale_fill_manual(values=ecosite_colors)+
  facet_wrap(~Mo, scales="free")

ggplot(data=glca_prism_norms)+
  geom_boxplot(aes(y=tminC, x=EcoSite, fill=EcoSite))+
  scale_fill_manual(values=ecosite_colors)+
  facet_wrap(~Mo, scales="free")

ggplot(data=glca_prism_norms)+
  geom_boxplot(aes(y=TmaxC, x=EcoSite, fill=EcoSite))+
  scale_fill_manual(values=ecosite_colors)+
  facet_wrap(~Mo, scales="free")

ggplot(data=glca_prism_norms)+
  geom_boxplot(aes(y=vpdmax, x=EcoSite, fill=EcoSite))+
  scale_fill_manual(values=ecosite_colors)+
  facet_wrap(~Mo, scales="free")
