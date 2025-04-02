#terrain and elevation
#load packages
library(tidyverse) 
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
terrain_elev<-read.csv("Data/Calculated_Terrain_Elev.csv")
        
glca_terrain_elev<-terrain_elev%>%
          filter(Park %in% "GLCA")
#which sites have highest elev? How much variation is there?  
elev<-ggplot(glca_terrain_elev)+
          geom_boxplot(aes(y=mean_elev, x=EcoSite, fill=EcoSite))+
          geom_point(aes(y=mean_elev, x=EcoSite, fill=EcoSite), alpha = .4, shape=21)+
          scale_fill_manual(values=ecosite_colors)
elev 
        
#how about terrain (Terrain ranges from -1 to 1. Steep south-facing slopes trend toward -1 and steep north-facing slopes trend toward 1.)
terrain<-ggplot(glca_terrain_elev)+
          geom_boxplot(aes(y=Terrain, x=EcoSite, fill=EcoSite))+
        geom_point(aes(y=Terrain, x=EcoSite, fill=EcoSite), alpha = .4, shape=21)+
        scale_fill_manual(values=ecosite_colors)
terrain
#slope
slope<-ggplot(glca_terrain_elev)+
        geom_boxplot(aes(y=mean_slope, x=EcoSite, fill=EcoSite))+
        geom_point(aes(y=mean_slope, x=EcoSite, fill=EcoSite), alpha = .4, shape=21)+
        scale_fill_manual(values=ecosite_colors)
slope
#aspect
aspect<-ggplot(glca_terrain_elev)+
        geom_boxplot(aes(y=mean_aspect, x=EcoSite, fill=EcoSite))+
        geom_point(aes(y=mean_aspect, x=EcoSite, fill=EcoSite), alpha = .4, shape=21)+
        scale_fill_manual(values=ecosite_colors)
aspect
        
ggarrange(slope, aspect, elev, terrain, ncol = 2, nrow = 2, legend = "bottom", common.legend = T)
ggsave("Figures/SummaryFigs/physical_ecosite_summary.png", width = 8, height=8, units = "in")
        