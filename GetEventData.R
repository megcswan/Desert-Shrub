#desert shrub get data from database
library(DBI)
library(tidyverse)
#setwd for data files
setwd("C:\\Users\\mswan\\OneDrive - DOI\\Documents\\Documents\\R\\Projects\\SCPN_Upland\\Desert-Shrub")
#connect to database
conUpE <- DBI::dbConnect(odbc::odbc(),
                           Driver   = "ODBC Driver 17 for SQL Server",
                           Server   = "INPSCPNMS01\\CLPSandbox",
                           Database = "SCPN_UplandEvent",
                           Trusted_Connection = "yes",
                           Port     = 1433
)


#view all tables
dbListTables(conUpE)
#cover class table required if using midpoint
coverclass<-dbReadTable(conUpE, "tlu_CoverClass", as.is=FALSE, stringsAsFactors = FALSE)
#get base level data
#change query park when necessary
event <- dbGetQuery(conUpE, "SELECT * FROM view_Event_Sampled WHERE Park = 'GLCA'")
write.csv(event, "view_Event_Sampled_All_2007-2024.csv", row.names = FALSE)

fxn.data <- dbGetQuery(conUpE, "SELECT * FROM view_FunctionalGroups_All WHERE Park = 'GLCA'", as.is=FALSE, stringsAsFactors = FALSE)
write.csv(fxn.data, "view_FunctionalGroups_All_2007-2024.csv", row.names = FALSE)

sur.fea<-dbGetQuery(conUpE, "SELECT * FROM view_SurfaceFeatures_All WHERE Park = 'GLCA'", as.is=FALSE, stringsAsFactors = FALSE)
write.csv(sur.fea, "view_SurfaceFeatures_All_2007-2024.csv", row.names = FALSE)

soil.stab<-dbGetQuery(conUpE, "SELECT * FROM view_SoilStability_All WHERE Park = 'GLCA'", as.is=FALSE, stringsAsFactors = FALSE)
write.csv(soil.stab, "view_SoilStability_All_2007-2024.csv", row.names = FALSE)

species.base <-dbGetQuery(conUpE, "SELECT * FROM vtbl_NestedSpecies_All WHERE Park = 'GLCA'", as.is=FALSE, stringsAsFactors = FALSE)
write.csv(species.base, "view_Species_All_2007-2024.csv", row.names = FALSE)
dbDisconnect(conUpE)
#close(conUpE)

#test
dalea<-species.base%>%
  filter(grepl('Dalea',CurrentSpecies))%>%
  filter(SpeciesPresentInQuadratForNested == 1)%>%
  group_by(EcoSite, CurrentSpecies, EventYear)%>%
  summarize(Count=n())%>%
  pivot_wider(names_from = EventYear, values_from = Count)

