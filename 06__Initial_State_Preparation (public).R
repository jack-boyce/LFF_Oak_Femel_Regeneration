
## Author: Jack Boyce

## preparing initial state files for Sachsenforst plots (2023)
##
## source code for following initial states included:
##
##  Sachsenforst:
##   0.3 ha Oak femel
##   0.4 ha Oak femel
##   0.5 ha Oak femel
##
##  Paußnitz Überflutung:
##   0.1 ha Oak femel

### Packages
library(tidyverse)
library(openxlsx)

### Loading in data

Sachsenforst_Data <- read.xlsx("[insert file path]", sheet = 2) %>% 
  mutate(DBH = DBH_mm/10) %>% select(-c(DBH_mm, Shaded, TreeID, StemID, Species)) %>% rename(Species = Species_Adjusted) %>% relocate(DBH, .after = Species)


### Sachsenforst All femels (Plots 15, 16, 17, 18) [0.24 ha measured]

Sachsenforst_All_Oak_Plots <- Sachsenforst_Data %>% filter(PlotID == 15 | PlotID == 16 | PlotID == 17 | PlotID == 18) %>% filter(Species == "SEI") %>% select(-Comments)

Sachsenforst_All_Oak_Plots$Species %>% unique() #1 species
Sachsenforst_All_Oak_Plots$DBH %>% max() #32 largest DBH
Sachsenforst_All_Oak_Plots$DBH %>% min() #6 smallest DBH

dbh.classes.All_Oak = 6:32 # smallest and largest DBH
midpoints.All_Oak = dbh.classes.All_Oak[-length(dbh.classes.All_Oak)]+(diff(dbh.classes.All_Oak)/2)

dbhsplit1ha.All_Oak = cut(Sachsenforst_All_Oak_Plots$DBH, breaks = dbh.classes.All_Oak, right = F) # btest would be the dataframe that has the information on the individuals


abun1ha.All_Oak = tapply(Sachsenforst_All_Oak_Plots$DBH, list(Sachsenforst_All_Oak_Plots$Species,dbhsplit1ha.All_Oak), length)/0.24 # this should be the area (in ha) that was measured
abun1ha.All_Oak = replace(abun1ha.All_Oak,is.na(abun1ha.All_Oak),0)

filled = (1*26) - sum(abun1ha.All_Oak == 0) # 1: number of species, 26: number of diameter classes
init = data.frame(1:filled,1:filled,1:filled,1:filled)

count = 1
for(sp in 1:1) # species
  for(d in 1:26)  # diameter classes
  {
    if(abun1ha.All_Oak[sp,d] > 0) 
    {
      init[count,1:2] = c(as.numeric(midpoints.All_Oak[d]), round(as.numeric(abun1ha.All_Oak[sp,d]), digits=4))
      init[count,3] = 1
      init[count,4] = rownames(abun1ha.All_Oak)[sp]
      count = count+1  
    }
  }

write.table(init,file="Sachsenforst_All_Oak_femels_initial_state.txt", sep="\t", row.names=F, col.names=F)


### Sachsenforst 0.3 ha Oak femel (Plot 18) [0.06 ha measured]

Plot_18 <- Sachsenforst_Data %>% filter(PlotID == 18) %>% filter(Species == "SEI")

Plot_18$Species %>% unique() #1 species
Plot_18$DBH %>% max() #28 largest DBH
Plot_18$DBH %>% min() #7 smallest DBH

dbh.classes.03_Oak = 7:28 # smallest and largest DBH
midpoints.03_Oak = dbh.classes.03_Oak[-length(dbh.classes.03_Oak)]+(diff(dbh.classes.03_Oak)/2)

dbhsplit1ha.03_Oak = cut(Plot_18$DBH, breaks = dbh.classes.03_Oak, right = F) # btest would be the dataframe that has the information on the individuals


abun1ha.03_Oak = tapply(Plot_18$DBH, list(Plot_18$Species,dbhsplit1ha.03_Oak), length)/0.06 # this should be the area (in ha) that was measured
abun1ha.03_Oak = replace(abun1ha.03_Oak,is.na(abun1ha.03_Oak),0)

filled = (1*21) - sum(abun1ha.03_Oak == 0) # 1: number of species, 21: number of diamater classes
init = data.frame(1:filled,1:filled,1:filled,1:filled)

count = 1
for(sp in 1:1) # species
  for(d in 1:21)  # diameter classes
  {
    if(abun1ha.03_Oak[sp,d] > 0)
    {
      init[count,1:2] = c(as.numeric(midpoints.03_Oak[d]), round(as.numeric(abun1ha.03_Oak[sp,d]), digits=4))
      init[count,3] = 1
      init[count,4] = rownames(abun1ha.03_Oak)[sp]
      count = count+1
    }
  }

write.table(init,file="Sachsenforst_.03_Oak_femel_initial_state.txt", sep="\t", row.names=F, col.names=F)


### Sachsenforst 0.4 ha Oak femel (Plot 15) [0.09 ha measured]

Plot_15 <- Sachsenforst_Data %>% filter(PlotID == 15) %>% filter(Species == "SEI")

Plot_15$Species %>% unique() #1 species
Plot_15$DBH %>% max() #32 largest DBH
Plot_15$DBH %>% min() #6 smallest DBH

dbh.classes.04_Oak = 6:32 # smallest and largest DBH
midpoints.04_Oak = dbh.classes.04_Oak[-length(dbh.classes.04_Oak)]+(diff(dbh.classes.04_Oak)/2)

dbhsplit1ha.04_Oak = cut(Plot_15$DBH, breaks = dbh.classes.04_Oak, right = F) # btest would be the dataframe that has the information on the individuals


abun1ha.04_Oak = tapply(Plot_15$DBH, list(Plot_15$Species,dbhsplit1ha.04_Oak), length)/0.09 # this should be the area (in ha) that was measured
abun1ha.04_Oak = replace(abun1ha.04_Oak,is.na(abun1ha.04_Oak),0)

filled = (1*26) - sum(abun1ha.04_Oak == 0) # 1: number of species, 26: number of diamater classes
init = data.frame(1:filled,1:filled,1:filled,1:filled)

count = 1
for(sp in 1:1) # species
  for(d in 1:26)  # diameter classes
  {
    if(abun1ha.04_Oak[sp,d] > 0)
    {
      init[count,1:2] = c(as.numeric(midpoints.04_Oak[d]), round(as.numeric(abun1ha.04_Oak[sp,d]), digits=4))
      init[count,3] = 1
      init[count,4] = rownames(abun1ha.04_Oak)[sp]
      count = count+1
    }
  }

write.table(init,file="Sachsenforst_.04_Oak_femel_initial_state.txt", sep="\t", row.names=F, col.names=F)


### Sachsenforst 0.5 ha Oak femel (Plot 16) [0.0675 ha measured] Q3 excluded

Plot_16 <- Sachsenforst_Data %>% filter(PlotID == 16) %>% filter(Species == "SEI")

Plot_16$Species %>% unique() #1 species
Plot_16$DBH %>% max() #28 largest DBH
Plot_16$DBH %>% min() #8 smallest DBH

dbh.classes.05_Oak = 8:28 # smallest and largest DBH
midpoints.05_Oak = dbh.classes.05_Oak[-length(dbh.classes.05_Oak)]+(diff(dbh.classes.05_Oak)/2)

dbhsplit1ha.05_Oak = cut(Plot_16$DBH, breaks = dbh.classes.05_Oak, right = F) # btest would be the dataframe that has the information on the individuals


abun1ha.05_Oak = tapply(Plot_16$DBH, list(Plot_16$Species,dbhsplit1ha.05_Oak), length)/0.0675 # this should be the area (in ha) that was measured
abun1ha.05_Oak = replace(abun1ha.05_Oak,is.na(abun1ha.05_Oak),0)

filled = (1*20) - sum(abun1ha.05_Oak == 0) # 1: number of species, 20: number of diamater classes
init = data.frame(1:filled,1:filled,1:filled,1:filled)

count = 1
for(sp in 1:1) # species
  for(d in 1:20)  # diameter classes
  {
    if(abun1ha.05_Oak[sp,d] > 0)
    {
      init[count,1:2] = c(as.numeric(midpoints.05_Oak[d]), round(as.numeric(abun1ha.05_Oak[sp,d]), digits=4))
      init[count,3] = 1
      init[count,4] = rownames(abun1ha.05_Oak)[sp]
      count = count+1
    }
  }

write.table(init,file="Sachsenforst_.05_Oak_femel_initial_state.txt", sep="\t", row.names=F, col.names=F)


### Sachsenforst 0.6 ha Oak femel (Plot 17) [0.0225 ha measured] only Q1 included

Plot_17 <- Sachsenforst_Data %>% filter(PlotID == 17) %>% filter(Species == "SEI")

Plot_17$Species %>% unique() #1 species
Plot_17$DBH %>% max() #22 largest DBH
Plot_17$DBH %>% min() #8 smallest DBH

dbh.classes.06_Oak = 8:22 # smallest and largest DBH
midpoints.06_Oak = dbh.classes.06_Oak[-length(dbh.classes.06_Oak)]+(diff(dbh.classes.06_Oak)/2)

dbhsplit1ha.06_Oak = cut(Plot_17$DBH, breaks = dbh.classes.06_Oak, right = F) # btest would be the dataframe that has the information on the individuals


abun1ha.06_Oak = tapply(Plot_17$DBH, list(Plot_17$Species,dbhsplit1ha.06_Oak), length)/0.0225 # this should be the area (in ha) that was measured
abun1ha.06_Oak = replace(abun1ha.06_Oak,is.na(abun1ha.06_Oak),0)

filled = (1*14) - sum(abun1ha.06_Oak == 0) # 1: number of species, 20: number of diamater classes
init = data.frame(1:filled,1:filled,1:filled,1:filled)

count = 1
for(sp in 1:1) # species
  for(d in 1:14)  # diameter classes
  {
    if(abun1ha.06_Oak[sp,d] > 0)
    {
      init[count,1:2] = c(as.numeric(midpoints.06_Oak[d]), round(as.numeric(abun1ha.06_Oak[sp,d]), digits=4))
      init[count,3] = 1
      init[count,4] = rownames(abun1ha.06_Oak)[sp]
      count = count+1
    }
  }

write.table(init,file="Sachsenforst_.06_Oak_femel_initial_state.txt", sep="\t", row.names=F, col.names=F)


### Paußnitz Überflutung [0.04 ha measured]

Paußnitz <- Sachsenforst_Data %>% filter(PlotID == "Paußnitz") %>% filter(Species == "SEI")

Paußnitz$Species %>% unique() #1 species
Paußnitz$DBH %>% max() #15 largest DBH
Paußnitz$DBH %>% min() #5 smallest DBH

dbh.classes.Paußnitz = 5:15 # smallest and largest DBH
midpoints.Paußnitz = dbh.classes.Paußnitz[-length(dbh.classes.Paußnitz)]+(diff(dbh.classes.Paußnitz)/2)

dbhsplit1ha.Paußnitz = cut(Paußnitz$DBH, breaks = dbh.classes.Paußnitz, right = F) # btest would be the dataframe that has the information on the individuals


abun1ha.Paußnitz = tapply(Paußnitz$DBH, list(Paußnitz$Species,dbhsplit1ha.Paußnitz), length)/0.04 # this should be the area (in ha) that was measured
abun1ha.Paußnitz = replace(abun1ha.Paußnitz,is.na(abun1ha.Paußnitz),0)

filled = (1*10) - sum(abun1ha.Paußnitz == 0) # 1: number of species, 20: number of diamater classes
init = data.frame(1:filled,1:filled,1:filled,1:filled)

count = 1
for(sp in 1:1) # species
  for(d in 1:10)  # diameter classes
  {
    if(abun1ha.Paußnitz[sp,d] > 0)
    {
      init[count,1:2] = c(as.numeric(midpoints.Paußnitz[d]), round(as.numeric(abun1ha.Paußnitz[sp,d]), digits=4))
      init[count,3] = 1
      init[count,4] = rownames(abun1ha.Paußnitz)[sp]
      count = count+1
    }
  }

write.table(init,file="Paußnitz_femel_initial_state.txt", sep="\t", row.names=F, col.names=F)