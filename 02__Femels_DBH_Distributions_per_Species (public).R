
## Author: Jack Boyce

### Creating tables for each plot with number of individuals per hectare and mean DBH for each tree species and each canopy layer


# Loading packages
library(tidyverse)
library(dplyr)
library(openxlsx)
library(ggplot2)

## Loading in data
i_2023_Data <- read.xlsx("[insert file path to Adjusted_auwald_data_femel_Sachsenforst_2023.xlsx in Femel Inventory Data]") %>% 
  mutate(DBH = DBH_mm/10) %>% select(-c(DBH_mm, Comments)) %>% relocate(DBH, .after = Species)

## Individuals

#Plot 15: size = 900m2
Individuals_Plot_15 <- i_2023_Data %>% filter(PlotID == 15) %>% 
  group_by(PlotID, Species, Shaded) %>% 
  count() %>% mutate(`Plot size` = "900") %>% 
  mutate(`Individuals per m2` = n/900) %>% 
  mutate(`Individuals per hectare` = `Individuals per m2`*10000) %>% ungroup()

#Plot 16: size = 900m2
Individuals_Plot_16 <- i_2023_Data %>% filter(PlotID == 16) %>% 
  group_by(PlotID, Species, Shaded) %>% 
  count() %>% mutate(`Plot size` = "900") %>% 
  mutate(`Individuals per m2` = n/900) %>% 
  mutate(`Individuals per hectare` = `Individuals per m2`*10000) %>% ungroup()

#Plot 17: size = 900m2
Individuals_Plot_17 <- i_2023_Data %>% filter(PlotID == 17) %>% 
  group_by(PlotID, Species, Shaded) %>% 
  count() %>% mutate(`Plot size` = "900") %>% 
  mutate(`Individuals per m2` = n/900) %>% 
  mutate(`Individuals per hectare` = `Individuals per m2`*10000) %>% ungroup()

#Plot 18: size = 600m2
Individuals_Plot_18 <- i_2023_Data %>% filter(PlotID == 18) %>% 
  group_by(PlotID, Species, Shaded) %>% 
  count() %>% mutate(`Plot size` = "600") %>% 
  mutate(`Individuals per m2` = n/600) %>% 
  mutate(`Individuals per hectare` = `Individuals per m2`*10000) %>% ungroup()

#All Individuals
All_individuals <- rbind(Individuals_Plot_15, Individuals_Plot_16, Individuals_Plot_17, Individuals_Plot_18)


## Mean DBH per species and canopy layer

#Plot 15
Overstory_15 <- i_2023_Data %>% select(-c(TreeID, StemID)) %>% 
  filter(PlotID == "15") %>% 
  filter(Shaded == "n") %>%
  group_by(Species) %>% 
  mutate(`Mean DBH` = mean(DBH)) %>% select(-DBH) %>% distinct()

Partial_15 <- i_2023_Data %>% select(-c(TreeID, StemID)) %>% 
  filter(PlotID == "15") %>% 
  filter(Shaded == "p") %>%
  group_by(Species) %>% 
  mutate(`Mean DBH` = mean(DBH)) %>% select(-DBH) %>% distinct()

Understory_15 <- i_2023_Data %>% select(-c(TreeID, StemID)) %>% 
  filter(PlotID == "15") %>% 
  filter(Shaded == "y") %>%
  group_by(Species) %>% 
  mutate(`Mean DBH` = mean(DBH)) %>% select(-DBH) %>% distinct()

DBH_canopy_15 <- rbind(Overstory_15, Partial_15, Understory_15)


#Plot 16
Overstory_16 <- i_2023_Data %>% select(-c(TreeID, StemID)) %>% 
  filter(PlotID == "16") %>% 
  filter(Shaded == "n") %>%
  group_by(Species) %>% 
  mutate(`Mean DBH` = mean(DBH)) %>% select(-DBH) %>% distinct()

Partial_16 <- i_2023_Data %>% select(-c(TreeID, StemID)) %>% 
  filter(PlotID == "16") %>% 
  filter(Shaded == "p") %>%
  group_by(Species) %>% 
  mutate(`Mean DBH` = mean(DBH)) %>% select(-DBH) %>% distinct()

Understory_16 <- i_2023_Data %>% select(-c(TreeID, StemID)) %>% 
  filter(PlotID == "16") %>% 
  filter(Shaded == "y") %>%
  group_by(Species) %>% 
  mutate(`Mean DBH` = mean(DBH)) %>% select(-DBH) %>% distinct()

DBH_canopy_16 <- rbind(Overstory_16, Partial_16, Understory_16)


#Plot 17
Overstory_17 <- i_2023_Data %>% select(-c(TreeID, StemID)) %>% 
  filter(PlotID == "17") %>% 
  filter(Shaded == "n") %>%
  group_by(Species) %>% 
  mutate(`Mean DBH` = mean(DBH)) %>% select(-DBH) %>% distinct()

Partial_17 <- i_2023_Data %>% select(-c(TreeID, StemID)) %>% 
  filter(PlotID == "17") %>% 
  filter(Shaded == "p") %>%
  group_by(Species) %>% 
  mutate(`Mean DBH` = mean(DBH)) %>% select(-DBH) %>% distinct()

Understory_17 <- i_2023_Data %>% select(-c(TreeID, StemID)) %>% 
  filter(PlotID == "17") %>% 
  filter(Shaded == "y") %>%
  group_by(Species) %>% 
  mutate(`Mean DBH` = mean(DBH)) %>% select(-DBH) %>% distinct()

DBH_canopy_17 <- rbind(Overstory_17, Partial_17, Understory_17)


#Plot 18
Overstory_18 <- i_2023_Data %>% select(-c(TreeID, StemID)) %>% 
  filter(PlotID == "18") %>% 
  filter(Shaded == "n") %>%
  group_by(Species) %>% 
  mutate(`Mean DBH` = mean(DBH)) %>% select(-DBH) %>% distinct()

Partial_18 <- i_2023_Data %>% select(-c(TreeID, StemID)) %>% 
  filter(PlotID == "18") %>% 
  filter(Shaded == "p") %>%
  group_by(Species) %>% 
  mutate(`Mean DBH` = mean(DBH)) %>% select(-DBH) %>% distinct()

Understory_18 <- i_2023_Data %>% select(-c(TreeID, StemID)) %>% 
  filter(PlotID == "18") %>% 
  filter(Shaded == "y") %>%
  group_by(Species) %>% 
  mutate(`Mean DBH` = mean(DBH)) %>% select(-DBH) %>% distinct()

DBH_canopy_18 <- rbind(Overstory_18, Partial_18, Understory_18)


## Final Table Outputs

#Plot 15
Table_15 <- Individuals_Plot_15 %>% left_join(DBH_canopy_15)

#Plot 16
Table_16 <- Individuals_Plot_16 %>% left_join(DBH_canopy_16)

#Plot 17
Table_17 <- Individuals_Plot_17 %>% left_join(DBH_canopy_17)

#Plot 18
Table_18 <- Individuals_Plot_18 %>% left_join(DBH_canopy_18)

#Final Output
Final_Output <- rbind(
  Table_15, Table_16, Table_17, Table_18) %>% 
  select(-c(`Individuals per m2`)) %>% relocate(`Plot size`, .after = PlotID)

write.xlsx(Final_Output, "[insert file path]")
