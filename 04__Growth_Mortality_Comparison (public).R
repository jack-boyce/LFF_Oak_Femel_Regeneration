
## Author: Jack Boyce

## Comparing growth from planting to 2010 census to 2023 census in Burgaue Plots
library(tidyverse)
library(openxlsx)
library(patchwork)
library(cowplot)


######## Loading in data -----------------------------------------------

# 2010 femel inventory data
i_2010_Data <- read.xlsx("[insert file path to Inventory_Data_2010.xlsx in Femel Inventory Data]") %>% mutate(PlotID = Feld, Species = Baumart, DBH = Durchmesser)%>% select(-Bemerkung, -Feld, -Baumart, -Durchmesser)

i_2010_15 <- i_2010_Data %>% filter(PlotID == "15") %>% filter(Species == "SEI")
i_2010_16 <- i_2010_Data %>% filter(PlotID == "16") %>% filter(Species == "SEI")
i_2010_17 <- i_2010_Data %>% filter(PlotID == "17") %>% filter(Species == "SEI")
i_2010_18 <- i_2010_Data %>% filter(PlotID == "18") %>% filter(Species == "SEI")


i_2010_Data_bound <- rbind(
  i_2010_15, i_2010_16, i_2010_17, i_2010_18)
i_2010_Data_bound$PlotID <- as.character(i_2010_Data_bound$PlotID)

i_2010_grouped_1 <- i_2010_Data_bound %>% group_by(PlotID, Species) %>% mutate(`Mean DBH 2010` = mean(DBH)) %>% ungroup() %>% 
  group_by(PlotID, Species, `Mean DBH 2010`) %>% summarise() %>% ungroup()
i_2010_grouped_2 <- GJ_bound %>% group_by(PlotID, Species) %>% count() %>% ungroup()

i_2010_summary <- i_2010_grouped_1 %>% left_join(i_2010_grouped_2) %>% mutate(`N 2010` = n) %>% select(-n)



# 2023 femel inventory data
i_2023_Data <- read.xlsx("[insert file path to Inventory_Data_2023.xlsx in Femel Inventory Data]", sheet = 2) %>% 
  mutate(DBH = DBH_mm/10) %>% select(-c(DBH_mm, Comments, TreeID, StemID)) %>% relocate(DBH, .after = Species)

i_2023_15 <- Fieldwork_Census %>% filter(PlotID == "15") %>% filter(Species_Adjusted == "SEI")
i_2023_16 <- Fieldwork_Census %>% filter(PlotID == "16") %>% filter(Species_Adjusted == "SEI")
i_2023_17 <- Fieldwork_Census %>% filter(PlotID == "17") %>% filter(Species_Adjusted == "SEI")
i_2023_18 <- Fieldwork_Census %>% filter(PlotID == "18") %>% filter(Species_Adjusted == "SEI")

i_2023_Data_bound <- rbind(
  i_2023_15, i_2023_16, i_2023_17, i_2023_18)


i_2023_grouped_1 <- i_2023_Data_bound %>% group_by(PlotID, Species_Adjusted, Shaded) %>% mutate(`Mean DBH 2023` = mean(DBH)) %>% ungroup() %>% 
  group_by(PlotID, Species_Adjusted, Shaded, `Mean DBH 2023`) %>% summarise() %>% ungroup()
i_2023_grouped_2 <- i_2023_Data_bound %>% group_by(PlotID, Species_Adjusted, Shaded) %>% count() %>% ungroup()

i_2023_summary <- Fieldwork_1 %>% left_join(Fieldwork_2) %>% rename(`N 2023` = n)
i_2023_summary$PlotID <- as.character(i_2023_summary$PlotID)


# preparing Paußnitz data separately
i_2023_Paußnitz <- i_2023_Data %>% 
  filter(PlotID == "P") %>% filter(Species_Adjusted == "SEI")

i_2023_P_summary <- i_2023_Paußnitz %>% group_by(PlotID, Species_Adjusted, Shaded) %>% mutate(`Mean DBH 2023` = mean(DBH)) %>% ungroup() %>% 
  group_by(PlotID, Species_Adjusted, Shaded, `Mean DBH 2023`) %>% summarise() %>% ungroup() %>% mutate(`Mean DBH 2014` = 0) %>% mutate(`Planting_Year` = "2011") %>% rename(Species = `Species_Adjusted`) %>% 
  relocate(`Mean DBH 2014`, .before = `Mean DBH 2023`) %>% 
  mutate(`Years Null DBH to 2023 DBH` = 9) %>% 
  mutate(`Null to 2023 Growth Rate` = `Mean DBH 2023`/`Years Null DBH to 2023 DBH`) %>% 
  mutate(Plot_Label = "P")

i_2023_P_simplified <- i_2023_P_summary %>% select(PlotID, Species, Shaded, `Planting_Year`, `Null to 2023 Growth Rate`) %>% rename(Growth_Rate = `Null to 2023 Growth Rate`) %>% 
  mutate(Age = 12) %>% mutate(Femel_size = 0.1) %>% filter(Shaded != "p")

colnames(i_2023_P_simplified)

# joining Sachsenforst femels
i_2010_2023_joined <- i_2023_summary %>% left_join(i_2010_summary) %>% relocate(`Mean DBH 2010`, .after = Shaded) %>% relocate(`N 2010`, .after = `Mean DBH 2010`) %>% relocate(Shaded, .after = `N 2023`) %>% 
  mutate(
    `Planting Year` = case_when(PlotID %in% c("15") ~ "1997", 
    TRUE ~ `PlotID`),
    `Planting Year` = case_when(PlotID %in% c("16") ~ "1997", 
    TRUE ~ `Planting Year`),
    `Planting Year` = case_when(PlotID %in% c("17") ~ "1997", 
    TRUE ~ `Planting Year`),
    `Planting Year` = case_when(PlotID %in% c("18") ~ "1996", 
    TRUE ~ `Planting Year`)
    ) %>% 
  relocate(`Planting Year`, .after = Species) %>% 
  select(-Species_Adjusted)

i_2010_2023_joined$`Planting Year` <- as.numeric(i_2010_2023_joined$`Planting Year`)

# Time from 0 DBH at planting to 2010 DBH (2007 and 2020 used instead of 2010 and 2023 because foresters said trees take ~3 years to reach 1.3m DBH)
Null_2010_2023_DBH <- All_joined %>% mutate(`Years Null DBH to 2010 DBH` = 2007-`Planting Year`) %>% relocate(`Years Null DBH to 2010 DBH`, .after = `Planting Year`) %>% 
                      mutate(`Years Null DBH to 2023 DBH` = 2020-`Planting Year`) %>% relocate(`Years Null DBH to 2023 DBH`, .before = `Mean DBH 2023`)



######## Calculating Sachsenforst femels growth rates -----------------------------------------------

# Calculating DBH growth per year
Growth_rates <- Null_2010_2023_DBH %>% mutate(`Null to 2010 Growth Rate` = `Mean DBH 2010`/`Years Null DBH to 2010 DBH`) %>% relocate(`Null to 2010 Growth Rate`, .after = `N 2010`) %>%
  mutate(`2010 to 2023 Growth Rate` = (`Mean DBH 2023`-`Mean DBH 2010`)/13) %>% 
  mutate(`Null to 2023 Growth Rate` = `Mean DBH 2023`/`Years Null DBH to 2023 DBH`) %>% 
  relocate(Shaded, .after = `Mean DBH 2023`) %>% filter(Shaded != "p")

## condensing Sachsenforst rates

Sachsenforst_Paußnitz_condensed <- Growth_rates %>% 
  rename(`Census Period 1 Growth` = `Null to 2010 Growth Rate`) %>%
  rename(`Census Period 2 Growth` = `2010 to 2023 Growth Rate`) %>%
  rename(`Census Period 3 Growth` = `Null to 2023 Growth Rate`) %>%
  rename(`Age Census Period 1` = `Years Null DBH to 2010 DBH`) %>%
  rename(`Age Census Period 2` = `Years Null DBH to 2023 DBH`) %>%
  mutate(Femel_size = case_when(
    as.character(PlotID) %in% c("15") ~ "0.4",
    TRUE ~ as.character(PlotID))) %>%
  mutate(Femel_size = case_when(
    as.character(PlotID) %in% c("16") ~ "0.5",
    TRUE ~ as.character(Femel_size))) %>%
  mutate(Femel_size = case_when(
    as.character(PlotID) %in% c("17") ~ "0.6",
    TRUE ~ as.character(Femel_size))) %>%
  mutate(Femel_size = case_when(
    as.character(PlotID) %in% c("18") ~ "0.3",
    TRUE ~ as.character(Femel_size))) %>%
  select(PlotID, Shaded, Species, `Planting Year`, Femel_size, `Age Census Period 2`,`Census Period 3 Growth`) %>%
  rename(Growth_Rate = `Census Period 3 Growth`, Age = `Age Census Period 2`, Planting_Year = `Planting Year`) %>%
  rbind(P_simplified) %>% filter(Shaded != "p") %>% filter(PlotID == "15" | PlotID == "16" | PlotID == "17"  | PlotID == "18" | PlotID == "P") %>% 
  mutate(Shaded = case_when(as.character(Shaded) %in% "n" ~ "overstory", TRUE ~ as.character(Shaded)),
         Shaded = case_when(as.character(Shaded) %in% "y" ~ "understory", TRUE ~ as.character(Shaded)))

Sachsenforst_Paußnitz_condensed$Planting_Year <- as.numeric(Sachsenforst_Paußnitz_condensed$Planting_Year)
Sachsenforst_Paußnitz_condensed$`Growth_Rate` <- as.numeric(Sachsenforst_Paußnitz_condensed$`Growth_Rate`)
Sachsenforst_Paußnitz_condensed$Age <- as.numeric(Sachsenforst_Paußnitz_condensed$Age)
Sachsenforst_Paußnitz_condensed$Femel_size <- as.character(Sachsenforst_Paußnitz_condensed$Femel_size)


## Bringing in LL rates (already calculated by Lucian Elles)

LL_rates <- read.csv("[insert file path]") %>% select(sp_adj, cl, period, growth_rate, mort_annual) %>% rename(Species = sp_adj) %>%
  rename(Growth_Rate = growth_rate) %>% rename(Mortality_Rate = mort_annual) %>%
  mutate(Shaded = case_when(as.character(cl) %in% "1" ~ "n",
                            TRUE ~ as.character(cl)),
         Shaded = case_when(as.character(cl) %in% "2" ~ "y",
                            TRUE ~ as.character(Shaded))) %>%
  mutate(Age = 20) %>% 
  mutate(Femel_size = "NA", PlotID = "Forest inventory average") %>% mutate(Mortality_Rate = Mortality_Rate*100) %>% 
  relocate(c(PlotID, Femel_size, Species, Shaded, Growth_Rate), .before = Mortality_Rate) %>% 
  select(-cl, -period)

LL_oak_growth <- LL_rates %>% filter(Species == "SEI") %>% select(PlotID, Femel_size, Species, Shaded, Growth_Rate) %>% 
  mutate(Shaded = case_when(as.character(Shaded) %in% "n" ~ "overstory", TRUE ~ as.character(Shaded)),
         Shaded = case_when(as.character(Shaded) %in% "y" ~ "understory", TRUE ~ as.character(Shaded)),
         Shaded = case_when(as.character(Shaded) %in% "u" ~ "unspecified", TRUE ~ as.character(Shaded)))
                            
LL_mortality <- LL_rates %>% select(PlotID, Femel_size, Species, Shaded, Mortality_Rate)


######## Calculating Sachsenforst Mortality Rates -----------------------------------------------

# getting total sums for each species in 2010 and 2023 to get mortality rates per species (regardless of canopy layer)
# 2010 trees per species
trees_species_2010 <- GJ_summary %>% mutate(
  `N per Ha 2010` = case_when(PlotID %in% c("15") ~ "2378",
                         TRUE ~ `PlotID`),
  `N per Ha 2010` = case_when(PlotID %in% c("16") ~ "2182",
                         TRUE ~ `N per Ha 2010`),
  `N per Ha 2010` = case_when(PlotID %in% c("17") ~ "1938",
                         TRUE ~ `N per Ha 2010`),
  `N per Ha 2010` = case_when(PlotID %in% c("18") ~ "2375",
                         TRUE ~ `N per Ha 2010`)
)
trees_species_2010$`N per Ha 2010` <- as.numeric(trees_species_2010$`N per Ha 2010`)

trees_species_2023 <- Fieldwork_bound %>% group_by(PlotID, Species) %>% count() %>% rename(`N 2023` = n) %>% mutate(
  `N per Ha 2023` = case_when(PlotID %in% c("15") ~ "979",
                              TRUE ~ PlotID),
  `N per Ha 2023` = case_when(PlotID %in% c("16") ~ "803",
                         TRUE ~ `N per Ha 2023`),
  `N per Ha 2023` = case_when(PlotID %in% c("17") ~ "767",
                         TRUE ~ `N per Ha 2023`),
  `N per Ha 2023` = case_when(PlotID %in% c("18") ~ "843",
                         TRUE ~ `N per Ha 2023`)
)
trees_species_2023$`N per Ha 2023` <- as.numeric(trees_species_2023$`N per Ha 2023`)


mortality_all_trees <- trees_species_2010 %>% left_join(trees_species_2023) %>% mutate(`N Ha Dead` = `N per Ha 2010` - `N per Ha 2023`) %>% 
  mutate(`Mortality p` = `N Ha Dead`/`N per Ha 2010`) %>%
  mutate(`Annual Mortality Rate` = round(-log(1-`Mortality p`)/13, digits = 3)) %>% 
  relocate(`N 2023`, .after = `N 2010`) %>% mutate(`Annual Mortality Rate` = `Annual Mortality Rate`*100)


### joining growth and mortality rates for Sachsenforst femels


All_Rates <- mortality_all_trees %>% select(-c(`N 2023`)) %>% right_join(Growth_rates) %>% relocate(`N per Ha 2010`, .after = `Null to 2023 Growth Rate`) %>% 
  relocate(`N per Ha 2023`, .after = `N per Ha 2010`) %>% 
  relocate(`N Ha Dead`, .after = `N per Ha 2023`) %>% 
  relocate(`Mortality p`, .after = `N Ha Dead`) %>% 
  relocate(`Annual Mortality Rate`, .after = `Mortality p`) 

Sachsenforst_growth_mortality <- All_Rates %>% select(PlotID, Species, Shaded, `Null to 2023 Growth Rate`, `Annual Mortality Rate`) %>% 
  mutate(
    Femel_size = case_when(
      as.character(PlotID) %in% "15" ~ "0.4",
      TRUE ~ as.character(PlotID)),
    Femel_size = case_when(
      as.character(PlotID) %in% "16" ~ "0.5",
      TRUE ~ as.character(Femel_size)),
    Femel_size = case_when(
      as.character(PlotID) %in% "17" ~ "0.6",
      TRUE ~ as.character(Femel_size)),
    Femel_size = case_when(
      as.character(PlotID) %in% "18" ~ "0.3",
      TRUE ~ as.character(Femel_size)),
    ) %>% relocate(Femel_size, .after = PlotID) %>% 
  rename(Growth_Rate = `Null to 2023 Growth Rate`) %>% rename(Mortality_Rate = `Annual Mortality Rate`)


####### Filtering down to Oak rates -----------------------------------------------

## Oak growth

Oak_Rates_growth <- Sachsenforst_Paußnitz_condensed %>% 
  full_join(LL_rates) %>% 
  filter(Species == "SEI") %>% 
  mutate(Shaded = case_when(as.character(Shaded) %in% "n" ~ "overstory",
                            TRUE ~ as.character(Shaded)),
         Shaded = case_when(as.character(Shaded) %in% "y" ~ "understory",
                            TRUE ~ as.character(Shaded))) %>%
  mutate(Femel_size = case_when(as.character(Femel_size) %in% "NA" ~ "LL",
                            TRUE ~ as.character(Femel_size))) %>% 
  #filter(PlotID != "Forest inventory average") %>% 
  select(-Mortality_Rate)

Oak_Rates_growth$Age <- as.numeric(Oak_Rates_growth$Age)
Oak_Rates_growth$Femel_size <- as.character(Oak_Rates_growth$Femel_size)
Oak_Rates_growth$Growth_Rate <- as.numeric(Oak_Rates_growth$Growth_Rate)

Oak_Rates_growth_no_LL <- Oak_Rates_growth %>% filter(PlotID != "Forest inventory average")
Oak_Rates_growth_no_LL$Femel_size <- as.numeric(Oak_Rates_growth_no_LL$Femel_size)


Oak_femels_overstory_growth_avg <- Oak_Rates_growth_no_LL %>% filter(Shaded == "overstory") %>% mutate(avg_growth = mean(Growth_Rate))
Oak_femels_understory_growth_avg <- Oak_Rates_growth_no_LL %>% filter(Shaded == "understory") %>% mutate(avg_growth = mean(Growth_Rate))


## Oak mortality

Oak_Rates_mortality <- Sachsenforst_growth_mortality %>% 
  full_join(LL_mortality) %>% 
  filter(Species == "SEI") %>% 
  mutate(Shaded = case_when(as.character(Shaded) %in% "n" ~ "overstory",
                            TRUE ~ as.character(Shaded)),
         Shaded = case_when(as.character(Shaded) %in% "y" ~ "understory",
                            TRUE ~ as.character(Shaded)),
         Shaded = case_when(as.character(Shaded) %in% "u" ~ "unspecified",
                            TRUE ~ as.character(Shaded))) %>% 
  mutate(Femel_size = case_when(Femel_size %in% "0,3" ~ "0.3", TRUE ~ Femel_size),
         Femel_size = case_when(Femel_size %in% "0,4" ~ "0.4", TRUE ~ Femel_size),
         Femel_size = case_when(Femel_size %in% "0,5" ~ "0.5", TRUE ~ Femel_size),
         Femel_size = case_when(Femel_size %in% "0,6" ~ "0.6", TRUE ~ Femel_size),
         Femel_size = case_when(Femel_size %in% "NA" ~ "LL", TRUE ~ Femel_size)
  ) %>% select(-Growth_Rate)


# Oak_Rates_mortality_no_LL <- Oak_Rates_mortality %>% filter(PlotID != "Forest inventory average")
# Oak_Rates_mortality_no_LL$Femel_size <- as.numeric(Oak_Rates_mortality_no_LL$Femel_size)

######## Plotting Rate Comparisons -----------------------------------------------

### Plotting Growth Rate Comparisons

## Plotting Femel Growth with LL rates - Age & Size comparison

Femel_Growth_Rates <- ggplot(Oak_Rates_growth, aes(x = `Femel_size`, y = `Growth_Rate`, shape = Shaded, label = Femel_size)) +
  geom_point(size = 8)+
  scale_x_discrete(breaks = c(0.1,0.2,0.3,0.4,0.5,0.6,"LL")) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2), limits = c(0,1.2)) +
  scale_size_continuous(range  = c(0, 15),
                        limits = c(0,25),
                        breaks = c(12,23)) +
  labs(x = "Femel Size (ha)", y = "DBH Growth Rate (cm / year)", 
       #title = "Oak Growth Rate per Femel Size", 
       shape = "Canopy Layer"
  ) +
  guides(shape = guide_legend(override.aes = list(size=8, alpha = 1)), order = 2) +
  theme(plot.title=element_text(size=22),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "none")

Femel_Growth_Rates

# Plotting Femel Mortality with LL rates

Oak_Rates_mortality$Femel_size <- as.character(Oak_Rates_mortality$Femel_size)
Oak_Rates_mortality <- Oak_Rates_mortality %>% mutate(Shaded = case_when(as.character(Femel_size) %in% c("0.3","0.4","0.5","0.6") ~ "unspecified",
                                                                         TRUE ~ as.character(Shaded)))


Oak_Mortality_Sachsenforst_LL <- ggplot(Oak_Rates_mortality, aes(x = Femel_size, y = Mortality_Rate, shape = Shaded)) +
  geom_point(size = 8) +
  labs(x = "Femel Size (ha)", y = "Mortality Rate (% / year)", shape = "Canopy Layer", 
       #title = "Oak Mortality Rate per Femel Size"
       ) +
  theme(plot.title=element_text(size=22),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))+
  ylim(0, 16)

Oak_Mortality_Sachsenforst_LL


### plotting growth and mortality together

Femel_Growth_Rates + Oak_Mortality_Sachsenforst_LL + plot_annotation(tag_levels = c('A'), tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 24, face = 'bold'))

#plot_grid(Femel_Growth_Rates, Oak_Mortality_Sachsenforst_LL, labels = c('a','b)'), label_size = 22, label_fontface = "bold")
