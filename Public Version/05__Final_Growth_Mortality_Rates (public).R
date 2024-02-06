
## Author: Jack Boyce

## Growth and mortality rates for all management sites (plus recruitment for natural regeneration)

library(tidyverse)
library(openxlsx)
library(cowplot)

### Data inputs

LL_rates <- read.csv("[insert file path]") %>% group_by(sp, sp_adj, cl) %>% mutate(avg_growth = mean(growth_rate)) %>% relocate(avg_growth, .after = growth_rate) %>% 
  mutate(avg_mort_annual = mean(mort_annual)) %>% 
  ungroup() %>%
  group_by(sp, sp_adj, cl, avg_growth, avg_mort_annual, rec_year, rec_year_ha) %>% 
  summarise() %>% 
  ungroup() %>% 
  select(-sp) %>% 
  rename(sp = sp_adj)

Sachsenforst_rates <- read.xlsx("[insert file path]") 


### Oak

Sachsenforst_rates_oak <- Sachsenforst_rates %>% filter(Species == "SEI") %>%
  mutate(Site = "Sachsenforst") %>% relocate(Site, .before = PlotID) %>% distinct()

LL_rates_oak <- LL_rates %>% filter(sp == "SEI")

# LL growth for when trees reach maturity

LL_oak_growth_over <- LL_rates_oak %>% filter(cl == 1) %>% rename(G1_LL = avg_growth) %>% select(-c(cl, rec_year, rec_year_ha, avg_mort_annual))
LL_oak_growth_under <- LL_rates_oak %>% filter(cl == 2) %>% rename(G2_LL = avg_growth) %>% select(-c(cl, rec_year, rec_year_ha, avg_mort_annual))


## Overstory 
Oak_Femels_Overstory <- Sachsenforst_rates_oak %>% filter(Shaded == "n")

# Growth
Oak_Femels_Overstory_Growth_max <- Oak_Femels_Overstory %>% mutate(overstory_max_growth = max(Null.to.2023.Growth.Rate)) %>% select(Site, Species, overstory_max_growth) %>% distinct() %>% rename(sp = Species)
Oak_Femels_Overstory_Growth_min <- Oak_Femels_Overstory %>% mutate(overstory_min_growth = min(Null.to.2023.Growth.Rate)) %>% select(Site, Species, overstory_min_growth) %>% distinct() %>% rename(sp = Species)
Oak_Femels_Overstory_Growth_mean <- Oak_Femels_Overstory %>% mutate(overstory_mean_growth = mean(Null.to.2023.Growth.Rate)) %>% select(Site, Species, overstory_mean_growth) %>% distinct() %>% rename(sp = Species)

Oak_Femels_Overstory_Growth_values <- Oak_Femels_Overstory_Growth_max %>% left_join(Oak_Femels_Overstory_Growth_mean) %>% left_join(Oak_Femels_Overstory_Growth_min)

# Mortality - using LL rates
Oak_Femels_overstory_mortality <- LL_rates_oak %>% filter(cl == "1") %>% select(sp, avg_mort_annual, rec_year_ha) %>% rename(overstory_mortality = avg_mort_annual)

## Understory
Oak_Femels_Understory <- Sachsenforst_rates_oak %>% filter(Shaded == "y")

# Growth
Oak_Femels_Understory_Growth_max <- Oak_Femels_Understory %>% mutate(understory_max_growth = max(Null.to.2023.Growth.Rate)) %>% select(Site, Species, understory_max_growth) %>% distinct() %>% rename(sp = Species)
Oak_Femels_Understory_Growth_min <- Oak_Femels_Understory %>% mutate(understory_min_growth = min(Null.to.2023.Growth.Rate)) %>% select(Site, Species, understory_min_growth) %>% distinct() %>% rename(sp = Species)
Oak_Femels_Understory_Growth_mean <- Oak_Femels_Understory %>% mutate(understory_mean_growth = mean(Null.to.2023.Growth.Rate)) %>% select(Site, Species, understory_mean_growth) %>% distinct() %>% rename(sp = Species)

Oak_Femels_Understory_Growth_values <- Oak_Femels_Understory_Growth_max %>% left_join(Oak_Femels_Understory_Growth_mean) %>% left_join(Oak_Femels_Understory_Growth_min)

# Mortality - using LL rates
Oak_Femels_understory_mortality <- LL_rates_oak %>% filter(cl == "2") %>% select(sp, avg_mort_annual, rec_year_ha) %>% rename(understory_mortality = avg_mort_annual)


## Adding allometrie data

Allometrie_Data <- read.csv("[insert file path]")
Oak_allometrie <- Allometrie_Data %>% filter(sp == "SEI") %>% select(-c(sp_full, h_a, h_b, h_c))

max_oak <- Oak_allometrie %>% left_join(Oak_Femels_Overstory_Growth_max) %>% left_join(Oak_Femels_Understory_Growth_max) %>% select(-Site) %>% 
  #left_join(Oak_Femels_overstory_mortality) %>% left_join(Oak_Femels_understory_mortality) %>% 
  #left_join(LL_oak_growth_over) %>% left_join(LL_oak_growth_under) %>% 
  relocate(c(param1, param2), .after = inflection) %>% 
  #relocate(c(inflection, param1, param2), .after = understory_mortality) %>% 
  #relocate(rec_year_ha, .after = understory_mortality) %>% 
  relocate(steepness, .after = inflection) %>% 
  rename(G1 = overstory_max_growth) %>% rename(G2 = understory_max_growth) %>% relocate(c(G1, G2), .before = inflection)

write.table(max_oak, "[insert file path]")

mean_oak <- Oak_allometrie %>% left_join(Oak_Femels_Overstory_Growth_mean) %>% left_join(Oak_Femels_Understory_Growth_mean) %>% select(-Site) %>% 
  relocate(c(param1, param2), .after = inflection) %>% 
  relocate(steepness, .after = inflection) %>% 
  rename(G1 = overstory_mean_growth) %>% rename(G2 = understory_mean_growth) %>% relocate(c(G1, G2), .before = inflection)

write.table(mean_oak, "[insert file path]")

min_oak <- Oak_allometrie %>% left_join(Oak_Femels_Overstory_Growth_min) %>% left_join(Oak_Femels_Understory_Growth_min) %>% select(-Site) %>% 
  relocate(c(param1, param2), .after = inflection) %>% 
  relocate(steepness, .after = inflection) %>% 
  rename(G1 = overstory_min_growth) %>% rename(G2 = understory_min_growth) %>% relocate(c(G1, G2), .before = inflection)

write.table(min_oak, "[insert file path]")