
## Author: Jack Boyce

### Here we calculate and visualize DBH growth per tree species in cm per year in the Lebendige Luppe reference plots, 
### plotted against the initial DBH
###
### Calculations made using only trees that were living in both timepoints; visualizations at the bottom

## Tree Species abbreviations:
## SEI = Stieleiche (Pedunculate oak)
## FAH = Feldahorn (Field maple)
## SAH = Spitzahorn (Norway maple)
## WLI = Winterlinde (Lime)
## HBU = Hainbuche (European hornbeam)
## UL = Ulme (Elm)
## BAH = Bergahorn (Sycamore maple)
## GES = Gemeine Esche (European ash)


library(googlesheets4)
library(tidyverse)
library(openxlsx)
library(readr)
library(cowplot)

options(scipen = 999)


## bringing in data
raw_LL_tree_inventory <- read.xlsx("[insert file path]")

colnames(raw_LL_tree_inventory)

## selecting relevant tree species

# LL
LL_filtered_trees <- raw_LL_tree_inventory %>% mutate(
  Baumart = case_when(   
    as.character(BAUMART17) %in% c("Til_cor", "Til_pla") ~ "Til_all", #Linde
    TRUE ~ as.character(BAUMART17) ),
  Baumart = case_when(   
    as.character(BAUMART17) %in% c("Ulm_min", "Ulm_spe", "Ulm_gla") ~ "Ulm_all", #Ulme
    TRUE ~ as.character(Baumart) ),
  Baumart = case_when(
    is.na(Baumart)  ~ BAUMART17,
    TRUE ~ as.character(Baumart) ),
  ) %>% 
  filter(Baumart == "Ace_cam" | Baumart == "Ace_pla" | Baumart == "Ace_pse" | Baumart == "Car_bet" | Baumart == "Fra_exc" | Baumart == "Que_rob" | Baumart == "Til_all" | Baumart == "Ulm_all") %>% 
  select(BAUM_CODE, PLOTID, Status, BAUMART, Baumart, BHD17, BHD20, veg_period, veg_peri_1, SCHIRM17, SCHIRM20)

LL_filtered_trees$BHD17 <- as.numeric(LL_filtered_trees$BHD17)
LL_filtered_trees$BHD20 <- as.numeric(LL_filtered_trees$BHD20)


## distinguishing between two different initial inventory years for LL data, calculating DBH growth per year
LL_trees_2013 <- LL_filtered_trees %>% filter(veg_period == 2013) %>% 
  mutate(DBH_year = (BHD20-BHD17)/7) %>% 
  relocate(DBH_year, .after = BHD20)

LL_trees_2016 <- LL_filtered_trees %>% filter(veg_period == 2016) %>% 
  mutate(DBH_year = (BHD20-BHD17)/4) %>% 
  relocate(DBH_year, .after = BHD20)


# rejoining data
LL_all_trees <- rbind(LL_trees_2013, LL_trees_2016) %>% 
  mutate(DBH_1 = BHD17) %>% 
  mutate(DBH_2 = BHD20)

colnames(raw_LL_tree_inventory)

## LL Oak Mortality
# getting surviving oaks
LL_surviving_oak_13 <- LL_all_trees %>% filter(Status == "2017 und 2020 vorhanden") %>% filter(Baumart == "Que_rob") %>% filter(veg_period == "2013") %>% group_by(Baumart) %>% summarise(`N Surviving Oak` = sum(Baumart == "Que_rob"))
LL_surviving_oak_16 <- LL_all_trees %>% filter(Status == "2017 und 2020 vorhanden") %>% filter(Baumart == "Que_rob") %>% filter(veg_period == "2016") %>% group_by(Baumart) %>% summarise(`N Surviving Oak` = sum(Baumart == "Que_rob"))


LL_surviving_oak_13_overstory <-LL_all_trees %>% filter(Status == "2017 und 2020 vorhanden") %>% filter(Baumart == "Que_rob") %>% filter(veg_period == "2013") %>% filter(SCHIRM17 == "nein") %>% group_by(Baumart) %>% summarise(`N Surviving Oak` = sum(Baumart == "Que_rob"))
LL_surviving_oak_13_understory <- LL_all_trees %>% filter(Status == "2017 und 2020 vorhanden") %>% filter(Baumart == "Que_rob") %>% filter(veg_period == "2013") %>% filter(SCHIRM17 == "ja") %>% group_by(Baumart) %>% summarise(`N Surviving Oak` = sum(Baumart == "Que_rob"))

LL_surviving_oak_16_overstory <-LL_all_trees %>% filter(Status == "2017 und 2020 vorhanden") %>% filter(Baumart == "Que_rob") %>% filter(veg_period == "2016") %>% filter(SCHIRM17 == "nein") %>% group_by(Baumart) %>% summarise(`N Surviving Oak` = sum(Baumart == "Que_rob"))
LL_surviving_oak_16_understory <- LL_all_trees %>% filter(Status == "2017 und 2020 vorhanden") %>% filter(Baumart == "Que_rob") %>% filter(veg_period == "2016") %>% filter(SCHIRM17 == "ja") %>% group_by(Baumart) %>% summarise(`N Surviving Oak` = sum(Baumart == "Que_rob"))

# getting dead oaks
LL_dead_oak_13 <- LL_all_trees %>% filter(Status == "tot") %>% filter(veg_period == "2013") %>% filter(Baumart == "Que_rob") %>% group_by(Baumart) %>% summarise(`N Dead Oak` = sum(Baumart == "Que_rob"))
LL_dead_oak_16 <- LL_all_trees %>% filter(Status == "tot") %>% filter(veg_period == "2016") %>% filter(Baumart == "Que_rob") %>% group_by(Baumart) %>% summarise(`N Dead Oak` = sum(Baumart == "Que_rob"))


LL_dead_oak_13_overstory <- LL_all_trees %>% filter(Status == "tot") %>% filter(veg_period == "2013") %>% filter(Baumart == "Que_rob") %>% filter(SCHIRM17 == "nein") %>% group_by(Baumart) %>% summarise(`N Dead Oak` = sum(Baumart == "Que_rob"))
LL_dead_oak_13_understory <- LL_all_trees %>% filter(Status == "tot") %>% filter(veg_period == "2013") %>% filter(Baumart == "Que_rob") %>% filter(SCHIRM17 == "ja") %>% group_by(Baumart) %>% summarise(`N Dead Oak` = sum(Baumart == "Que_rob"))

LL_dead_oak_16_overstory <- LL_all_trees %>% filter(Status == "tot") %>% filter(veg_period == "2016") %>% filter(Baumart == "Que_rob") %>% filter(SCHIRM17 == "nein") %>% group_by(Baumart) %>% summarise(`N Dead Oak` = sum(Baumart == "Que_rob"))
LL_dead_oak_16_understory <- LL_all_trees %>% filter(Status == "tot") %>% filter(veg_period == "2016") %>% filter(Baumart == "Que_rob") %>% filter(SCHIRM17 == "ja") %>% group_by(Baumart) %>% summarise(`N Dead Oak` = sum(Baumart == "Que_rob"))

# calculating morality rates
# no canopy distinction
LL_oak_mortality_13_20_overall <- LL_surviving_oak_13 %>% left_join(LL_dead_oak_13) %>% mutate(`Mortality p` = `N Dead Oak`/`N Surviving Oak`) %>% 
  mutate(`Annual Mortality Rate` = round(-log(1-`Mortality p`)/7, digits = 3)*100)

LL_oak_mortality_16_20_overall <- LL_surviving_oak_16 %>% left_join(LL_dead_oak_16) %>% mutate(`Mortality p` = `N Dead Oak`/`N Surviving Oak`) %>% 
  mutate(`Annual Mortality Rate` = round(-log(1-`Mortality p`)/4, digits = 3)*100)

LL_overall_combined_oak_mort <- LL_oak_mortality_13_20_overall %>% full_join(LL_oak_mortality_16_20_overall) %>% mutate(cl = NA) %>% mutate(`combined average mortality` = mean(`Annual Mortality Rate`))

#with canopy distinction
LL_oak_mortality_13_20_overstory <- LL_surviving_oak_13_overstory %>% left_join(LL_dead_oak_13_overstory) %>% mutate(`Mortality p` = `N Dead Oak`/`N Surviving Oak`) %>% 
  mutate(`Annual Mortality Rate` = round(-log(1-`Mortality p`)/7, digits = 3)*100)
LL_oak_mortality_13_20_understory <- LL_surviving_oak_13_understory %>% left_join(LL_dead_oak_13_understory) %>% mutate(`Mortality p` = `N Dead Oak`/`N Surviving Oak`) %>% 
  mutate(`Annual Mortality Rate` = round(-log(1-`Mortality p`)/7, digits = 3)*100)

LL_oak_mortality_16_20_overstory <- LL_surviving_oak_16_overstory %>% left_join(LL_dead_oak_16_overstory) %>% mutate(`Mortality p` = `N Dead Oak`/`N Surviving Oak`) %>% 
  mutate(`Annual Mortality Rate` = round(-log(1-`Mortality p`)/7, digits = 3)*100)
LL_oak_mortality_16_20_understory <- LL_surviving_oak_16_understory %>% left_join(LL_dead_oak_16_understory) %>% mutate(`Mortality p` = `N Dead Oak`/`N Surviving Oak`) %>% 
  mutate(`Annual Mortality Rate` = round(-log(1-`Mortality p`)/7, digits = 3)*100)


## creating common column for tree species
LL_all_trees <- LL_all_trees %>% mutate(
  species = case_when(   
    as.character(Baumart) %in% c("Til_all") ~ "WLI",
    TRUE ~ as.character(Baumart) ),
  species = case_when(   
    as.character(Baumart) %in% c("Ulm_all") ~ "UL",
    TRUE ~ as.character(species) ),
  species = case_when(   
    as.character(Baumart) %in% c("Que_rob") ~ "SEI",
    TRUE ~ as.character(species) ),
  species = case_when(   
    as.character(Baumart) %in% c("Ace_cam") ~ "FAH",
    TRUE ~ as.character(species) ),
  species = case_when(   
    as.character(Baumart) %in% c("Car_bet") ~ "HBU",
    TRUE ~ as.character(species) ),
  species = case_when(   
    as.character(Baumart) %in% c("Ace_pse") ~ "BAH",
    TRUE ~ as.character(species) ),
  species = case_when(   
    as.character(Baumart) %in% c("Ace_pla") ~ "SAH",
    TRUE ~ as.character(species) ),
  species = case_when(   
    as.character(Baumart) %in% c("Fra_exc") ~ "GES",
    TRUE ~ as.character(species) ),
)


## calculating LL mortality
LL_all_mortality = LL_all_trees %>% group_by(Baumart) %>% 
  summarise(n_total = sum(Baumart),
            n_dead = sum(mort))



## selecting common columns across data sets
LL <- LL_all_trees %>% select(species, DBH_1, DBH_year, SCHIRM17, SCHIRM20) %>% mutate(Dataset = "LL") %>% 
  filter(SCHIRM17 != "teilweise") %>% 
  mutate(SCHIRM17 = case_when(as.character(SCHIRM17) %in% "ja" ~ "understory",
                              TRUE ~ as.character(SCHIRM17))) %>% 
  mutate(SCHIRM17 = case_when(as.character(SCHIRM17) %in% "nein" ~ "overstory",
                              TRUE ~ as.character(SCHIRM17)))


## visualizing data
SEI <- LL %>% filter(species == "SEI") 
  
SEI_plot <- ggplot(SEI, aes(x=DBH_1, y=DBH_year, color=SCHIRM17)) + geom_point() +
  ggtitle("LL Oak DBH Change") +
  theme_classic(base_size = 18) +
  theme(plot.title = element_text(size = 18)) +
  geom_smooth(aes(group=SCHIRM17), method = "lm", se = FALSE) +
  labs(y= "DBH Change per Year (cm)", x = "Initial DBH (cm)") +
  guides(color=guide_legend("Canopy Layer"))
  
FAH <- LL %>% filter(species == "FAH") 

FAH_plot <- ggplot(FAH, aes(x=DBH_1, y=DBH_year)) + geom_point() +
    ggtitle("LL Field Maple DBH Change") +
    theme_classic(base_size = 14) +
    theme(plot.title = element_text(size = 11)) +
    geom_smooth(method = "lm", se = FALSE, color = "orange") +
    labs(y= "DBH Change per Year (cm)", x = "Initial DBH (cm)")
  
SAH <- LL %>% filter(species == "SAH") 

SAH_plot <- ggplot(SAH, aes(x=DBH_1, y=DBH_year)) + geom_point() +
    ggtitle("LL Norway Maple DBH Change") +
    theme_classic(base_size = 14) +
    theme(plot.title = element_text(size = 11)) +
    geom_smooth(method = "lm", se = FALSE, color = "orange") +
    labs(y= "DBH Change per Year (cm)", x = "Initial DBH (cm)")
  
WLI <- LL %>% filter(species == "WLI") 

WLI_plot <- ggplot(WLI, aes(x=DBH_1, y=DBH_year)) + geom_point() +
    ggtitle("LL Winter Linde DBH Change") +
    theme_classic(base_size = 14) +
    theme(plot.title = element_text(size = 11)) +
    geom_smooth(method = "lm", se = FALSE, color = "orange") +
    labs(y= "DBH Change per Year (cm)", x = "Initial DBH (cm)")

HBU <- LL %>% filter(species == "HBU") 

HBU_plot <- ggplot(HBU, aes(x=DBH_1, y=DBH_year)) + geom_point() +
    ggtitle("LL Hornbeam DBH Change") +
    theme_classic(base_size = 14) +
    theme(plot.title = element_text(size = 11)) +
    geom_smooth(method = "lm", se = FALSE, color = "orange") +
    labs(y= "DBH Change per Year (cm)", x = "Initial DBH (cm)")  
  
UL <- LL %>% filter(species == "UL") 

UL_plot <- ggplot(UL, aes(x=DBH_1, y=DBH_year)) + geom_point() +
    ggtitle("LL Elm DBH Change") +
    theme_classic(base_size = 14) +
    theme(plot.title = element_text(size = 11)) +
    geom_smooth(method = "lm", se = FALSE, color = "orange") +
    labs(y= "DBH Change per Year (cm)", x = "Initial DBH (cm)")
  
BAH <- LL %>% filter(species == "BAH") 

BAH_plot <- ggplot(BAH, aes(x=DBH_1, y=DBH_year)) + geom_point() +
    ggtitle("LL Sycamore Maple DBH Change") +
    theme_classic(base_size = 14) +
    theme(plot.title = element_text(size = 11)) +
    geom_smooth(method = "lm", se = FALSE, color = "orange") +
    labs(y= "DBH Change per Year (cm)", x = "Initial DBH (cm)")

GES <- LL %>% filter(species == "GES") 
  
GES_plot <- ggplot(GES, aes(x=DBH_1, y=DBH_year)) + geom_point() +
    ggtitle("LL Ash DBH Change") +
    theme_classic(base_size = 14) +
    theme(plot.title = element_text(size = 11)) +
    geom_smooth(method = "lm", se = FALSE, color = "orange") +
    labs(y= "DBH Change per Year (cm)", x = "Initial DBH (cm)")

plot_grid(SEI_plot, FAH_plot, SAH_plot, WLI_plot, HBU_plot, UL_plot, BAH_plot, GES_plot, ncol = 4)

## plotting LL mortality rates

LL_rates <- read.csv("[insert file path]") %>% 
  mutate(cl = case_when(as.character(cl) %in% "1" ~ "overstory",
                        TRUE ~ as.character(cl))) %>% 
  mutate(cl = case_when(as.character(cl) %in% "2" ~ "understory",
                        TRUE ~ as.character(cl)))

LL_SEI_rates <- LL_rates %>% filter(sp_adj == "SEI") %>% select(sp_adj, cl, period, mort_annual) %>% rename(sp = sp_adj) %>% mutate(Dataset = "LL")

Sachsenforst_rates <- read.xlsx("[insert file path]") %>% 
  mutate(Shaded = case_when(as.character(Shaded) %in% "n" ~ "understory",
                        TRUE ~ as.character(Shaded))) %>%
  mutate(Shaded = case_when(as.character(Shaded) %in% "y" ~ "overstory",
                            TRUE ~ as.character(Shaded))) 

Sachsenforst_SEI_rates <- Sachsenforst_rates %>% filter(Species == "SEI") %>% select(Species, Shaded, `Annual.Mortality.Rate`) %>% rename(sp = Species) %>% rename(cl = Shaded) %>% 
  rename(mort_annual = `Annual.Mortality.Rate`) %>% mutate(Dataset = "Sachsenforst") %>% mutate(period = "2023") %>% distinct()

LL_Sachsenforst_SEI_mort_rates <- rbind(LL_SEI_rates, Sachsenforst_SEI_rates)

LL_Sachsenforst_SEI_mort_plot <- ggplot(LL_Sachsenforst_SEI_mort_rates, aes(x=cl, y=mort_annual, color=Dataset, shape=period)) + geom_point(size=2.5) +
  ggtitle("LL Sachsenforst Oak Mortality Comparison") +
  theme_classic(base_size = 20) +
  theme(plot.title = element_text(size = 20)) +
  geom_smooth(aes(group=Dataset), method = "lm", se = FALSE, color = "orange") +
  labs(y= "Annual Mortality (%)", x = "Canopy Layer")