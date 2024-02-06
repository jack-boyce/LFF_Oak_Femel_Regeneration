
## Author: Jack Boyce

## Comparison of Gregor Janus data from 2010 with April fieldwork data in Sachsenforst managed plots

# Loading packages
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(patchwork)
library(cowplot)

# Loading in data
GJ_Data <- read.xlsx("[insert file path]")
Fieldwork_Data <- read.xlsx("[insert file path]", sheet = 2) %>% 
  mutate(DBH = DBH_mm/10) %>% select(-c(DBH_mm, Shaded, TreeID, StemID)) %>% relocate(DBH, .after = Species)

## average dbh
average_oak_dbh_2010 <- GJ_Data %>% filter(Baumart == "SEI") %>% mutate(average_dbh = mean(Durchmesser)) %>% select(average_dbh) %>% distinct()
average_oak_dbh_2023 <- Fieldwork_Data %>% filter(Species_Adjusted == "SEI") %>% mutate(average_dbh = mean(DBH)) %>% select(average_dbh) %>% distinct()


## Wrangling for abundance per size class per hectare

# Gregor Janus data from 2010
GJ_15 <- GJ_Data %>% mutate(
  `Data Set` = "2010",
  PlotID = Feld,
  Species = Baumart,
  DBH = Durchmesser
) %>% 
  mutate(`Size Class` = DBH %/% 1) %>% 
  select(-c(Feld, Baumart, Durchmesser, Bemerkung)) %>% 
  filter(Species == "SEI") %>%
  #filter(`Size Class` > 4) %>% 
  filter(PlotID == 15) %>% 
  group_by(`Data Set`, `Size Class`, Species) %>% 
  count() %>% 
  mutate(`Abundance per Hectare` = n*22.22)  # Plot size = 450; 10000/450 = 22.22

GJ_16 <- GJ_Data %>% mutate(
  `Data Set` = "2010",
  PlotID = Feld,
  Species = Baumart,
  DBH = Durchmesser
) %>% 
  mutate(`Size Class` = DBH %/% 1) %>% 
  select(-c(Feld, Baumart, Durchmesser, Bemerkung)) %>% 
  filter(Species == "SEI") %>%
  #filter(`Size Class` > 4) %>% 
  filter(PlotID == 16) %>% 
  group_by(`Data Set`, `Size Class`, Species) %>% 
  count() %>% 
  mutate(`Abundance per Hectare` = n*18.18)  # Plot size = 550; 10000/550 = 18.18

GJ_17 <- GJ_Data %>% mutate(
  `Data Set` = "2010",
  PlotID = Feld,
  Species = Baumart,
  DBH = Durchmesser
) %>% 
  mutate(`Size Class` = DBH %/% 1) %>% 
  select(-c(Feld, Baumart, Durchmesser, Bemerkung)) %>% 
  filter(Species == "SEI") %>%
  #filter(`Size Class` > 4) %>% 
  filter(PlotID == 17) %>% 
  group_by(`Data Set`, `Size Class`, Species) %>% 
  count() %>% 
  mutate(`Abundance per Hectare` = n*15.38)  # Plot size = 650; 10000/650 = 15.38

GJ_18 <- GJ_Data %>% mutate(
  `Data Set` = "2010",
  PlotID = Feld,
  Species = Baumart,
  DBH = Durchmesser
) %>% 
  mutate(`Size Class` = DBH %/% 1) %>% 
  select(-c(Feld, Baumart, Durchmesser, Bemerkung)) %>% 
  filter(Species == "SEI") %>%
  #filter(`Size Class` > 4) %>% 
  filter(PlotID == 18) %>% 
  group_by(`Data Set`, `Size Class`, Species) %>% 
  count() %>% 
  mutate(`Abundance per Hectare` = n*25)  # Plot size = 400; 10000/400 = 25


## Fieldwork data from 2023

FW_15 <- Fieldwork_Data %>% mutate(
  `Data Set` = "2023",
  `Size Class` = DBH %/% 1,
) %>% 
  select(-c(Comments)) %>% 
  filter(Species_Adjusted == "SEI") %>%
  filter(PlotID == 15) %>% 
  group_by(`Data Set`, `Size Class`, Species_Adjusted) %>% 
  count() %>% 
  mutate(`Abundance per Hectare` = n*11.11) %>%  # Plot size = 900; 10000/900 = 11.11
  rename(Species = Species_Adjusted)

FW_16 <- Fieldwork_Data %>% mutate(
  `Data Set` = "2023",
  `Size Class` = DBH %/% 1,
) %>% 
  select(-c(Comments)) %>% 
  filter(Species_Adjusted == "SEI") %>%
  filter(PlotID == 16) %>% 
  group_by(`Data Set`, `Size Class`, Species_Adjusted) %>% 
  count() %>% 
  mutate(`Abundance per Hectare` = n*14.81) %>%  # Plot size = 675; 10000/675 = 14.81 (b/c Q3 removed)
  rename(Species = Species_Adjusted)

FW_17 <- Fieldwork_Data %>% mutate(
  `Data Set` = "2023",
  `Size Class` = DBH %/% 1,
) %>%
  select(-c(Comments)) %>% 
  filter(Species_Adjusted == "SEI") %>%
  filter(PlotID == 17) %>% 
  filter(`Size Class` < 40) %>% 
  group_by(`Data Set`, `Size Class`, Species_Adjusted) %>% 
  count() %>% 
  mutate(`Abundance per Hectare` = n*44.44) %>%  # Plot size = 225; 10000/225 = 44.44 (b/c only including Q1)
  rename(Species = Species_Adjusted)

FW_18 <- Fieldwork_Data %>% mutate(
  `Data Set` = "2023",
  `Size Class` = DBH %/% 1,
) %>% 
  select(-c(Comments)) %>% 
  filter(Species_Adjusted == "SEI") %>%
  filter(PlotID == 18) %>% 
  group_by(`Data Set`, `Size Class`, Species_Adjusted) %>% 
  count() %>% 
  mutate(`Abundance per Hectare` = n*16.67) %>%  # Plot size = 600; 10000/600 = 16.67
  rename(Species = Species_Adjusted)

FW_P <- Fieldwork_Data %>% mutate(
  `Data Set` = "2023",
  `Size Class` = DBH %/% 1,
) %>% 
  select(-c(Comments)) %>% 
  filter(Species_Adjusted == "SEI") %>% 
  filter(PlotID == "P") %>% 
  group_by(`Data Set`, `Size Class`, Species_Adjusted) %>% 
  count() %>% 
  mutate(`Abundance per Hectare` = n*25) %>%  # Plot size = 400; 10000/400 = 25
  rename(Species = Species_Adjusted)

## combining data
Plot_15 <- rbind(GJ_15, FW_15) %>% mutate(Plot = "15") %>% relocate(Plot, .before = `Data Set`)
Plot_16 <- rbind(GJ_16, FW_16) %>% mutate(Plot = "16") %>% relocate(Plot, .before = `Data Set`)
Plot_17 <- rbind(GJ_17, FW_17) %>% mutate(Plot = "17") %>% relocate(Plot, .before = `Data Set`)
Plot_18 <- rbind(GJ_18, FW_18) %>% mutate(Plot = "18") %>% relocate(Plot, .before = `Data Set`)
Plot_P <- FW_P %>% mutate(Plot = "P") %>% relocate(Plot, .before = `Data Set`)

All_SEI_plots <- rbind(Plot_15, Plot_16, Plot_17, Plot_18, Plot_P)

## visualizing data
count_15 <- Plot_15 %>%
  group_by(`Data Set`) %>%
  summarize(n = sum(`Abundance per Hectare`)) %>% mutate(
    `n Adjusted` = case_when(as.character(`Data Set`) %in% c("2010") ~ "2378",
                             TRUE ~ as.character(`Data Set`) ),
    `n Adjusted` = case_when(as.character(`Data Set`) %in% c("2023") ~ "979",
                             TRUE ~ as.character(`n Adjusted`) )
  )  %>% mutate(Plot = "15") %>% rename(`Census Year` = `Data Set`)

Plot_15_gg <- Plot_15 %>% ggplot() +
  geom_bar(data = Plot_15, aes(x=`Size Class`, y=`Abundance per Hectare`, fill=`Data Set`), stat="identity", position = "dodge", width = .6) +
  #labs(title = "0.4 ha Oak Femel", x = "DBH Size Class (cm)") +
  #guides(fill = guide_legend(title = "Census Year")) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30), limits = c(0,32)) +
  scale_y_continuous(breaks = c(0,100,200,300,400,500,600), limits = c(0,625)) +
  scale_fill_manual(values = c("2010" = "steelblue4",
                               "2023" = "orange3")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        plot.title=element_blank(),
        axis.text=element_text(size=18),
        axis.title=element_blank(),
        legend.title=element_blank(), 
        legend.text=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_text(data = count_15, aes(x=c(28, 28), y = c(270,210), col = `Census Year`, label = paste0("N = ", `n Adjusted`)),
            vjust = -0.5, size = 6)+
  annotate(
    geom = "text",
    x = 17,
    y = 550,
    label = "0.4 ha",
    size = 8
  ) +
  scale_colour_manual(values=c("steelblue4", "orange3"))

count_16 <- Plot_16 %>%
  group_by(`Data Set`) %>%
  summarize(n = sum(`Abundance per Hectare`)) %>% mutate(
    `n Adjusted` = case_when(as.character(`Data Set`) %in% c("2010") ~ "2182",
                             TRUE ~ as.character(`Data Set`) ),
    `n Adjusted` = case_when(as.character(`Data Set`) %in% c("2023") ~ "803",
                             TRUE ~ as.character(`n Adjusted`) )
  ) %>% mutate(Plot = "16") %>% rename(`Census Year` = `Data Set`)

Plot_16_gg <- Plot_16 %>% ggplot() +
  geom_bar(data = Plot_16, aes(x=`Size Class`, y=`Abundance per Hectare`, fill=`Data Set`), stat="identity", position = "dodge", width = .6) +
  #labs(title = "0.5 ha Oak Femel", x = "DBH Size Class (cm)") +
  #guides(fill = guide_legend(title = "Census Year")) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30), limits = c(0,32)) +
  scale_y_continuous(breaks = c(0,100,200,300,400,500,600), limits = c(0,625)) +
  scale_fill_manual(values = c("2010" = "steelblue4",
                               "2023" = "orange3")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        plot.title=element_blank(),
        axis.text=element_text(size=18),
        axis.title=element_blank(),
        legend.title=element_blank(), 
        legend.text=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_text(data = count_16, aes(x=c(28, 28), y = c(270,210), col = `Census Year`, label = paste0("N = ", `n Adjusted`)),
            vjust = -0.5, size = 6)+
  annotate(
    geom = "text",
    x = 17,
    y = 550,
    label = "0.5 ha",
    size = 8
  ) +
  scale_colour_manual(values=c("steelblue4", "orange3"))


count_17 <- Plot_17 %>%
  group_by(`Data Set`) %>%
  summarize(n = sum(`Abundance per Hectare`)) %>% mutate(
    `n Adjusted` = case_when(as.character(`Data Set`) %in% c("2010") ~ "1938",
                             TRUE ~ as.character(`Data Set`) ),
    `n Adjusted` = case_when(as.character(`Data Set`) %in% c("2023") ~ "767",
                             TRUE ~ as.character(`n Adjusted`) )
  ) %>% mutate(Plot = "17") %>% rename(`Census Year` = `Data Set`)

Plot_17_gg <- Plot_17 %>% 
  #filter(`Size Class` < 4) %>% 
  ggplot() +
  geom_bar(data = Plot_17, aes(x=`Size Class`, y=`Abundance per Hectare`, fill=`Data Set`), stat="identity", position = "dodge", width = .6) +
  #labs(title = "0.6 ha Oak Femel", x = "DBH Size Class (cm)") +
  #guides(fill = guide_legend(title = "Census Year")) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30), limits = c(0,32)) +
  scale_y_continuous(breaks = c(0,100,200,300,400,500,600), limits = c(0,625)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  scale_fill_manual(values = c("2010" = "steelblue4",
                               "2023" = "orange3")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        plot.title=element_blank(),
        axis.text=element_text(size=18),
        axis.title=element_blank(),
        legend.title=element_blank(), 
        legend.text=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_text(data = count_17, aes(x=c(28, 28), y = c(270,210), col = `Census Year`, label = paste0("N = ", `n Adjusted`)),
            vjust = -0.5, size = 6)+
  annotate(
    geom = "text",
    x = 17,
    y = 550,
    label = "0.6 ha",
    size = 8
  ) +
  scale_colour_manual(values=c("steelblue4", "orange3"))

count_18 <- Plot_18 %>%
  group_by(`Data Set`) %>%
  summarize(n = sum(`Abundance per Hectare`)) %>% mutate(
    `n Adjusted` = case_when(as.character(`Data Set`) %in% c("2010") ~ "2375",
                             TRUE ~ as.character(`Data Set`) ),
    `n Adjusted` = case_when(as.character(`Data Set`) %in% c("2023") ~ "843",
                             TRUE ~ as.character(`n Adjusted`) )
  )  %>% mutate(Plot = "18") %>% rename(`Census Year` = `Data Set`)

Plot_18_gg <- Plot_18 %>% ggplot() +
  geom_bar(data = Plot_18, aes(x=`Size Class`, y=`Abundance per Hectare`, fill=`Data Set`), stat="identity", position = "dodge", width = .6) +
  #labs(title = "0.3 ha Oak Femel", x = "DBH Size Class (cm)") +
  guides(fill = guide_legend(title = "Census Year")) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30), limits = c(0,32)) +
  scale_y_continuous(breaks = c(0,100,200,300,400,500,600), limits = c(0,625)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  scale_fill_manual(values = c("2010" = "steelblue4",
                               "2023" = "orange3")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        plot.title=element_blank(),
        axis.text=element_text(size=18),
        axis.title=element_blank(),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=18))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_text(data = count_18, aes(x=c(28, 28), y = c(270,210), col = `Census Year`, label = paste0("N = ", `n Adjusted`)),
            vjust = -0.5, size = 6)+
  annotate(
    geom = "text",
    x = 17,
    y = 550,
    label = "0.3 ha",
    size = 8
  ) +
  scale_colour_manual(values=c("steelblue4", "orange3"))

count_P <- Plot_P %>%
  group_by(`Data Set`) %>%
  summarize(n = sum(`Abundance per Hectare`)) %>% mutate(
    `n Adjusted` = case_when(as.character(`Data Set`) %in% c("2023") ~ "3500",
                             TRUE ~ as.character(`Data Set`) ))  %>%
  mutate(Plot = "P") %>% rename(`Census Year` = `Data Set`)

Plot_P_gg <- Plot_P %>% ggplot() +
  geom_bar(data = Plot_P, aes(x=`Size Class`, y=`Abundance per Hectare`, fill=`Data Set`), stat="identity", position = "dodge", width = .6) +
  labs(
    #title = "0.1 ha Oak Femel", 
       x = "DBH Size Class (cm)") +
  #guides(fill = guide_legend(title = "Census Year")) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30), limits = c(0,32)) +
  scale_y_continuous(breaks = c(0,100,200,300,400,500,600), limits = c(0,625)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  scale_fill_manual(values = c("2023" = "orange3"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        plot.title=element_text(size=22, face="bold"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=14))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_text(data = count_P, aes(x=c(28), y = c(210), col = `Census Year`, label = paste0("N = ", `n Adjusted`)),
            vjust = -0.5, size = 6)+
  annotate(
    geom = "text",
    x = 17,
    y = 550,
    label = "0.1 ha",
    size = 8
  ) +
  scale_colour_manual(values=c("orange3"))

legend_census_years <- get_legend(Plot_18_gg)

plot_grid(Plot_P_gg + theme(legend.position="none"), Plot_18_gg + theme(legend.position="none"), Plot_15_gg + theme(legend.position="none"), Plot_16_gg + theme(legend.position="none"), Plot_17_gg + theme(legend.position="none"), ncol = 3, 
          #labels = c('a.','b.','c.','d.','e.'), label_size = 22, 
          legend_census_years)

## joining all census counts into one data frame

# All_counts <- rbind(count_13, count_14, count_15, count_16, count_17, count_18)

