##
##
## plotting model outputs to determine time point at which oaks reach 40% of canopy area
library(tidyverse)
library(openxlsx)
library(patchwork)
library(cowplot)
options(scipen=999)


#### 2013-2020 census interval ------------------------

# 2013-2020 census interval min, mean, and max oak femel growth rates

2013_min_mean_max_oak_canopy_percentage <- "[Local File Path to "2013_min_mean_max_oak_canopy_percentage.csv" in Model Scenario Outputs]"

plot_2013_oak_femel_rates <- 2013_min_mean_max_oak_canopy_percentage %>%
  ggplot() +
  geom_point(aes(x = time, y = ca_percent, color = oak_rate, shape = "single femel"), size = 2) +
  geom_point(aes(x = time, y = cum_avg_percentage, color = oak_rate, shape= "cumulative average"), size = 2) +
  scale_color_manual(values = c("min" = "tomato3",
                                "mean" = "royalblue2",
                                "max" = "darkgreen"
  ), breaks = c("min", "mean", "max"))+
  geom_hline(yintercept=40, linetype="dashed", color = "gray", size = 0.8) +
  geom_vline(xintercept=270, linetype="dashed", color = "tomato3", size = 1 )+
  geom_vline(xintercept=271, linetype="dashed", color = "royalblue2", size = 1 )+
  geom_vline(xintercept=272, linetype="dashed", color = "darkgreen", size = 1 )+
  theme(plot.title=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x = "Time (years)", y = "Percentage Cover of Oak in Overstory",
       title = "2013-2020 LL / GW Class: Intermediate / DBH LL Rates: 40 cm",
       color = "Oak Femel Growth", shape = "") +
  guides(color = guide_legend(order = 1, override.aes = list(size = 5, alpha = 1)), 
         #shape = FALSE
         shape = guide_legend(order = 2, override.aes = list(size = 5, alpha = 1))
  ) +
  scale_shape_manual(values = c(16, 1)) +
  #scale_shape_manual(values = c(1)) +
  scale_y_continuous(breaks = c(0,20,40,60,80,100)) +
  scale_x_continuous(breaks = c(26,271))


# 2013-2020 census interval DBH thresholds for ambient oak growth rates

2013_30_40_50_oak_canopy_percentage <- "[Local File Path to "2013_30_40_50_oak_canopy_percentage.csv" in Model Scenario Outputs]"

plot_2013_ambient_DBH <- 2013_30_40_50_oak_canopy_percentage %>%
  ggplot() +
  geom_point(aes(x = time, y = ca_percent, color = factor(LL_Rates_cm, levels = c("30 cm", "50 cm", "40 cm")), shape = "single femel"), size = 2) +
  geom_point(aes(x = time, y = cum_avg_percentage, color = factor(LL_Rates_cm, levels = c("30 cm", "50 cm", "40 cm")), shape= "cumulative average"), size = 2) +
  scale_color_manual(values = c("30 cm" = "tomato3", "40 cm" = "royalblue2", "50 cm" = "darkgreen"),
  breaks = c("30 cm", "40 cm", "50 cm"))+
  geom_hline(yintercept=40, linetype="dashed", color = "gray", size = 0.8) +
  geom_vline(xintercept=246, linetype="dashed", color = "tomato3", size = 1 )+
  geom_vline(xintercept=271, linetype="dashed", color = "royalblue2", size = 1 )+
  geom_vline(xintercept=296, linetype="dashed", color = "darkgreen", size = 1 )+
  theme(plot.title=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x = "Time (years)", y = "Percentage Cover of Oak in Overstory",
       title = "2013-2020 LL / GW Class: Intermediate / Femel Growth: Mean",
       color = "DBH for Ambient Rates",
       shape = ""
  ) +
  guides(color = guide_legend(order = 1, override.aes = list(size = 5, alpha = 1)), 
         #shape = FALSE
         shape = guide_legend(order = 2, override.aes = list(size = 5, alpha = 1))
  ) +
  scale_shape_manual(values = c(16, 1)) +
  #scale_shape_manual(values = c(1)) + 
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_x_continuous(breaks = c(26, 271)) +
  xlab(NULL)


## 2013-2020 census interval groundwater distance classes

2013_GW_classes_oak_canopy_percentage <- "[Local File Path to "2013_GW_classes_oak_canopy_percentage.csv" in Model Scenario Outputs]"

plot_2013_all_GW <- 2013_GW_classes_oak_canopy_percentage %>% 
  ggplot() + 
  geom_point(aes(x = time, y = ca_percent, color = groundwater_class, shape = "single femel"), size = 2) +
  geom_point(aes(x = time, y = cum_avg_percentage, color = groundwater_class, shape= "cumulative average"), size = 2) +
  scale_color_manual(values = c("dry" = "tomato3",
                                "intermediate" = "darkgreen",
                                "moist" = "royalblue2"
  )) +
  geom_hline(yintercept=40, linetype="dashed", color = "gray", size = 0.8) +
  geom_vline(xintercept=386, linetype="dashed", color = "tomato3", size = 1 )+
  geom_vline(xintercept=191, linetype="dashed", color = "royalblue2", size = 1 )+
  geom_vline(xintercept=271, linetype="dashed", color = "darkgreen", size = 1 )+
  theme(plot.title=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x = "Time (years)", y = "Percentage Cover of Oak in Overstory", 
       title = "2013-2020 LL / DBH LL Rates: 40 cm / Femel Growth: Mean", 
       color = "Groundwater Class", shape = "") +
  guides(color = guide_legend(order = 1, override.aes = list(size = 5, alpha = 1)), 
         #shape = FALSE
         shape = guide_legend(order = 2, override.aes = list(size = 5, alpha = 1))
  ) +
  scale_shape_manual(values = c(16, 1)) +
         #scale_shape_manual(values = c(1)) + 
  scale_y_continuous(breaks = c(0,20,40,60,80,100)) +
  scale_x_continuous(breaks = c(26,191,271,386)) +
  xlab(NULL)


#### 2016-2020 census interval ------------------------

## 2016-2020 census interval min, mean, and max oak femel growth rates

2016_min_mean_max_oak_canopy_percentage <- "[Local File Path to "2016_min_mean_max_oak_canopy_percentage.csv" in Model Scenario Outputs]"

plot_2016_oak_femel_rates <- 2016_min_mean_max_oak_canopy_percentage %>%
  ggplot() +
  geom_point(aes(x = time, y = ca_percent, color = oak_rate, shape = "single femel"), size = 2) +
  geom_point(aes(x = time, y = cum_avg_percentage, color = oak_rate, shape= "cumulative average"), size = 2) +
  scale_color_manual(values = c("min" = "tomato3",
                                "max" = "darkgreen",
                                "mean" = "royalblue2"
  )) +
  geom_hline(yintercept=40, linetype="dashed", color = "gray", size = 0.8) +
  geom_vline(xintercept=596, linetype="dashed", color = "tomato3", size = 1 )+
  geom_vline(xintercept=591, linetype="dashed", color = "royalblue2", size = 1 )+
  geom_vline(xintercept=589, linetype="dashed", color = "darkgreen", size = 1 )+
  theme(plot.title=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x = "Time (years)", y = "Percentage Cover of Oak in Overstory",
       title = "2016-2020 LL / GW Class: Intermediate / DBH LL Rates: 40 cm",
       color = "Oak Femel Growth", shape = "") +
  scale_shape_manual(values = c(16, 1)) +
  #scale_shape_manual(values = c(1)) +
  guides(color = FALSE,
         shape = FALSE
         #color = guide_legend(order = 1, override.aes = list(size = 5, alpha = 1)),
         #shape = guide_legend(order = 2, override.aes = list(size = 5, alpha = 1))
  ) +
  scale_y_continuous(breaks = c(0,20,40,60,80,100)) +
  scale_x_continuous(breaks = c(26,591)) +
  ylab(NULL)


## 2016-2020 census interval DBH thresholds for ambient oak growth rates

2016_30_40_50_oak_canopy_percentage <- "[Local File Path to "2016_30_40_50_oak_canopy_percentage.csv" in Model Scenario Outputs]"

plot_2016_ambient_DBH <- 2016_30_40_50_oak_canopy_percentage %>%
  ggplot() +
  geom_point(aes(x = time, y = ca_percent, color = LL_Rates_cm, shape = "single femel"), size = 2) +
  geom_point(aes(x = time, y = cum_avg_percentage, color = LL_Rates_cm, shape= "cumulative average"), size = 2) +
  scale_color_manual(values = c("30 cm" = "tomato3",
                                "50 cm" = "darkgreen",
                                "40 cm" = "royalblue2"
  )) +
  geom_hline(yintercept=40, linetype="dashed", color = "gray", size = 0.8) +
  geom_vline(xintercept=566, linetype="dashed", color = "tomato3", size = 1 )+
  geom_vline(xintercept=591, linetype="dashed", color = "royalblue2", size = 1 )+
  geom_vline(xintercept=621, linetype="dashed", color = "darkgreen", size = 1 )+
  theme(plot.title=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_blank(), #element_text(size=18)
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x = "Time (years)", y = "Percentage Cover of Oak in Overstory",
       title = "2016-2020 LL / GW Class: Intermediate / Femel Growth: Mean",
       color = "DBH for Ambient Rates", shape = "") +
  scale_shape_manual(values = c(16, 1)) +
  #scale_shape_manual(values = c(1)) +
  guides(color = FALSE,
         shape = FALSE
         #color = guide_legend(order = 1, override.aes = list(size = 5, alpha = 1)),
         #shape = guide_legend(order = 2, override.aes = list(size = 5, alpha = 1))
  ) +
  scale_y_continuous(breaks = c(0,20,40,60,80,100)) +
  scale_x_continuous(breaks = c(26,591))


## 2016-2020 census interval groundwater distance classes

2016_GW_classes_oak_canopy_percentage <- "[Local File Path to "2016_GW_classes_oak_canopy_percentage.csv" in Model Scenario Outputs]"

plot_2016_all_GW <- 2016_GW_classes_oak_canopy_percentage %>% 
  ggplot() + 
  geom_point(aes(x = time, y = ca_percent, color = groundwater_class, shape = "single femel"), size = 2) +
  geom_point(aes(x = time, y = cum_avg_percentage, color = groundwater_class, shape= "cumulative average"), size = 2) +
  scale_color_manual(values = c("dry" = "tomato3",
                                "intermediate" = "darkgreen",
                                "moist" = "royalblue2"
  )) +
  geom_hline(yintercept=40, linetype="dashed", color = "gray", size = 0.8) +
  geom_vline(xintercept=326, linetype="dashed", color = "tomato3", size = 1 )+
  geom_vline(xintercept=1056, linetype="dashed", color = "royalblue2", size = 1 )+
  geom_vline(xintercept=591, linetype="dashed", color = "darkgreen", size = 1 )+
  theme(plot.title=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_blank(), #element_text(size=18)
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x = "Time (years)", y = "Percentage Cover of Oak in Overstory",
       title = "2016-2020 LL / DBH LL Rates: 40 cm / Femel Growth: Mean",
       color = "Groundwater Class", shape = "") +
  scale_shape_manual(values = c(16, 1)) +
  #scale_shape_manual(values = c(1)) +
  guides(color = FALSE,
         shape = FALSE
         #color = guide_legend(order = 1, override.aes = list(size = 5, alpha = 1)),
         #shape = guide_legend(order = 2, override.aes = list(size = 5, alpha = 1))
  ) +
  scale_y_continuous(breaks = c(0,20,40,60,80,100)) +
  scale_x_continuous(breaks = c(26,326,591,1056))


#### combined census intervals ------------------------

## combined census intervals min, mean, and max oak femel growth rates

combined_min_mean_max_oak_canopy_percentage <- "[Local File Path to "combined_min_mean_max_oak_canopy_percentage.csv" in Model Scenario Outputs]"

plot_combined_oak_femel_rates <- all_combined_intermediate_min_mean_max_oak_canopy_percentage %>% filter(time < 400) %>% 
  ggplot() +
  geom_point(aes(x = time, y = ca_percent, color = oak_rate, shape = "single femel"), size = 2) +
  geom_point(aes(x = time, y = cum_avg_percentage, color = oak_rate, shape= "cumulative average"), size = 2) +
  scale_color_manual(values = c("min" = "tomato3",
                                "max" = "darkgreen",
                                "mean" = "royalblue2"
  )) +
  geom_hline(yintercept=40, linetype="dashed", color = "gray", size = 0.8) +
  geom_vline(xintercept=380, linetype="dashed", color = "tomato3", size = 1 )+
  geom_vline(xintercept=381, linetype="dashed", color = "royalblue2", size = 1 )+
  geom_vline(xintercept=382, linetype="dashed", color = "darkgreen", size = 1 )+
  theme(plot.title=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x = "Time (years)", y = "Percentage Cover of Oak in Overstory",
       title = "Combined LL / GW Class: Intermediate / DBH LL Rates: 40 cm",
       color = "Oak Femel Growth", shape = "") +
  scale_shape_manual(values = c(16, 1)) +
  #scale_shape_manual(values = c(1)) +
  guides(color = FALSE,
         shape = FALSE
         #color = guide_legend(order = 1, override.aes = list(size = 5, alpha = 1)),
         #shape = guide_legend(order = 2, override.aes = list(size = 5, alpha = 1))
  ) +
  scale_y_continuous(breaks = c(0,20,40,60,80,100)) +
  scale_x_continuous(breaks = c(26,381)) +
  ylab(NULL)


## combined census intervals DBH thresholds for ambient oak growth rates

combined_30_40_50_oak_canopy_percentage.csv <- "[Local File Path to "combined_30_40_50_oak_canopy_percentage.csv" in Model Scenario Outputs]"

plot_combined_ambient_DBH <- all_combined_intermediate_30_40_50_oak_canopy_percentage %>% filter(time < 400) %>% 
  ggplot() +
  geom_point(aes(x = time, y = ca_percent, color = LL_Rates_cm, shape = "single femel"), size = 2) +
  geom_point(aes(x = time, y = cum_avg_percentage, color = LL_Rates_cm, shape= "cumulative average"), size = 2) +
  scale_color_manual(values = c("30 cm" = "tomato3",
                                "50 cm" = "darkgreen",
                                "40 cm" = "royalblue2"
  )) +
  geom_hline(yintercept=40, linetype="dashed", color = "gray", size = 0.8) +
  geom_vline(xintercept=366, linetype="dashed", color = "tomato3", size = 1 )+
  geom_vline(xintercept=381, linetype="dashed", color = "royalblue2", size = 1 )+
  geom_vline(xintercept=386, linetype="dashed", color = "darkgreen", size = 1 )+
  theme(plot.title=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_blank(), #element_text(size=18)
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x = "Time (years)", y = "Percentage Cover of Oak in Overstory",
       title = "Combined LL / GW Class: Intermediate / Femel Growth: Mean",
       color = "DBH for Ambient Rates", shape = "") +
  scale_shape_manual(values = c(16, 1)) +
  #scale_shape_manual(values = c(1)) +
  guides(color = FALSE,
         shape = FALSE
         #color = guide_legend(order = 1, override.aes = list(size = 5, alpha = 1)),
         #shape = guide_legend(order = 2, override.aes = list(size = 5, alpha = 1))
  ) +
  scale_y_continuous(breaks = c(0,20,40,60,80,100)) +
  scale_x_continuous(breaks = c(26,381))


## combined census intervals groundwater distance classes

combined_GW_classes_oak_canopy_percentage <- "[Local File Path to "combined_GW_classes_oak_canopy_percentage.csv" in Model Scenario Outputs]"

# single femel cover

combined_GW_classes_oak_canopy_percentage %>% 
  ggplot() + 
  geom_point(aes(x = time, y = ca_percent, color = groundwater_class, shape = "single femel"), size = 2) +
  scale_color_manual(values = c("dry" = "tomato3",
                                "intermediate" = "darkgreen",
                                "moist" = "royalblue2"
  )) +
  geom_point(x = 70, y = 100, color = "black", size = 3) +
  geom_point(x = 67, y = 88, color = "black", size = 3) +
  geom_point(x = 140, y = 69, color = "black", size = 3) +
  geom_hline(yintercept=40, linetype="dashed", color = "gray", size = 0.8) +
  theme(plot.title=element_text(size=22),
        axis.text=element_text(size=16),
        axis.title=element_text(size=20),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=18)
  ) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x = "Time (years)", y = "Percentage Cover of Oak in Overstory", 
       #title = "Oak Canopy Cover Across Time", 
       color = "Groundwater Class", 
  ) +
  guides(color = guide_legend(order = 1, override.aes = list(size = 5, alpha = 1)), 
         shape = guide_legend(order = 2, override.aes = list(size = 5, alpha = 1))) +
  scale_shape_manual(values = c(1)) + guides(shape = FALSE) +
  scale_y_continuous(breaks = c(0,20,40,60,69,80,88,100)) +
  scale_x_continuous(breaks = c(26,70,100,140,200,300,400))


# cumulative average cover

combined_GW_classes_oak_canopy_percentage %>% 
  ggplot() + 
  geom_point(aes(x = time, y = ca_percent, color = groundwater_class, shape = "single femel"), size = 2) +
  geom_point(aes(x = time, y = cum_avg_percentage, color = groundwater_class, shape= "cumulative average"), size = 2) +
  scale_color_manual(values = c("dry" = "tomato3",
                                "intermediate" = "darkgreen",
                                "moist" = "royalblue2"
  )) +
  geom_hline(yintercept=40, linetype="dashed", color = "gray", size = 0.8) +
  geom_vline(xintercept=336, linetype="dashed", color = "tomato3", size = 1 )+
  geom_vline(xintercept=361, linetype="dashed", color = "royalblue2", size = 1 )+
  geom_vline(xintercept=381, linetype="dashed", color = "darkgreen", size = 1 )+
  theme(plot.title=element_text(size=22),
        axis.text=element_text(size=16),
        axis.title=element_text(size=20),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=18)
  ) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x = "Time (years)", y = "Percentage Cover of Oak in Overstory", 
       #title = "Oak Canopy Cover Across Time", 
       color = "Groundwater Class", 
       shape = ""
       ) +
  guides(color = guide_legend(order = 1, override.aes = list(size = 5, alpha = 1)), 
         shape = guide_legend(order = 2, override.aes = list(size = 5, alpha = 1))) +
  scale_shape_manual(values = c(16, 1)) + 
  scale_y_continuous(breaks = c(0,20,40,60,80,100)) +
  scale_x_continuous(breaks = c(26,100,200,300,336,361,381,400))


# for plotting all scenarios on one figure

plot_combined_all_GW <- combined_GW_classes_oak_canopy_percentage %>% 
  ggplot() + 
  geom_point(aes(x = time, y = ca_percent, color = groundwater_class, shape = "single femel"), size = 2) +
  geom_point(aes(x = time, y = cum_avg_percentage, color = groundwater_class, shape= "cumulative average"), size = 2) +
  scale_color_manual(values = c("dry" = "tomato3",
                                "intermediate" = "darkgreen",
                                "moist" = "royalblue2"
  )) +
  geom_hline(yintercept=40, linetype="dashed", color = "gray", size = 0.8) +
  geom_vline(xintercept=336, linetype="dashed", color = "tomato3", size = 1 )+
  geom_vline(xintercept=361, linetype="dashed", color = "royalblue2", size = 1 )+
  geom_vline(xintercept=381, linetype="dashed", color = "darkgreen", size = 1 )+
  theme(plot.title=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_blank(), #element_text(size=18)
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x = "Time (years)", y = "Percentage Cover of Oak in Overstory",
       title = "Combined LL / DBH LL Rates: 40 cm / Femel Growth: Mean",
       color = "Groundwater Class", shape = "") +
  scale_shape_manual(values = c(16, 1)) +
  #scale_shape_manual(values = c(1)) +
  guides(color = FALSE,
         shape = FALSE
         #color = guide_legend(order = 1, override.aes = list(size = 5, alpha = 1)),
         #shape = guide_legend(order = 2, override.aes = list(size = 5, alpha = 1))
  ) +
  scale_y_continuous(breaks = c(0,20,40,60,80,100)) +
  scale_x_continuous(breaks = c(26,336,361,381))


# supplement S1 figure plotting  --------------------------------------------------------

# Dimensions: 12.5x17.0

plot_grid(
  plot_2013_all_GW,
  plot_2016_all_GW,
  plot_combined_all_GW,
  plot_2013_ambient_DBH,
  plot_2016_ambient_DBH,
  plot_combined_ambient_DBH,
  plot_2013_oak_femel_rates,
  plot_2016_oak_femel_rates,
  plot_combined_oak_femel_rates,
  ncol = 3,
  labels = c('A)','B)','C)','D)','E)','F)','G)','H)','I)', label_size = 10))


