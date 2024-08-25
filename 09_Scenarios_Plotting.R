##
##
## plotting outputs from all_cohorts_out.csv files to determine time point at which oaks reach 40% of canopy area
library(tidyverse)
library(openxlsx)
library(patchwork)
library(cowplot)
options(scipen=999)


################################################################################ 2013 census
# 2013 dry rates----------------------------------------------------------------

## min
## min oak 40cm model output
min_oaks_2013_dry_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2013 Rates/Dry/Min/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         # sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

min_oak_2013_dry_40_cohort_sum <- min_oaks_2013_dry_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

min_oak_2013_dry_40_cum_sum <- min_oaks_2013_dry_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

min_oak_2013_dry_40_canopy_percentage <- min_oak_2013_dry_40_cohort_sum %>% left_join(min_oak_2013_dry_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "min") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "dry") %>% mutate(time = time+26)

min_oak_2013_dry_40_canopy_percentage$cum_avg_percentage <- cummean(min_oak_2013_dry_40_canopy_percentage$ca_percent)


## mean
## mean oak 30cm model output
mean_oaks_2013_dry_30_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2013 Rates/Dry/Mean/all_cohorts_out_30.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         # sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_2013_dry_30_cohort_sum <- mean_oaks_2013_dry_30_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_2013_dry_30_cum_sum <- mean_oaks_2013_dry_30_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_2013_dry_30_canopy_percentage <- mean_oak_2013_dry_30_cohort_sum %>% left_join(mean_oak_2013_dry_30_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "30 cm") %>% mutate(groundwater_class = "dry") %>% mutate(time = time+26)

mean_oak_2013_dry_30_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_2013_dry_30_canopy_percentage$ca_percent)


## mean oak 40cm model output
mean_oaks_2013_dry_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2013 Rates/Dry/Mean/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         # sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_2013_dry_40_cohort_sum <- mean_oaks_2013_dry_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_2013_dry_40_cum_sum <- mean_oaks_2013_dry_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_2013_dry_40_canopy_percentage <- mean_oak_2013_dry_40_cohort_sum %>% left_join(mean_oak_2013_dry_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "dry") %>% mutate(time = time+26)

mean_oak_2013_dry_40_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_2013_dry_40_canopy_percentage$ca_percent)


## mean oak 50cm model output
mean_oaks_2013_dry_50_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2013 Rates/Dry/mean/all_cohorts_out_50.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         # sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_2013_dry_50_cohort_sum <- mean_oaks_2013_dry_50_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_2013_dry_50_cum_sum <- mean_oaks_2013_dry_50_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_2013_dry_50_canopy_percentage <- mean_oak_2013_dry_50_cohort_sum %>% left_join(mean_oak_2013_dry_50_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "50 cm") %>% mutate(groundwater_class = "dry") %>% mutate(time = time+26)

mean_oak_2013_dry_50_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_2013_dry_50_canopy_percentage$ca_percent)


## max
## max oak 40cm model output
max_oaks_2013_dry_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2013 Rates/Dry/Max/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         # sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

max_oak_2013_dry_40_cohort_sum <- max_oaks_2013_dry_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

max_oak_2013_dry_40_cum_sum <- max_oaks_2013_dry_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

max_oak_2013_dry_40_canopy_percentage <- max_oak_2013_dry_40_cohort_sum %>% left_join(max_oak_2013_dry_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "max") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "dry") %>% mutate(time = time+26)

max_oak_2013_dry_40_canopy_percentage$cum_avg_percentage <- cummean(max_oak_2013_dry_40_canopy_percentage$ca_percent)


# 2013 intermediate rates ----------------------------------------------------------

## min
## min oak 40cm moodel output
min_oaks_2013_intermediate_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2013 Rates/intermediate/Min/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         # sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

min_oak_2013_intermediate_40_cohort_sum <- min_oaks_2013_intermediate_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

min_oak_2013_intermediate_40_cum_sum <- min_oaks_2013_intermediate_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

min_oak_2013_intermediate_40_canopy_percentage <- min_oak_2013_intermediate_40_cohort_sum %>% left_join(min_oak_2013_intermediate_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "min") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "intermediate") %>% mutate(time = time+26)

min_oak_2013_intermediate_40_canopy_percentage$cum_avg_percentage <- cummean(min_oak_2013_intermediate_40_canopy_percentage$ca_percent)


## mean
## mean oak 30cm model output
mean_oaks_2013_intermediate_30_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2013 Rates/intermediate/Mean/all_cohorts_out_30.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         # sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_2013_intermediate_30_cohort_sum <- mean_oaks_2013_intermediate_30_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_2013_intermediate_30_cum_sum <- mean_oaks_2013_intermediate_30_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_2013_intermediate_30_canopy_percentage <- mean_oak_2013_intermediate_30_cohort_sum %>% left_join(mean_oak_2013_intermediate_30_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "30 cm") %>% mutate(groundwater_class = "intermediate") %>% mutate(time = time+26)

mean_oak_2013_intermediate_30_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_2013_intermediate_30_canopy_percentage$ca_percent)


# mean oak 40cm model output
mean_oaks_2013_intermediate_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2013 Rates/intermediate/Mean/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         # sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_2013_intermediate_40_cohort_sum <- mean_oaks_2013_intermediate_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_2013_intermediate_40_cum_sum <- mean_oaks_2013_intermediate_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_2013_intermediate_40_canopy_percentage <- mean_oak_2013_intermediate_40_cohort_sum %>% left_join(mean_oak_2013_intermediate_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "intermediate") %>% mutate(time = time+26)

mean_oak_2013_intermediate_40_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_2013_intermediate_40_canopy_percentage$ca_percent)


# mean oak 50cm model output
mean_oaks_2013_intermediate_50_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2013 Rates/intermediate/Mean/all_cohorts_out_50.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         # sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_2013_intermediate_50_cohort_sum <- mean_oaks_2013_intermediate_50_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_2013_intermediate_50_cum_sum <- mean_oaks_2013_intermediate_50_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_2013_intermediate_50_canopy_percentage <- mean_oak_2013_intermediate_50_cohort_sum %>% left_join(mean_oak_2013_intermediate_50_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "50 cm") %>% mutate(groundwater_class = "intermediate") %>% mutate(time = time+26)

mean_oak_2013_intermediate_50_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_2013_intermediate_50_canopy_percentage$ca_percent)


## max
## max oak 40 cm model output
max_oaks_2013_intermediate_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2013 Rates/intermediate/max/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         # sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

max_oak_2013_intermediate_40_cohort_sum <- max_oaks_2013_intermediate_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

max_oak_2013_intermediate_40_cum_sum <- max_oaks_2013_intermediate_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

max_oak_2013_intermediate_40_canopy_percentage <- max_oak_2013_intermediate_40_cohort_sum %>% left_join(max_oak_2013_intermediate_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "max") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "intermediate") %>% mutate(time = time+26)

max_oak_2013_intermediate_40_canopy_percentage$cum_avg_percentage <- cummean(max_oak_2013_intermediate_40_canopy_percentage$ca_percent)



# plotting
all_2013_intermediate_min_mean_max_oak_canopy_percentage <- min_oak_2013_intermediate_40_canopy_percentage %>% full_join(mean_oak_2013_intermediate_40_canopy_percentage) %>% full_join(max_oak_2013_intermediate_40_canopy_percentage)

#write.csv(all_2013_intermediate_min_mean_max_oak_canopy_percentage, "C:/Users/idivjb92jaka/Documents/Thesis Data/Data Frames for Plotted Outputs/2013_min_mean_max_oak_canopy_percentage.csv")

plot_2013_oak_femel_rates <- all_2013_intermediate_min_mean_max_oak_canopy_percentage %>%
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



all_2013_intermediate_30_40_50_oak_canopy_percentage <- mean_oak_2013_intermediate_30_canopy_percentage %>% full_join(mean_oak_2013_intermediate_40_canopy_percentage) %>% full_join(mean_oak_2013_intermediate_50_canopy_percentage)

#write.csv(all_2013_intermediate_30_40_50_oak_canopy_percentage, "C:/Users/idivjb92jaka/Documents/Thesis Data/Data Frames for Plotted Outputs/2013_30_40_50_oak_canopy_percentage.csv")

plot_2013_ambient_DBH <- all_2013_intermediate_30_40_50_oak_canopy_percentage %>%
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

 
# 2013 moist rates --------------------------------------------------------
############################################################### 2013 moist rates

## max
## max oak 40cm model output
max_oaks_2013_moist_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2013 Rates/Moist/Max/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         # sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

max_oak_2013_moist_40_cohort_sum <- max_oaks_2013_moist_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

max_oak_2013_moist_40_cum_sum <- max_oaks_2013_moist_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

max_oak_2013_moist_40_canopy_percentage <- max_oak_2013_moist_40_cohort_sum %>% left_join(max_oak_2013_moist_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "max") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "moist") %>% mutate(time = time+26)

max_oak_2013_moist_40_canopy_percentage$cum_avg_percentage <- cummean(max_oak_2013_moist_40_canopy_percentage$ca_percent)


## mean
## mean oak 30cm model output
mean_oaks_2013_moist_30_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2013 Rates/moist/Mean/all_cohorts_out_30.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         # sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_2013_moist_30_cohort_sum <- mean_oaks_2013_moist_30_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_2013_moist_30_cum_sum <- mean_oaks_2013_moist_30_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_2013_moist_30_canopy_percentage <- mean_oak_2013_moist_30_cohort_sum %>% left_join(mean_oak_2013_moist_30_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "30 cm") %>% mutate(groundwater_class = "moist") %>% mutate(time = time+26)

mean_oak_2013_moist_30_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_2013_moist_30_canopy_percentage$ca_percent)


## mean oak 40cm model output
mean_oaks_2013_moist_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2013 Rates/Moist/Mean/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         # sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_2013_moist_40_cohort_sum <- mean_oaks_2013_moist_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_2013_moist_40_cum_sum <- mean_oaks_2013_moist_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_2013_moist_40_canopy_percentage <- mean_oak_2013_moist_40_cohort_sum %>% left_join(mean_oak_2013_moist_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "moist") %>% mutate(time = time+26)

mean_oak_2013_moist_40_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_2013_moist_40_canopy_percentage$ca_percent)


## mean oak 50cm model output
mean_oaks_2013_moist_50_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2013 Rates/Moist/mean/all_cohorts_out_50.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         # sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_2013_moist_50_cohort_sum <- mean_oaks_2013_moist_50_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_2013_moist_50_cum_sum <- mean_oaks_2013_moist_50_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_2013_moist_50_canopy_percentage <- mean_oak_2013_moist_50_cohort_sum %>% left_join(mean_oak_2013_moist_50_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "50 cm") %>% mutate(groundwater_class = "moist") %>% mutate(time = time+26)

mean_oak_2013_moist_50_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_2013_moist_50_canopy_percentage$ca_percent)


## min
## min oak 40cm model output
min_oaks_2013_moist_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2013 Rates/Moist/Min/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         # sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

min_oak_2013_moist_40_cohort_sum <- min_oaks_2013_moist_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

min_oak_2013_moist_40_cum_sum <- min_oaks_2013_moist_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

min_oak_2013_moist_40_canopy_percentage <- min_oak_2013_moist_40_cohort_sum %>% left_join(min_oak_2013_moist_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "min") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "moist") %>% mutate(time = time+26)

min_oak_2013_moist_40_canopy_percentage$cum_avg_percentage <- cummean(min_oak_2013_moist_40_canopy_percentage$ca_percent)


## plotting

all_GW_mean_oak_canopy_percentage_2013 <- mean_oak_2013_dry_40_canopy_percentage %>% full_join(mean_oak_2013_intermediate_40_canopy_percentage) %>% full_join(mean_oak_2013_moist_40_canopy_percentage)

#write.csv(all_GW_mean_oak_canopy_percentage_2013, "C:/Users/idivjb92jaka/Documents/Thesis Data/Data Frames for Plotted Outputs/2013_GW_classes_oak_canopy_percentage.csv")

plot_2013_all_GW <- all_GW_mean_oak_canopy_percentage_2013 %>% 
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


################################################################################ 2016 census
# 2016 dry rates ----------------------------------------------------------

## min
## min oak 40cm model output
min_oaks_2016_dry_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2016 Rates/Dry/Min/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         # sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

min_oak_2016_dry_40_cohort_sum <- min_oaks_2016_dry_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

min_oak_2016_dry_40_cum_sum <- min_oaks_2016_dry_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

min_oak_2016_dry_40_canopy_percentage <- min_oak_2016_dry_40_cohort_sum %>% left_join(min_oak_2016_dry_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "min") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "dry") %>% mutate(time = time+26)

min_oak_2016_dry_40_canopy_percentage$cum_avg_percentage <- cummean(min_oak_2016_dry_40_canopy_percentage$ca_percent)


## mean
## mean oak 30cm model output
mean_oaks_2016_dry_30_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2016 Rates/Dry/Mean/all_cohorts_out_30.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         # sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_2016_dry_30_cohort_sum <- mean_oaks_2016_dry_30_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_2016_dry_30_cum_sum <- mean_oaks_2016_dry_30_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_2016_dry_30_canopy_percentage <- mean_oak_2016_dry_30_cohort_sum %>% left_join(mean_oak_2016_dry_30_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "30 cm") %>% mutate(groundwater_class = "dry") %>% mutate(time = time+26)

mean_oak_2016_dry_30_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_2016_dry_30_canopy_percentage$ca_percent)


# mean oak 40cm model output

mean_oaks_2016_dry_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2016 Rates/Dry/Mean/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         # sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_2016_dry_40_cohort_sum <- mean_oaks_2016_dry_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_2016_dry_40_cum_sum <- mean_oaks_2016_dry_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_2016_dry_40_canopy_percentage <- mean_oak_2016_dry_40_cohort_sum %>% left_join(mean_oak_2016_dry_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "dry") %>% mutate(time = time+26)

mean_oak_2016_dry_40_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_2016_dry_40_canopy_percentage$ca_percent)


## mean oak 50cm model output

mean_oaks_2016_dry_50_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2016 Rates/Dry/mean/all_cohorts_out_50.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         # sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_2016_dry_50_cohort_sum <- mean_oaks_2016_dry_50_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_2016_dry_50_cum_sum <- mean_oaks_2016_dry_50_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_2016_dry_50_canopy_percentage <- mean_oak_2016_dry_50_cohort_sum %>% left_join(mean_oak_2016_dry_50_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "50 cm") %>% mutate(groundwater_class = "dry") %>% mutate(time = time+26)

mean_oak_2016_dry_50_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_2016_dry_50_canopy_percentage$ca_percent)


## max
## max oak 40cm model output

max_oaks_2016_dry_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2016 Rates/Dry/Max/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         # sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

max_oak_2016_dry_40_cohort_sum <- max_oaks_2016_dry_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

max_oak_2016_dry_40_cum_sum <- max_oaks_2016_dry_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

max_oak_2016_dry_40_canopy_percentage <- max_oak_2016_dry_40_cohort_sum %>% left_join(max_oak_2016_dry_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "max") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "dry") %>% mutate(time = time+26)

max_oak_2016_dry_40_canopy_percentage$cum_avg_percentage <- cummean(max_oak_2016_dry_40_canopy_percentage$ca_percent)


# 2016 intermediate rates ----------------------------------------------------------

## min
## min oak 40cm model output
min_oaks_2016_intermediate_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2016 Rates/intermediate/Min/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         # sp_name = case_when(as.character(sp) %in% "5" ~ "SAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

min_oak_2016_intermediate_40_cohort_sum <- min_oaks_2016_intermediate_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

min_oak_2016_intermediate_40_cum_sum <- min_oaks_2016_intermediate_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

min_oak_2016_intermediate_40_canopy_percentage <- min_oak_2016_intermediate_40_cohort_sum %>% left_join(min_oak_2016_intermediate_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "min") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "intermediate") %>% mutate(time = time+26)

min_oak_2016_intermediate_40_canopy_percentage$cum_avg_percentage <- cummean(min_oak_2016_intermediate_40_canopy_percentage$ca_percent)
 

# mean
# mean oak 30cm model output
mean_oaks_2016_intermediate_30_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2016 Rates/intermediate/Mean/all_cohorts_out_30.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         # sp_name = case_when(as.character(sp) %in% "5" ~ "SAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_2016_intermediate_30_cohort_sum <- mean_oaks_2016_intermediate_30_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_2016_intermediate_30_cum_sum <- mean_oaks_2016_intermediate_30_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_2016_intermediate_30_canopy_percentage <- mean_oak_2016_intermediate_30_cohort_sum %>% left_join(mean_oak_2016_intermediate_30_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "30 cm") %>% mutate(groundwater_class = "intermediate") %>% mutate(time = time+26)

mean_oak_2016_intermediate_30_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_2016_intermediate_30_canopy_percentage$ca_percent)


## mean oak 40cm model output
mean_oaks_2016_intermediate_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2016 Rates/intermediate/Mean/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         # sp_name = case_when(as.character(sp) %in% "5" ~ "SAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_2016_intermediate_40_cohort_sum <- mean_oaks_2016_intermediate_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_2016_intermediate_40_cum_sum <- mean_oaks_2016_intermediate_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_2016_intermediate_40_canopy_percentage <- mean_oak_2016_intermediate_40_cohort_sum %>% left_join(mean_oak_2016_intermediate_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "intermediate") %>% mutate(time = time+26)

mean_oak_2016_intermediate_40_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_2016_intermediate_40_canopy_percentage$ca_percent)


## mean oak 50cm model output
mean_oaks_2016_intermediate_50_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2016 Rates/intermediate/Mean/all_cohorts_out_50.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         # sp_name = case_when(as.character(sp) %in% "5" ~ "SAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_2016_intermediate_50_cohort_sum <- mean_oaks_2016_intermediate_50_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_2016_intermediate_50_cum_sum <- mean_oaks_2016_intermediate_50_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_2016_intermediate_50_canopy_percentage <- mean_oak_2016_intermediate_50_cohort_sum %>% left_join(mean_oak_2016_intermediate_50_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "50 cm") %>% mutate(groundwater_class = "intermediate") %>% mutate(time = time+26)

mean_oak_2016_intermediate_50_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_2016_intermediate_50_canopy_percentage$ca_percent)

# max
# max oak 40cm model output
max_oaks_2016_intermediate_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2016 Rates/intermediate/Max/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         # sp_name = case_when(as.character(sp) %in% "5" ~ "SAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

max_oak_2016_intermediate_40_cohort_sum <- max_oaks_2016_intermediate_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

max_oak_2016_intermediate_40_cum_sum <- max_oaks_2016_intermediate_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

max_oak_2016_intermediate_40_canopy_percentage <- max_oak_2016_intermediate_40_cohort_sum %>% left_join(max_oak_2016_intermediate_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "max") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "intermediate") %>% mutate(time = time+26)

max_oak_2016_intermediate_40_canopy_percentage$cum_avg_percentage <- cummean(max_oak_2016_intermediate_40_canopy_percentage$ca_percent)


## plotting

all_2016_intermediate_min_mean_max_oak_canopy_percentage <- min_oak_2016_intermediate_40_canopy_percentage %>% full_join(mean_oak_2016_intermediate_40_canopy_percentage) %>% full_join(max_oak_2016_intermediate_40_canopy_percentage) %>% filter(time <= 651)

#write.csv(all_2016_intermediate_min_mean_max_oak_canopy_percentage, "C:/Users/idivjb92jaka/Documents/Thesis Data/Data Frames for Plotted Outputs/2016_min_mean_max_oak_canopy_percentage.csv")

plot_2016_oak_femel_rates <- all_2016_intermediate_min_mean_max_oak_canopy_percentage %>%
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


all_2016_intermediate_30_40_50_oak_canopy_percentage <- mean_oak_2016_intermediate_30_canopy_percentage %>% full_join(mean_oak_2016_intermediate_40_canopy_percentage) %>% full_join(mean_oak_2016_intermediate_50_canopy_percentage) %>% filter(time <= 651)

#write.csv(all_2016_intermediate_30_40_50_oak_canopy_percentage, "C:/Users/idivjb92jaka/Documents/Thesis Data/Data Frames for Plotted Outputs/2016_30_40_50_oak_canopy_percentage.csv")

plot_2016_ambient_DBH <- all_2016_intermediate_30_40_50_oak_canopy_percentage %>%
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



# 2016 moist rates --------------------------------------------------------
############################################################### 2016 moist rates

## min
## min oak 40cm model output

min_oaks_2016_moist_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2016 Rates/Moist/Min/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         # sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

min_oak_2016_moist_40_cohort_sum <- min_oaks_2016_moist_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

min_oak_2016_moist_40_cum_sum <- min_oaks_2016_moist_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

min_oak_2016_moist_40_canopy_percentage <- min_oak_2016_moist_40_cohort_sum %>% left_join(min_oak_2016_moist_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "min") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "moist") %>% mutate(time = time+26)

min_oak_2016_moist_40_canopy_percentage$cum_avg_percentage <- cummean(min_oak_2016_moist_40_canopy_percentage$ca_percent)


## mean
## mean oak 30cm model output

mean_oaks_2016_moist_30_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2016 Rates/moist/Mean/all_cohorts_out_30.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         # sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_2016_moist_30_cohort_sum <- mean_oaks_2016_moist_30_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_2016_moist_30_cum_sum <- mean_oaks_2016_moist_30_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_2016_moist_30_canopy_percentage <- mean_oak_2016_moist_30_cohort_sum %>% left_join(mean_oak_2016_moist_30_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "30 cm") %>% mutate(groundwater_class = "moist") %>% mutate(time = time+26)

mean_oak_2016_moist_30_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_2016_moist_30_canopy_percentage$ca_percent)


# mean oak 40cm model output

mean_oaks_2016_moist_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2016 Rates/Moist/Mean/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         # sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_2016_moist_40_cohort_sum <- mean_oaks_2016_moist_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_2016_moist_40_cum_sum <- mean_oaks_2016_moist_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_2016_moist_40_canopy_percentage <- mean_oak_2016_moist_40_cohort_sum %>% left_join(mean_oak_2016_moist_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "moist") %>% mutate(time = time+26)

mean_oak_2016_moist_40_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_2016_moist_40_canopy_percentage$ca_percent)


# mean oak 50cm model output

mean_oaks_2016_moist_50_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2016 Rates/Moist/Mean/all_cohorts_out_50.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         # sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_2016_moist_50_cohort_sum <- mean_oaks_2016_moist_50_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_2016_moist_50_cum_sum <- mean_oaks_2016_moist_50_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_2016_moist_50_canopy_percentage <- mean_oak_2016_moist_50_cohort_sum %>% left_join(mean_oak_2016_moist_50_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "50 cm") %>% mutate(groundwater_class = "moist") %>% mutate(time = time+26)

mean_oak_2016_moist_50_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_2016_moist_50_canopy_percentage$ca_percent)


## max
## max oak 40cm model output

max_oaks_2016_moist_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/2016 Rates/Moist/Max/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         # sp_name = case_when(as.character(sp) %in% "4" ~ "SAH",
         #                     TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

max_oak_2016_moist_40_cohort_sum <- max_oaks_2016_moist_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

max_oak_2016_moist_40_cum_sum <- max_oaks_2016_moist_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

max_oak_2016_moist_40_canopy_percentage <- max_oak_2016_moist_40_cohort_sum %>% left_join(max_oak_2016_moist_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "max") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "moist") %>% mutate(time = time+26)

max_oak_2016_moist_40_canopy_percentage$cum_avg_percentage <- cummean(max_oak_2016_moist_40_canopy_percentage$ca_percent)


## plotting

all_GW_mean_40_oak_canopy_percentage_2016 <- mean_oak_2016_dry_40_canopy_percentage %>% full_join(mean_oak_2016_intermediate_40_canopy_percentage) %>% full_join(mean_oak_2016_moist_40_canopy_percentage)

#write.csv(all_GW_mean_40_oak_canopy_percentage_2016, "C:/Users/idivjb92jaka/Documents/Thesis Data/Data Frames for Plotted Outputs/2016_GW_classes_oak_canopy_percentage.csv")

plot_2016_all_GW <- all_GW_mean_40_oak_canopy_percentage_2016 %>% 
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



################################################################################ combined census
# combined dry rates ----------------------------------------------------------

## min
## min oak 40cm model output
min_oaks_combined_dry_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/Combined Rates/Dry/Min/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "8" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

min_oak_combined_dry_40_cohort_sum <- min_oaks_combined_dry_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

min_oak_combined_dry_40_cum_sum <- min_oaks_combined_dry_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

min_oak_combined_dry_40_canopy_percentage <- min_oak_combined_dry_40_cohort_sum %>% left_join(min_oak_combined_dry_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "min") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "dry") %>% mutate(time = time+26)

min_oak_combined_dry_40_canopy_percentage$cum_avg_percentage <- cummean(min_oak_combined_dry_40_canopy_percentage$ca_percent)


## mean
## mean oak 30cm model output
mean_oaks_combined_dry_30_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/Combined Rates/Dry/Mean/all_cohorts_out_30.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "8" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_combined_dry_30_cohort_sum <- mean_oaks_combined_dry_30_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_combined_dry_30_cum_sum <- mean_oaks_combined_dry_30_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_combined_dry_30_canopy_percentage <- mean_oak_combined_dry_30_cohort_sum %>% left_join(mean_oak_combined_dry_30_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "30 cm") %>% mutate(groundwater_class = "dry") %>% mutate(time = time+26)

mean_oak_combined_dry_30_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_combined_dry_30_canopy_percentage$ca_percent)


## mean oak 40cm model output
mean_oaks_combined_dry_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/Combined Rates/Dry/Mean/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "8" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_combined_dry_40_cohort_sum <- mean_oaks_combined_dry_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_combined_dry_40_cum_sum <- mean_oaks_combined_dry_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_combined_dry_40_canopy_percentage <- mean_oak_combined_dry_40_cohort_sum %>% left_join(mean_oak_combined_dry_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "dry") %>% mutate(time = time+26)

mean_oak_combined_dry_40_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_combined_dry_40_canopy_percentage$ca_percent)


## mean oak 50cm model output
mean_oaks_combined_dry_50_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/Combined Rates/Dry/Mean/all_cohorts_out_50.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "8" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_combined_dry_50_cohort_sum <- mean_oaks_combined_dry_50_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_combined_dry_50_cum_sum <- mean_oaks_combined_dry_50_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_combined_dry_50_canopy_percentage <- mean_oak_combined_dry_50_cohort_sum %>% left_join(mean_oak_combined_dry_50_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "50 cm") %>% mutate(groundwater_class = "dry") %>% mutate(time = time+26)

mean_oak_combined_dry_50_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_combined_dry_50_canopy_percentage$ca_percent)


## max
## max oak 40cm model output
max_oaks_combined_dry_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/Combined Rates/Dry/Max/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "8" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

max_oak_combined_dry_40_cohort_sum <- max_oaks_combined_dry_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

max_oak_combined_dry_40_cum_sum <- max_oaks_combined_dry_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

max_oak_combined_dry_40_canopy_percentage <- max_oak_combined_dry_40_cohort_sum %>% left_join(max_oak_combined_dry_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "max") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "dry") %>% mutate(time = time+26)

max_oak_combined_dry_40_canopy_percentage$cum_avg_percentage <- cummean(max_oak_combined_dry_40_canopy_percentage$ca_percent)


# combined intermediate rates ----------------------------------------------------------
################################################################ combined intermediate rates

# min
# min oak 40cm model output
min_oaks_combined_intermediate_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/Combined Rates/intermediate/min/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "8" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

min_oak_combined_intermediate_40_cohort_sum <- min_oaks_combined_intermediate_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

min_oak_combined_intermediate_40_cum_sum <- min_oaks_combined_intermediate_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

min_oak_combined_intermediate_40_canopy_percentage <- min_oak_combined_intermediate_40_cohort_sum %>% left_join(min_oak_combined_intermediate_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "min") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "intermediate") %>% mutate(time = time+26)

min_oak_combined_intermediate_40_canopy_percentage$cum_avg_percentage <- cummean(min_oak_combined_intermediate_40_canopy_percentage$ca_percent)


# mean
# mean oak 30cm model output
mean_oaks_combined_intermediate_30_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/Combined Rates/intermediate/Mean/all_cohorts_out_30.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "8" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_combined_intermediate_30_cohort_sum <- mean_oaks_combined_intermediate_30_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_combined_intermediate_30_cum_sum <- mean_oaks_combined_intermediate_30_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_combined_intermediate_30_canopy_percentage <- mean_oak_combined_intermediate_30_cohort_sum %>% left_join(mean_oak_combined_intermediate_30_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "30 cm") %>% mutate(groundwater_class = "intermediate") %>% mutate(time = time+26)

mean_oak_combined_intermediate_30_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_combined_intermediate_30_canopy_percentage$ca_percent)


## mean oak 40cm model output
mean_oaks_combined_intermediate_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/Combined Rates/intermediate/Mean/all_cohorts_out_40.csv") %>% 
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "8" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_combined_intermediate_40_cohort_sum <- mean_oaks_combined_intermediate_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>% 
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_combined_intermediate_40_cum_sum <- mean_oaks_combined_intermediate_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_combined_intermediate_40_canopy_percentage <- mean_oak_combined_intermediate_40_cohort_sum %>% left_join(mean_oak_combined_intermediate_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>% 
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "intermediate") %>% mutate(time = time+26)

mean_oak_combined_intermediate_40_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_combined_intermediate_40_canopy_percentage$ca_percent)


## mean oak 50cm model output
mean_oaks_combined_intermediate_50_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/Combined Rates/intermediate/Mean/all_cohorts_out_50.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "8" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_combined_intermediate_50_cohort_sum <- mean_oaks_combined_intermediate_50_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_combined_intermediate_50_cum_sum <- mean_oaks_combined_intermediate_50_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_combined_intermediate_50_canopy_percentage <- mean_oak_combined_intermediate_50_cohort_sum %>% left_join(mean_oak_combined_intermediate_50_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "50 cm") %>% mutate(groundwater_class = "intermediate") %>% mutate(time = time+26)

mean_oak_combined_intermediate_50_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_combined_intermediate_50_canopy_percentage$ca_percent)


## max
## max oak 40cm model output
max_oaks_combined_intermediate_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/Combined Rates/intermediate/max/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "8" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

max_oak_combined_intermediate_40_cohort_sum <- max_oaks_combined_intermediate_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

max_oak_combined_intermediate_40_cum_sum <- max_oaks_combined_intermediate_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

max_oak_combined_intermediate_40_canopy_percentage <- max_oak_combined_intermediate_40_cohort_sum %>% left_join(max_oak_combined_intermediate_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "max") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "intermediate") %>% mutate(time = time+26)

max_oak_combined_intermediate_40_canopy_percentage$cum_avg_percentage <- cummean(max_oak_combined_intermediate_40_canopy_percentage$ca_percent)


# plotting

all_combined_intermediate_min_mean_max_oak_canopy_percentage <- min_oak_combined_intermediate_40_canopy_percentage %>% full_join(mean_oak_combined_intermediate_40_canopy_percentage) %>% full_join(max_oak_combined_intermediate_40_canopy_percentage)

#write.csv(all_combined_intermediate_min_mean_max_oak_canopy_percentage, "C:/Users/idivjb92jaka/Documents/Thesis Data/Data Frames for Plotted Outputs/combined_min_mean_max_oak_canopy_percentage.csv")

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



all_combined_intermediate_30_40_50_oak_canopy_percentage <- mean_oak_combined_intermediate_30_canopy_percentage %>% full_join(mean_oak_combined_intermediate_40_canopy_percentage) %>% full_join(mean_oak_combined_intermediate_50_canopy_percentage)

#write.csv(all_combined_intermediate_30_40_50_oak_canopy_percentage, "C:/Users/idivjb92jaka/Documents/Thesis Data/Data Frames for Plotted Outputs/combined_30_40_50_oak_canopy_percentage.csv")

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



# combined moist rates --------------------------------------------------------
############################################################## combined moist rates

## min
## min oak 40cm model output

min_oaks_combined_moist_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/Combined Rates/Moist/Min/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "8" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

min_oak_combined_moist_40_cohort_sum <- min_oaks_combined_moist_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

min_oak_combined_moist_40_cum_sum <- min_oaks_combined_moist_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

min_oak_combined_moist_40_canopy_percentage <- min_oak_combined_moist_40_cohort_sum %>% left_join(min_oak_combined_moist_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "min") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "moist") %>% mutate(time = time+26)

min_oak_combined_moist_40_canopy_percentage$cum_avg_percentage <- cummean(min_oak_combined_moist_40_canopy_percentage$ca_percent)


## mean
## mean oak 30cm model output
mean_oaks_combined_moist_30_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/Combined Rates/moist/Mean/all_cohorts_out_30.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "8" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_combined_moist_30_cohort_sum <- mean_oaks_combined_moist_30_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_combined_moist_30_cum_sum <- mean_oaks_combined_moist_30_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_combined_moist_30_canopy_percentage <- mean_oak_combined_moist_30_cohort_sum %>% left_join(mean_oak_combined_moist_30_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "30 cm") %>% mutate(groundwater_class = "moist") %>% mutate(time = time+26)

mean_oak_combined_moist_30_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_combined_moist_30_canopy_percentage$ca_percent)


## mean oak 40cm model output
mean_oaks_combined_moist_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/Combined Rates/Moist/Mean/all_cohorts_out_40.csv") %>% 
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "8" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_combined_moist_40_cohort_sum <- mean_oaks_combined_moist_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>% 
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_combined_moist_40_cum_sum <- mean_oaks_combined_moist_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_combined_moist_40_canopy_percentage <- mean_oak_combined_moist_40_cohort_sum %>% left_join(mean_oak_combined_moist_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>% 
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "moist") %>% mutate(time = time+26)

mean_oak_combined_moist_40_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_combined_moist_40_canopy_percentage$ca_percent)


## mean oak 50cm model output
mean_oaks_combined_moist_50_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/Combined Rates/Moist/mean/all_cohorts_out_50.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "8" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

mean_oak_combined_moist_50_cohort_sum <- mean_oaks_combined_moist_50_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

mean_oak_combined_moist_50_cum_sum <- mean_oaks_combined_moist_50_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

mean_oak_combined_moist_50_canopy_percentage <- mean_oak_combined_moist_50_cohort_sum %>% left_join(mean_oak_combined_moist_50_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "mean") %>% mutate(LL_Rates_cm = "50 cm") %>% mutate(groundwater_class = "moist") %>% mutate(time = time+26)

mean_oak_combined_moist_50_canopy_percentage$cum_avg_percentage <- cummean(mean_oak_combined_moist_50_canopy_percentage$ca_percent)


## max
## max oak 40cm model output
max_oaks_combined_moist_40_output <- read.csv("C:/Users/idivjb92jaka/Documents/Auenwald_Thesis/Model Initializations/Sachsenforst 2023 Oak Femels/Combined Rates/Moist/Max/all_cohorts_out_40.csv") %>%
  mutate(sp_name = case_when(as.character(sp) %in% "1" ~ "BAH",
                             TRUE ~ as.character(sp)),
         sp_name = case_when(as.character(sp) %in% "2" ~ "FAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "3" ~ "GES",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "4" ~ "HBU",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "5" ~ "SAH",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "6" ~ "SEI",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "7" ~ "UL",
                             TRUE ~ as.character(sp_name)),
         sp_name = case_when(as.character(sp) %in% "8" ~ "WLI",
                             TRUE ~ as.character(sp_name))
  )

max_oak_combined_moist_40_cohort_sum <- max_oaks_combined_moist_40_output %>% filter(cl == 1) %>% filter(sp_name == "SEI") %>% group_by(sp_name, cl, time) %>% summarise(ca_cohort_sum = sum(ca_cohort)) %>%
  group_by(time)  %>% select(-cl) %>% mutate(ca_ha = ca_cohort_sum/10000)

max_oak_combined_moist_40_cum_sum <- max_oaks_combined_moist_40_output %>% filter(cl == 1) %>% group_by(cl, time) %>% summarise(cum_ca = sum(ca_cohort)) %>% ungroup() %>% select(-cl)

max_oak_combined_moist_40_canopy_percentage <- max_oak_combined_moist_40_cohort_sum %>% left_join(max_oak_combined_moist_40_cum_sum) %>% mutate(ca_percent = (ca_cohort_sum/cum_ca)*100) %>%
  mutate(oak_rate = "max") %>% mutate(LL_Rates_cm = "40 cm") %>% mutate(groundwater_class = "moist") %>% mutate(time = time+26)

max_oak_combined_moist_40_canopy_percentage$cum_avg_percentage <- cummean(max_oak_combined_moist_40_canopy_percentage$ca_percent)


# main scenario figure plotting  --------------------------------------------------------

all_mean_oak_canopy_percentage_combined <- mean_oak_combined_dry_40_canopy_percentage %>% full_join(mean_oak_combined_intermediate_40_canopy_percentage) %>% full_join(mean_oak_combined_moist_40_canopy_percentage) %>% filter(time <= 400)

#write.csv(all_mean_oak_canopy_percentage_combined, "C:/Users/idivjb92jaka/Documents/Thesis Data/Data Frames for Plotted Outputs/combined_GW_classes_oak_canopy_percentage.csv")

# single femel cover

all_mean_oak_canopy_percentage_combined %>% 
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


# cumulative average cover cumulative average

all_mean_oak_canopy_percentage_combined %>% 
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


# for figure S1

plot_combined_all_GW <- all_mean_oak_canopy_percentage_combined %>% 
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


