
## Author: Markus Schorn

library(ggpubr)
library(tidyverse)

source("[insert file path to the model]")

a=main_cohorts_tidy(mainfolder = "[insert file path to main folder]", 
                    spdata_path = "[insert file path to rates]", 
                    initdata_path = "[insert file path to initial state]", 
                    growth = "['select management+LL30', 'management+LL40', or 'management+LL50']", 
                    mort = "rates")
