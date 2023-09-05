# PROJECT:  big_dreams
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  FY23 Q2 request to pull total number of makes 18-49 who have either 
#           been reached by DREAMS_GEND_NORM or violence prevention activities in 
#           FY22 Q1-Q3
# REF ID:   371be9e6
# LICENSE:  GPL v3 +
# DATE:     2023-06-26
# UPDATED:

# DEPENDENCIES ----------------------------------------------------------------

library(tidyverse)
library(readr)
library(gagglr)
library(glue)
library(scales)
library(extrafont)
library(tidytext)
library(patchwork)
library(ggtext)
library(googlesheets4)
library(ggrepel)
library(janitor)

# FUNCTIONS --------------------------------------------------------------------


# global vars -----------------------------------------------------------------

ref_id <- "371be9e6"
date <- today()
path <- "Data/"

# import -----------------------------------------------------------------------

load_secrets()

# read in data from CI Dashboard's Result Trends by Indicators page
# selected FY22 Q1-Q3, DREAMS_GEND_NORM, All OUs, all psnus, all Mech Names, 
#          ages 15-49, Males, Activity Type disgag, Population/Other = "All"
df_gend_norm_n <-  path %>% 
  return_latest("AgeSex TRENDS_data") %>% 
  read_csv()

# read in data from CI Dashboard's Reporting Trends page
# selected Report Full Data for Country Reporting Measure, DREAMS, All OUs, psnus, Mech Names
df_reporting_n <-  path %>% 
  return_latest("Reporting_Full Data_data") %>% 
  read_csv()

# MUNGE ------------------------------------------------------------------------


# Number of males ages 15-49 who received DREAMS_GEND_NORM services in FY22 Q1-Q3

df_filt_ppl <- df_gend_norm_n %>%
  clean_names() %>%
  group_by(indicator, sex) %>%
  summarise((across(val,\(x) comma(sum(x, na.rm = TRUE))))) %>%
  ungroup() 

# Number of OUs who reported DREAMS_GEND_NORM for FVY22 Q1-Q3

df_filt_ous <- df_reporting_n %>%
  clean_names() %>%
  filter(indicator_n_d == "DREAMS_GEND_NORM") %>%
  distinct(country) %>%
  arrange()