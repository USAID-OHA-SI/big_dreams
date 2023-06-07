# PROJECT:  big_dreams
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  FY23 Q2 request for DREAMS program review
# REF ID:   890bz935
# LICENSE:  GPL v3 +
# DATE:     2023-06-07

# DEPENDENCIES ----------------------------------------------------------------

library(tidyverse)
library(extrafont)
library(gagglr)
library(janitor)
library(glue)
library(gt)
library(scales)
library(assertr)

# IMPORT ----------------------------------------------------------------------

# SI specific paths/functions
load_secrets()

# list of files to read in 

merdata <- file.path(si_path("path_msd"))

# "FY17-20" file only has AGYW_PREV data from 2019 and does not include South Sudan
# "FY17-18" file does not contain AGYW_PREV
# "FY18-21" file contains AGYW_PREV data from 2019-2020 only and does not contain 
# AGYW_PREV for SS, first year for data from SS comes from 2021
file_path <- return_latest(folderpath = merdata,
                           pattern = "PSNU_IM_DREAMS_FY18")

# MUNGE -----------------------------------------------------------------------
  
df_1821 <- read_psd(file_path) %>%
  filter(indicator == "AGYW_PREV")

summarized_1821 <- df_1821 %>%
  group_by(fiscal_year, operatingunit, dsnu,
           indicator, numeratordenom, standardizeddisaggregate, 
           otherdisaggregate_sub, sex, ageasentered) %>%
  summarise(n = sum(cumulative, na.rm = T), .groups = "drop") %>%
  filter(stringr::str_detect(standardizeddisaggregate, "Total") == FALSE, 
         sex == "Female", 
         numeratordenom == "D")

# EXPORT ----------------------------------------------------------------------

summarized_1821 %>%
  write_excel_csv("Dataout/AGYW_PREV_FY19-20.csv")
