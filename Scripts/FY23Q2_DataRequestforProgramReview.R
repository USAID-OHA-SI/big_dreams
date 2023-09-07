# PROJECT:  big_dreams
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  FY23 data requests for DREAMS program review, requested by Brian 
# and/or Beth 
# REF ID:   890bz935
# LICENSE:  GPL v3 +
# DATE:     2023-09-07

# DEPENDENCIES ----------------------------------------------------------------

library(tidyverse)
library(extrafont)
library(gagglr)
library(janitor)
library(glue)
library(gt)
library(scales)
library(assertr)

# global vars ------------------------------------------------------------------

# requested indicators
req_inds <- c("AGYW_PREV", "HTS_TST", "HTS_TST_NEG", "HTS_TST_POS", "GEND_GBV", 
              "OVC_SERV", "PP_PREV", "PREP_NEW", "TX_NEW", "TX_CURR", "VMMC_CIRC")

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

df_1821 <- read_psd(file_path) %>%
  filter(indicator == "AGYW_PREV")

# "FY18-21" full data set
# program review team requested the following:
# OU: Zimbabwe
# Year: FY19-20
# Indicators: see list above for requested indicators
file_path_full <- return_latest(folderpath = merdata,
                           pattern = "PSNU_IM_FY18-21")

df_1821_full <- read_psd(file_path_full) 

# MUNGE -----------------------------------------------------------------------

summarized_1821 <- df_1821 %>%
  group_by(fiscal_year, operatingunit, dsnu,
           indicator, numeratordenom, standardizeddisaggregate, 
           otherdisaggregate_sub, sex, ageasentered) %>%
  summarise(n = sum(cumulative, na.rm = T), .groups = "drop") %>%
  filter(stringr::str_detect(standardizeddisaggregate, "Total") == FALSE, 
         sex == "Female", 
         numeratordenom == "D")

df_filt_zim_1920 <- df_1821_full %>%
  filter(operatingunit == "Zimbabwe", 
         fiscal_year %in% c("2019", "2020"), 
         indicator %in% req_inds)

# EXPORT ----------------------------------------------------------------------

summarized_1821 %>%
  write_excel_csv("Dataout/AGYW_PREV_FY19-20.csv")

df_filt_zim_1920 %>%
  write_excel_csv("Dataout/Zimbabwe_FY19-20_selectedindicators.csv")
