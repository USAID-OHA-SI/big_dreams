# PROJECT:  big_dreams
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  FY23 Q2 QC: Recreating the data in each page as
#                       a table for quick checking
# REF ID:   899ad9f4
# LICENSE:  GPL v3 +
# DATE:     2023-05-23
# UPDATED:

# DEPENDENCIES ----------------------------------------------------------------

library(tidyverse)
library(extrafont)
library(gagglr)
library(janitor)
library(gt)
library(scales)

# IMPORT ----------------------------------------------------------------------

# SI specific paths/functions
load_secrets()

merdata <- file.path(si_path("path_msd"))

file_path <- return_latest(folderpath = merdata,
                           pattern = "PSNU_IM_DREAMS")

# Grab metadata
get_metadata(file_path)

df <- read_psd(file_path)

# MUNGE -----------------------------------------------------------------------

df_recent <- df %>%
  filter(fiscal_year == max(fiscal_year))

# Global PEPFAR DREAMS numbers
# All OUs, All age bands, FY23Q2

df_recent %>%
  filter(indicator == "AGYW_PREV",
         ageasentered %in% c("10-14", "15-19", "20-24")) %>%
  clean_indicator() %>%
  group_by(fiscal_year, indicator) %>%
  summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE))) %>%
  reshape_msd(include_type = FALSE) %>%
  pivot_wider(
    names_from = indicator,
    names_glue = "{tolower(indicator)}") %>%
  filter(period == metadata$curr_pd) %>%
  mutate(reached = comma(agyw_prev_d), 
         completed = comma(agyw_prev)) %>%
  select(period, reached, completed)

df_recent %>%
  filter(indicator == "AGYW_PREV",
         standardizeddisaggregate %in% c("Age/Sex/Time/Started", 
                                         "Age/Sex/Time/Incomplete",
                                         "Age/Sex/Time/Complete",
                                         "Age/Sex/Time/Complete+"),
         ageasentered %in% c("10-14", "15-19", "20-24")) %>%
  clean_indicator() %>%
  group_by(fiscal_year, indicator, standardizeddisaggregate) %>%
  summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE))) %>%
  reshape_msd(include_type = FALSE) %>%
  pivot_wider(
    names_from = indicator,
    names_glue = "{tolower(indicator)}") %>%
  filter(period == metadata$curr_pd) %>%
  select(-agyw_prev) %>%
  mutate(value = comma(agyw_prev_d), 
         standardizeddisaggregate = factor(standardizeddisaggregate, levels = c("Age/Sex/Time/Started", "Age/Sex/Time/Incomplete", 
                                                      "Age/Sex/Time/Complete", "Age/Sex/Time/Complete+")),
         std_disagg_label = case_when(
           standardizeddisaggregate == "Age/Sex/Time/Started" ~ "DREAMS AGYW who started but did not complete a service",
           standardizeddisaggregate == "Age/Sex/Time/Incomplete" ~ "DREAMS AGYW who completed at least one service but not the primary package",
           standardizeddisaggregate == "Age/Sex/Time/Complete" ~ "DREAMS AGYW who completed the primary package but not a secondary service",
           standardizeddisaggregate == "Age/Sex/Time/Complete+" ~ "DREAMS AGYW who completed the primary package and at least one secondary service")) %>%
  select(period, std_disagg_label, value)
