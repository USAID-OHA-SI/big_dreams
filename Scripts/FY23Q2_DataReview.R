# PROJECT:  big_dreams
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  FY23 Q2 data review: 
#           Replacing PrEP calc for FY22 Data Review spreadsheet
# REF ID:   890ae9f5
# LICENSE:  GPL v3 +
# DATE:     2023-06-02
# UPDATED:

# DEPENDENCIES ----------------------------------------------------------------

library(tidyverse)
library(extrafont)
library(gagglr)
library(janitor)
library(gt)
library(scales)
library(ggplot2)
library(glue)
library(ggtext)

# IMPORT ----------------------------------------------------------------------

# SI specific paths/functions
load_secrets()

merdata <- file.path(si_path("path_msd"))

file_path <- return_latest(folderpath = merdata,
                           pattern = "PSNU_IM_DREAMS")

# Grab metadata
get_metadata(file_path)

df <- read_psd(file_path)

ref_id <- "890ae9f5"

date <- today()

# MUNGE ------------------------------------------------------------------------

# quarterly achievement for PrEP_NEW
df_filt_prep <- df %>%
  clean_indicator() %>%
  clean_agency() %>%
  filter(fiscal_year %in% c("2022", "2023"), 
         indicator == "PrEP_NEW", 
         funding_agency %in% c("USAID", "CDC"), 
         sex == "Female",
         ageasentered %in% c("10-14", "15-19", "20-24", "25-29")) %>%
  group_by(fiscal_year, funding_agency, indicator) %>%
  summarise((across(c(starts_with("qtr"), cumulative, targets),
                    \(x) sum(x, na.rm = TRUE)))) %>%
  reshape_msd(direction = "quarters", include_type = FALSE) %>%
  adorn_achievement() %>%
  arrange(period)

# CES services provided in FY23 Q2

df_filt_cesprov <- df %>%
  clean_indicator() %>%
  clean_agency() %>%
  filter(fiscal_year == "2023",
         standardizeddisaggregate == "ComprehensiveEconomicStrengthening") %>%
  group_by(fiscal_year, operatingunit) %>%
  summarise((across(qtr2,\(x) sum(x, na.rm = TRUE)))) %>%
  arrange(operatingunit)

# number of DSNUs reporting CES - denominator of pct
# /total number of districts in each OU in CES

# df_filt_ces <- df %>%
#   clean_indicator() %>%
#   clean_agency() %>%
#   filter(fiscal_year == "2023",
#          standardizeddisaggregate %in% c("ComprehensiveEconomicStrengthening",
#                                          "EducationSupport",
#                                          "ViolencePrevention")) %>%
#   group_by(fiscal_year, operatingunit, dsnu) %>%
#   count() %>%
#   group_by(fiscal_year, operatingunit) %>%
#   summarise(dsnus_reporting_servicetype = sum(n))

# number of dsnus per ou reporting providing CES in Q2 - numerator of pct
# of districts who reported CES in FY23Q2

# df_filt_cesprov <- df %>%
#   clean_indicator() %>%
#   clean_agency() %>%
#   filter(fiscal_year == "2023",
#          standardizeddisaggregate == "ComprehensiveEconomicStrengthening") %>%
#   group_by(fiscal_year, operatingunit, dsnu) %>%
#   count() %>%
#   group_by(fiscal_year, operatingunit) %>%
#   summarise(dsnus_reporting_ces = sum(n))

# combine

# df_pct_ces <- df_filt_cesprov %>%
#   left_join(df_filt_ces, by = c("fiscal_year", "operatingunit")) %>%
#   mutate(pct_reporting_ces = dsnus_reporting_ces/dsnus_reporting_servicetype)
# 

# visual -----------------------------------------------------------------------

# What does the quarterly achievement for PrEP_NEW look like by agency in 
# FY22 and FY23?

df_filt_prep %>%
  ggplot(aes(x = period)) +
  geom_col(aes(y = targets), alpha = 0.5, fill = usaid_lightgrey,
           position = position_dodge(width = .65)) +
  geom_col(aes(y = results_cumulative, alpha = .8, fill = funding_agency),
           position = position_dodge(width = .65)) +
  facet_wrap(~funding_agency) +
  geom_text(aes(label = glue("{percent(achievement_qtrly)}"), y = 0),
            position = position_dodge(width = 0.75), color = "#FFFFFF",
            family = "Source Sans Pro", size = 12 / .pt,
            vjust = -.5, na.rm = TRUE) +
  # geom_text(aes(label = glue("{comma(results_cumulative)}")
  #               , y = results_cumulative),
  #           position = position_dodge(width = 0.75), color = usaid_darkgrey,
  #           family = "Source Sans Pro", size = 12 / .pt,
  #           vjust = -.5, na.rm = TRUE) +
  scale_x_discrete(breaks = unique(df_filt$period)[grep("Q(2|4)", unique(df_filt$period))]) +
  scale_fill_manual(values = c("USAID" = denim,
                               "CDC" = scooter_med)) +
  scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
  labs(
    x = NULL, y = NULL, fill = NULL, group = NULL,
    caption = glue("{metadata$caption} | Ref id: {ref_id} | OHA/SI Analytics")) +
  si_style_yline() +
  theme(
    panel.spacing = unit(.5, "line"),
    legend.position = "none",
    plot.title = element_blank(),
    strip.text = element_markdown()
  )

si_save(glue("Images/FY23Q2_DataReview_slide10_{date}.png"))

# How many CES services were provided in FY22 Q2?

# Kenya
ces_kenya <- df_filt_cesprov %>%
  filter(operatingunit == "Kenya") %>%
  ggplot() +
  geom_col(aes(y = forcats::fct_reorder(operatingunit, qtr2), 
               x = qtr2, alpha = .9), fill = genoa,
               position = position_dodge(width = .65)) +
  scale_x_continuous(label = label_number(scale_cut = cut_short_scale())) +
  labs(
    x = NULL, y = NULL,
    subtitle = glue("CES Services Provided in {metadata$curr_pd}")) +
  si_style_xgrid() +
  theme(
    plot.subtitle = element_text(family = "Source Sans Pro", size = 22),
    axis.text = element_text(family = "Source Sans Pro", size = 16),
    panel.spacing = unit(.5, "line"),
    legend.position = "none",
    plot.title = element_blank()
    )

# rest of OUs
ces_notkenya <- df_filt_cesprov %>%
  filter(operatingunit != "Kenya") %>%
  ggplot() +
  geom_col(aes(y = forcats::fct_reorder(operatingunit, qtr2), 
               x = qtr2, alpha = .9), fill = genoa,
           position = position_dodge(width = .65)) +
  scale_x_continuous(label = label_number(scale_cut = cut_short_scale())) +
  labs(
    x = NULL, y = NULL,
    caption = glue("{metadata$caption} | Ref id: {ref_id} | OHA/SI Analytics")) +
  si_style_xgrid() +
  theme(
    axis.text = element_text(family = "Source Sans Pro", size = 16),
    panel.spacing = unit(.5, "line"),
    legend.position = "none",
    plot.title = element_blank()
  )

# TODO: fix this and include labels on the slide with how many Namibia has + 
# label for Kenya
cowplot::plot_grid(ces_kenya, ces_notkenya, 1:1, nrow = 2, ncol = 1)

si_save(glue("Images/FY23Q2_DataReview_slide12left_{date}.png"))

# What percentage of DSNUs are reporting CES in FY23 Q2 by OU?


         
         
         
         