# PROJECT:  big_dreams
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  FY23 Q2 data review: 
#           Replacing PrEP calc for FY22 Data Review spreadsheet
# REF ID:   890ae9f5
# LICENSE:  GPL v3 +
# DATE:     2023-05-31
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

# MUNGE ------------------------------------------------------------------------

# mention to LW discrep. w/ spreadsheet
df_filt <- df %>%
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

# visual -----------------------------------------------------------------------

# What does the quarterly achievement for PrEP_NEW look like by agency in 
# FY22 and FY23?

df_filt %>%
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

si_save("Images/FY23Q2_DataReview_slide10.png")
         
         
         
         