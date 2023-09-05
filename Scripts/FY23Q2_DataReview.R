# PROJECT:  big_dreams
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  FY23 Q2 data review visuals made in addition to those from DREAMS and 
#           CI Tableau dashboards
# REF ID:   890ae9f5
# LICENSE:  GPL v3 +
# DATE:     2023-07-25
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

# global vars -----------------------------------------------------------------

ref_id <- "890ae9f5"
date <- today()

# import -----------------------------------------------------------------------

load_secrets()

df_msd <- si_path() %>% 
  return_latest("PSNU_IM_DREAMS_FY21-23") %>% 
  read_psd()

get_metadata(type = "MER")
metadata_msd <- metadata
rm(metadata)

#Downloaded from the Primary Package Completion 13+ months page in the DREAMS workbook
df_tableau <- readxl::read_excel(here::here("Data/FY23Q2_DREAMSPrimaryPackageCompletion.xlsx"))

# MUNGE ------------------------------------------------------------------------
 
# for edited Tableau visual from DREAMS Primary Package Completion tab -----

df_ppc <- df_tableau %>%
  clean_names() %>%
  rename(ou = x1, 
         completion = x2, 
         pct = percent_contribution_along_standardized_disaggregate_2_levels) %>%
  # fix ou col
  fill(ou) %>%
  filter(completion == "Primary Package Completed") %>%
  add_row(
    ou = "Cote d'Ivoire**", 
    completion = "Primary Package Completed",
    cumulative = 0, 
    pct = 0) %>%
  add_row(
    ou = "South Sudan**", 
    completion = "Primary Package Completed",
    cumulative = 0, 
    pct = 0) %>%
  group_by(ou, completion) %>%
  # create color grouping by percent completion
  mutate(
    cumul_lab = if_else(pct > 0.9 | pct == .9, 
                        glue("{label_number(1.1, scale_cut = cut_short_scale())(cumulative)}"),
                        ""),
    color_cat = if_else(pct < 0.9, 1, 2), 
    pct_lab = if_else(pct > 0.9, 
                      glue::glue("{scales::percent(pct, 2)}"), 
                      glue::glue("")))


# quarterly achievement for PrEP_NEW ----
# hold for DREAMS deep dive
df_filt_prep <- df_msd %>%
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

# CES services provided in FY23 Q2 -----

df_filt_cesprov <- df_msd %>%
  clean_indicator() %>%
  clean_agency() %>%
  filter(fiscal_year == "2023",
         standardizeddisaggregate == "ComprehensiveEconomicStrengthening") %>%
  group_by(fiscal_year, operatingunit) %>%
  summarise((across(qtr2,\(x) sum(x, na.rm = TRUE)))) %>%
  arrange(operatingunit) %>%
  ungroup() %>%
  add_row(
    fiscal_year = 2023,
    operatingunit = "Uganda*", 
    qtr2 = 0) %>%
  add_row(
    fiscal_year = 2023,
    operatingunit = "South Sudan*", 
    qtr2 = 0) %>%
  add_row(
    fiscal_year = 2023,
    operatingunit = "Lesotho*", 
    qtr2 = 0) %>%
  mutate(
    qtr_lab = glue("{label_number(1.1, scale_cut = cut_short_scale())(qtr2)}"), 
    qtr_lab = if_else(qtr2 > 20000, qtr_lab, glue::glue("")),
    color_cat = if_else(qtr2 < 20000, 1, 2))

# number of DSNUs in each OU who reported data for FY23Q2

df_ndsnus <- df_msd %>%
  clean_indicator() %>%
  filter(fiscal_year == "2023",
         is.na(qtr2) == FALSE) %>%
  group_by(operatingunit, dsnu) %>%
  select(operatingunit, dsnu) %>%
  distinct() %>%
  count(dsnu) %>%
  ungroup %>%
  group_by(operatingunit) %>%
  reframe(n_dsnus = sum(n))

# number of DSNUS reporting CES for FY23Q2

df_nces <- df_msd %>%
  clean_indicator() %>%
  filter(fiscal_year == "2023",
         standardizeddisaggregate == "ComprehensiveEconomicStrengthening", 
         is.na(qtr2) == FALSE) %>%
  group_by(operatingunit, dsnu) %>%
  select(operatingunit, dsnu) %>%
  distinct() %>%
  count(dsnu) %>%
  ungroup %>%
  group_by(operatingunit) %>%
  reframe(n_dsnus_ces = sum(n))

# df for viz

df_pct_ces <- left_join(df_ndsnus,df_nces) %>%
  mutate(
    pct = n_dsnus_ces/n_dsnus, 
    pct = if_else(is.na(pct), 0, pct),
    color_cat = if_else(pct < 1, 2, 1),
    pct_lab = if_else(pct != 1 & pct != 0, 
                      glue::glue("{scales::percent(pct, 1)}"), 
                      glue::glue("")), 
    operatingunit = if_else(pct == 0, glue("{operatingunit}*"), operatingunit))

# Education Services ----

df_filt_eduprov <- df_msd %>%
  clean_indicator() %>%
  filter(fiscal_year == "2023",
         standardizeddisaggregate == "EducationSupport") %>%
  group_by(fiscal_year, operatingunit) %>%
  summarise((across(qtr2,\(x) sum(x, na.rm = TRUE)))) %>%
  arrange(operatingunit) %>%
  ungroup() %>%
  add_row(
    fiscal_year = 2023,
    operatingunit = "Botswana*", 
    qtr2 = 0) %>%
  add_row(
    fiscal_year = 2023,
    operatingunit = "South Sudan*", 
    qtr2 = 0) %>%
  add_row(
    fiscal_year = 2023,
    operatingunit = "South Africa*", 
    qtr2 = 0) %>%
  mutate(
    qtr_lab = glue("{label_number(1.1, scale_cut = cut_short_scale())(qtr2)}"), 
    qtr_lab = if_else(qtr2 > 20000, qtr_lab, glue::glue("")),
    color_cat = if_else(qtr2 < 20000, 1, 2))
  
# visualize --------------------------------------------------------------------

# What does the quarterly achievement for PrEP_NEW look like by agency in 
# FY22 and FY23?

df_filt_prep %>%
  ggplot(aes(x = period)) +
  geom_col(aes(y = targets), alpha = 0.5, fill = usaid_lightgrey,
           position = position_dodge(width = .65)) +
  geom_col(aes(y = results_cumulative, alpha = .9, fill = funding_agency),
           position = position_dodge(width = .65)) +
  facet_wrap(~funding_agency) +
  geom_text(aes(label = glue("{percent(achievement_qtrly)}"), y = 0),
            position = position_dodge(width = 0.75), color = "#FFFFFF",
            family = "Source Sans Pro", size = 12 / .pt,
            vjust = -.5, na.rm = TRUE) +
  scale_x_discrete(breaks = unique(df_filt_prep$period)[grep("Q(2|4)", unique(df_filt_prep$period))]) +
  scale_fill_manual(values = c("USAID" = usaid_medblue,
                               "CDC" = scooter_med)) +
  scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
  labs(
    x = NULL, y = NULL, fill = NULL, group = NULL,
    caption = glue("{metadata_msd$caption} | Ref id: {ref_id} | OHA/SI Analytics")) +
  si_style_yline() +
  theme(axis.text = element_text(family = "Gill Sans MT", 
                                 color = usaid_darkgrey, size = 14),
        legend.position = "none",
        strip.text = element_text(family = "Gill Sans MT", 
                                  color = usaid_darkgrey, size = 14),
        panel.spacing = unit(.5, "picas"))


si_save(glue("Images/FY23Q2_PrEPAgencyComparison_{date}.png"))

# How many CES services were provided in FY22 Q2?

df_filt_cesprov %>%
  filter(operatingunit != "Kenya") %>%
  ggplot(aes(y = forcats::fct_reorder(operatingunit, qtr2), x = qtr2)) +
  geom_col(aes(group = as.factor(color_cat), fill = as.factor(color_cat)),position = position_dodge(width = .65)) +
  geom_text(aes(label = qtr_lab), hjust = 1.5, family = "Gill Sans MT", 
            color = "#FFFFFF", size = 4.5) +
  scale_x_continuous(label = label_number(scale_cut = cut_short_scale())) +
  scale_fill_manual(values = c("#9e94e0", "#000a45")) +
  labs(x = NULL, y = NULL,
       caption = glue("Note: *These OUs did not report any data for this service type for this period. 
                      Data reported from Kenya (289,9883) has been excluded as an outlier.
                      Figure shows all active DREAMS participants who have started or completed any 
                      DREAMS service/intervention as of the end of the reporting period (AGYW_PREV denominator) in PEPFAR
                      Source: FY23Q2c MSD | USAID DREAMS & SI| Ref id: {ref_id}| Created using FY23Q2_DataReview.R")) +
  si_style_xline() +
  theme(axis.text = element_text(family = "Gill Sans MT", 
                                 color = usaid_darkgrey, size = 14),
        legend.position = "none",
        strip.text = element_markdown(),
        panel.spacing = unit(.5, "picas"))

si_save(glue("Images/FY23Q2_CESbyOU_{date}.png"))

# What percentage of DSNUs are reporting CES in FY23 Q2 by OU?
df_pct_ces %>%
  filter(operatingunit != "Kenya") %>%
  ggplot(aes(y = forcats::fct_reorder(operatingunit, -pct), x = pct)) +
  geom_col(aes(group = as.factor(color_cat), fill = as.factor(color_cat)), position = position_dodge(width = .65)) +
  geom_text(aes(label = pct_lab), hjust = 1.5, family = "Gill Sans MT", 
            color = "#FFFFFF", size = 4.5) +
  scale_x_continuous(labels = scales::percent, name = NULL, 
                     limits = c(0,1.1), 
                     breaks = seq(0,1.1, by = .25),
                     oob = oob_squish) + 
  scale_fill_manual(values = c("#9e94e0", "#000a45")) +
  labs(x = NULL, y = NULL,
       caption = glue("Note: *These OUs did not report any data for this service type this period. 
                      Data reported from Kenya has been excluded as an outlier.
                      Source: FY23Q2i MSD | USAID DREAMS & SI| Ref id: {ref_id}| Created using FY23Q2_DataReview.R")) +
  si_style_xline() +
  theme(axis.text = element_text(family = "Gill Sans MT", 
                                 color = usaid_darkgrey, size = 14),
        legend.position = "none",
        strip.text = element_markdown(),
        panel.spacing = unit(.5, "picas"))

si_save(glue("Images/FY23Q2_CESPCTReportedbyOU_{date}.png"))

viz_hiv_prev_DREAMS(df_prev)

si_save(glue("Images/HIV_prev_AGYW_ABYW_15_24_{cntry}_{date}.png"))

viz_viral_load_kp_agyw(df_vl)

si_save(glue("Images/VLC_VLS_gaps_AGYW_ABYW_15_24_{cntry}_{date}.png"))

df_ppc %>%
  ggplot(aes(x = pct, y = fct_reorder(ou, pct), 
             group = as.factor(color_cat), fill = as.factor(color_cat))) +
  geom_col() +
  geom_text(aes(label = pct_lab), hjust = -0.25,
            family = "Gill Sans MT", fontface = "bold", 
            color = "#001b0e", size = 4.5) +
  geom_text(aes(label = cumul_lab), hjust = 2.5,
            family = "Gill Sans MT", 
            color = "#FFFFFF", size = 4.5) +
  geom_vline(xintercept = 0.9, linetype = "dashed", color = "#D3D3D3") +
  scale_fill_manual(values = c("#5cac9e", "#001b0e")) +
  si_style_xline() +
  scale_x_continuous(labels = scales::percent, name = NULL, 
                     limits = c(0,1.0), 
                     breaks = seq(0,1.1, by = .25),
                     oob = oob_squish) + 
  labs(x = NULL, y = NULL,
       caption = glue("Note: *AGYW are females ages 10-24
                             **Data were not reported from South Sudan or Cote d'Ivoire, 
                               Numbers on bars show the number of AGYW in DREAMS for 13+ months in each OU
         Source: DREAMS Quarterly Workbook Data Download from Primary Package Completion 13+ Months tab, FY23Q2c
          USAID DREAMS & SI | Ref id: {ref_id}| Created using FY23Q2_DataReview.R")) +
  theme(axis.text = element_text(family = "Gill Sans MT", 
                                 color = usaid_darkgrey, size = 14),
        legend.position = "none",
        strip.text = element_markdown(),
        panel.spacing = unit(.5, "picas"))

si_save(glue("Images/AGYW_percent_primary_completion_{date}.png"))      

# How many education services were provided?

df_filt_eduprov %>%
  filter(operatingunit != "Kenya") %>%
  ggplot(aes(y = forcats::fct_reorder(operatingunit, qtr2), x = qtr2)) +
  geom_col(aes(group = as.factor(color_cat), fill = as.factor(color_cat)),
           position = position_dodge(width = .65)) +
  geom_text(aes(label = qtr_lab), hjust = 1.5, family = "Gill Sans MT", 
            color = "#FFFFFF", size = 4.5) +
  scale_x_continuous(label = label_number(scale_cut = cut_short_scale())) +
  scale_fill_manual(values = c("#fc7a83", "#480000")) +
  labs(x = NULL, y = NULL,
       caption = glue("Note: *These OUs did not report any data for this service type for this period. 
                      Data reported from Kenya (113,581) has been excluded as an outlier.
                      Numbers on bars show the number of AGYW who received educational services by OU
                      Source: FY23Q2c MSD | USAID DREAMS & SI| Ref id: {ref_id}| Created using FY23Q2_DataReview.R")) +
  si_style_xline() +
  theme(axis.text = element_text(family = "Gill Sans MT", 
                                 color = usaid_darkgrey, size = 14),
        legend.position = "none",
        strip.text = element_markdown(),
        panel.spacing = unit(.5, "picas"))

si_save(glue("Images/FY23Q2_EDUbyOU_{date}.png"))
