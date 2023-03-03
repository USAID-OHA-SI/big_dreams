# PROJECT:  big_dreams
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  Understand the AGYW_PREV Results Against Targets viz in R
# REF ID:   dcd3c09d 
# LICENSE:  MIT
# DATE:     2023-03-03
# UPDATED: 

# DEPENDENCIES ----------------------------------------------------------------
  
  library(tidyverse)
  library(extrafont)
  library(gagglr)
  library(readr)
  library(janitor)
  library(forcats)
  library(scales)
  library(glue)
  library(ggtext)

# Summarizes cumulative quarterly progress on selected indicators

ou_achv_cumul <- function(.path, .indicator, .ou, 
                          .type, .title, .funding_agency, ...) {
  
  # reference id for this figure
  ref_id <- "4c86a407"
  
  df_ou <- si_path() %>%
    return_latest(.path) %>%
    read_msd() %>%
    clean_agency()
  
  curr_pd <- as.character(source_info(si_path() %>% 
                                        return_latest("MER_Structured_Datasets_OU_IM"),
                                      return = "period"))
  
  # Validating the assumption that the current fiscal year label 
  # matches the format we expect
  validate_that(str_detect(curr_pd, "FY[1-2][0-9]Q[1-4]"), 
                msg = glue("Error in reading the current period from the input MSD. 
          Please check the cumul_achv function and the MSD you are reading in."))
  
  curr_fy_lab <- as.character(source_info(si_path() %>%
                                            return_latest("MER_Structured_Datasets_OU_IM"),
                                          return = "fiscal_year_label"))
  
  # Validating the assumption that the current fiscal year label 
  # matches the format we expect
  validate_that(str_detect(curr_fy_lab, "FY[1-2][0-9]"), 
                msg = glue("Error in reading the current fiscla year from the input MSD. 
          Please check the cumul_achv function and the MSD you are reading in."))
  
  qtrs_to_keep <- curr_pd %>%
    convert_qtr_to_date() %>%
    seq.Date(by = "-3 months", length = 6) %>%
    convert_date_to_qtr()
  
  # Validating the assumption that we are only keeping the last 6 qtrs
  validate_that(str_length(qtrs_to_keep)[1] == 6, 
                msg = "Error in retaining the expected number of quarters. 
          Please check the cumul_achv function and the MSD you are reading in.")
  
  # filter for type, Total or Adults/Children
  {if (.type == "Total") {
    
    df_ou <- df_ou %>%
      filter(
        indicator %in% .indicator,
        operatingunit == .ou) %>%
      resolve_knownissues() %>%
      pluck_totals()
    
  }
    else {
      
      peds <- c("<01", "01-04", "05-09", "10-14")
      adults <- c(
        "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
        "50-54", "55-59", "60-64", "65+")
      
      df_ou <- df_ou %>%
        filter(
          indicator %in% .indicator,
          operatingunit %in% .ou,
          (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% peds) |
            (standardizeddisaggregate == "Total Numerator") |
            (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% adults)) %>%
        resolve_knownissues() %>%
        mutate(type = case_when(
          standardizeddisaggregate == "Total Numerator" ~ "Total",
          standardizeddisaggregate == "Age/Sex/HIVStatus" &
            ageasentered %in% adults ~ "Adults (15+)",
          standardizeddisaggregate == "Age/Sex/HIVStatus" &
            ageasentered %in% peds ~ "Children (<15)")) %>%
        # verify the assumption that the categories were created correctly
        verify(if_else(ageasentered %in% peds, type == "Children (<15)",
                       type == "Total" | type == "Adults (15+)"),
               error_fun = err_text(glue("Age Type Children (<15) has not been created correctly. 
                                Please check the filter for type section in cumul_achv() in loom.R")),
               description = glue("Verify that the Child category was created correctly")) %>%
        verify(if_else(ageasentered %in% adults, type == "Adults (15+)",
                       type == "Total" | type == "Children (<15)"),
               error_fun = err_text(glue("Age Type Adults (15+) has not been created correctly. 
                                Please check the filter for type section in cumul_achv() in loom.R")),
               description = glue("Verify that the Adult category was created correctly")) %>%
        verify(if_else(is.na(ageasentered), type == "Total",
                       type == "Adults (15+)" | type == "Children (<15)"),
               error_fun = err_text(glue("Age Type Total has not been created correctly. 
                                Please check the filter for type section in cumul_achv() in loom.R")),
               description = glue("Verify that the Total category was created correctly")) %>%
        filter(type %in% .type)
      
    }}
  
  if ((.funding_agency != as.character("All") & 
       (as.character(.funding_agency) %in% df_ou$funding_agency) == TRUE)) {
    
    # list of all expected agencies even though user currently only has 
    # option to filter for USAID, DOD, PC, or CDC
    expected_agencies <- c("USAID", "DOD", "HHS/CDC", "CDC", "STATE", 
                           "HRSA", "DEFAULT", "DEDUP", "PC")
    
    df_ou <- df_ou %>%
      verify(as.character(.funding_agency) %in% expected_agencies,
             error_fun = err_text(glue(
               "Error: Unexpected funding agency input. 
              To include this funding agency in expected inputs, 
             please add it to the expected_agencies object in the 
             funding agency filter section in cumul_achv() in loom.R")),
             description = glue("Verify that selected agency is a valid input")) %>%
      filter(funding_agency == as.character(.funding_agency))
    
    # Example of a unit test with a custom, informative error message
    assert_that(
      ((.funding_agency != as.character("All") & 
          as.character(.funding_agency) %in% df_ou$funding_agency == TRUE)), 
      msg = "This funding agency is not available for this combination of inputs.")
    
  }
  
  df_final <- df_ou %>%
    resolve_knownissues() %>%
    group_by(operatingunit, indicator, fiscal_year) %>%
    summarise(across(c(starts_with("qtr"), cumulative, targets),
                     sum,
                     na.rm = TRUE), .groups = "keep") %>%
    reshape_msd(direction = "quarters") %>%
    adorn_achievement() %>%
    arrange(period) %>%
    mutate(
      period_num = as.numeric(str_sub(period, -1)),
      qtr_target = targets / ((4 - period_num) + 1),
      fiscal_year2 = str_extract(period, "FY[1-2][0-9]"),
      results_lab = case_when(period == curr_pd |
                                fiscal_year2 == curr_fy_lab ~
                                glue("{comma(results_cumulative)}")),
      achv_pct_label = case_when(period == curr_pd |
                                   fiscal_year2 == curr_fy_lab ~
                                   glue("{percent(achievement_qtrly)}")),
      ind_period = str_c(indicator, period, sep = "_")) %>%
    filter(period %in% qtrs_to_keep)
  
  
  df_final %>%
    ggplot(aes(x = period)) +
    geom_col(aes(y = qtr_target),
             alpha = .7, fill = usaid_lightgrey,
             position = position_dodge(width = .65)) +
    geom_col(aes(y = results_cumulative),
             alpha = .7, fill = scooter_med,
             position = position_dodge(width = .65)) +
    geom_text(aes(label = achv_pct_label, y = 0),
              position = position_dodge(width = 0.75), color = "#FFFFFF",
              family = "Source Sans Pro", size = 12 / .pt,
              vjust = -.5, na.rm = TRUE) +
    geom_text(aes(label = results_lab, y = results_cumulative),
              position = position_dodge(width = 0.75), color = scooter_med,
              family = "Source Sans Pro", size = 12 / .pt,
              vjust = -.5, na.rm = TRUE) +
    scale_x_discrete(breaks = unique(df_final$period)[grep("Q(2|4)", unique(df_final$period))]) +
    # how to dynamically add just a little excess to the
    # max. value to accommodate the geom_text?
    scale_y_continuous(
      limits = c(0, max(df_final$qtr_target) + 5000),
      label = label_number(scale_cut = cut_short_scale())) +
    labs(
      x = NULL, y = NULL, fill = NULL,
      title = glue("{.title}"),
      # subtitle = glue(""),
      caption = glue("Source: {curr_pd} MSD | Ref id: {ref_id} | US Agency for International Development")) +
    si_style_yline() +
    theme(
      panel.spacing = unit(.5, "line"),
      legend.position = "none",
      plot.title = element_markdown(),
      strip.text = element_markdown()
    )
  
}

# GLOBAL VARIABLES ------------------------------------------------------------
  
  ref_id <- "dcd3c09d"

# IMPORT ----------------------------------------------------------------------
  
  path <- "Data/DREAMSDash_Global_resultstargets.csv"
  
  df <- read_csv(path) %>%
    clean_names()
  
tabyl(df$indicator)

# MUNGE ------------------------------------------------------------------------

df_filt <- df %>%
  select(operating_unit, fy, indicator, numerator_denom1, targets, cumulative) %>%
  # adorn achivement requires this col name
  rename(fiscal_year = fy) %>%
  filter(fiscal_year == 2022) %>%
  # summarize across all age bands and disaggs
  group_by(operating_unit, fiscal_year, indicator,numerator_denom1) %>%
  summarise(across(c(cumulative, targets),\(x) sum(x, na.rm = TRUE)), 
            .groups = "keep") %>%
  adorn_achievement() %>%
  arrange(operating_unit)

# VIZ --------------------------------------------------------------------------

# ordered by achievement
df_filt %>%
  ggplot(aes(x = fct_reorder(operating_unit, -achievement))) +
  geom_col(aes(y = cumulative), fill = denim, alpha = 0.7) +
  geom_point(aes(y = targets), fill = trolley_grey_light, alpha = 0.3) +
  geom_text(aes(label = scales::percent(achievement), y = cumulative),
            position = position_dodge(width = 0.75), color = denim,
            family = "Source Sans Pro", size = 12 / .pt,
            vjust = -.5, na.rm = TRUE) +
  scale_y_continuous(
    limits = c(0, 800000),
    label = label_number(scale_cut = cut_short_scale())) +
  scale_fill_si(palette = "denims") +
  si_style_ygrid() +
  coord_flip() %>%
  labs(
    x = NULL, y = NULL, fill = NULL,
    title = glue("What is OU-level achievement of results against targets?"),
    subtitle = glue("FY{df_filt$fiscal_year[1]} | {df_filt$indicator[1]} ({df_filt$numerator_denom1[1]})"),
    caption = glue("Source: MSD | Ref id: {ref_id} | US Agency for International Development")) +
  si_style_yline() +
  theme(
    panel.spacing = unit(.5, "line"),
    legend.position = "none",
    plot.title = element_markdown(),
    strip.text = element_markdown()
  )
  

# alphabetical order
df_filt %>%
  ggplot(aes(x = operating_unit)) +
  geom_col(aes(y = cumulative), fill = denim, alpha = 0.7) +
  geom_col(aes(y = targets), fill = trolley_grey_light, alpha = 0.4) +
  geom_text(aes(label = scales::percent(achievement), y = cumulative),
            position = position_dodge(width = 0.75), color = denim,
            family = "Source Sans Pro", size = 12 / .pt,
            vjust = -.5, na.rm = TRUE) +
  scale_y_continuous(
    limits = c(0, 800000),
    label = label_number(scale_cut = cut_short_scale())) +
  scale_fill_si(palette = "denims")+
  si_style_ygrid() +
  coord_flip() %>%
  labs(
    x = NULL, y = NULL, fill = NULL,
    title = glue("What is OU-level achievement of results against targets?"),
    subtitle = glue("FY{df_filt$fiscal_year[1]} | {df_filt$indicator[1]} ({df_filt$numerator_denom1[1]})"),
    caption = glue("Source: MSD | Ref id: {ref_id} | US Agency for International Development")) +
  si_style_yline() +
  theme(
    panel.spacing = unit(.5, "line"),
    legend.position = "none",
    plot.title = element_markdown(),
    strip.text = element_markdown()
  )

