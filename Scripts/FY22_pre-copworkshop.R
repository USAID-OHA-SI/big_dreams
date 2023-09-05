# PROJECT:  big_dreams
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  pre-COP workshop 2022, AGYW_PREV achievement over time 
# REF ID:   4a199427 
# LICENSE:  MIT
# DATE:     2022-12-21

# DEPENDENCIES ----------------------------------------------------------------
  
  library(tidyverse)
  library(extrafont)
  library(gagglr)
  library(glue)
  library(scales)
  library(ggtext)

# GLOBAL VARIABLES ------------------------------------------------------------
  
  ref_id <- "4a199427"
  output_path <- "Images"
  msd_path_ou <- "MER_Structured_Datasets_OU_IM_FY20-23"

# IMPORT ----------------------------------------------------------------------
  
  df <- si_path() %>%
    return_latest(msd_path_ou) %>%
    read_msd()
  
  # metadata
  si_path() %>%
    return_latest(msd_path_ou) %>%
    get_metadata()
  
# MUNGE -----------------------------------------------------------------------
  
  # Cumulative
  
  df_cum <- df %>%
    filter(
      indicator == "AGYW_PREV",
      operatingunit %in% c("Botswana", "Cote d'Ivoire", "Eswatini", 
                           "Haiti", "Kenya", "Lesotho", "Malawi", 
                           "Mozambique", "Namibia", "Rwanda", "South Africa", 
                           "South Sudan", "Tanzania", "Uganda", "Zambia", 
                           "Zimbabwe"), 
    fiscal_year %in% c("2021", "2022")) %>%
    pluck_totals() %>%
    group_by(operatingunit, indicator, fiscal_year) %>%
    summarise(across(c(starts_with("qtr"), cumulative, targets),
                     sum,
                     na.rm = TRUE), .groups = "keep") %>%
    reshape_msd(direction = "quarters") %>%
    adorn_achievement() %>%
    arrange(period) %>%
    filter(targets > 0 & results > 0) %>%
    mutate(
      period_num = as.numeric(str_sub(period, -1)),
      qtr_target = targets / ((4 - period_num) + 1),
      fiscal_year2 = str_extract(period, "FY[1-2][0-9]"),
      results_lab = case_when(period == metadata$curr_pd |
                                fiscal_year2 == metadata$curr_fy_lab ~
                                glue("{comma(results_cumulative)}")),
      achv_pct_label = case_when(period == metadata$curr_pd |
                                   fiscal_year2 == metadata$curr_fy_lab ~
                                   glue("{percent(achievement_qtrly)}")),
      ind_period = str_c(indicator, period, sep = "_"))
  
  # Quarterly
  
  df_qtr <- df %>%
    filter(
      operatingunit == .ou,
      indicator == .indicator,
      (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% peds) |
        (standardizeddisaggregate == "Total Numerator") |
        (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% adults)) %>%
    pluck_totals() %>%
    group_by(fiscal_year, operatingunit, indicator, type) %>%
    summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE),
              .groups = "drop") %>%
    reshape_msd("quarters") %>%
    select(-results_cumulative) %>%
    arrange(type, operatingunit, period)
  
  df_qtr_2 <- df_qtr %>%
    mutate(
      growth_rate_req =
        case_when(period == metadata$curr_pd ~
                    ((targets / results)^(1 / (4 - metadata$curr_qtr))) - 1)) %>%
    group_by(type) %>%
    fill(growth_rate_req, .direction = "updown") %>%
    mutate(
      growth_rate = (results / lag(results, order_by = period)) - 1,
      growth_rate = na_if(growth_rate, Inf)) %>%
    ungroup() %>%
    mutate(
      geo_gr_lab = case_when(
        is.infinite(growth_rate_req) ~ glue("{toupper(operatingunit)}"),
        growth_rate_req < 0 ~ glue("{toupper(operatingunit)}\nTarget achieved"),
        growth_rate_req < .1 ~ glue("{toupper(operatingunit)}\n{percent(growth_rate_req, 1)}"),
        TRUE ~ glue("{toupper(operatingunit)}\n{percent(growth_rate_req, 1)}")),
      gr_lab = case_when(fiscal_year == metadata$curr_fy ~
                           glue("{percent(growth_rate, 1)}")),
      gr_lab = stringr::str_replace(gr_lab, "NA", "0"),
      gr_label_position = 0,
      results_lab = case_when(fiscal_year == metadata$curr_fy ~
                                glue("{comma(results)}")),
      disp_targets = case_when(fiscal_year == metadata$curr_fy ~ targets),
      unit_label = glue("(Operating Unit)"),
      amount_diff = targets - results,
      pct_change = round_half_up((results - targets) / abs(targets) * 100), 0)
  
    pct_change_new <- df_qtr_2 %>%
      filter(type == .type) %>%
      select(fiscal_year, pct_change) %>%
      filter(pct_change == max(as.numeric(pct_change))) %>%
      pull()
  
  
  # VIZ -----------------------------------------------------------------------
  
  # Cumulative
  
  df_cum %>%
    ggplot(aes(x = period)) +
    geom_col(aes(y = qtr_target),
             alpha = .7, fill = usaid_lightgrey,
             position = position_dodge(width = .65)) +
    geom_col(aes(y = results_cumulative),
             alpha = .7, fill = usaid_medgrey,
             position = position_dodge(width = .65)) +
    facet_wrap(~operatingunit, scales = "free_y") +
    geom_text(aes(label = achv_pct_label, y = 0),
              position = position_dodge(width = 0.75), color = "#FFFFFF",
              family = "Source Sans Pro", size = 12 / .pt,
              vjust = -.5, na.rm = TRUE) +
    geom_text(aes(label = results_lab, y = results_cumulative),
              position = position_dodge(width = 0.75), color = usaid_medgrey,
              family = "Source Sans Pro", size = 12 / .pt,
              vjust = -.5, na.rm = TRUE) +
    scale_x_discrete(breaks = unique(df_cum$period)[grep("Q(2|4)", unique(df_cum$period))]) +
    # how to dynamically add just a little excess to the
    # max. value to accommodate the geom_text?
    scale_y_continuous(
      limits = c(0, max(df_cum$qtr_target) + 5000),
      label = label_number(scale_cut = cut_short_scale())) +
    labs(
      x = NULL, y = NULL, fill = NULL,
      # subtitle = glue("{.subtitle}"),
      caption = glue("{metadata$caption} | US Agency for International Development")) +
    si_style_yline() +
    theme(
      panel.spacing = unit(.5, "line"),
      legend.position = "none",
      plot.title = element_markdown(),
      strip.text = element_markdown())
  

    # Quarterly
    
    df_qtr %>%
      ggplot(aes(period, results, fill = as.character(period))) +
      geom_col(na.rm = TRUE, alpha = .7, width = 1) +
      facet_wrap(~operatingunit) +
      geom_text(aes(label = results_lab, y = results),
                family = "Source Sans Pro", color = usaid_darkgrey, size = 9 / .pt,
                vjust = -.5, na.rm = TRUE) +
      geom_text(aes(label = gr_lab, y = gr_label_position),
                family = "Source Sans Pro", color = "white", size = 9 / .pt,
                vjust = -.5, na.rm = TRUE) +
      scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
      scale_x_discrete(breaks = unique(df_new$period)[grep("Q(4)", unique(df_new$period))]) +
      scale_fill_manual(values = c(
        usaid_lightgrey, usaid_lightgrey, usaid_lightgrey, usaid_lightgrey,
        usaid_lightgrey, usaid_lightgrey, usaid_lightgrey, usaid_lightgrey,
        usaid_darkgrey, usaid_darkgrey, usaid_darkgrey, usaid_darkgrey)) +
      labs(
        x = NULL, y = NULL,
        title = NULL,
        subtitle = glue("{.subtitle}"),
        caption = glue("Note: Adults = Ages 15+ and Children= Ages <15
                     {metadata$caption} | US Agency for International Development")) +
      si_style_ygrid() +
      theme(
        legend.position = "none",
        panel.spacing = unit(.5, "picas"),
        axis.text.x = element_text(size = 8))