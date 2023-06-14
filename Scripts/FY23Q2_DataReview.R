# PROJECT:  big_dreams
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  FY23 Q2 data review visuals made in addition to those from DREAMS and 
#           CI Tableau dashboards
# REF ID:   890ae9f5
# LICENSE:  GPL v3 +
# DATE:     2023-06-09
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

# adapted from 20_dreams_prev-gaps_psnu.R in hardapoart

prep_hiv_prev_DREAMS <- function(df, cntry) {
  
  #clean exit if no data
  if(cntry %ni% unique(df$country))
    return(NULL)
  
  ## PSNU/Age/Sex Summaries
  df_pops <- df %>% 
    dplyr::filter(country == cntry, 
                  indicator %in% c("PLHIV", "POP_EST"),
                  # removing ages 10-14 as requested
                  ageasentered %in% c("15-19", "20-24"))
  
  #clean exit if no data
  if(nrow(df_pops) == 0)
    return(NULL)
  
  #include overall total and aggregate
  df_pops <- df_pops %>%
    dplyr::bind_rows(df_pops %>% 
                       dplyr::mutate(dplyr::across(c(psnu, psnuuid), \(x) x = "OVERALL"))) %>% 
    dplyr::mutate(group = ifelse(sex == "Female", "AGYW", "ABYM")) %>% 
    dplyr::group_by(fiscal_year,  country, 
                    psnuuid, psnu, indicator, group) %>% 
    dplyr::summarise(value = sum(targets, na.rm = T), .groups = "drop")
  
  #pivot wider to calc prevalence
  df_prev <- df_pops %>% 
    tidyr::pivot_wider(names_from = "indicator",
                       names_glue = "{tolower(indicator)}") 
  
  #clean exit if no data
  if("pop_est" %ni% names(df_prev))
    return(NULL)
  
  if("plhiv" %ni% names(df_prev))
    df_prev <- dplyr::mutate(df_prev, plhiv = NA_real_)
  
  df_prev <- df_prev %>% 
    dplyr::mutate(prevalence = plhiv / pop_est)
  
  #create a psnu level prevalence to order plot on
  df_prev <- df_prev %>% 
    dplyr::group_by(psnuuid) %>% 
    dplyr::mutate(prevalence_psnu = sum(plhiv, na.rm = TRUE) / sum(pop_est, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(prevalence_order = dplyr::case_when(fiscal_year == max(fiscal_year) ~ prevalence_psnu))
  
  #apply aes for plotting
  df_prev <- df_prev %>% 
    dplyr::mutate(fill_color = ifelse(group == "AGYW", moody_blue, genoa),
                  fill_alpha = ifelse(psnu == "OVERALL", .9, .75),
                  viz_group = glue::glue("{fiscal_year}{psnu}"))
  
  return(df_prev)
}
viz_hiv_prev_DREAMS <- function(df, save = F) {
  
  q <- glue::glue("Are there clear HIV prevalence gaps by AGYW/ABYM?") %>% toupper
  
  if(is.null(df) || nrow(df) == 0)
    return(dummy_plot(q))
  
  # OU/Country Reference line
  
  ref_id <- "70241287"
  ref_psnu <- "OVERALL"
  vrsn <- 1.1
  
  # Foot note for reduced datasets
  n_max <- 21
  
  cap_note <- ifelse(nrow(df) > n_max, "Note: Limited to the largest 20 HIV Prevalence PSNUs\n", "")
  
  # Display only a subset
  v_lim_uids <- df %>% 
    dplyr::filter(fiscal_year == max(fiscal_year)) %>% 
    dplyr::distinct(psnu, psnuuid, prevalence_psnu) %>% 
    dplyr::slice_max(order_by = prevalence_psnu, n = 21) %>% 
    dplyr::pull(psnuuid)
  
  df_viz <- df %>% 
    dplyr::filter(psnuuid %in% v_lim_uids) 
  
  if ("OVERALL" %ni% df_viz$psnu) {
    df_viz <- df %>% 
      filter(psnu == "OVERALL") %>% 
      bind_rows(df_viz, .)
  }
  
  # Viz
  viz <- df_viz %>% 
    ggplot(aes(x = prevalence,
               y = forcats::fct_reorder(psnu, prevalence_order, .fun = max, .na_rm = TRUE), 
               color = fill_color,
               alpha = fill_alpha,
               group = viz_group)) +
    geom_vline(xintercept = 0, color = "#D3D3D3") +
    geom_hline(yintercept = ref_psnu,
               linewidth = .8, linetype = "dashed", color = usaid_darkgrey) +
    geom_line(linewidth = 2, alpha = 1, color = "white", na.rm = TRUE) +
    geom_line(linewidth = 2, alpha = .6, color = grey30k, na.rm = TRUE) +
    geom_errorbar(aes(xmin = prevalence_psnu, xmax = prevalence_psnu), linewidth = 1, color = "white", na.rm = TRUE) +
    geom_point(size = 3, color = "white", alpha = 1, na.rm = TRUE) +
    geom_point(size = 3, na.rm = TRUE) +
    facet_wrap(~fiscal_year, nrow = 1, ncol = 3) +
    scale_color_identity() +
    scale_alpha_identity() +
    scale_x_continuous(labels = scales::percent) +
    labs(x = "", y = "",
         title = {q},
         # updated subtitle to reflect age band 15-24 instead of 10-24
         subtitle = glue::glue("{unique(df$country)} HIV prevalence gap between <span style='color:{genoa}'>ABYM</span> & <span style='color:{moody_blue}'>AGYW</span> ages 15-24"),
         caption = glue::glue("{cap_note}{metadata_natsubnat$source} | USAID DREAMS & SI|  Ref id: {ref_id} v{vrsn}")) +
    si_style_nolines() +
    theme(plot.subtitle = element_markdown(),
          axis.text.y = element_markdown())
  
  print(viz)
  
  if (save) {
    glitr::si_save(
      plot = viz,
      filename = glue::glue("./Graphics/{unique(df$fiscal_year)} - {toupper(unique(df$country))} HIV Prevalence AGYW-ABYM.png"))
  }
}

# adapted from 17_vl-coverage-and-suppression_kp-psnu.R

prep_viral_load_kp_agyw <- function(df, cntry, agency){
  
  young <- c("10-14","15-19", "20-24") #DREAMS AGYW age band based on DREAMS guidance
  
  #filter to select indicators + country
  df_vl <- df %>%  
    filter(indicator %in% c("TX_CURR", "TX_PVLS", "TX_PVLS_D"),
           country == cntry,
           funding_agency == agency) %>%
    clean_indicator()
  
  #clean exit for missing data
  if(nrow(df_vl) == 0)
    return(NULL)
  
  #create overall value
  df_vl <- df_vl %>% 
    bind_rows(df_vl %>% 
                mutate(psnu = "OVERALL"))
  
  #clean define groups - Total, KP, AGYW, Non-AGYW
  df_vl <- df_vl %>% 
    clean_indicator() %>%
    mutate(type = case_when(sex=="Female" & ageasentered %in% young ~ "AGYW",
                            str_detect(standardizeddisaggregate, "Total|KeyPop", negate = TRUE) ~ "Non-AGYW",
                            TRUE ~ str_extract(standardizeddisaggregate, "Total|KeyPop")))
  
  #aggregate & reshape long
  df_vl <- df_vl %>% 
    clean_indicator() %>%
    group_by(fiscal_year, funding_agency, country, psnu, indicator, type) %>% 
    summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    reshape_msd(include_type = FALSE)
  
  #pivot wide in order to subtract KP from GP (Total)
  df_vl <- df_vl %>% 
    pivot_wider(names_from = type,
                values_fill = 0) 
  
  #create KeyPop if missing
  if("KeyPop" %ni% names(df_vl))
    df_vl <- mutate(df_vl, KeyPop = 0)
  
  #subtract KP from GP (Total)
  df_vl <- df_vl %>%
    mutate(GenPop = Total - KeyPop,
           GenPop = ifelse(GenPop < 0, 0, GenPop)) %>% 
    select(-Total) %>% 
    pivot_longer(-where(is.character),
                 names_to = "type") %>% 
    mutate(group = case_when(type %in% c("KeyPop", "GenPop") ~ "KP-GP",
                             type %in% c("AGYW", "Non-AGYW") ~ "AGYW"),
           .before = type)
  
  #reshape wider by indicator and create lag for VLC
  df_vl <- df_vl %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    group_by(country, psnu, group, type) %>% 
    mutate(tx_curr_lag2 = lag(tx_curr, n = 2, order_by = period)) %>% 
    ungroup()
  
  #calculate VLC/S
  df_vl <- df_vl %>%
    mutate(vlc = tx_pvls_d/tx_curr_lag2,
           vls = tx_pvls/tx_pvls_d) %>% 
    filter(!is.nan(vlc))
  
  #limit to latest period
  df_vl <- filter(df_vl, period == max(period))
  
  #color
  df_viz <- df_vl %>% 
    select(-c(tx_pvls, tx_pvls_d, tx_curr_lag2)) %>% 
    mutate(fill_color = case_when(type == "KeyPop" ~ scooter,
                                  type == "AGYW" ~ genoa,
                                  TRUE ~ grey30k)) %>% 
    pivot_longer(c(vls, vlc), 
                 names_to = "indicator") %>% 
    mutate(group_viz = ifelse(group == "AGYW",
                              glue("**<span style='color:{genoa}'>AGYW</span> vs <span style='color:{grey30k}'>non-AGYW</span> {toupper(indicator)}**"),
                              glue("**<span style='color:{scooter}'>KeyPop</span> vs <span style='color:{grey30k}'>GenPop</span> {toupper(indicator)}**")))
  
  return(df_viz)
}
viz_viral_load_kp_agyw <- function(df){
  
  q <- glue::glue("Are certain key populations being missed when trying to reach the 3rd 95?") %>% toupper
  
  if(is.null(df) || nrow(df) == 0)
    return(dummy_plot(q))
  
  ref_id <- "f5d17218" #id for adorning to plots, making it easier to find on GH
  
  vrsn <- 1 
  
  cap_note <- ifelse(nrow(df) > 21, "| Limited to the largest 20 TX_CURR PSNUs\n", "")
  
  #limit to 21 bars (overall + 20 psnus)
  v_top <- df %>% 
    filter(group == "AGYW") %>% 
    count(psnu, wt = tx_curr, sort = TRUE) %>% 
    slice_head(n = 21) %>% 
    pull(psnu)
  
  df <- filter(df, psnu %in% v_top) 
  
  #viz
  df %>% 
    ggplot(aes(value, fct_reorder(psnu, tx_curr, max, na.rm = TRUE), color = fill_color, group = psnu)) +
    geom_vline(xintercept = 0, color = "#D3D3D3") +
    geom_vline(xintercept = 1, color = "#D3D3D3", linetype = "dashed") +
    geom_line(color = "#d3d3d3", na.rm = TRUE) +
    geom_point(size = 2, color = "white", na.rm = TRUE) +
    geom_point(size = 2, alpha = .6, na.rm = TRUE) +
    scale_color_identity() +
    facet_wrap(~group_viz, nrow = 1) +
    scale_x_continuous(labels = scales::percent, name = NULL, 
                       limits = c(0,1.1), 
                       breaks = seq(0,1.1, by = .25),
                       oob = oob_squish) + 
    labs(x = NULL, y = NULL,
         title = {q},
         subtitle = glue("{unique(df$period)} {unique(df$funding_agency)}/{unique(df$country)} VLC/S gaps between different population groups"),
         caption = glue("Note: VL capped at 110% {cap_note}{metadata_msd$caption} | USAID DREAMS & SI| Ref id: {ref_id} v{vrsn}")) +
    si_style_xline() +
    theme(legend.position = "none",
          strip.text = element_markdown(),
          panel.spacing = unit(.5, "picas"))
  
}

# global vars -----------------------------------------------------------------

cntry <- "Zimbabwe"

ref_id <- "890ae9f5"
date <- today()

# import -----------------------------------------------------------------------

load_secrets()

df_nat <- si_path() %>% 
  return_latest("NAT") %>% 
  read_psd()

get_metadata(type = "NAT_SUBNAT")
metadata_natsubnat <- metadata
rm(metadata)

df_msd <- si_path() %>% 
  return_latest("PSNU_IM_DREAMS_FY21-23") %>% 
  read_psd()

get_metadata(type = "MER")
metadata_msd <- metadata
rm(metadata)

df_tableau <- readxl::read_excel(here::here("Data/FY23Q2_DREAMSPrimaryPackageCompletion.xlsx"))

# MUNGE ------------------------------------------------------------------------

# Counting the number of DREAMS SNUs ------------

# Globally

df_summ_global <- df_msd %>%
  clean_indicator() %>%
  # in 2023, reported Q2 data
  filter(fiscal_year == "2023", 
         is.na(qtr2) == FALSE) %>%
  group_by(operatingunit, dsnu) %>%
  select(operatingunit, dsnu) %>%
  distinct() %>%
  count(dsnu) %>%
  ungroup() %>%
  reframe(n_dsnus = sum(n))

# by Agency - check with KM on this as it does not distinguish between 
# SNUs which are suported by multiple agencies

df_summ_agency <- df_msd %>%
  clean_indicator() %>%
  clean_agency() %>%
  # in 2023, reported Q2 data
  filter(fiscal_year == "2023", 
         is.na(qtr2) == FALSE) %>%
  group_by(funding_agency, operatingunit, dsnu) %>%
  select(funding_agency, operatingunit, dsnu) %>%
  distinct() %>%
  count(dsnu) %>%
  ungroup() %>%
  group_by(funding_agency) %>%
  reframe(n_dsnus = sum(n)) %>%
  mutate(pct_contribution_agency = percent(n_dsnus/df_summ_global$n_dsnus))

# for POART visuals --------------

df_prev <- prep_hiv_prev_DREAMS(df = df_nat, cntry = cntry) %>%
  filter(fiscal_year %in% c("2022", "2023"))

df_vl <- prep_viral_load_kp_agyw(df = df_msd, 
                                 cntry = cntry, agency = "USAID") %>%
  filter(fiscal_year %in% c("2022", "2023"))

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
    ou = "Cote d'Ivoire*", 
    completion = "Primary Package Completed",
    cumulative = 0, 
    pct = 0) %>%
  add_row(
    ou = "South Sudan*", 
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
  arrange(period) %>%
  mutate(
    color_cat = if_else(period == metadata_msd$curr_pd,
                        glue("{funding_agency}_{period}"), glue("{funding_agency}")))

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
  geom_col(aes(y = results_cumulative, alpha = .9, fill = color_cat),
           position = position_dodge(width = .65)) +
  facet_wrap(~funding_agency) +
  geom_text(aes(label = glue("{percent(achievement_qtrly)}"), y = 0),
            position = position_dodge(width = 0.75), color = "#FFFFFF",
            family = "Source Sans Pro", size = 12 / .pt,
            vjust = -.5, na.rm = TRUE) +
  scale_x_discrete(breaks = unique(df_filt_prep$period)[grep("Q(2|4)", unique(df_filt_prep$period))]) +
  scale_fill_manual(values = c("USAID" = usaid_medblue,
                               "CDC" = scooter_med, 
                               "USAID_FY23Q2" = usaid_blue, 
                               "CDC_FY23Q2" = scooter)) +
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
                      Data reported from Kenya has been excluded as an outlier.
                      Source: FY23Q2i MSD | USAID DREAMS & SI| Ref id: {ref_id}| Created using FY23Q2_DataReview.R")) +
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
       caption = glue("Note: *Data were not reported from South Sudan or Cote d'Ivoire, 
                              Numbers on bars show the number of AGYW in DREAMS for 13+ months in each OU
         Source: DREAMS Quarterly Workbook Data Download from Primary Package Completion 13+ Months tab, FY23Q2i
          USAID DREAMS & SI| Ref id: {ref_id}| Created using FY23Q2_DataReview.R")) +
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
                      Data reported from Kenya has been excluded as an outlier.
                      Numbers on bars show the number of AGYW who received educational services by OU
                      Source: FY23Q2i MSD | USAID DREAMS & SI| Ref id: {ref_id}| Created using FY23Q2_DataReview.R")) +
  si_style_xline() +
  theme(axis.text = element_text(family = "Gill Sans MT", 
                                 color = usaid_darkgrey, size = 14),
        legend.position = "none",
        strip.text = element_markdown(),
        panel.spacing = unit(.5, "picas"))

si_save(glue("Images/FY23Q2_EDUbyOU_{date}.png"))
