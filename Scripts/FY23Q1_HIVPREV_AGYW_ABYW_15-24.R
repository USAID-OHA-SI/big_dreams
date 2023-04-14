# PROJECT:  big_dreams
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  reproduce DREAMS COP visuals with a different age group (15-24)
# REF ID:   dcd3c23a
# LICENSE:  MIT
# DATE:     2023-04-14

# DEPENDENCIES -----------------------------------------------------------------

library(tidyverse)
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

# GLOBAL VARS ------------------------------------------------------------------

  cntry <- "Cote d'Ivoire"
  
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
           caption = glue::glue("{cap_note}{metadata_natsubnat$source} | USAID/OHA/SIEI |  Ref id: {ref_id} v{vrsn}")) +
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
  
# IMPORT -----------------------------------------------------------------------

  df_nat <- si_path() %>% 
    return_latest("NAT") %>% 
    read_psd()
  
  get_metadata(type = "NAT_SUBNAT")
  metadata_natsubnat <- metadata
  rm(metadata)
  
# MUNGE ------------------------------------------------------------------------

  df_prev <- prep_hiv_prev_DREAMS(df = df_nat, cntry = cntry)

# VIZ --------------------------------------------------------------------------

  viz_hiv_prev_DREAMS(df_prev)
  
  si_save("Images/HIV_prev_AGYW_ABYW_15_24.png")