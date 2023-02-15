# PROJECT:  big_dreams
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  Population Size Estimation for Eswatini
# REF ID:   0ffffad3 
# LICENSE:  MIT
# DATE:     2022-12-08
# UPDATED:  2023-02-15

# DEPENDENCIES ----------------------------------------------------------------
  
  library(tidyverse)
  library(extrafont)
  library(gagglr)
  library(skimr)
  library(janitor)
  library(finalfit)
  library(mice)
  library(lattice)
  library(ggplot2)
  library(GGally)
  library(googlesheets4)
  library(gt)
  library(glue)

# GLOBAL VARIABLES ------------------------------------------------------------
  
  ref_id <- "0ffffad3"
  
  load_secrets()
  
  seed <- set.seed(22222)

# IMPORT ----------------------------------------------------------------------

 df <-  read_sheet("1GdrrRY3Ebhb_My0UWgRHyOx_muk5cyrGdqFUwmt4KQQ")

# MUNGE -----------------------------------------------------------------------
       
  total_agyw <- df %>%
    clean_names() %>%
    select(country, householdid, personid, centroidid, region,
           intwt0, hivstatusfinal, agegroup, outofschool, orphanhood,
           pregnancyhistory, transactional, alcuse, sti_all,
           sexpartners, irregular, sexualviolence, adattck, eversex_1014) %>%
    # This is a risk factor for these groups in SA's methods and I'm not 
    # seeing it in the data
    mutate(
      hivstatusfinal = na_if(hivstatusfinal, 99),
      # create "any_risk" wherein an individual has any one or more of the risk factors 
      # for their age group
      # 1= 10-14 years
      # 2= 15-19 years
      # 3= 20-24 years
      # 4= 25-29 years
      any_risk_1014 = if_else((agegroup == 1) & 
                              (eversex_1014 == 1 |
                              pregnancyhistory == 1 |
                              sexualviolence == 1 |
                              adattck == 1 |
                              alcuse == 1 |
                              outofschool == 1|
                              orphanhood == 1), 
                              1, 0),
      # is adattck not a risk factor for 15-19 or 20-24 yr olds?
      # i am seeing it in SA's methods for 10-14 yr olds only
      any_risk_1519 = if_else((agegroup == 2) &
                              (sexpartners == 1 |
                              pregnancyhistory == 1 | 
                              sti_all == 1 |
                              irregular == 1 |
                              transactional == 1 | 
                              sexualviolence == 1 |
                              alcuse == 1 |
                              outofschool == 1|
                              orphanhood == 1), 
                              1, 0),
      any_risk_2024 = if_else((agegroup == 3) &
                              (sexpartners == 1 |
                              pregnancyhistory == 1 | 
                              sti_all == 1 |
                              irregular == 1 |
                              transactional == 1 | 
                              sexualviolence == 1 |
                              alcuse == 1), 
                              1, 0),
      any_risk_2529 = if_else((agegroup == 4) &
                              (sexpartners == 1 |
                               pregnancyhistory == 1 | 
                               sti_all == 1 |
                               irregular == 1 |
                               transactional == 1 | 
                               sexualviolence == 1 |
                               alcuse == 1), 
                              1, 0)) 
  
  # Check for completeness of important vars
  
  skim_results <- total_agyw %>% skim()
  
  # 1096 participants (24%) missing HIV status
  # this is consistent with SA's experience
  
  # does it vary across ages?
  
  tabyl(total_agyw, agegroup, hivstatusfinal)
  
  # agegroup   1   2 NA_
  # 1         20 569 673
  # 2         74 979 138
  # 3        205 717 153
  # 4        326 497 132
  
  # yes, most who are missing this are between ages 10-14
  # note: most who are positive are in age group 4, 25-29

  # How many AGYW are missing all risk factors for their age group? ------------
  
  missing_all_rfs <- total_agyw %>%
    # 10-14
    filter((agegroup == 1 &
               is.na(outofschool) & 
               is.na(orphanhood) & 
               is.na(pregnancyhistory) & 
               is.na(transactional) & 
               is.na(alcuse) &
               is.na(sti_all) &
               is.na(sexpartners) &
               is.na(irregular) &
               is.na(sexualviolence) &
               is.na(adattck) &
               is.na(eversex_1014)) |
    # 15-19
            (agegroup == 2 & 
              is.na(outofschool) & 
              is.na(orphanhood) & 
              is.na(pregnancyhistory) & 
              is.na(transactional) & 
              is.na(alcuse) &
              is.na(sti_all) &
              is.na(sexpartners) &
              is.na(irregular) &
              is.na(sexualviolence)) |
    # 25-29
           (agegroup == 3 &
              is.na(transactional) & 
              is.na(alcuse) &
              is.na(sti_all) &
              is.na(sexpartners) &
              is.na(irregular) &
              is.na(sexualviolence)) |
           (agegroup == 4) &
             is.na(transactional) & 
             is.na(alcuse) &
             is.na(sti_all) &
             is.na(sexpartners) &
             is.na(irregular) &
             is.na(sexualviolence))
  
  # 936 (21%) participants missing all risk factors 
  
  # does this vary by age?
  
  tabyl(missing_all_rfs, agegroup)
  
  # agegroup   n   percent
  # 1 658 0.70299145
  # 2 102 0.10897436
  # 3  97 0.10363248
  # 4  79 0.08440171
  
  # yes, most (70%) of those missing all RFs for their age are in the 10-14 age group

  # filter out those missing HIV status or all risk factors for their age
  # to do a "complete case analysis"
  total_agyw_known <- total_agyw %>%
    drop_na(hivstatusfinal) %>%
    filter(!personid %in% missing_all_rfs$personid)
  
  # How many HIV+ AGYW are in our sample? --------------------------------------
  
  hiv_pos_agyw <- total_agyw_known %>%
    filter(hivstatusfinal == 1)
  
  # What is the HIV positivity/prevalence in our sample overall and by age group? -----
  
  # overall
  hiv_pos_overall <- (nrow(hiv_pos_agyw)/nrow(total_agyw_known)) * 100
  
  # 10-14
  hiv_pos_agyw_1 <- total_agyw_known %>%
    filter(hivstatusfinal == 1 & agegroup == 1)
  
  total_agyw_1 <- total_agyw_known %>%
    filter(agegroup == 1)
  
  hiv_pos_1 <- (nrow(hiv_pos_agyw_1)/nrow(total_agyw_1)) * 100
  
  # 15-19
  hiv_pos_2 <- total_agyw_known %>%
    filter(hivstatusfinal == 1 & agegroup == 2)
  
  total_agyw_2 <- total_agyw_known %>%
    filter(agegroup == 2)
  
  hiv_pos_2 <- (nrow(hiv_pos_2)/nrow(total_agyw_2)) * 100
  
  # 20-24
  
  hiv_pos_3 <- total_agyw_known %>%
    filter(hivstatusfinal == 1 & agegroup == 3)
  
  total_agyw_3 <- total_agyw_known %>%
    filter(agegroup == 3)
  
  hiv_pos_3 <- (nrow(hiv_pos_3)/nrow(total_agyw_3)) * 100
  
  # 25-29
  
  hiv_pos_4 <- total_agyw_known %>%
    filter(hivstatusfinal == 1 & agegroup == 4)
  
  total_agyw_4 <- total_agyw_known %>%
    filter(agegroup == 4)
  
  hiv_pos_4 <- (nrow(hiv_pos_4)/nrow(total_agyw_4)) * 100
          
  # We know the HIV status of 58% of our sample, 
  # is this still a sufficient sample?
  
  # Calculate the number of HIV- AGYW at risk ----------------------------------
  
  hiv_neg_atrisk <- total_agyw_known %>%
    filter(hivstatusfinal == 2 & 
           any_risk_1014 == 1 |
           any_risk_1519 == 1 |
           any_risk_2024 == 1 |
           any_risk_2529 == 1) %>%
    mutate(agegroup_text = case_when(
      agegroup == 1 ~ "10-14", 
      agegroup == 2 ~ "15-19", 
      agegroup == 3 ~ "20-24", 
      agegroup == 4 ~ "25-29"))
  
  # Calculate population most vulnerable to HIV acquisition --------------------
  
  total_AGYW_atrisk = round((nrow(total_agyw_known) - 
                               nrow(hiv_pos_agyw)) * 
                              (sum(nrow(hiv_neg_atrisk)/nrow(total_agyw_known))), 0)
  
  # 2396
  
  # AGYW at-risk for HIV by age group formatted table --------------------------
  tabyl(hiv_neg_atrisk, region, agegroup_text) %>%
    # add row percentages
    adorn_percentages("row") %>%
    # add percentage formatting
    adorn_pct_formatting() %>%
    # add sample size in parens. for each percentage
    adorn_ns() %>%
    gt() %>%
    # add custom title
    tab_header(
      title = "AGYW at-risk for acquiring HIV",
      subtitle = glue("Source: Eswatini SHIMS2, 
                       SI Analytics")) %>%
    # relabel columns nicely
    cols_label( 
      region = "Region")

  
  
 
  
  
  
  
  
  