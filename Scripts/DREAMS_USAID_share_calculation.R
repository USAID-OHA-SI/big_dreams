# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  USAID DREAMS share - waffle chart
# REF ID:   6eeeca89 
# LICENSE:  MIT
# DATE:     2023-11-20
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(glamr)
  library(tidyverse)
  library(glitr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(readxl)
  library(googlesheets4)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  # Google sheet ID
  g_id <- "1YaGWvpkiXVPiAwA3KyHwFtbZHrVeCz6DijWPCKpUN-Q"
  
  ref_id <- "6eeeca89"
  
  #MSD path
  msd_path <- si_path() %>% 
    return_latest("PSNU_IM_DREAMS_FY21-24")
  
  get_metadata(msd_path)

# IMPORT ------------------------------------------------------------------
  
  #import DREAMS DSNU crosswalk
    #recommend adding psnu_uids to this sheet
 dsnu_list <- read_sheet(g_id, sheet = "CURRENT (FY23/COP22)")

#rename names
names(dsnu_list)<-c("operatingunit", "psnu", "dsnu", "agencies_FY23")


df_msd <- read_psd(msd_path) %>% 
  filter(fiscal_year == metadata$curr_fy)

## Join - keeping only observations in MER
df_join <- df_msd %>% left_join(dsnu_list, by=c("operatingunit", "psnu", "dsnu"))
  

# MUNGE -------------------------------------------------------------------

#For total DREAMS programming, use all 4 disaggs + ages 10-29
viz_package <- df_join %>%
  filter(indicator=="AGYW_PREV",
         standardizeddisaggregate %in% c("Age/Sex/Time/Complete+",
                                         "Age/Sex/Time/Complete",
                                         "Age/Sex/Time/Started",
                                         "Age/Sex/Time/Incomplete"),
         numeratordenom=="D",
         age_2019 %in% c("10-14","15-19","20-24","25-29"),
         sex=="Female")

## Now filter for USAID only numbers - cumulative
viz1_USAID<- viz_package %>%
  filter(str_detect(agencies_FY23, "USAID")) %>% 
  summarize(across(cumulative, \(x) sum(x, na.rm = TRUE)),.groups="drop")


# Can use these codes code to check #s by country or DSNU against the DREAMS dashboard for Q2 
viz1_usaid_ou <- viz_package %>%
filter(str_detect(agencies_FY23, "USAID")) %>%
  group_by(operatingunit) %>%
  summarize(across(cumulative, \(x) sum(x, na.rm = TRUE)),.groups="drop") %>%
  arrange() %>% print()

#crosswalk
viz_package %>%
  filter(str_detect(agencies_FY23, "USAID")) %>% 
  count(operatingunit, psnu, dsnu) %>% 
  mutate(match = ifelse(psnu == dsnu, TRUE, FALSE))
