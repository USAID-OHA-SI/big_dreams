# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  DREAMS infographic
# REF ID:   26c98831 
# LICENSE:  MIT
# DATE:     2023-11-22
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

# Google sheet ID for DREAMS DSNU list
g_id <- "1YaGWvpkiXVPiAwA3KyHwFtbZHrVeCz6DijWPCKpUN-Q"

ref_id <- "26c98831"

#MSD path
msd_path <- si_path() %>% 
  return_latest("PSNU_IM_DREAMS_FY21-24")

#extract MSD metadata
get_metadata(msd_path)

# IMPORT -----------------------------------------------------------------
#import DREAMS DSNU crosswalk
#recommend adding psnu_uids to this sheet
dsnu_list <- read_sheet(g_id, sheet = "CURRENT (FY23/COP22)")

#rename names
names(dsnu_list)<-c("operatingunit", "psnu", "dsnu", "agencies_FY23")

#import msd and filter to current year
df_msd <- read_psd(msd_path) %>% 
  filter(fiscal_year == metadata$curr_fy)

# RAISED PRIORITIZATION ISSUE - MUNGING ---------------------------------------

# ISSUE: some OUs raised the prioritization levels in COP23 which has changed
#        the DSNU levels in the MSD. We will use the cop22_psnu column to adjust
#        these manually to match the internal list 

# First, let's check which OUs in the internal list have PSNUS and DSNUs that dont match
#eswatini and rwanda psnu and dsnu that dont match
dsnu_list %>% 
  mutate(dsnu_psnu_match = ifelse(psnu == dsnu, TRUE, FALSE)) %>% 
  count(operatingunit, dsnu_psnu_match)

#Now, create a crosswalk of the OU, PSNU, DSNU, and cop22_psnu from MSD
# add a logical for OUs that raised prioritization levels
# then mutate a new dsnu column for those with raised levels to use cop22_psnu names instead of dsnu
msd_dsnu_xwalk <- df_msd %>% 
  count(operatingunit, operatingunituid, psnu, psnuuid, cop22_psnu, cop22_psnuuid, dsnu, dsnuuid) %>% 
  mutate(raised_lvl = ifelse(psnuuid != cop22_psnuuid, TRUE, FALSE)) %>% 
  mutate(dsnu_new = case_when(operatingunit != "Uganda" & raised_lvl == TRUE ~ cop22_psnu,
                              TRUE ~ dsnu),
         dsnuuid_new = case_when(operatingunit != "Uganda" & raised_lvl == TRUE ~ cop22_psnuuid,
                                 TRUE ~ dsnuuid))

#join the new xwalk back to msd and then join the internal dsnu list to the msd
df_join <- df_msd %>% 
  left_join(msd_dsnu_xwalk) %>% 
  left_join(dsnu_list, by=c("operatingunit", "cop22_psnu" = "psnu", "dsnu_new"= "dsnu"))

# MUNGE ----------------------------------------------------------------

#all 4 disaggs
viz_package<- df_join %>% filter(indicator=="AGYW_PREV", 
                            standardizeddisaggregate %in% c("Age/Sex/Time/Complete+",
                                                            "Age/Sex/Time/Complete",
                                                            "Age/Sex/Time/Started",
                                                            "Age/Sex/Time/Incomplete"),
                            numeratordenom=="D",
                            age_2019 %in% c("10-14","15-19","20-24","25-29"),
                            sex=="Female")

#service type disagg
viz_service<- df_join %>% filter(indicator=="AGYW_PREV", 
                            standardizeddisaggregate %in% c("ComprehensiveEconomicStrengthening",
                                                            "EducationSupport"),
                            numeratordenom=="D")

#prep
viz_prep <- df_join %>% filter(indicator=="PrEP_NEW", 
                          standardizeddisaggregate %in% c("Age/Sex"),
                          age_2019 %in% c("10-14","15-19","20-24","25-29"),
                          sex=="Female")


##### VISUAL 2 #####################


### Number for purple box box - package completion
## Filter for only complete disaggregates & USAID
viz2_completed<-viz_package %>%
  filter(str_detect(agencies_FY23, "USAID"),
         standardizeddisaggregate %in% c("Age/Sex/Time/Complete+",
                                         "Age/Sex/Time/Complete")) %>%
  #group_by(operatingunit, dsnu_new) %>% 
  summarize(across(cumulative, \(x) sum(x, na.rm = TRUE)),.groups="drop") %>%
  print()

## Numbers for education & econ boxes
## Filter for education and econ disaggs + USAID
viz2_service<-viz_service %>%
  filter(str_detect(agencies_FY23, "USAID")) %>%
  group_by(standardizeddisaggregate) %>%
  summarize(across(cumulative, \(x) sum(x, na.rm = TRUE)),.groups="drop") %>%
  print()


## Numbers for PrEP - shouldnt we just filter for USAID here?
viz2_prep <- viz_prep %>%
  #filter(str_detect(agencies_FY23, "USAID")) %>%
  filter(funding_agency == "USAID") %>% 
  #group_by(operatingunit) %>% 
  summarize(across(cumulative, \(x) sum(x, na.rm = TRUE)),.groups="drop") %>% print()





