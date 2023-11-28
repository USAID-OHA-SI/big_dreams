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
  library(waffle)

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  # Google sheet ID for DREAMS DSNU list
  g_id <- "1YaGWvpkiXVPiAwA3KyHwFtbZHrVeCz6DijWPCKpUN-Q"
  
  #for viz tracking
  ref_id <- "6eeeca89"
  
  #MSD path
  msd_path <- si_path() %>% 
    return_latest("PSNU_IM_DREAMS_FY21-24")
  
  #extract MSD metadata
  get_metadata(msd_path)

# IMPORT ------------------------------------------------------------------
  
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

#crosswalk
df_join %>% count(operatingunit, cop22_psnu, psnu, dsnu, dsnu_new) %>% View()
  
# MUNGE FOR VIZ -------------------------------------------------------------------

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

#do a check to see what DSNUs are missing agency info (share with DREAMS team to check data issues)
  #this is how we initially uncovered the DSNU raised prioritization issue - resolved in join above
viz_package %>% 
  mutate(usaid_dsnu = ifelse(str_detect(agencies_FY23, "USAID"), "USAID", "non-USAID")) %>% 
  filter(is.na(usaid_dsnu)) %>% 
  count(operatingunit, psnu, dsnu_new)

#create shares by USAID and non-USAID contributing DSNUs
df_viz1 <- viz_package %>% 
  mutate(usaid_dsnu = ifelse(str_detect(agencies_FY23, "USAID"), "USAID", "non-USAID")) %>% 
  group_by(indicator, fiscal_year, usaid_dsnu) %>% 
  summarize(across(cumulative, \(x) sum(x, na.rm = TRUE)),.groups="drop") %>% 
  pivot_wider(names_from = "usaid_dsnu", values_from = 'cumulative') %>% 
  mutate(total = USAID + `non-USAID` + `NA`,
         usaid_share = USAID/total)

# FUNCTION FOR WAFFLE CHART
make_waffle <- function(n, clr = grey20k){
  c(n) %>% 
    as.table() %>% 
    waffle::waffle(color = clr, flip = T,
                   reverse = T, size = 0.5) +
    theme(legend.position = "none")
}

#function to create tile breakdown
gen_tile_fill <- function(prop){
  x <- round(prop, 2) * 100
  y <- 100 - (round(prop, 2) * 100)
  return(c(x, y))
}

usaid_prop <- df_viz1 %>% pull(usaid_share)
usaid_ban <- df_viz1 %>% pull(USAID)

gen_tile_fill(usaid_prop)

x_pos = 3
y_pos = 9.5
fontfam = "Soure Sans Pro"

usaid_agyw <- make_waffle(100) + labs(title = "USAID DREAMS SHARE") + 
  annotate("text", x = x_pos, y = y_pos, label = label_number_si(accuracy = 0.1)(usaid_ban), size = 26/.pt, family = fontfam, fontface = 2)

usaid_waffle <- make_waffle(gen_tile_fill(usaid_prop), clr = c("#2F2E79", grey20k)) + labs(title = "USAID DREAMS Share") +
  annotate("text", x = x_pos, y = y_pos, label = label_number_si(accuracy = 0.1)(usaid_ban), color= "white", size = 26/.pt, family = fontfam, fontface = 2)


