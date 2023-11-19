library(tidyverse)
library(gagglr)
library(vroom)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(gophr)
library(readxl)


file_path <-"C:/Users/npetrovic/Documents/Data/Other/DREAMS_DSNU_by_Agency.xlsx"
dsnu_list <- read_excel(file_path, sheet="CURRENT (FY23COP22)") 
names(dsnu_list)<-c("operatingunit", "psnu", "dsnu", "agencies_FY23")

merdata <- file.path(glamr::si_path("path_msd"))
file_path <- return_latest(folderpath = merdata,
                           pattern = "PSNU_IM_DREAMS_FY21-24")
df_orig<- read_psd(file_path) %>% filter(fiscal_year=="2023")

## Join - keeping only observations in MER
df<- df_orig %>% left_join(dsnu_list, by=c("operatingunit", "psnu", "dsnu"))


## For all viz we want ages 10-24
## For Viz1 we will use all 4 disaggs, for Viz2 we will filter for package completion

viz_package<- df %>% filter(indicator=="AGYW_PREV", 
              standardizeddisaggregate %in% c("Age/Sex/Time/Complete+",
                                              "Age/Sex/Time/Complete",
                                              "Age/Sex/Time/Started",
                                              "Age/Sex/Time/Incomplete"),
              numeratordenom=="D",
              age_2019 %in% c("10-14","15-19","20-24","25-29"),
              sex=="Female")

viz_service<- df %>% filter(indicator=="AGYW_PREV", 
                    standardizeddisaggregate %in% c("ComprehensiveEconomicStrengthening",
                                                    "EducationSupport"),
                    numeratordenom=="D")

viz_prep <- df %>% filter(indicator=="PrEP_NEW", 
                          standardizeddisaggregate %in% c("Age/Sex"),
                          age_2019 %in% c("10-14","15-19","20-24","25-29"),
                          sex=="Female")

###### VISUAL 1 ##########################

viz1_all <- viz_package %>% 
              summarize(across(cumulative, \(x) sum(x, na.rm = TRUE)),.groups="drop")

# Can use this code to check #s by country against the DREAMS dashboard for Q2 since
# it we are still working to get all country data to match
# group_by(operatingunit) %>% 
#  summarize(across(cumulative, \(x) sum(x, na.rm = TRUE)),.groups="drop") %>% print()

# NOTES: Botswana, Eswatini, Rwanda, South Africa, South Sudan, Zimbabwe match  
# Cote & Kenya match once you switch the DSNU that changed
# Malawi doesn't due to error that needs to be fixed in DSNU sheet
# Namibia - close to matching (nukundu issue)
# Tanzania does not (due to minor issue)
# Uganda, Lesotho, Haiti still outstanding on our end


## Now filter for USAID only numbers
viz1_USAID<- viz_package %>%
  filter(str_detect(agencies_FY23, "USAID")) %>% 
  summarize(across(cumulative, \(x) sum(x, na.rm = TRUE)),.groups="drop")


##### VISUAL 2 #####################


### Number for bottom box - package completion
## Filter for only complete disaggregates & USAID
viz2_completed<-viz_package %>% filter(str_detect(agencies_FY23, "USAID"),
                               standardizeddisaggregate %in% c("Age/Sex/Time/Complete+",
                                               "Age/Sex/Time/Complete")) %>%
  summarize(across(cumulative, \(x) sum(x, na.rm = TRUE)),.groups="drop") %>% print()

## Numbers for education & econ boxes
## Filter for education and econ disaggs + USAID
viz2_service<-viz_service %>% filter(str_detect(agencies_FY23, "USAID")) %>%
             group_by(standardizeddisaggregate) %>%
             summarize(across(cumulative, \(x) sum(x, na.rm = TRUE)),.groups="drop") %>% print()
      

## Numbers for PrEP
viz2_prep <- viz_prep %>% filter(str_detect(agencies_FY23, "USAID")) %>%
             summarize(across(cumulative, \(x) sum(x, na.rm = TRUE)),.groups="drop") %>% print()


### TESTING #####
#dlist<-unique(dsnu_list$dsnu)
#msdlist<-unique(df_orig$dsnu)
#setdiff(dlist, msdlist)
#setdiff(msdlist, dlist)

## Temeke MC is TZ and looks like it is not a USAID district
## Uganda issues: "Lira City", "Kampala District","Kayunga District","Masaka City"        
## ["Mbarara City","Fort Portal City","Gulu City" -- all Uganda 


