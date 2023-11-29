# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  WAD 2023: DREAMS USAID DSNU Country MAPS
# REF ID:   2f10a1f0 
# LICENSE:  MIT
# DATE:     2023-11-27
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
  library(gisr)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()
  
  ref_id <- "2f10a1f0"
  
  # Set up paths
  merdata <- glamr::si_path("path_msd")
  rasdata <- glamr::si_path("path_raster")
  shpdata <- glamr::si_path("path_vector")
  datim   <- glamr::si_path("path_datim")
  dir_terr <- glamr::si_path("path_raster")
  
  # Google sheet ID for DREAMS DSNU list
  g_id <- "1YaGWvpkiXVPiAwA3KyHwFtbZHrVeCz6DijWPCKpUN-Q"

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
  
  msd_dsnu_xwalk <- msd_dsnu_xwalk %>% 
    left_join(dsnu_list, by=c("operatingunit", "cop22_psnu" = "psnu", "dsnu_new"= "dsnu")) %>% 
    filter(str_detect(agencies_FY23, "USAID"))
  # shape files ----------------------------------------------------------
  
  # Load the shapefiles to grab boundaries from below
  spdf_pepfar <- get_vcpolygons(path = shpdata, name = "VcPepfarPolygons.shp")
  cntry <- "Swaziland"
  
  adm0 <- gisr::get_admin0(cntry)
  adm1 <- gisr::get_admin1(cntry)
  
  get_shp_boundaries <- function(cntry, lvl) {
    
    ouuid = grabr::get_ouuid(cntry, username = datim_user(), password = datim_pwd())
    
    orgs <- grabr::get_ouorgs(
      ouuid = ouuid,
      level = lvl,
      username = datim_user(),
      password = datim_pwd()) %>% 
      mutate(org_level = lvl)
    
    # filter spdf
    spdf_cntry <- spdf_pepfar %>%
      dplyr::left_join(orgs, by = "uid") %>%
      dplyr::filter(!is.na(orgunit))
    
    return(spdf_cntry)
    
  }
  
  cntry <- "Namibia"
  
  plot_map <- function(cntry) {
    
    spdf_cntry <- get_shp_boundaries(cntry, 3)
    
    if (cntry %in% c("Lesotho")) {
      spdf_snu <- get_shp_boundaries(cntry, 4)
    } else {
      spdf_snu <- get_shp_boundaries(cntry, 5)
    }
    
   map <- spdf_snu %>% 
      left_join(msd_dsnu_xwalk %>% filter(operatingunit == cntry), by = c("uid" = "dsnuuid_new")) %>% 
      mutate(fill_color = ifelse(is.na(dsnu_new), trolley_grey_light, moody_blue)) %>% 
      ggplot() +
      geom_sf(fill = NA) +
      geom_sf(data = spdf_cntry, fill = gray(.92), lty = "dashed") +
      geom_sf(aes(fill = fill_color)) +
      scale_fill_identity() +
      labs(title = cntry %>% toupper(),
           subtitle = "<span style='color:#8980cb'>**DREAMS districts where USAID**</span> contributes to DREAMS programming") +
      # ggplot2::geom_sf_text(data = spdf_snu,
      #                       ggplot2::aes(label = orgunit),
      #                       #color = "white",
      #                       family = "Source Sans Pro Light") +
      si_style_map() +
      theme(plot.subtitle = element_markdown())
   
   return(map)
  }

  dreams_countries <- c("Lesotho", "Eswatini", "Zambia", "Namibia")
  
  map(dreams_countries, ~ si_save(paste0("Graphics/", .x, "_map.svg"), plot_map(.x)))
  map(dreams_countries, ~ si_save(paste0("Images/", .x, "_map.png"), plot_map(.x)))
  
  plot_map("Lesotho")  
  plot_map("Eswatini")  #add footnote to map about the Ikhundala regions
  plot_map("Zambia") 
  plot_map("Namibia")
 
  
  
  
