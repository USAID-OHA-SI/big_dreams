# PROJECT:  big_dreams
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  Population Size Estimation for Eswatini
# REF ID:   0ffffad3 
# LICENSE:  MIT
# DATE:     2022-12-08
# UPDATED: 

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

  # # Install developmental FIESTAutils first
  # remotes::install_github("USDAForestService/FIESTAutils",
  #                         dependencies = TRUE)
  # 
  # # Then install developmental FIESTA
  # remotes::install_github("USDAForestService/FIESTA",
  #                         dependencies = TRUE)

# GLOBAL VARIABLES ------------------------------------------------------------
  
  ref_id <- "0ffffad3"
  
  load_secrets()
  
  seed <- set.seed(22222)

# IMPORT ----------------------------------------------------------------------

  path <- "Data/finaldataset.csv"
  
  df <- read_csv(path, show_col_types = FALSE)

# MUNGE -----------------------------------------------------------------------
  
 # what kind of missing data do we have?
 # are data missing at random?
  
  skim_results <- clean_df %>%skim()
  
  clean_df <- df %>%
    clean_names() %>%
    mutate(
      hivstatusfinal = na_if(hivstatusfinal, 99), 
      hivstatusfinal = if_else(hivstatusfinal == 2, 0, hivstatusfinal)) %>%
    # cannot include, all obs are missing
    select(country, personid, centroidid, 
           intwt0, hivstatusfinal, age, school, 
           pregnancy, transactional, 
           sexpartners, irregular, sexviolence)
  
  # if we can safely assume data are MAR 
  # we would proceed to select which to include/impute
  
  # skim_results <- clean_df%>%
  #   skim() %>%
  #   select(skim_variable, n_missing, complete_rate) %>%
  #   arrange(complete_rate)
  # 
  # skim_results_2 <- skim_results %>%
  #   filter(complete_rate > 0)
  
  
 
  
  
  
  
  
  