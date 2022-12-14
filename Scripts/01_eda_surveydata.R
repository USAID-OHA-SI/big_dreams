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

# GLOBAL VARIABLES ------------------------------------------------------------
  
  ref_id <- "0ffffad3"
  
  load_secrets()
  
  seed <- set.seed(22222)

# IMPORT ----------------------------------------------------------------------

  path <- "Data/finaldataset.csv"
  
  df <- read_csv(path, show_col_types = FALSE)

# MUNGE -----------------------------------------------------------------------
  
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
  
  age_pop <- clean_df %>%
    select(country, age) %>%
    mutate(
      age = as.numeric(age)) %>%
    filter(age > 14 & age < 25) %>%
    mutate(
      yi = sum(nrow(.)),
      sd = sd(age), 
      cv = raster::cv(age))
  
  # Do I have the data I need for a Simple area-level model
  
  # yi = number of agyw 15-24
  # sd = std. dev of yi
  # cv = coef. of var for yi
  # SmallArea = areas of interest (ideally DSNU, likely PSNU or SNU)
  # ni = small area sample sizes  (ideally, DSNU, likely PSNU or SNU sample sizes)
  # MajorArea = major areas containing the smaller areas of interest (ideally, district, likely OU)
  
  model <- df$yi ~ as.factor(df$MajorArea)
  
  # Mean Squared Error estimator
  df_fh_model_mse <- mseFH(model, df$SD^2)
  
  # create the dataframe with results
  df_results_df <- data.frame(Area = df$SmallArea, 
                                SampleSize = df$ni, 
                                DIR = df$yi, 
                                cv.DIR = 100 * df$CV, 
                                eblup.FH = df_fh_model_mse$est$eblup) %>%
    mutate(
      # CV estimator for MSE estimator 
      df_cv_fh = as.numeric(100 * sqrt(df_fh_model_mse$mse) / df_fh_model_mse$est$eblup)) %>%
    arrange(-SampleSize) %>%
    pivot_longer(cols = c(DIR, eblup.FH, cv.DIR, df_cv_fh),
                 names_to = "estimator_type", 
                 values_to = "value")
  
  # Can we use the survey response data?
  
  # what kind of missing data do we have?
  # are data missing at random?
  # if we can safely assume data are MAR (TBD!)
  # we would proceed to select which to include/impute
  
  # skim_results <- clean_df%>%
  #   skim() %>%
  #   select(skim_variable, n_missing, complete_rate) %>%
  #   arrange(complete_rate)
  # 
  # skim_results_2 <- skim_results %>%
  #   filter(complete_rate > 0)
  
  
 
  
  
  
  
  
  