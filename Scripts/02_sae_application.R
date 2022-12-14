# PROJECT:  big_dreams
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  Testing SAE models on survey data
# REF ID:   3c230858 
# LICENSE:  MIT
# DATE:     2022-12-09
# UPDATED: 

# DEPENDENCIES ----------------------------------------------------------------
  
  library(tidyverse)
  library(dplyr)
  library(extrafont)
  library(gagglr)
  library(sae)
  library(ggplot2)
  library(readxl)
  library(janitor)

# GLOBAL VARIABLES ------------------------------------------------------------
  
  ref_id <- "3c230858"
  set.seed(22222)

# IMPORT ----------------------------------------------------------------------
  
  path <- "Data/finaldataset.csv"
  
  # https://population.un.org/wpp/Download/Standard/CSV/
  un_data <- "Data/WPP2022_POP_F01_3_POPULATION_SINGLE_AGE_FEMALE.xlsx"
 
  df <- read_csv(path, show_col_types = FALSE)
  
  esw_df <- read_xlsx(un_data)

# MUNGE -----------------------------------------------------------------------
  
  # population size estimate for each region
  # need std dev and coef of vars around each estimate

   esw_15_24_year <- esw_df %>%
    clean_names() %>%
    filter(x3 == "Eswatini" | x1 == "Index" |
           x12 == "Female population by single age (thousands)") %>%
    row_to_names(2, remove_rows_above = FALSE) %>%
    rename(
      "fem_pop_by_age_thousands" = `0`) %>%
    drop_na(Index) %>%
    dplyr::select(-Notes) %>%
    filter(Year > 2020) %>%
    pivot_longer(cols = `1`:`100+`,
                 names_to = "age",
                 values_to = "population") %>%
    mutate(
      # this age bracket goes to 100+, how high over 25+ do we
      # want to go?
      # NAs introduced because one of the values is "100+"
      age = as.numeric(age),
      population = as.numeric(population),
      population = as.numeric(population * 1000)) %>%
    filter(age < 26 & age > 14) %>%
    dplyr::select(Variant, `Region, subregion, country or area *`,
           `ISO3 Alpha-code`,
           Year,
           age,
           population) %>%
    group_by(Year, age) %>%
    summarize(population)
  
  # Pest = population estimate for a given year
  # n = number of months from P1 census (2016) 
  #      to date of estimate (2016?)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Alternatively from :
  # https://measureevaluation.org/resources/training/online-courses-and-resources/non-certificate-courses-and-mini-tutorials/population-analysis-for-planners/lesson-5.html
  
  # PestyearA = P1 + (n/N)(P2-P1)

# P1 = year x census count 
# P2 = year y census count 
# n = number of months from P1 to date of estimate
# N = number of months between census periods

# popest for 1999: 
# 1990 census pop = 181,835, 
# 2000 census pop = 223,314
# n = 111 (N - years between year to est and year x)
# N = 120 (12 * 10)

# Pest for 1999 = 
# Pest for 1999 =  181, 835 + (111/120) (223,314-181,835)
# Pest for 1999 =  181, 835 + (0.925)(41,479)
# Pest for 1999 = 220,203

# assumptions:
# the same number of people is added to the population each year and that growth is linear. 
# In most cases, locales do not increase by the same number of residents each year.
  # note: should multiply by annual population change factor available in UNWomen data
  
# Model ------------------------------------------------------------------------
  
  # Simple area-level model
  # yi = population of women ages 15-24 in Eswatini
  # sd = std. dev of yi
  # cv = coef. of var for yi
  # SmallArea = areas of interest
  # ni = small area sample sizes
  # MajorArea = major areas containing the smaller areas of interest
  
  milk_model <- milk$yi ~ as.factor(milk$MajorArea)
  
  # Mean Squared Error estimator
  milk_fh_model_mse <- mseFH(model, milk$SD^2)
  
  # create the dataframe with results
  milk_results_df <- data.frame(Area = milk$SmallArea, 
                                SampleSize = milk$ni, 
                                DIR = milk$yi, 
                                cv.DIR = 100 * milk$CV, 
                                eblup.FH = milk_fh_model_mse$est$eblup) %>%
    mutate(
      # CV estimator for MSE estimator 
      milk_cv_fh = as.numeric(100 * sqrt(milk_fh_model_mse$mse) / milk_fh_model_mse$est$eblup)) %>%
    arrange(-SampleSize) %>%
    pivot_longer(cols = c(DIR, eblup.FH, cv.DIR, milk_cv_fh),
                 names_to = "estimator_type", 
                 values_to = "value")
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  