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

# GLOBAL VARIABLES ------------------------------------------------------------
  
  ref_id <- "3c230858"
  set.seed(22222)

# IMPORT ----------------------------------------------------------------------
  
  path <- "Data/finaldataset.csv"
  
  df <- read_csv(path, show_col_types = FALSE)

# MUNGE -----------------------------------------------------------------------
  
  # population size estimate for each region
  # world_pop or any other estimates available?
  
  data(world_pop)
  libratr
  
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  