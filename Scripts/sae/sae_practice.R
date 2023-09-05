# PROJECT:  big_dreams
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  practicing implementation of SAE in R
# REF ID:   46872ff8 
# LICENSE:  MIT
# DATE:     2022-12-09
# UPDATED: 
# NOTES: Examples and data come from sae: 
#         An R Package for Small Area
#         Estimation

# DEPENDENCIES ----------------------------------------------------------------
  
  library(tidyverse)
  library(extrafont)
  library(gagglr)
  library(sae)
  library(ggplot2)

# GLOBAL VARIABLES ------------------------------------------------------------
  
  ref_id <- "46872ff8"
  set.seed(22222)

# IMPORT ----------------------------------------------------------------------
  
  data(milk)
  
  data(grapes)
  data(grapesprox)
  
  data(spacetime)
  data(spacetimeprox)
  
  data(cornsoybean)
  data(cornsoybeanmeans)

# MUNGE -----------------------------------------------------------------------

# Example 1 - milk
  
  # Simple area-level model
  # yi = avg. expenditure on fresh milk in 1989
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
  
# Example 2 - grapes
  
  # grapehect = mean surface area used for grape production
  #             in each municipality
  # var = sampling variance of graphect
  
  # auxilary vars: 
  # area = surface area used for grape production
  #        in each municipality
  # workdays = avg. num of working days in the reference year
  
  grapes_model <- grapehect ~ area + workdays - 1
  
  grapes_sfh <- mseSFH(grapes_model, var, grapesprox, 
                       data = grapes)

  grapes_results <- data.frame(
                        Area = grapes$area,
                        DIR = grapes$grapehect,
                        cv.DIR = 100 * abs(sqrt(grapes$var) / grapes$grapehect),
                        eblup.SFH = grapes_sfh$est$eblup) %>%
    mutate(grapes_cv_sfh = as.numeric(100 * sqrt(grapes_sfh$mse) / grapes_sfh$est$eblup)) %>%
    arrange(cv.DIR) %>%
    pivot_longer(cols = c(DIR, cv.DIR, eblup.SFH, grapes_cv_sfh),
                 names_to = "estimator_type", 
                 values_to = "value")
  
  # Example 3 - spacetime
  # parametric boostrap MSE ests
  
  # means for each area and period of time (direct est)
  # Area = area code
  # Time = period of time
  # Var = sampling variance of direct est
  
  # number of areas
  n_areas <- nrow(spacetimeprox)
  
  # number of time periods
  n_times <- length(unique(spacetime$Time)) 
  
  # direct est = period of time 1 + period of time 2
  st_model <- Y ~ X1 + X2
  
  # fit the model with 250 bootstrap iterations
  st_stfh <- pbmseSTFH(st_model, n_areas, n_times, B = 250,
                       Var, spacetimeprox, data = spacetime)
  
  st_results <- data.frame(Area = spacetime$Area, 
                           Time = spacetime$Time,
                           DIR = spacetime$Y,
                           eblup.STFH = st_stfh$est$eblup) %>%
    mutate(
      st_cv_dir = as.numeric(100 * sqrt(spacetime$Var) / spacetime$Y),
      st_cv_stfh = as.numeric(100 * sqrt(st_stfh$mse) / st_stfh$est$eblup)) %>%
    # filter for most recent time point
    filter(Time == 3) %>%
    pivot_longer(cols = c(DIR, st_cv_dir, 
                          eblup.STFH, 
                          st_cv_stfh),
                 names_to = "estimator_type", 
                 values_to = "value")
  
  # Example 4 - county means of corn crop hectares
  
  x_mean <- cornsoybeanmeans %>%
            dplyr::select(CountyIndex, starts_with("Mean"))
  
  pop_n <- cornsoybeanmeans %>%
            dplyr::select(CountyIndex, PopnSegments)
  
  cs_df <- cornsoybean %>%
    filter(!(County == 12 & CornHec == 88.59 & 
            CornPix == 340 & SoyBeansPix == 87))
  
  cs_BHF <- pbmseBHF(CornHec ~ CornPix + SoyBeansPix, 
                     dom = County, meanxpop = x_mean,
                     popnsize = pop_n, B = 200, data = cornsoybean)
  
  cs_results <- data.frame(CountyIndex = cs_BHF$est$eblup$domain,
                           CountyName = cornsoybeanmeans$CountyName,
                           SampleSize = cs_BHF$est$eblup$sampsize,
                           eblup.BHF = cs_BHF$est$eblup$eblup) %>%
    mutate(
      cv_BHF = 100 * sqrt(cs_BHF$mse$mse) / cs_BHF$est$eblup$eblup)
  
# VIZ -------------------------------------------------------------------------
  
# milk example figures
  
# Figure 1 left
# Direct and Estimated Values by Area
  milk_results_df %>%
    filter(estimator_type %in% c("DIR", "eblup.FH")) %>%
    ggplot(aes(x = Area)) +
    geom_point(aes(y = value, color = estimator_type)) +
    geom_line(aes(y = value, color = estimator_type)) +
    scale_color_manual(labels = c("Direct Estimate", 
                      "Empirical best linear unbiased predictors (EBLUPs) Estimate"),
                       values = c(denim, 
                                  denim_light)) +
    scale_y_continuous(limits = c(0.0, 1.7)) +
    scale_x_continuous(limits = c(0.0, 45)) +
    si_style_nolines() +
    theme(legend.title = element_blank())
  
# Figure 1 right
# CVs of Direct and Estimated Values by Area
  
  milk_results_df %>%
    filter(estimator_type %in% c("cv.DIR", "milk_cv_fh")) %>%
    ggplot(aes(x = Area)) +
    geom_point(aes(y = value, color = estimator_type)) +
    geom_line(aes(y = value, color = estimator_type)) +
    scale_color_manual(labels = c("Direct Estimate", 
                      "Empirical best linear unbiased predictors (EBLUPs) Estimate"),
                       values = c(denim, 
                                  denim_light)) +
    scale_y_continuous(limits = c(0.0, 45)) +
    scale_x_continuous(limits = c(0.0, 45)) +
    si_style_nolines() +
    theme(legend.title = element_blank())
  
# grapes example figures
  
  # Figure 2 left
  # Direct and Estimated Values by Area
  
  grapes_results %>%
    filter(estimator_type %in% c("DIR", "eblup.SFH")) %>%
    ggplot(aes(x = Area)) +
    geom_point(aes(y = value, color = estimator_type)) +
    scale_color_manual(labels = c("Direct Estimate", 
                       "Spatial empirical best linear unbiased predictors (EBLUP) model estimate"),
                       values = c(moody_blue, 
                                  moody_blue_light)) +
    scale_y_continuous(limits = c(0.0, 500)) +
    scale_x_continuous(limits = c(0.0, 3000)) +
    si_style_nolines() +
    theme(legend.title = element_blank())
  
  # Figure 2 right
  # CVs of Direct and Estimated Values by Area
  
  grapes_results %>%
    filter(estimator_type %in% c("cv.DIR", "grapes_cv_sfh")) %>%
    ggplot(aes(x = Area)) +
    geom_point(aes(y = value, color = estimator_type)) +
    scale_color_manual(labels = c("Direct Estimate", 
                                  "Spatial empirical best linear unbiased predictors (EBLUP) model estimate"),
                       values = c(moody_blue, 
                                  moody_blue_light)) +
    scale_y_continuous(limits = c(0.0, 20000)) +
    scale_x_continuous(limits = c(0.0, 3000)) +
    si_style_nolines() +
    theme(legend.title = element_blank())
  
# spacetime example figures
  
  # Figure 3 left
  # Direct and Estimated Values by Area (time = 3)
  
  st_results %>%
    filter(estimator_type %in% c("DIR", "eblup.STFH")) %>%
    ggplot(aes(x = Area)) +
    geom_point(aes(y = value, color = estimator_type)) +
    geom_line(aes(y = value, color = estimator_type)) +
    scale_color_manual(labels = c("Direct Estimate", 
                                  "Spatio-Temporal empirical best linear unbiased predictors (EBLUP) model estimate"),
                       values = c(genoa, 
                                  genoa_light)) +
    scale_y_continuous(limits = c(0.0, 0.5)) +
    scale_x_continuous(limits = c(0.0, 55)) +
    si_style_nolines() +
    theme(legend.title = element_blank())
  
  # Figure 3 right
  # CVs of Direct and Estimated Values by Area (time = 3)
  
  st_results %>%
    filter(estimator_type %in% c("st_cv_dir", "st_cv_stfh")) %>%
    ggplot(aes(x = Area)) +
    geom_point(aes(y = value, color = estimator_type)) +
    geom_line(aes(y = value, color = estimator_type)) +
    scale_color_manual(labels = c("Direct Estimate", 
                                  "Spatial empirical best linear unbiased predictors (EBLUP) model estimate"),
                       values = c(genoa, 
                                  genoa_light)) +
    scale_y_continuous(limits = c(0.0, 30)) +
    scale_x_continuous(limits = c(0.0, 55)) +
    si_style_nolines() +
    theme(legend.title = element_blank())
  
  