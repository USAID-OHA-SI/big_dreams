# PROJECT:  big_dreams
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  FY23 Q2 Tableau dashboards users review
# REF ID:   890ae8ea
# LICENSE:  GPL v3 +
# DATE:     2023-07-20
# UPDATED:

# DEPENDENCIES ----------------------------------------------------------------

library(tidyverse)
library(readr)
library(gagglr)
library(glue)
library(scales)
library(extrafont)
library(tidytext)
library(patchwork)
library(ggtext)
library(googlesheets4)
library(ggrepel)
library(janitor)
library(lubridate)

# FUNCTIONS --------------------------------------------------------------------

# remove views of the cover page
flag_cover_pg <- function(df, remove = FALSE){
  df <- df %>% 
    mutate(is_cover = str_detect(view_name,
                                 "^Cover |^Cover$|CoverPage|Cover Page"))
  
  if(remove == TRUE)
    df <- filter(df, is_cover != TRUE)
  
  return(df)
}

# global vars ------------------------------------------------------------------

data_in <- "Data/Unique Users by Dashboard (HQ vs. Field)_Full Data_data (2).csv"

# duplicated users
bonus_hq <- c("Betz|Mcdavid|Rose|Schelar")
bonus_mission <- c("Ojehomon")

# users with incomplete or unclear division information
siei <- c("Petrovic|Carroll")
pct <- c("Crow|Rodrigues")
aa <- c("Greensides|Frost")

# import -----------------------------------------------------------------------

df_usage <- read_csv(data_in) %>%
  clean_names()

# munge ------------------------------------------------------------------------

df_prep <- df_usage %>%
  clean_names() %>%
  rename(week_visited = week_of_user_login_at, 
         login_date_time = user_login_at) %>%
  mutate(week_visited = mdy(week_visited), 
         login_date_time = mdy_hms(login_date_time), 
         day_visited = ymd(str_extract(login_date_time, "^\\S*")), 
         user_type = if_else(user_type == "Field", "Mission", user_type), 
         division = if_else(is.na(division) == TRUE, user_or_group_name, division), 
         division = if_else(str_detect(division, "GH.OHA") == TRUE, "GH/OHA", division), 
         division = if_else(str_detect(division, "GH.PEPFAR.Field") == TRUE, "GH/Mission", division)) %>%
  distinct(view_name, friendly_name, user_type, division, week_visited, day_visited) %>%
  select(view_name, friendly_name, user_type, division, week_visited, day_visited) %>%
  # fix known errors with some users listed under both Mission and HQ, and 
  # some users not listed under their correct division, remove dashboard
  # maintainers, and remove views of cover pages
  filter(!friendly_name %in% c("Watson, Lakeshia M. (GH:Public Health Institute)", 
                               "Hoehner, Jessica (GH/OHA/SIEI)", 
                               "Petrovic, Nada")) %>% 
  mutate(
    user_type = case_when(
      str_detect(friendly_name, bonus_hq) ~ "HQ", 
      str_detect(friendly_name, bonus_mission) ~ "Mission",
      TRUE ~ user_type), 
    division = case_when(
      str_detect(friendly_name, siei) ~ "SIEI", 
      str_detect(friendly_name, pct) ~ "PCT",
      str_detect(friendly_name, aa) ~ "AA",
      TRUE ~ division)) %>%
  flag_cover_pg(., remove = TRUE)

# identify users with more than one type, this will result in double counting
# and over estimating usage

user_dups <- df_prep %>%
  select(friendly_name, user_type, division) %>%
  distinct() %>%
  group_by(friendly_name) %>%
  filter(n() > 1)
# add test to verify that there are no users listed twice with different user types
# and check for novel cases

# Summarize --------------------------------------------------------------------

date_range <- df_prep %>% 
  summarise(min_date = min(day_visited, na.rm = TRUE),
            max_date = max(day_visited, na.rm = TRUE)) %>% 
  mutate(range = glue("{as.Date(min_date) %>% format('%B %d, %Y')} to {as.Date(max_date) %>% format('%B %d, %Y')}")) %>% 
  pull()

# how many unique visitors in dashboard

# per week?

df_week <- df_prep %>%
  group_by(week_visited) %>%
  distinct(week_visited, friendly_name) %>%
  count()

# per day
df_day <- df_prep %>%
  group_by(day_visited) %>%
  distinct(day_visited, friendly_name) %>%
  count()

# who are the most frequent visitors per week?

  # HQ or Mission?

df_usertype <- df_prep %>%
  group_by(week_visited) %>%
  distinct(week_visited, user_type, friendly_name) %>%
  count(week_visited, user_type) 

  # Division

df_division <- df_prep %>%
  group_by(week_visited) %>%
  distinct(division, friendly_name) %>%
  count(division) 

  # Individuals

df_name <- df_prep %>%
  count(friendly_name, division) %>%
  distinct()

# "power users" (who accessed > 10 times in a week)

df_power <- df_name %>%
  filter(n > 10)

  # which pages have the most unique views/visits?
df_views <- df_prep %>%
  distinct(week_visited, view_name, friendly_name) %>%
  count(week_visited, view_name)

  # Are certain views more popular with HQ vs field?
df_views_type <- df_prep %>%
  distinct(week_visited, view_name, friendly_name, user_type) %>%
  count(week_visited, view_name, user_type)

# Who is looking at which pages?
#Ex: DREAMS_FP analysis
df_views_user <- df_prep %>%
  count(week_visited, view_name, friendly_name)

# viz --------------------------------------------------------------------------

# how has viewership varied week to week

# views by week, by HQ/Mission, Division

Q1i <- as_date("2023-02-19")
Q1c <- as_date("2023-03-26")

df_week %>%
  ggplot(aes(week_visited, n)) +
  geom_col(aes(alpha = 0.7)) +
  geom_vline(linetype = "dotted", color = trolley_grey, 
             aes(xintercept = as.numeric(Q1i))) +
  geom_vline(linetype = "dotted", color = trolley_grey, 
             aes(xintercept = as.numeric(Q1c))) +
  scale_x_date(date_breaks = "1 week",
               date_labels = "%b %d") +
  scale_y_continuous(limits = c(0, 13)) +
  labs(x = NULL, y = NULL,
       title = "Heaviest usage around COP co-planning meetings, no spikes around new data releases",
       subtitle = "Unique Weekly Visitors",
       caption = glue("A visitor is a unique person who accessed at least one dashboard page between {date_range}.  
       Source: DREAMS Tableau Usage data")) +
  si_style_ygrid()+
  theme(legend.position = "none")

# Who are these users?

df_usertype %>%
  ggplot(aes(week_visited, n, 
             group_by = user_type, fill = user_type)) +
  geom_col() +
  geom_vline(linetype = "dotted", color = trolley_grey, 
             aes(xintercept = as.numeric(Q1i))) +
  geom_vline(linetype = "dotted", color = trolley_grey, 
             aes(xintercept = as.numeric(Q1c))) +
  scale_x_date(date_breaks = "1 week",
               date_labels = "%b %d") +
  scale_y_continuous(limits = c(0, 13)) +
  scale_fill_manual(values = c("Mission" = golden_sand, 
                               "HQ" = moody_blue)) +
  labs(x = NULL, y = NULL,
       title = "Largely used by HQ Staff overall, Mission staff during COP",
       subtitle = "Unique Weekly Visitors by User Type",
       caption = glue("A visitor is a unique person who accessed at least one dashboard page between {date_range}.  
       Source: DREAMS Tableau Usage data")) +
  si_style_ygrid()+
  theme(legend.position = "none")

# Which Divisions use this dashboard the most?

df_division %>%
  ggplot(aes(week_visited, n, 
             group_by = division, fill = division)) +
  geom_col() +
  geom_vline(linetype = "dotted", color = trolley_grey, 
             aes(xintercept = as.numeric(Q1i))) +
  geom_vline(linetype = "dotted", color = trolley_grey, 
             aes(xintercept = as.numeric(Q1c))) +
  scale_x_date(date_breaks = "1 week",
               date_labels = "%b %d") +
  scale_y_continuous(limits = c(0, 13)) +
  scale_fill_manual(values = si_palettes$siei) +
  labs(x = NULL, y = NULL,
       title = "Used by Mission Staff ahead of COP and consistently by SIEI and PPIR",
       subtitle = "Unique Weekly Visitors by Division including Missions, SIEI, PPIR, PCT, AA, and SCC",
       caption = glue("A visitor is a unique person who accessed at least one dashboard page between {date_range}.  
       Source: DREAMS Tableau Usage data")) +
  si_style_ygrid() +
  theme(legend.position = "none")

# Which pages are most popular overall? 

df_views %>%
  complete(week_visited, view_name) %>%
  mutate(n = replace_na(n, 0)) %>%
  ggplot(aes(week_visited, 
             fct_reorder(view_name, n, .desc = FALSE))) +
  geom_tile(aes(fill = n)) +
   # geom_text(aes(label = n, size = 1), 
   #           color = "white") +
  scale_fill_viridis_c(limits = c(0, 10), 
                       breaks = c(0, 5, 10)) +
  labs(x = NULL, y = NULL,
       title = "Target Completion and Targets vs Results most popular",
       subtitle = "Unique Weekly Visitors by each page",
       caption = glue("A view is a unique person who accessed at least one dashboard page between {date_range}.  
       Source: DREAMS Tableau Usage data")) +
  si_style_ygrid() +
  theme(legend.position = "none")

# Do we see different pages being popular between Mission and HQ users?

df_views_type %>%
  complete(week_visited, view_name, user_type) %>%
  mutate(n = replace_na(n, 0)) %>%
  ggplot(aes(week_visited, 
             fct_reorder(view_name, n, .desc = FALSE))) +
  geom_tile(aes(fill = n)) +
  geom_text(aes(label = n, size = 1), 
               color = "white") +
  facet_wrap(~user_type) +
  scale_fill_viridis_c(limits = c(0, 10), 
                       breaks = c(0, 5, 10)) +
  labs(x = NULL, y = NULL,
       title = "HQ Users make up the most active users of all pages, some pages not viewed at all",
       subtitle = "Unique Weekly Visitors to each page by User Type",
       caption = glue("A visitor is a unique person who accessed at least one dashboard page between {date_range}.
                       Views of the cover page have been excluded.
       Source: DREAMS Tableau Usage data")) +
  si_style_nolines() +
  theme(legend.position = "none")
