library(tidyverse)
library(lubridate)
library(tsibble)
library(tsibbledata)
library(janitor)
library(here)
library(readxl)


here::here()


jobs_northeast <-
  read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
             sheet = "1. North East",
             range = "A6:V97") %>% 
  clean_names() %>% 
  select(-x2) %>%
  rename("date" = "sic_2007_section") %>%
  mutate(date = str_replace(date, " \\([pr]\\)", "")) %>% 
  mutate(date = str_replace(date, " 9", "-199")) %>% 
  mutate(date = str_replace(date, " 0", "-200")) %>% 
  mutate(date = str_replace(date, " 1", "-201")) %>% 
  mutate(date_date = my(date, tz = "GMT"), .after = date) %>% 
  select(-date) %>% 
  rename("date" = "date_date") %>% 
  mutate(quarter = tsibble::yearquarter(date), .after = date) %>% 
  select(-date) %>% 
  pivot_longer(cols = -quarter, 
               names_to = "industry", 
               values_to = "no_jobs_000") %>% 
  group_by(quarter, industry) %>% 
  mutate(avg_jobs_000 = mean(no_jobs_000)) %>% 
  select(-no_jobs_000) %>% 
  # filter(quarter >= 1998 & year <= 2016) %>%  # to be consistent with EU data set
  unique()

head(jobs_northeast)