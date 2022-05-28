library(tidyverse)
library(janitor)
library(here)
library(readxl)
library(openxlsx)



here::here()


# Explore the data

excel_sheets(here("data/International_Labour_Productivity_Europe.xls"))
excel_sheets(here("data/UK_Education_Productivity.xlsx"))
excel_sheets(here("data/UK_Labour_Productivity_Industry_division.xls"))
excel_sheets(here("data/UK_Labour_Productivity_Jobs_in_Regions_by_Industry.xls"))
excel_sheets(here("data/UK_Labour_Productivity_Region_by_Industry.xls"))

# Create a dictionary of job categories;

industry <- read_excel(here("data/UK_Labour_Productivity_Jobs_in_Regions_by_Industry.xls"),
                       sheet = "14. Great Britain",
                       range = "C5:V6") %>% 
  clean_names() %>% 
  pivot_longer(cols = everything()) %>% 
  relocate(value, .before = "name") %>% 
  write_csv(here("clean_data/industry_dict.csv"))

# UK ranking by industry type within Europe;

europe_labour_prod <- read_excel(here("data/International_Labour_Productivity_Europe.xls"),
                                 sheet = "Table 1",
                                 range = "A4:AE13") %>% 
  clean_names() %>% 
  rename("industry" = "a_10_excl_l",
         "industry_group" = "nace_industry") %>% 
  pivot_longer(cols = -c("industry", "industry_group"),
               names_to = "country") %>% 
  write_csv(here("clean_data/europe_labour_productivity.csv"))
