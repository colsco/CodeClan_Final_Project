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

industry_dict <- read_excel(here("data/UK_Labour_Productivity_Jobs_in_Regions_by_Industry.xls"),
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



# Possibilities for linear regression predictors - begin setup...

region_by_industry_output_per_hour <-   
  read_excel(here("data/UK_Labour_Productivity_Region_by_Industry.xls"),
                                   sheet = "OpH (CVM)",
                                   range = "A6:HN25") %>% 
  clean_names() %>% 
  rename("year" = "x1") %>% 
  pivot_longer(cols = -year, 
               names_to = "industry", 
               values_to = "hourly_output_cvm") %>% 
  mutate(year = as.integer(year))

# It would be helpful to separate 'industry' into two columns now that they've 
# been pivoted.

region_by_industry_output_per_hour <- region_by_industry_output_per_hour %>%
 mutate(industry = str_replace(industry, "_ireland", "ireland"), 
        industry = str_replace(industry, "north_west", "northwest"),
        industry = str_replace(industry, "north_east", "northeast"),
        industry = str_replace(industry, "south_west", "southwest"),
        industry = str_replace(industry, "south_east", "southeast"),
        industry = str_replace(industry, "_and_", ""),
        industry = str_replace(industry, "the_", ""),
        industry = str_replace(industry, "_midlands", "midlands")) %>% 
  separate(industry, into = c("industry", "region"), sep = "_")






# write_csv(here("clean_data/region_by_industry_output_per_hour.csv"))
  
