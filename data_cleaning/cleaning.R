library(tidyverse)
library(janitor)
library(here)
library(readxl)
library(openxlsx)



here::here()


# Explore the data

excel_sheets(here("data/International Labour Productivity - Europe.xls"))
excel_sheets(here("data/UK Education Productivity.xlsx"))
excel_sheets(here("data/UK Labour Productivity Industry division.xls"))
excel_sheets(here("data/UK Labour Productivity Jobs in Regions by Industry.xls"))
excel_sheets(here("data/UK Labour Productivity Region by Industry.xls"))

# Create a dictionary of job categories;

industry_dict <- 
  read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
                       sheet = "14. Great Britain",
                       range = "C5:V6") %>% 
  clean_names() %>% 
  pivot_longer(cols = everything()) %>% 
  relocate(value, .before = "name") %>% 
  write_csv(here("clean_data/industry_dict.csv"))

# UK ranking by industry type within Europe;

europe_labour_prod <- read_excel(here("data/International Labour Productivity - Europe.xls"),
                                 sheet = "Table 1",
                                 range = "A4:AE13") %>% 
  clean_names() %>% 
  rename("industry" = "a_10_excl_l",
         "industry_group" = "nace_industry") %>% 
  pivot_longer(cols = -c("industry", "industry_group"),
               names_to = "country") %>% 
  write_csv(here("clean_data/europe_labour_productivity.csv"))



# Possibilities for linear regression predictors - begin setup...


# Try to handle merged cells from .xls; 



region_by_industry_output_per_hour <-   
  read_excel(here("data/UK Labour Productivity - Region by Industry.xls"),
                                   sheet = "OpH (CVM)",
                                   range = "A6:HN26") %>% 
  clean_names() 
  
  
region_by_industry_output_per_hour <- region_by_industry_output_per_hour %>% 
  rename("uk" = c("x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", 
                  "x12", "x13", "x14", "x15", "x16", "x17", "x18"),
         
         "northeast" = c("x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", 
                         "x28", "x29", "x30", "x31", "x32", "x33", "x34", "x35"),
         
         "northwest" = c("x37", "x38", "x39", "x40", "x41", "x42", "x43", "x44", 
                         "x45", "x46", "x47", "x48", "x49", "x50", "x51", "x52"),
         
         "yorkshirehumber" = c("x54", "x55", "x56", "x57", "x58", "x59", "x60", 
                               "x61", "x62", "x63", "x64", "x65", "x66", "x67", 
                               "x68", "x69"),
         
         "eastmidlands" = c("x71", "x72", "x73", "x74", "x75", "x76", "x77", 
                            "x78", "x79", "x80", "x81", "x82", "x83", "x84", 
                            "x85", "x86"),
         
         "westmidlands" = c("x88", "x89", "x90", "x91", "x92", "x93", "x94", 
                            "x95", "x96", "x97", "x98", "x99", "x100", "x101", 
                            "x102", "x103"),
         
         "east" = c("x105", "x106", "x107", "x108", "x109", "x110", "x111", "x112", 
                    "x113", "x114", "x115", "x116", "x117", "x118", "x119", "x120"),
         
         "london" = c("x122", "x123", "x124", "x125", "x126", "x127", "x128", 
                      "x129", "x130", "x131", "x132", "x133", "x134", "x135", 
                      "x136", "x137"),
         
         "southeast" = c("x139", "x140", "x141", "x142", "x143", "x144", "x145", 
                         "x146", "x147", "x148", "x149", "x150", "x151", "x152", 
                         "x153", "x154"),
         
         "southwest" = c("x156", "x157", "x158", "x159", "x160", "x161", "x162", 
                         "x163", "x164", "x165", "x166", "x167", "x168", "x169", 
                         "x170", "x171"),
         
         "wales" = c("x173", "x174", "x175", "x176", "x177", "x178", "x179", 
                     "x180", "x181", "x182", "x183", "x184", "x185", "x186", 
                     "x187","x188"),
         
         "scotland" = c("x190", "x191", "x192", "x193", "x194", "x195", "x196", 
                        "x197", "x198", "x199", "x200", "x201", "x202", "x203", 
                        "x204", "x205"),
         
         "northernireland" = c("x207", "x208", "x209", "x210", "x211", "x212", 
                               "x213", "x214", "x215", "x216", "x217", "x218", 
                               "x219", "x220", "x221", "x222")
  ) 

names(region_by_industry_output_per_hour) <- 
  paste(names(region_by_industry_output_per_hour),
        region_by_industry_output_per_hour[1,], 
        sep = "_") 

region_by_industry_output_per_hour <- region_by_industry_output_per_hour %>% 
  rename("year" = "x1_NA") %>% 
  clean_names() %>% 
  filter(uk_all_industries != "All Industries") %>% 
  pivot_longer(cols = -year, 
               names_to = "industry", 
               values_to = "hourly_output_cvm") %>% 
  mutate(year = as.integer(year),
         industry = str_replace_all(industry, "[0-9]", ""),
         industry = str_replace(industry, "_east", "east"),
         industry = str_replace(industry, "_mid", "mid"),
         industry = str_replace(industry, "_west", "west"),
         industry = str_replace(industry, "_and", ""),
         industry = str_replace(industry, "northern_ireland", "northernireland"),
         industry = str_replace(industry, "_the_", ""))





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
  
