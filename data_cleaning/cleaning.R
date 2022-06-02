library(tidyverse)
library(janitor)
library(here)
library(readxl)
library(scales)
library(modelr)
library(infer)
library(GGally)
library(ggfortify)
library(leaps)
library(fable)
library(fabletools)
library(tsibble)
library(tsibbledata)
library(urca)
library(lubridate)




# Set the home directory ----

here::here()


# Install functions ----

source(here("data_cleaning/functions.R"))


# Explore the data ----

excel_sheets(here("data/International Labour Productivity - Europe.xls"))
excel_sheets(here("data/UK Education Productivity.xlsx"))
excel_sheets(here("data/UK Labour Productivity - Industry division.xls"))
excel_sheets(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"))
excel_sheets(here("data/UK Labour Productivity - Region by Industry.xls"))
excel_sheets(here("data/UK Labour Productivity - Industry division.xls"))

# Create a dictionary of job categories ----

industry_dict <- 
  read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
                       sheet = "14. Great Britain",
                       range = "C5:V6") %>% 
  clean_names() %>% 
  pivot_longer(cols = everything(), 
               names_to = "industry_group", 
               values_to = "industry") %>% 
  relocate(industry, .before = "industry_group") %>% 
  mutate(industry_group = str_replace(industry_group, "[0-9]", ""),
         industry_group = str_replace(industry_group, "_$", ""),
         industry_group = str_replace_all(industry_group, "_", " "),
         industry_group = str_replace(industry_group, "mod at", "modat")) %>% 
  write_csv(here("clean_data/industry_dict_clean.csv"))

# UK ranking by industry type within Europe ----

europe_labour_prod <- read_excel(here("data/International Labour Productivity - Europe.xls"),
                                 sheet = "Table 1",
                                 range = "A4:AE13") %>% 
  clean_names() %>% 
  rename("industry" = "a_10_excl_l",
         "industry_group" = "nace_industry") %>%
  mutate(industry = str_replace(industry, "B-E", "BCDE"),
         industry = str_replace(industry, "G-I", "GHI"),
         industry = str_replace(industry, "M-N", "MN"),
         industry = str_replace(industry, "O-Q", "OPQ"),
         industry = str_replace(industry, "R-U", "RSTU")) %>% 
  pivot_longer(cols = -c("industry", "industry_group"),
               names_to = "country") %>% 
  mutate(country = str_replace(country, "_", " "),
         country = str_to_title(country)) %>% 
  write_csv(here("clean_data/europe_labour_productivity_clean.csv"))



# UK Regional Output Per Hour CVM by Industry ----


# Try to handle merged cells from .xls; 

region_by_industry_output_per_hour <-   
  read_excel(here("data/UK Labour Productivity - Region by Industry.xls"),
                                   sheet = "OpH (value)",
                                   range = "A6:HN27") %>% 
  clean_names() 
  
# Rename "unnamed" Column Headers

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

# Combine the two headers that came from the excel merged cells ----

names(region_by_industry_output_per_hour) <- 
  paste(names(region_by_industry_output_per_hour),
        region_by_industry_output_per_hour[1,], 
        sep = "_") 

# Tidy up the column headers following the merge and get rid of the 
# "wrong" first row before pivoting longer for easier processing;

region_by_industry_output_per_hour <- region_by_industry_output_per_hour %>% 
  rename("year" = "x1_NA") %>% 
  clean_names() %>% 
  filter(uk_all_industries != "All Industries") %>% 
  pivot_longer(cols = -year, 
               names_to = "industry", 
               values_to = "pounds_per_hour_worked") %>% 
  mutate(year = as.integer(year),
         industry = str_replace_all(industry, "[0-9]", ""),
         industry = str_replace(industry, "_east", "east"),
         industry = str_replace(industry, "_mid", "mid"),
         industry = str_replace(industry, "_west", "west"),
         industry = str_replace(industry, "_and", ""),
         industry = str_replace(industry, "_ireland", "ireland"),
         industry = str_replace(industry, "_the_", ""),
         industry = str_replace(industry, "all_", "all"),
         industry = str_replace(industry, "a_b_d_e", "abde"),
         industry = str_replace(industry, "s_t", "st"),
         pounds_per_hour_worked = as.numeric(pounds_per_hour_worked))


# Separate 'industry' into two columns (industry / region) now that they've been pivoted.

region_by_industry_output_per_hour <- region_by_industry_output_per_hour %>%
  separate(industry, into = c("region", "industry"), sep = "_") %>% 
  mutate(industry = str_to_upper(industry)) %>% 
  write_csv(here("clean_data/region_by_industry_output_per_hour_clean.csv"))
  

# No. Jobs per Region UK ----


jobs_northeast <-
  read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
             sheet = "1. North East",
             range = "A6:V97") %>% 
  cleanup() %>% 
  mutate(region = "northeast")


jobs_northwest <-
  read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
             sheet = "2. North West",
             range = "A6:V97") %>% 
  cleanup() %>% 
  mutate(region = "northwest")

jobs_york <-
  read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
             sheet = "3. Yorkshire and The Humber",
             range = "A6:V97") %>% 
  cleanup() %>% 
  mutate(region = "yorkshirehumber")

jobs_eastmidlands <-
  read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
             sheet = "4. East Midlands",
             range = "A6:V97") %>% 
  cleanup() %>% 
  mutate(region = "eastmidlands")

jobs_westmidlands <-
  read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
             sheet = "5. West Midlands",
             range = "A6:V97") %>% 
  cleanup() %>% 
  mutate(region = "westmidlands")

jobs_eastengland <-
  read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
             sheet = "6. East of England",
             range = "A6:V97") %>% 
  cleanup() %>% 
  mutate(region = "east")

jobs_london <-
  read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
             sheet = "7. London",
             range = "A6:V97") %>% 
  cleanup() %>% 
  mutate(region = "london")

jobs_southeast <-
  read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
             sheet = "8. South East",
             range = "A6:V97") %>% 
  cleanup() %>% 
  mutate(region = "southeast")

jobs_southwest <-
  read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
             sheet = "9. South West",
             range = "A6:V97") %>% 
  cleanup() %>% 
  mutate(region = "southwest")

jobs_wales <-
  read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
             sheet = "11. Wales",
             range = "A6:V97") %>% 
  cleanup() %>% 
  mutate(region = "wales")

jobs_scotland <-
  read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
             sheet = "12. Scotland",
             range = "A6:V97") %>% 
  cleanup() %>% 
  mutate(region = "scotland")

jobs_nireland <-
  read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
             sheet = "13. Northern Ireland",
             range = "A6:V97") %>% 
  cleanup() %>% 
  mutate(region = "northernireland")


# Bind rows to create one data set of jobs per region in the uk;

jobs_regional_bound <- bind_rows(jobs_eastengland,
                                 jobs_eastmidlands,
                                 jobs_london,
                                 jobs_nireland,
                                 jobs_northeast,
                                 jobs_northwest,
                                 jobs_scotland,
                                 jobs_southeast,
                                 jobs_southwest,
                                 jobs_wales,
                                 jobs_westmidlands,
                                 jobs_york) %>% 
  left_join(industry_dict, by = "industry")



# Now that the no. jobs per region is set up (jobs_regional_bound) it needs to be 
# joined to region_by_industry_output_per_hour, but first the industry groupings 
# 'ABDE' and 'ST' need to be sorted ----


abde_sum <- jobs_regional_bound %>%
  group_by(quarter, region) %>%
  filter(industry == "A"|
           industry == "B"|
           industry == "D"|
           industry == "E") %>%
  summarise(abde_sum = sum(avg_jobs_000)) %>%
  mutate(industry = "ABDE",
         industry_group = "agriculture, mining, water, electricity") %>% 
  rename("avg_jobs_000" = "abde_sum") %>% 
  relocate("industry", .after = "quarter") %>% 
  relocate("region", .after = "avg_jobs_000")


st_sum <- jobs_regional_bound %>%
  group_by(quarter, region) %>%
  filter(industry == "S"|
         industry == "T") %>%
  summarise(st_sum = sum(avg_jobs_000)) %>%
  mutate(industry = "ST",
         industry_group = "other services and domestic")%>% 
  rename("avg_jobs_000" = "st_sum") %>% 
  relocate("industry", .after = "quarter")%>% 
  relocate("region", .after = "avg_jobs_000")

jobs_regional_bound_grouped <- jobs_regional_bound %>%
  filter(!(industry %in% c("A", "B", "D", "E", "S", "T"))) %>%
  bind_rows(abde_sum) %>%
  bind_rows(st_sum) %>%
  arrange(quarter, industry)


# Join `region_by_industry_output_per_hour` to start the base model ----

jobs_regional_bound_grouped_join <- jobs_regional_bound_grouped %>% 
  mutate(year = year(quarter), .after = quarter) %>% 
  filter(year %in% 1997:2016)  # filter to match timescale of joining object

model_base_data <- region_by_industry_output_per_hour %>%
  filter(industry != "ALLINDUSTRIES") %>%
  filter(region != "uk") %>%
  right_join(jobs_regional_bound_grouped_join, 
             by = c("year", "industry", "region"))

# and check for NAs

model_base_data %>%
  summarise(across(.cols = everything(), ~sum(is.na(.x))))

# No NAs.

# At this point some memory can be cleared out to make things more efficient;

rm(jobs_eastengland,
   jobs_eastmidlands,
   jobs_london,
   jobs_nireland,
   jobs_northeast,
   jobs_northwest,
   jobs_scotland,
   jobs_southeast,
   jobs_southwest,
   jobs_wales,
   jobs_westmidlands,
   jobs_york,
   abde_sum,
   st_sum)


# Try to incorporate employment age demographic ----

ages <- read_excel(here("data/additional_data/Labour market - Age Group (not seasonally adjusted).xls"),
                   sheet = "People (16+)",
                   range = "A5:BM328") %>% 
  clean_names()


names(ages) <-   paste(names(ages),ages[1:2,], sep = "_")

ages <- ages %>% 
  rename("year" = "x1_c(NA, NA)") %>% 
  select(c("year",contains("c(\"Employment\", \"level\")")),
         -c( "aged_16_and_over_c(\"Employment\", \"level\")", 
             "aged_16_64_c(\"Employment\", \"level\")")) %>%
  rename( "employed_age_16_17" = "aged_16_17_c(\"Employment\", \"level\")",
          "employed_age_18_24" = "aged_18_24_c(\"Employment\", \"level\")",
          "employed_age_25_34" = "aged_25_34_c(\"Employment\", \"level\")",
          "employed_age_35_49" = "aged_35_49_c(\"Employment\", \"level\")",
          "employed_age_50_64" = "aged_50_64_c(\"Employment\", \"level\")",
          "employed_age_65" = "age_65_c(\"Employment\", \"level\")"    )


# Check for NAs

ages %>% 
  summarise(across(.cols = everything(), ~sum(is.na(.x))))

# The only NAs in the data are where header rows were merged in the excel file.
# These can be removed, then set employment values as integers.

ages <- ages %>% 
  filter(!is.na(year)) %>% 
  mutate(across(.cols = -year, .fns = as.integer))

# `ages` now contains useable data, but the year column needs to be separated
# and then summarised by year;

ages <- ages %>% 
  separate(year, into = c("months", "year_split"), sep = " ", convert = TRUE) %>% 
  select(-months)

ages_annual <- ages %>% 
  group_by(year_split) %>% 
  summarise(age_16_17 = as.integer(mean(employed_age_16_17)),
            age_18_24 = as.integer(mean(employed_age_18_24)),
            age_25_34 = as.integer(mean(employed_age_25_34)),
            age_35_49 = as.integer(mean(employed_age_35_49)),
            age_50_64 = as.integer(mean(employed_age_50_64)),
            age_65 = as.integer(mean(employed_age_65))) %>% 
  rename("year" = "year_split") 


model_ages <- region_by_industry_output_per_hour %>% 
  filter(industry == "ALLINDUSTRIES" & region == "uk") %>% 
  left_join(ages_annual, by = "year")
  

# Add in UK Commuting Data ----

total_commutes <- read_excel(here("data/additional_data/Commuting Data UK.xlsx"),
                      sheet = "3",
                      range = "B33:J47") %>% 
  clean_names() %>% 
  rename(region = total) %>% 
  select(-c(x1, x2, x1_2)) %>% 
  rename(under_15min = x0_42808631519621082,
         btwn_16_30min = x0_32130028701091307,
         btwn_31_45min = x0_12080650991560932,
         btwn_46_60min = x8_4020775710665449e_2,
         over_61min = x4_578611216660132e_2) %>% 
  pivot_longer(cols = -region,
               names_to = "commute_time",
               values_to = "pcntge_workforce") %>% 
  mutate(region = str_to_lower(region),
         region = str_replace(region, " & the ", ""),
         region = str_replace(region, " ", ""))

# Join UK regional commute times to base model data ----

model_base_commute <- model_base_data %>% 
  filter(year == "2016") %>% 
  select(region, pounds_per_hour_worked, avg_jobs_000) %>% 
  group_by(region) %>% 
  summarise(mean_pphw = mean(pounds_per_hour_worked),
            mean_jobs = mean(avg_jobs_000)) 


model_commute_region <- model_base_commute %>% 
  filter(region != "Workplace otuside UK") %>% 
  left_join(total_commutes, by = "region") %>% 
  group_by(region)




# OECD Measurement of % of 25 - 64 year olds with Tertiary Education ----

# https://data.oecd.org/eduatt/population-with-tertiary-education.htm#indicator-chart


# The OECD value presented reflects the % of adults (25 - 64 yr old) who have
# studied at a tertiary education level (vocational, college, uni etc).

education <- read_excel(here("data/additional_data/OECD Adult Education Levels.xlsx")) %>% 
  clean_names() %>% 
  select(location, time, value) %>% 
  rename("education_score" = "value",
         "year" = "time") %>% 
  country_names()

# Check for NAs;

education %>% 
summarise(across(.cols = everything(), ~sum(is.na(.x))))


# No NAs.

# European Labour Productivity (aim to join with education) ----

euro_labour <- read_csv(here("data/additional_data/Labour Productivity - OECD Countries.csv")) %>% 
  clean_names() %>% 
  select(location, time, value) %>% 
  rename("year" = "time",
         "productivity_score" = "value") %>% 
  country_names()

# Check for NAs;

euro_labour %>% 
  summarise(across(.cols = everything(), ~sum(is.na(.x))))

# No NAs.

# Join education with productivity score and reduce the overall timescale to
# eliminate NAs; 

euro_labour_join <- euro_labour %>% 
  filter(year %in% c(2005:2015)) %>% 
  group_by(country, year) %>% 
  summarise(avg_prod = mean(productivity_score))

education_join <- education %>% 
  filter(year %in% c(2005:2015))
# Ireland 2016 education data missing.

eu_lab_edu <- bind_cols(euro_labour_join, education_join$education_score) %>% 
  rename("education_score" = "...4") %>% 
  ungroup()

eu_NA <- eu_lab_edu %>% 
  summarise(across(.cols = everything(), ~ sum(is.na(.x))))

# No NAs.

# Second Explanatory Model Setup

base_2 <- region_by_industry_output_per_hour %>% 
  filter(region == "uk" & industry == "ALLINDUSTRIES") %>% 
  select(-industry)

education_uk <- education %>% 
  filter(country == "United Kingdom") %>% 
  filter(year < 2017)

base_2 <- base_2 %>% 
  bind_cols(education_uk$education_score) %>% 
  rename("education_score" = "...4")


jobs_base_2 <- jobs_regional_bound %>% 
  separate(quarter, into = c("year", "quarter"), sep = " ") %>% 
  select(-c(quarter, region, industry, industry_group)) %>% 
  filter(year %in% (1997:2016)) %>% 
  group_by(year) %>% 
  summarise(jobs_000 = sum(avg_jobs_000))

base_2 <- base_2 %>% 
  bind_cols(jobs_base_2$jobs_000) %>% 
  rename("jobs_000" = "...5")



# Data models ready for analysis ----


model_base_data %>% 
  write_csv(here("clean_data/model_base_clean.csv"))

model_ages %>% 
  write_csv(here("clean_data/age_model_clean.csv"))

total_commutes %>% 
  write_csv(here("clean_data/uk_regional_commute_times.csv"))

eu_lab_edu %>% 
  write_csv(here("clean_data/eu_education_productivity"))
