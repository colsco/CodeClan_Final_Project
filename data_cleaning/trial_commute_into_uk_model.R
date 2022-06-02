base_2 <- region_by_industry_output_per_hour %>% 
  filter(region == "uk" & industry == "ALLINDUSTRIES") %>% 
  select(-industry)

education_uk <- education %>% 
  filter(country == "United Kingdom") %>% 
  filter(year < 2017)

base_2 <- base_2 %>% 
  inner_join(education_uk, by = "year") %>% 
  select(-region)
