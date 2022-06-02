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
