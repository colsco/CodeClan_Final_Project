# Regional Excel Worksheets Cleanup Function  cleanup() ----



cleanup <- function(jobs_region){

  jobs_region_clean <- jobs_region %>% 
    clean_names() %>% 
    select(-c(x2)) %>%
    rename("date" = "sic_2007_section") %>%
    mutate(date = str_replace(date, " \\([pr]\\)", "")) %>%
    separate(date, into = c("month", "year"), sep = " ") %>%
    mutate(year = if_else(year >= 96, paste0("19", year), paste0("20", year)),
           year = as.integer(year)) %>% 
    pivot_longer(cols = -c(month, year), 
                 names_to = "industry", 
                 values_to = "no_jobs_000") %>% 
    mutate(industry = str_to_upper(industry)) %>% 
    group_by(year, industry) %>% 
    mutate(avg_jobs_000 = mean(no_jobs_000)) %>% 
    select(-c(month, no_jobs_000)) %>% 
    filter(year >= 1998 & year <= 2016) %>%  # to be consistent with EU data set
    unique()
    

  return(jobs_region_clean)

}


  
  
  
  
  

