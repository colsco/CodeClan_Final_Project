# Regional Excel Worksheets Cleanup Function  cleanup() ----

print("****************** READING FUNCTIONS ****************** ")

cleanup <- function(jobs_region){
  
  jobs_region_clean <- jobs_region %>% 
    clean_names() %>% 
    select(-x2) %>%
    rename("date" = "sic_2007_section") %>%
    mutate(date = str_replace(date, " \\([pr]\\)", "")) %>% 
    mutate(date = str_replace(date, " 9", "-199")) %>% 
    mutate(date = str_replace(date, " 0", "-200")) %>% 
    mutate(date = str_replace(date, " 1", "-201")) %>% 
    mutate(date_date = my(date), .after = date) %>% 
    select(-date) %>% 
    rename("date" = "date_date") %>% 
    mutate(quarter = tsibble::yearquarter(date), .after = date) %>% 
    select(-date) %>% 
    pivot_longer(cols = -quarter, 
                 names_to = "industry", 
                 values_to = "no_jobs_000") %>% 
    mutate(industry = str_to_upper(industry)) %>% 
    group_by(quarter, industry) %>% 
    mutate(avg_jobs_000 = mean(no_jobs_000)) %>% 
    select(-no_jobs_000) %>% 
    # filter(quarter >= 1998 & year <= 2016) %>%  # to be consistent with EU data set
    unique()
  
  
  return(jobs_region_clean)

}


 # Country Acronyms to Names Function ----


country_names <- function(acronyms_in){
  
  names_out <- acronyms_in %>% 

filter(location %in% c("NOR", "LUX", "BEL", "IRL", "NLD", "DNK", "CHE", "SWE", 
                       "FRA", "DEU", "AUT", "ESP", "ITA", "FIN", "SVK", "GBR", 
                       "CZE", "PRT", "LTU", "EST", "SVN", "POL", "HUN", "GRC", 
                       "LVA")) %>% 
  mutate(country = case_when(
    location == "NOR" ~ "Norway",
    location == "LUX" ~ "Luxembourg",
    location == "BEL" ~ "Belgium",
    location == "IRL" ~ "Ireland",
    location == "NLD" ~ "Netherlands",
    location == "DNK" ~ "Denmark",
    location == "CHE" ~ "Switzerland",
    location == "SWE" ~ "Sweden",
    location == "FRA" ~ "France",
    location == "DEU" ~ "Germany",
    location == "AUT" ~ "Austria",
    location == "ESP" ~ "Spain",
    location == "ITA" ~ "Italy",
    location == "FIN" ~ "Finland",
    location == "SVK" ~ "Slovakia",
    location == "GBR" ~ "United Kingdom",
    location == "CZE" ~ "Czech Republic",
    location == "PRT" ~ "Portugal",
    location == "LTU" ~ "Lithuania",
    location == "EST" ~ "Estonia",
    location == "SVN" ~ "Slovenia",
    location == "POL" ~ "Poland",
    location == "HUN" ~ "Hungary",
    location == "GRC" ~ "Greece",
    location == "LVA" ~ "Latvia")) %>% 
  relocate(country, .after = location) %>% 
  select(-location)
  
  return(names_out)
}
 