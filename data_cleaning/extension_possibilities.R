# EXTENSION POSSIBILITY ----
# Cleanup Function ----

# Need to import all jobs per region per industry, which will be many different
# worksheets from the same excel file.  All will need cleaned in the same way so
# it makes sense to write a function to do it.

# cleanup <- function(name){
#   
#   name <- clean_names(name) %>% 
#     select(-c(x2)) %>% 
#     rename("date" = "sic_2007_section") %>% 
#     mutate(date = str_replace(date, " \\([pr]\\)", "")) %>% 
#     separate(date, into = c("month", "year"), sep = " ") %>% 
#     mutate(year = if_else(year >= 96, paste0("19", year), paste0("20", year)),
#            year = as.integer(year))
#   
#   return(name)
#   
# }



# 
# jobs_northeast <- 
#   read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
#              sheet = "1. North East",
#              range = "A6:V97") 
# 
# jobs_northwest <- 
#   read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
#              sheet = "2. North West",
#              range = "A6:V97") 
# 
# jobs_york <- 
#   read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
#              sheet = "3. Yorkshire and The Humber",
#              range = "A6:V97") 
#   
# jobs_emid <- 
#   read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
#              sheet = "4. East Midlands",
#              range = "A6:V97") 
# 
# jobs_wmid <- 
#   read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
#              sheet = "5. West Midlands",
#              range = "A6:V97")
# 
# jobs_eengland <- 
#   read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
#              sheet = "6. East of England",
#              range = "A6:V97")
# 
# jobs_london <- 
#   read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
#              sheet = "7. London",
#              range = "A6:V97")
# 
# jobs_southeast <- 
#   read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
#              sheet = "8. South East",
#              range = "A6:V97")
# 
# jobs_southwest <- 
#   read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
#              sheet = "9. South West",
#              range = "A6:V97")
# 
# jobs_wales <- 
#   read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
#              sheet = "11. Wales",
#              range = "A6:V97")
# 
# jobs_scotland <- 
#   read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
#              sheet = "12. Scotland",
#              range = "A6:V97")
# 
# jobs_nireland <- 
#   read_excel(here("data/UK Labour Productivity - Jobs in Regions by Industry.xls"),
#              sheet = "13. Northern Ireland",
#              range = "A6:V97")

# END OF EXTENSION POSSIBILITY ----



