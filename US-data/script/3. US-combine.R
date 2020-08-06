######################################################################################################
######################################################################################################
################################           [US COMBINING DATA]            ############################
######################################################################################################
######################################################################################################

# Paper: [Title]
# Authors: [Authors]
# Date creation: 2020-07-31
# Last Update: 2020-08-06

#####################################################################################################




#### 0. House Cleaning ####

set.seed(7736) # From random.org


# Needed packages
pkgs <- c("tidyverse", "data.table", "USAboundaries", "rvest", "httr")


# Install and/or Update packages
install_update <- function(x) {
  if (x %in% rownames(installed.packages()) == F) {
    install.packages(x, dependencies = T, repos = "http://cran.us.r-project.org")
  }
  if (x %in% rownames(old.packages() == T)) {
    update.packages(x, repos = "http://cran.us.r-project.org")
  }
}

lapply(pkgs, install_update)

# Load packages
lapply(pkgs, require, character.only = T)
rm(pkgs, install_update)


# US boundaries
states <- state_codes %>%
  filter(jurisdiction_type != "territory") 




#####################################################################################################



#### 1. Combining Files ####

temp <- tibble(
  year = as.Date(rep(paste0(1700:2019,"-01-01"), times = 51)),
  state = rep(states$state_abbr, each = 320)) %>%
  left_join(., read_csv("./US-data/raw/US-governors.csv"), by = c("state", "year")) %>% #
  left_join(., read_csv("./US-data/raw/US-police-employee.csv"), by = c("state", "year")) %>% #
  left_join(., read_csv("./US-data/raw/US-crime-stats.csv"), by = c("state", "year")) %>%
  left_join(., read_csv("./US-data/raw/US-ucr-participation.csv"), by = c("state", "year")) %>% #fix states
  left_join(., read_csv("./US-data/raw/US-per-capita-personal-income.csv"), by = c("state", "year")) %>% #
  left_join(., read_csv("./US-data/raw/US-unemployment-rate.csv"), by = c("state", "year")) %>% #
  left_join(., read_csv("./US-data/raw/US-population.csv"), by = c("state", "year")) %>% #
  left_join(., read_csv("./US-data/raw/US-population-by-characteristics-v2.csv"), by = c("state", "year")) %>%
  left_join(., read_csv("./US-data/raw/US-prison-population.csv",
                        col_types = cols(.default = "?",
                                                          "const.prison.pop./.general.population" = "n",
                                                          "max_annual_pctg_change_year" = "c",
                                                          "year_highest_pctg_chance" = "c",
                                                          "peak_year_pctg_annual_change" = "c",
                                                          "max_annual_pctg_change_year" = "c",
                                                          "year_highest_pctg_chance" = "c"
                                         )), by = c("state", "year"))
  
  



################################################################################################################


  
#### 2. Data Wrangling ####

temp <- temp %>%
  filter(year >= as.Date("1945-01-01")) %>% # adjust minimum year here
  mutate(                                   # adjust age groups here
    age_00_17 = select(.,`00`:`17`) %>% rowSums(na.rm = T),
    age_18_21 = select(.,`18`:`21`) %>% rowSums(na.rm = T),
    age_21_30 = select(.,`21`:`30`) %>% rowSums(na.rm = T),
    age_31_40 = select(.,`31`:`40`) %>% rowSums(na.rm = T),
    age_41_50 = select(.,`41`:`50`) %>% rowSums(na.rm = T),
    age_51_60 = select(.,`51`:`60`) %>% rowSums(na.rm = T),
    age_60_85 = select(.,`60`:`85`) %>% rowSums(na.rm = T)
  ) %>%
  mutate(aux = select(., contains("age_")) %>% rowSums(na.rm = T)) %>%
  mutate_at(vars(contains("age_")), list(~ifelse(aux == 0, NA,.))) %>%
  select(-(`00`:`85`), -aux)



################################################################################################################



#### 3. Saving Data ####
write_csv(temp, path = "./US-data/US-combined.csv")
write_rds(temp, path = "./US-data/US-combined.rds")


################################################################################################################



#### End of File ####