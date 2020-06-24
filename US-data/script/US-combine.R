######################################################################################################
######################################################################################################
################################           [US COMBINING DATA]            ############################
######################################################################################################
######################################################################################################

# Paper: [Title]
# Authors: [Authors]
# Date creation: 2020-06-08
# Last Update: 2020-06-15

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
  left_join(., read_csv("./US-data/raw/US-governors.csv"), by = c("state", "year")) %>% #ok
  left_join(., read_csv("./US-data/raw/US-police-employee.csv"), by = c("state", "year")) %>% #ok
  left_join(., read_csv("./US-data/raw/US-crime-stats.csv"), by = c("state", "year")) %>% #ok
  left_join(., read_csv("./US-data/raw/US-ucr-participation.csv"), by = c("state", "year")) %>% #fix states
  left_join(., read_csv("./US-data/raw/US-per-capita-personal-income.csv"), by = c("state", "year")) %>% #ok
  left_join(., read_csv("./US-data/raw/US-unemployment-rate.csv"), by = c("state", "year")) %>% #ok
  left_join(., read_csv("./US-data/raw/US-population.csv"), by = c("state", "year")) %>% #ok
  left_join(., read_csv("./US-data/raw/US-population-by-characteristics.csv"), by = c("state", "year")) %>% #ok
  filter(year >= as.Date("1945-01-01"))
  

################################################################################################################



#### 2. Data Wrangling ####




################################################################################################################



#### 3. Saving Data ####

write_csv(temp, "US-combined.csv")


################################################################################################################



#### End of File ####