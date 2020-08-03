######################################################################################################
######################################################################################################
################################           [US ARCHIVE]            ###################################
######################################################################################################
######################################################################################################

# Paper: [Title]
# Authors: [Authors]
# Date creation: 2020-08-03
# Last Update: 2020-08-03

#####################################################################################################


#### 0. House Cleaning ####

set.seed(6887) # From random.org


# Needed packages
pkgs <- c("tidyverse", "data.table", "openxlsx", "USAboundaries", "rvest", "httr")


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



#### 1. Reading Files ####


#Cartel Federalism
cartel_federalism <- openxlsx::read.xlsx("./US-data/raw/Archive/cartel federalism copy.xlsx",
                           startRow=2,
                           skipEmptyRows=T,
                           skipEmptyCols=T) %>%
  select(-X1) %>%
  rename("state" = "X2",
         "year" = "X3")
  

#State Prison Population
prison_population <- openxlsx::read.xlsx("./US-data/raw/Archive/state prison populations copy.xlsx",
                                         startRow=2,
                                         skipEmptyRows=T,
                                         skipEmptyCols=T) %>%
  rename("year" = "X1")



#State Summaries
state_summaries_1 <- openxlsx::read.xlsx("./US-data/raw/Archive/state summaries copy.xlsx",
                                         startRow=3,
                                         sheet=1,
                                         skipEmptyRows=T,
                                         skipEmptyCols=T)


state_summaries_2 <- openxlsx::read.xlsx("./US-data/raw/Archive/state summaries copy.xlsx",
                                         startRow=1,
                                         sheet=2,
                                         skipEmptyRows=T,
                                         skipEmptyCols=T)


state_summaries_3 <- openxlsx::read.xlsx("./US-data/raw/Archive/state summaries copy.xlsx",
                                         startRow=2,
                                         sheet=3,
                                         skipEmptyRows=T,
                                         skipEmptyCols=T)


################################################################################################################



#### 2. Data Wrangling ####







################################################################################################################



#### End of File ####