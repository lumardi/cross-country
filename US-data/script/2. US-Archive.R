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
                           skipEmptyCols=T)
  

#State Prison Population
prison_population <- as_tibble(
  openxlsx::read.xlsx("./US-data/raw/Archive/state prison populations copy.xlsx",
                      startRow=2,
                      skipEmptyRows=T,
                      skipEmptyCols=T),
  .name_repair = "unique")



#State Summaries
state_summaries_1 <- openxlsx::read.xlsx("./US-data/raw/Archive/state summaries copy.xlsx",
                                         startRow=3,
                                         sheet=1,
                                         skipEmptyRows=T,
                                         skipEmptyCols=T)


state_summaries_2 <- openxlsx::read.xlsx("./US-data/raw/Archive/state summaries copy.xlsx",
                                         startRow=2,
                                         sheet=2,
                                         skipEmptyRows=T,
                                         skipEmptyCols=T,
                                         colNames=F)


state_summaries_3 <- openxlsx::read.xlsx("./US-data/raw/Archive/state summaries copy.xlsx",
                                         startRow=2,
                                         sheet=3,
                                         skipEmptyRows=T,
                                         skipEmptyCols=T)


################################################################################################################



#### 2. Data Wrangling ####


# 2.1. Cartel Federalism
cartel_federalism <- cartel_federalism %>%
  select(-X1) %>%
  rename("state" = "X2",
         "year" = "X3") %>%
  mutate(state = gsub("[^A-z]","", state),
         state = gsub("[`]","", state)) %>%
  right_join(., 
             tibble(
               year = rep(c(1880:2016), times = 50),
               state = rep(subset(states$state_abbr, states$state_abbr != "DC"), each = 137)
             ))


# 2.2 Prison Population

  # fixing column names
st <- rep(c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC"),
          each = 4)
vars <- rep(c("const.prison.pop.net", "annual.pct.change", "census.general.population", "(const prison pop / general pop) *100000"),
            times = 51)
vars <- paste0(st,"_",vars)
vars <- c("year", vars)

prison_population <- prison_population %>%
  rename("year" = "X1") %>%
  select(-`Fed:.const.prison.pop.net`, -`annual.%.change...207`)

colnames(prison_population) <- vars


  # transforming to tidy data format

aux <- prison_population %>%
  gather(key, value, -year) %>%
  mutate(state = gsub("_.*", "", key),
         key = gsub(".*_", "", key)) %>%
  spread(key, value)

#aux <- aux %>%
#  mutate(check = ifelse(grepl("average", year), "average", "reg")) %>%
#  gather(key,value,-state,-year) %>%
#  spread(key, value)




# 2.3 State Summaries 1


# 2.4 State Summaries 2

net_1940 <- state_summaries_2 %>%
  select(X1:X2) %>%
  rename("state" = 1,
         "1940_net" = 2) %>%
  na.omit()


percapita_1940 <- state_summaries_2 %>%
  select(X3:X4) %>%
  rename("state" = 1,
         "1940_percapita" = 2) %>%
  na.omit()


net_2016 <- state_summaries_2 %>%
  select(X5:X6) %>%
  rename("state" = 1,
         "1940_percapita" = 2) %>%
  na.omit()


percapita_2016 <- state_summaries_2 %>%
  select(X7:X8) %>%
  rename("state" = 1,
         "1940_percapita" = 2) %>%
  na.omit()


highest_change <- state_summaries_2 %>%
  select(X9:X11) %>%
  rename("state" = 1,
         "year_highest_pct_chance" = 2,
         "pct_change" = 3
         ) %>%
  na.omit()


avg_net <- state_summaries_2 %>%
  select(X12:X13) %>%
  rename("state" = 1,
         "1940_percapita" = 2) %>%
  na.omit()



# 2.5 State Summaries 3




################################################################################################################



#### End of File ####