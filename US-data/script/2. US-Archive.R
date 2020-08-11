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


state_summaries_3 <- as_tibble(
  openxlsx::read.xlsx("./US-data/raw/Archive/state summaries copy.xlsx",
                      startRow=2,
                      sheet=3,
                      skipEmptyRows=T,
                      skipEmptyCols=T),
  .name_repair = "unique")
  


state_summaries_4 <- openxlsx::read.xlsx("./US-data/raw/Archive/state summaries copy.xlsx",
                                         startRow=1,
                                         sheet=4,
                                         skipEmptyRows=T,
                                         skipEmptyCols=T,
                                         colNames = F)



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


#.const.prison.pop.net -- FIX

  # fixing column names
st <- rep(c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC"),
          each = 4)
vars <- rep(c("const.prison.pop.net", "annual.pct.change", "census.general.population", "(const prison pop / general pop) *100000"),
            times = 51)
vars <- paste0(st,"_",vars)
vars <- c("year", vars)

prison_population <- prison_population %>%
  rename("year" = "X1") %>%
  select(-`Fed:.const.prison.pop.net`, -`annual.%.change...207`) %>%
  `colnames<-`(vars)

  # transforming to tidy data format
prison_population <- prison_population %>%
  gather(key, value, -year) %>%
  mutate(state = gsub("_.*", "", key),
         key = gsub(".*_", "", key)) %>%
  spread(key, value)

aux <- prison_population %>%
  filter(grepl("average", year)) %>%
  rename_with(~paste0("average1940_",.), !contains("year") & !contains("state")) %>%
  select(-year)

prison_population <- prison_population %>%
  filter(!grepl("average", year)) %>%
  mutate(year = as.numeric(year)) %>%
  left_join(aux) %>%
  right_join(., 
             tibble(
               year = rep(c(1880:2016), times = 50),
               state = rep(subset(states$state_abbr, states$state_abbr != "DC"), each = 137)
             ))



# 2.3 State Summaries 1
state_summaries_1 <- state_summaries_1 %>%
  `colnames<-`(c("state",
                 "oldest_year", "oldest_pop",
                 "most_recent_year", "most_recent_pop",
                 "annual_avg_pop",
                 "max_pop_year","max_pop",
                 "min_pop_year", "min_pop",
                 "max_annual_pctg_change_year","max_annual_pctg_change_pop",
                 "min_annual_pctg_change_year","min_annual_pctg_change_pop",
                 "avg_pctg_change_pop",
                 "oldest_per_capita_year", "oldest_per_capita_pop",
                 "most_recent_per_capita_year", "most_recent_per_capita_pop",
                 "min_per_capita_year", "min_per_capita_pop",
                 "max_per_capita_year", "max_per_capita_pop",
                 "avg_per_capita_pop"
                 )) %>%
  mutate(state = gsub(" *\\(.*\\).*", "", state))


# 2.4 State Summaries 2 

state_summaries_2 <- state_summaries_2 %>%
  mutate_if(is.character, list(~gsub(" \\(.*\\)","",.)))

aux1 <- state_summaries_2 %>%
  select(X1:X2) %>%
  rename("state" = 1,
         "1940_net" = 2) %>%
  na.omit()

aux2 <- state_summaries_2 %>%
  select(X3:X4) %>%
  rename("state" = 1,
         "1940_percapita" = 2) %>%
  na.omit()

aux3 <- state_summaries_2 %>%
  select(X5:X6) %>%
  rename("state" = 1,
         "2016_net" = 2) %>%
  na.omit()

aux4 <- state_summaries_2 %>%
  select(X7:X8) %>%
  rename("state" = 1,
         "2016_percapita" = 2) %>%
  na.omit()

aux5 <- state_summaries_2 %>%
  select(X9:X11) %>%
  rename("state" = 1,
         "year_highest_pctg_change" = 2,
         "pct_change" = 3
         ) %>%
  na.omit()

aux6 <- state_summaries_2 %>%
  select(X12:X13) %>%
  rename("state" = 1,
         "annual_avg_net" = 2) %>%
  na.omit()

aux7 <- state_summaries_2 %>%
  select(X14:X15) %>%
  rename("state" = 1,
         "annual_avg_net_change" = 2) %>%
  na.omit()

state_summaries_2 <- list(aux1,aux2,aux3,aux4,aux5,aux6,aux7) %>%
  reduce(~full_join(.x,.y, by = "state"))


# 2.5 State Summaries 3
state_summaries_3 <- state_summaries_3 %>%
  mutate(state = gsub(" *\\(.*\\).*", "", state)) %>%
  rename("peak_year_net" = 2, # Identical to Sheet 1 max_pop_year
         "peak_year_net_pop" = 3, # Identical to Sheet 1 max_pop
         "peak_year_per_capita" = 4, # Identical to Sheet 1 max_per_capita_year
         "peak_year_per_capita_pop" = 5, # Identical to Sheet 1 max_per_capita_pop
         "peak_year_pctg_annual_change" = 6, # Identical to Sheet 1 max_annual_pctg_change_year
         "peak_year_pctg_annual_change_pop" = 7) # Identical to Sheet 1 max_annual_pctg_change_pop


# 2.6 State Summaries 4
state_summaries_4 <- state_summaries_4 %>%
  rename("state" = 1,
         "Sheet4_pctg" = 2)



################################################################################################################


#### 3. Joining Files ####


prison_pop <- list(state_summaries_1, state_summaries_2, state_summaries_3, state_summaries_4) %>%
  reduce(~full_join(.x,.y,by="state")) %>%
  full_join(cartel_federalism, by = "state") %>% 
  full_join(prison_population, by = c("state","year")) %>%
  mutate(year = as.Date(paste0(year, "-01-01"))) %>%
  mutate_if(is.character, list(~gsub("\\,", ";", .)))
  

################################################################################################################


#### 4. Saving File ####
write_csv(prison_pop, path = "./US-data/raw/US-prison-population.csv")



#### End of File ####