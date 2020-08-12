######################################################################################################
######################################################################################################
################################           [CROSS COUNTRY DATA]            ###########################
######################################################################################################
######################################################################################################

# Paper: [Title]
# Authors: [Authors]
# Date creation: 2020-06-05
# Last Update: 2020-06-05

#####################################################################################################


#### 0. House Cleaning ####

set.seed(29841) # From random.org


# Needed packages
pkgs <- c("tidyverse", "countrycode", "data.table")


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




#####################################################################################################


#### 1. Load Data ####


# Cross Country (our data)
cross_country <- read_csv("./cross-country/temp/cross-country2.csv")

# Quality of Government dataset 

qog_standard <- fread("http://www.qogdata.pol.gu.se/data/qog_std_ts_jan20.csv") %>%
  select(
    year,
    cname,
    contains("ccode"),
    contains("aii"),
    contains("fh"),
    contains("gcb"),
    contains("jaep"),
    contains("vdem"),
    contains("who"),
    contains("gle_gdp"),
    contains("gle_rgdpc"),
    contains("wdi_gdpgr"),
    contains("wdi_gdppppcon2011"),
    contains("wdi_gini"),
    contains("pwt_rgdp"),
    contains("arda_chprtpct"),
    contains("bti_eo"),
    contains("wdi_pop"),
    contains("polity")
  )



#### 2. Data Wrangling ####


# Country Codes
cross_country <- cross_country %>%
  mutate(
    country = recode(country,
                     "Botsuwana" = "Botswana",
                     "Lybia" = "Libya",
                     "Maldivas" = "Maldives",
                     "Micronesia" = "Federated States of Micronesia"),
    country_code = countrycode(sourcevar = country,
                               origin = "country.name.en",
                               destination = "iso3c"),
    country_code = ifelse(country == "Kosovo", "RKS", country_code)
  )



# Binding datasets 
cross_country <- cross_country %>%
  left_join(qog_standard, by = c("country_code" = "ccodealp", "year" = "year")) %>%
  rename("ccodealp" = "country_code") %>%
  select(country,
         country_code,
         contains("ccode"),
         -cname,
         everything())


#### 3. Saving Data ####
write_csv(cross_country, path = "./cross-country/cross-country3.csv")



#### End of File ####