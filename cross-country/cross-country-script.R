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
pkgs <- c("tidyverse", "countrycode")


# Install if not already installed
installIfNot <- function(x) {
  if (x %in% rownames(installed.packages()) == FALSE) {
    install.packages(x, dependencies = T, repos = "http://cran.us.r-project.org")
  }
}

lapply(pkgs, installIfNot)

# Load packages
lapply(pkgs, require, character.only = T)
rm(pkgs, installIfNot)


#####################################################################################################


#### 1. Load Data ####


# Cross Country (our data)
cross_country <- read_csv("./cross-country2.csv")

# Quality of Government dataset 

#Variables of Interest: 
#2.10 Judicial: aii*, fh*, gcb*, iaep*, vdem*, who*, 
#2.16. Public Economy: gle_gdp, gle_rgdpc; wdi_gdpgr, wdi_gdppppcon2011, wdi_gini, pwt_rgdp
#2.18 Religion: arda_chprtpct
#2.2 Civil Society: bti_eo, wdi_pop*, polity

qog_standard <- read_csv("./qog_std_cs_jan20.csv") %>%
  select(
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


#####################################################################################################


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
  left_join(qog_standard, by = c("country_code" = "ccodealp")) %>%
  select(country,
         country_code,
         contains("ccode"),
         everything())




#####################################################################################################


#### 3. Saving Data ####


write_csv(cross_country, path = "./cross-country3.csv")

write_csv(qog_standard, path = "qog_mod.csv")



#### End of File ####