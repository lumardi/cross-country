######################################################################################################
######################################################################################################
################################           [DATABASES METADATA]            ###########################
######################################################################################################
######################################################################################################

# Paper: [Title]
# Authors: [Authors]
# Date creation: 2020-06-08
# Last Update: 2020-06-08

#####################################################################################################


#### 0. House Cleaning ####

set.seed(4758) # From random.org


# Needed packages
pkgs <- c("tidyverse", "data.table", "rvest", "RSelenium")


# Install if not already installed
installIfNot <- function(x) {
  if (x %in% rownames(installed.packages()) == F) {
    install.packages(x, dependencies = T, repos = "http://cran.us.r-project.org")
  }
}

lapply(pkgs, installIfNot)

# Load packages
lapply(pkgs, require, character.only = T)
rm(pkgs, installIfNot)



#####################################################################################################


#### 1. Federal Reserve Bank of St. Louis (FRED) ####


temp <- read_html("https://fred.stlouisfed.org/tags/series?ob=pv&od=desc&t=&et=&pageID=1")



#####################################################################################################


#### 2. Federal Reserve Bank of St. Louis (FRED) - US, States only, Annually ####


temp <- list()

for(i in 1:30){
  scrape = paste0("https://fred.stlouisfed.org/tags/series?ob=pv&od=desc&t=annual%3Bstate%3Busa&et=&pageID=30", i) %>%
    read_html()
  
  title = html_nodes(scrape, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "col-sm-10", " " ))]') %>%
    html_text() %>%
    enframe(name = NULL) %>%
    mutate_all(list(~gsub("\n","", .))) %>%
    mutate_all(list(~gsub("\t","", .))) %>%
    mutate_all(list(~gsub("[ ]{2,}", " ", .))) %>%
    mutate_all(list(~gsub("[ ]{3,}","", .))) %>%
    mutate_all(list(~gsub("^[ ]","", .))) %>%
    mutate_all(list(~gsub("[ ]$","", .))) %>%
    filter(value != "")
  
  obs = html_nodes(scrape, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "attributes", " " ))]') %>%
    html_text() %>%
    enframe(name = NULL) %>%
    mutate_all(list(~gsub("\n","", .))) %>%
    mutate_all(list(~gsub("\t","", .))) %>%
    mutate_all(list(~gsub("[ ]{2,}", " ", .))) %>%
    mutate_all(list(~gsub("[ ]{3,}","", .))) %>%
    mutate_all(list(~gsub("^[ ]","", .))) %>%
    mutate_all(list(~gsub("[ ]$","", .))) %>%
    filter(value != "")
  
  period = html_nodes(scrape, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "series-meta-dates", " " ))]') %>%
    html_text() %>%
    enframe(name = NULL) %>%
    mutate_all(list(~gsub("\n","", .))) %>%
    mutate_all(list(~gsub("\t","", .))) %>%
    mutate_all(list(~gsub("[ ]{2,}", " ", .))) %>%
    mutate_all(list(~gsub("[ ]{3,}","", .))) %>%
    mutate_all(list(~gsub("^[ ]","", .))) %>%
    mutate_all(list(~gsub("[ ]$","", .))) %>%
    filter(value != "")
  
  link = html_nodes(scrape, "a") %>%
    html_attr("href") %>%
    enframe(name = NULL) %>%
    filter(grepl("series", value),
           !grepl("http", value)) %>%
    distinct() %>%
    mutate(value = paste0("https://fred.stlouisfed.org", value))
  
  temp[[i]] = bind_cols(title, period, obs, link)  
}


temp <- temp %>%
  reduce(bind_rows) %>%
  rename("title" = "value",
         "period" = "value1",
         "obs" = "value2",
         "link" = "value3")

write_csv(temp, path = "./metadata-dbs/fred-US-State-Annually.csv")
