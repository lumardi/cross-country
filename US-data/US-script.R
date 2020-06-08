######################################################################################################
######################################################################################################
################################           [US DATA]            ######################################
######################################################################################################
######################################################################################################

# Paper: [Title]
# Authors: [Authors]
# Date creation: 2020-06-08
# Last Update: 2020-06-08

#####################################################################################################


#### 0. House Cleaning ####

set.seed(29841) # From random.org


# Needed packages
pkgs <- c("tidyverse", "data.table", "USAboundaries")


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


# US boundaries
states <- state_codes %>%
  filter(jurisdiction_type != "territory") %>%
  select(state_abbr, state_code)

#####################################################################################################


#### 1. US Population 1900 - 2019 ####

# Data Extracted from: https://fred.stlouisfed.org/release?rid=118

# Creating links

links <- paste0(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=",
  states$state_abbr,
  "POP&scale=left&cosd=1900-01-01&coed=2019-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Annual&fam=avg&fgst=lin&fgsnd=2009-06-01&line_index=1&transformation=lin&vintage_date=",
  as.character(Sys.Date()),
  "&revision_date=",
  as.character(Sys.Date()),
  "&nd=1900-01-01"
  )

file <- paste0(getwd(),"/US-data/temp/pop-",states,".csv")


# Downloading Files 
for(i in 1:length(links)){
  download.file(links[i], file[i])
}  


# Unifying data
temp <- file.path(paste0(getwd(),"/US-data/temp/")) %>%
  list.files(pattern = "pop-*") %>%
  map_df(~{
    read_csv(file = paste0(getwd(),"/US-data/temp/", .x),
             col_types = cols(.default = "c")) %>%
    mutate_all(as.character) %>%
    rename("population" = 2,
           "date" = 1) %>%
    mutate(state = .x)
    }) %>%
  mutate(
    state = gsub("pop-", "", state),
    state = gsub("\\..*", "", state),
    date = as.Date(date),
    population = as.numeric(population)
         )

# Delete temp data
sapply(paste0(getwd(), "/US-data/temp/pop-", states$state_abbr, ".csv"), unlink)


# Save data
write_csv(temp, path = "./US-data/US-population.csv")


###############################################################################################################################################################################################################################


#### 2. US Per Capita Personal Income 1929 - 2019 ####

# Data Extracted from: https://fred.stlouisfed.org/tags/series?t=annual%3Bper%20capita%3Bpersonal%3Bstate%3Busa&ob=pv&od=desc

links <- paste0(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=",
  states$state_abbr,
  "PCPI&scale=left&cosd=1929-01-01&coed=2019-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Annual&fam=avg&fgst=lin&fgsnd=2009-06-01&line_index=1&transformation=lin&vintage_date=",
  as.character(Sys.Date()),
  "&revision_date=",
  as.character(Sys.Date()),
  "&nd=1929-01-01"
)

file <- paste0(getwd(),"/US-data/temp/pcpi-",states$state_abbr,".csv")


# Downloading Files 
for(i in 1:length(links)){
  download.file(links[i], file[i])
}  


# Unifying data
temp <- file.path(paste0(getwd(),"/US-data/temp/")) %>%
  list.files(pattern = "pcpi-*") %>%
  map_df(~{
    read_csv(file = paste0(getwd(),"/US-data/temp/", .x),
             col_types = cols(.default = "c")) %>%
      mutate_all(as.character) %>%
      rename("pcpi" = 2,
             "date" = 1) %>%
      mutate(state = .x)
  }) %>%
  mutate(
    state = gsub("pcpi-", "", state),
    state = gsub("\\..*", "", state),
    date = as.Date(date),
    pcpi = as.numeric(pcpi)
    )

# Delete temp data
sapply(paste0(getwd(), "/US-data/temp/pcpi-", states$state_abbr, ".csv"), unlink)


# Save data
write_csv(temp, path = "./US-data/US-per-capita-personal-income.csv")


###############################################################################################################################################################################################################################


#### 3. US Unemployment Rate 1976-2018/2019 ####

# Data Extracted from: https://fred.stlouisfed.org/searchresults/?nasw=0&st=unemployment%20rate&t=annual%3Bstate&ob=sr&od=desc&types=gen;geot

links <- paste0(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=LAUST",
  states$state_code,
  "0000000000003A&scale=left&cosd=1976-01-01&coed=2019-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Annual&fam=avg&fgst=lin&fgsnd=2009-06-01&line_index=1&transformation=lin&vintage_date=",
  as.character(Sys.Date()),
  "&revision_date=",
  as.character(Sys.Date()),
  "&nd=1976-01-01"
)

file <- paste0(getwd(),"/US-data/temp/unemp-",states$state_code,".csv")


# Downloading Files 
for(i in 1:length(links)){
  download.file(links[i], file[i])
}  


# Unifying data
temp <- file.path(paste0(getwd(),"/US-data/temp/")) %>%
  list.files(pattern = "unemp-*") %>%
  map_df(~{
    read_csv(file = paste0(getwd(),"/US-data/temp/", .x),
             col_types = cols(.default = "c")) %>%
      mutate_all(as.character) %>%
      rename("unemp" = 2,
             "date" = 1) %>%
      mutate(state_code = .x)
  }) %>%
  mutate(
    state_code = gsub("unemp-", "", state_code),
    state_code = gsub("\\..*", "", state_code),
    date = as.Date(date),
    unemp = (as.numeric(unemp))/100
    ) %>%
  left_join(states) %>%
  select(date, unemp, state_abbr) %>%
  rename("state" = "state_abbr")
  

# Delete temp data
sapply(paste0(getwd(), "/US-data/temp/unemp-", states$state_code, ".csv"), unlink)


# Save data
write_csv(temp, path = "./US-data/US-unemployment-rate.csv")


###############################################################################################################################################################################################################################


#### 4. US Population by Characteristics 1969-2018 ####




#### End of File ####