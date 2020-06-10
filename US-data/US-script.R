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
  filter(jurisdiction_type != "territory") 


#####################################################################################################


#### 1. US Population 1900 - 2019 ####

# Data Extracted from: https://fred.stlouisfed.org/release?rid=118

# Links
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


# Download Files 
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

# Links
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


# Download Files 
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

# Links
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


# Download Files 
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


#### 4. US Population by Characteristics (Grouped Age) 1969-2018 ####

# Data Extracted from: https://seer.cancer.gov/popdata/download.html

# Links
links <- "https://seer.cancer.gov/popdata/yr1969_2018.19ages/us.1969_2018.19ages.adjusted.txt.gz"
file <- paste0(getwd(),"/US-data/temp/pop-by-char.txt.gz")


# Download Files 
download.file(links, file)

# Loading Files
aux <- fread(input = paste0(getwd(),"/US-data/temp/pop-by-char.txt.gz"),
             colClasses = 'character', fill=T, header=F) %>%
  mutate(year = str_sub(V1, start = 1L, end = 4L),
         state = str_sub(V1, start = 5L, end = 6L),
         state_fips = str_sub(V1, start = 7L, end = 8L),
         county_fips = str_sub(V1, start = 9L, end = 11L),
         registry = str_sub(V1, start = 12L, end = 13L),
         race = str_sub(V1, start = 14L, end = 14L),
         origin = str_sub(V1, start = 15L, end = 15L),
         gender = str_sub(V1, start = 16L, end = 16L),
         age = str_sub(V1, start = 17L, end = 18L),
         population = str_sub(V1, start = 19L, end = -1L)
  ) %>%
  select(-V1, -origin, -registry, -state_fips) %>%
  mutate(population = as.numeric(population),
         year = as.Date(paste0(year, "-01-01")),
         gender = ifelse(gender == "1", "Male",
                         ifelse(gender == "2", "Female", NA)),
         race = ifelse(race == "1", "White",
                       ifelse(race == "2", "Black",
                              ifelse(race == "3", "Other", NA))),
         age = recode(age,
                          "00" = "0 years",
                          "01" = "1-4 years" ,
                          "02" = "5-9 years" ,
                          "03" = "10-14 years" ,
                          "04" = "15-19 years" ,
                          "05" = "20-24 years" ,
                          "06" = "25-29 years" ,
                          "07" = "30-34 years" ,
                          "08" = "35-39 years" ,
                          "09" = "40-44 years" ,
                          "10" = "45-49 years" ,
                          "11" = "50-54 years" ,
                          "12" = "55-59 years" ,
                          "13" = "60-64 years" ,
                          "14" = "65-69 years" ,
                          "15" = "70-74 years" ,
                          "16" = "75-79 years" ,
                          "17" = "80-84 years" ,
                          "18" = "85+ years")
  ) %>%
  group_by(year,state, race, gender, age) %>%
  summarise(population = sum(population, na.rm = T))


# Data Wrangling
race <- aux %>%
  group_by(year, state, race) %>%
  summarise(population = sum(population, na.rm = T)) %>%
  pivot_wider(names_from = "race", values_from = "population") 

gender <- aux %>%
  group_by(year, state, gender) %>%
  summarise(population = sum(population, na.rm = T)) %>%
  pivot_wider(names_from = "gender", values_from = "population") 

age <- aux %>%
  group_by(year, state, age) %>%
  summarise(population = sum(population, na.rm = T)) %>%
  pivot_wider(names_from = "age", values_from = "population") 

temp <- tibble(year = as.Date(paste0(1969:2019, "-01-01"))) %>%
  left_join(race) %>%
  left_join(gender) %>%
  left_join(age)
  

# Delete temp data
unlink(paste0(getwd(),"/US-data/temp/pop-by-char.txt.gz"))


###############################################################################################################################################################################################################################


#### 5. US Population by Characteristics (Single Ages) 1969-2018 ####

# Data Extracted from: https://seer.cancer.gov/popdata/download.html

# Links
links <- "https://seer.cancer.gov/popdata/yr1969_2018.singleages/us.1969_2018.singleages.adjusted.txt.gz"
file <- paste0(getwd(),"/US-data/temp/pop-by-char.txt.gz")


# Download Files 
download.file(links, file)

# Loading Files
aux <- fread(input = paste0(getwd(),"/US-data/temp/pop-by-char.txt.gz"),
             colClasses = 'character', fill=T, header=F) %>%
  mutate(year = str_sub(V1, start = 1L, end = 4L),
         state = str_sub(V1, start = 5L, end = 6L),
         state_fips = str_sub(V1, start = 7L, end = 8L),
         county_fips = str_sub(V1, start = 9L, end = 11L),
         registry = str_sub(V1, start = 12L, end = 13L),
         race = str_sub(V1, start = 14L, end = 14L),
         origin = str_sub(V1, start = 15L, end = 15L),
         gender = str_sub(V1, start = 16L, end = 16L),
         age = str_sub(V1, start = 17L, end = 18L),
         population = str_sub(V1, start = 19L, end = -1L)
  ) %>%
  select(-V1, -origin, -registry, -state_fips) %>%
  mutate(population = as.numeric(population),
         year = as.Date(paste0(year, "-01-01")),
         gender = ifelse(gender == "1", "Male",
                         ifelse(gender == "2", "Female", NA)),
         race = ifelse(race == "1", "White",
                       ifelse(race == "2", "Black",
                              ifelse(race == "3", "Other", NA)))) %>%
  group_by(year,state, race, gender, age) %>%
  summarise(population = sum(population, na.rm = T))


# Data Wrangling
race <- aux %>%
  group_by(year, state, race) %>%
  summarise(population = sum(population, na.rm = T)) %>%
  pivot_wider(names_from = "race", values_from = "population") 

gender <- aux %>%
  group_by(year, state, gender) %>%
  summarise(population = sum(population, na.rm = T)) %>%
  pivot_wider(names_from = "gender", values_from = "population") 

age <- aux %>%
  group_by(year, state, age) %>%
  summarise(population = sum(population, na.rm = T)) %>%
  pivot_wider(names_from = "age", values_from = "population") 

temp <- tibble(year = as.Date(paste0(1969:2019, "-01-01"))) %>%
  left_join(race) %>%
  left_join(gender) %>%
  left_join(age)


# Delete temp data
unlink(paste0(getwd(),"/US-data/temp/pop-by-char.txt.gz"))


# Save data
write_csv(temp, path = "./US-data/US-population-by-characteristics-V2.csv")



###############################################################################################################################################################################################################################


#### 6. US Governors 1775-2018 ####

# Extracted from: https://www.openicpsr.org/openicpsr/project/102000/version/V1/view
# Downloaded manually

# Loading Files
temp <- read_csv("./US-data/temp/governors.csv")

# Data Wrangling
temp <- temp %>%
  mutate(year = as.Date(paste0(year, "-01-01"))) %>%
  left_join(., states, by = c("state" = "state_name")) %>%
  filter(!is.na(state_abbr)) %>%
  select(year, state_abbr, governor, party)


# Delete temp data
unlink(paste0(getwd(),"/US-data/temp/governors.csv"))


# Save data
write_csv(temp, path = "./US-data/US-governors.csv")


################################################################################################################


#### End of File ####