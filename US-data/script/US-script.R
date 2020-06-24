######################################################################################################
######################################################################################################
################################           [US DATA]            ######################################
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


#### 1. US Population 1900 - 2019 ####

# Source: https://fred.stlouisfed.org/release?rid=118

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

file <- paste0(getwd(),"/US-data/temp/pop-",states$state_abbr,".csv")


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
    rename("population" = 2,
           "date" = 1) %>%
    mutate(state = .x)
    }) %>%
  mutate(
    state = gsub("pop-", "", state),
    state = gsub("\\..*", "", state),
    date = as.Date(date),
    population = gsub("\\.", "", population),
    population = as.numeric(population)
         )

# Delete temp data
sapply(paste0(getwd(), "/US-data/temp/pop-", states$state_abbr, ".csv"), unlink)


# Save data
write_csv(temp, path = "./US-data/raw/US-population.csv")


###############################################################################################################################################################################################################################


#### 2. US Per Capita Personal Income 1929-2019 ####

# Source: https://fred.stlouisfed.org/tags/series?t=annual%3Bper%20capita%3Bpersonal%3Bstate%3Busa&ob=pv&od=desc

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
      rename("pcpi" = 2,
             "year" = 1) %>%
      mutate(state = .x)
  }) %>%
  mutate(
    state = gsub("pcpi-", "", state),
    state = gsub("\\..*", "", state),
    year = as.Date(year),
    pcpi = as.numeric(pcpi)
    )

# Delete temp data
sapply(paste0(getwd(), "/US-data/temp/pcpi-", states$state_abbr, ".csv"), unlink)


# Save data
write_csv(temp, path = "./US-data/raw/US-per-capita-personal-income.csv")


###############################################################################################################################################################################################################################


#### 3. US Unemployment Rate 1976-2018/2019 ####

# Source: https://fred.stlouisfed.org/searchresults/?nasw=0&st=unemployment%20rate&t=annual%3Bstate&ob=sr&od=desc&types=gen;geot

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
      rename("unemp" = 2,
             "year" = 1) %>%
      mutate(state_code = .x)
  }) %>%
  mutate(
    state_code = gsub("unemp-", "", state_code),
    state_code = gsub("\\..*", "", state_code),
    year = as.Date(year),
    unemp = (as.numeric(unemp))/100
    ) %>%
  left_join(states) %>%
  select(year, unemp, state_abbr) %>%
  rename("state" = "state_abbr")
  

# Delete temp data
sapply(paste0(getwd(), "/US-data/temp/unemp-", states$state_code, ".csv"), unlink)


# Save data
write_csv(temp, path = "./US-data/raw/US-unemployment-rate.csv")


###############################################################################################################################################################################################################################


#### 4. US Population by Characteristics (Grouped Age) 1969-2018 ####

# Source: https://seer.cancer.gov/popdata/download.html

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


# Save data
write_csv(temp, path = "./US-data/raw/US-population-by-characteristics.csv")


###############################################################################################################################################################################################################################


#### 5. US Population by Characteristics (Single Ages) 1969-2018 ####

# Source: https://seer.cancer.gov/popdata/download.html

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
write_csv(temp, path = "./US-data/raw/US-population-by-characteristics-V2.csv")



###############################################################################################################################################################################################################################


#### 6. US Governors 1775-2018 ####

# Source: https://www.openicpsr.org/openicpsr/project/102000/version/V1/view
# Downloaded manually

# Loading Files
temp <- read_csv("./US-data/temp/governors.csv")

# Data Wrangling
temp <- temp %>%
  mutate(year = as.Date(paste0(year, "-01-01"))) %>%
  left_join(., states, by = c("state" = "state_name")) %>%
  filter(!is.na(state_abbr)) %>%
  select(year, state_abbr, governor, party) %>%
  rename("state" = "state_abbr")


# Delete temp data
unlink(paste0(getwd(),"/US-data/temp/governors.csv"))


# Save data
write_csv(temp, path = "./US-data/raw/US-governors.csv")


################################################################################################################



#### 7. Crime Statistics 1960-2018 ####

# Source: http://www.disastercenter.com/crime/
# Alternative: https://www.ucrdatatool.gov/


# Links
links <- paste0(
  "http://www.disastercenter.com/crime/",
  tolower(states$state_abbr) ,
  "crime.htm") %>%
  enframe(name = NULL) %>%
  filter(!grepl("dccrime",value)) %>%
  mutate(id = 1:nrow(.),
         value = gsub("kscrime","kncrime",value),
         value = ifelse(id %in% c(24:27, 30:31, 33:34, 36),
                        gsub("crime.htm", "crimn.htm", value), value)) %>% .$value


# Scraping Data
temp <- list()


for(i in 1:length(links)){
  Sys.sleep(round(rnorm(1, mean = 8, sd = 2), digits = 0))
  
  temp[[i]] = 
    tryCatch({
      read_html(links[i]) %>%
        html_table(fill = T) %>%
        .[[2]] %>%
        select(1:12) %>%
        filter(!grepl("[A-z]", X1),
               X1 != "")  %>%
        rename("year" = "X1",
               "population" = "X2",
               "index" = "X3",
               "violent" = "X4",
               "property" = "X5",
               "murder" = "X6",
               "forcible_rape" = "X7",
               "robbery" = "X8",
               "aggravated_assault" = "X9",
               "burglary" = "X10",
               "larceny_theft" = "X11",
               "vehicle_theft" = "X12"
        ) %>%
        mutate_all(list(~gsub("^(([0-9]){1}.*?)", "\\1,", .))) %>%
        mutate_all(list(~gsub(",\\.", ",", .))) %>%          # fix decimals
        mutate_all(list(~gsub("[^0-9\\.]", "", .))) %>%      # remove non-numeric characters
        mutate_all(list(as.numeric)) %>% 
        mutate(check_dupe = ifelse(year == lag(year), 1, NA)) %>%  
        filter(is.na(check_dupe)) %>%                        # remove repeated years
        full_join(tibble(year = 1960:2018)) %>%              # include all years
        mutate(state = gsub(".*crime/", "", links[i]),
               state = toupper(gsub("crime.htm", "", state)),
               id = 1:nrow(.),
               type = ifelse(id < 60, "total", "per100k")    
        ) %>%
        select(-id,-population,-check_dupe) %>%
        pivot_wider(id_cols = c("year","state"), 
                    names_from = type, 
                    values_from = -c("type","year","state")) 
    },
    error = function(e){NA}
    )
}


# Fixing i == 32 (NY)
i <- 32
temp[[i]] <- read_html(links[i]) %>%
  html_table(fill = T) %>%
  .[[2]] %>%
  select(1:12) %>%
  filter(!grepl("[A-z]", X1),
         X1 != "")  %>%
  rename("year" = "X1",
         "population" = "X2",
         "index" = "X3",
         "violent" = "X4",
         "property" = "X5",
         "murder" = "X6",
         "forcible_rape" = "X7",
         "robbery" = "X8",
         "aggravated_assault" = "X9",
         "burglary" = "X10",
         "larceny_theft" = "X11",
         "vehicle_theft" = "X12"
  ) %>%
  mutate_all(list(~gsub("^(([0-9]){1}.*?)", "\\1,", .))) %>%
  mutate_all(list(~gsub(",\\.", ",", .))) %>%          
  mutate_all(list(~gsub("[^0-9\\.]", "", .))) %>%      
  mutate_all(list(as.numeric)) %>% 
  mutate(check_dupe = ifelse(year == lag(year), 1, NA)) %>%  
  filter(is.na(check_dupe)) %>%                        
  mutate(state = gsub(".*crime/", "", links[i]),
         state = toupper(gsub("crime.htm", "", state)),
         id = 1:nrow(.),
         type = ifelse(id < 55, "total", "per100k")    
  ) %>%
  select(-id,-population,-check_dupe) %>%
  pivot_wider(id_cols = c("year","state"), 
              names_from = type, 
              values_from = -c("type","year","state")) %>%
  full_join(tibble(year = 1960:2018, state = "NY"))


# Data Wrangling
temp <- temp %>%
  bind_rows(temp) %>%
  mutate(state = gsub("KN", "KS", state),
         state = str_sub(state, start =  1, end = 2),
         year = as.Date(paste0(year, "-01-01"))
         )
  

# Save data
write_csv(temp, path = "./US-data/raw/US-crime-stats.csv")


################################################################################################################



#### 8. Police Employee Data 1960-2018 ####

# Source: https://crime-data-explorer.fr.cloud.gov/downloads-and-docs


# Links
links <- "http://s3-us-gov-west-1.amazonaws.com/cg-d4b776d0-d898-4153-90c8-8336f86bdfec/pe_1960_2018.csv"
file <- paste0(getwd(),"/US-data/temp/police-employee.csv")


# Download Files 
download.file(links, file)


# Loading Files & Data Wrangling
temp <- read_csv(file = paste0(getwd(),"/US-data/temp/police-employee.csv"),
                 col_types = cols(.default = "c")) %>%
    rename("year" = "data_year",
           "state" = "state_abbr") %>%
  mutate(across(c(contains("_ct"), year), ~ as.numeric(.x)),
         year = as.Date(paste0(year, "-01-01"))) %>%
  group_by(year, state) %>%
  summarise(male_officer = sum(male_officer_ct, na.rm = T),
            male_civilian = sum(male_civilian_ct, na.rm = T),
            female_officer = sum(female_officer_ct, na.rm = T),
            female_civilian = sum(female_civilian_ct, na.rm = T)
            ) %>%
  mutate_if(is.numeric, list(~ifelse(male_officer == female_officer &
                                       male_officer == female_officer &
                                       male_officer == male_civilian &
                                       male_officer == female_civilian &
                                       male_officer == 0,
                                     NA,
                                     .
                                       )))
  

# Delete temp data
unlink(paste0(getwd(),"/US-data/temp/police-employee.csv"))


# Save data
write_csv(temp, path = "./US-data/raw/US-police-employee.csv")



################################################################################################################



#### 9. UCR Program Participation Data 1960-2018 ####

# Source: https://crime-data-explorer.fr.cloud.gov/downloads-and-docs


# Links
links <- "http://s3-us-gov-west-1.amazonaws.com/cg-d4b776d0-d898-4153-90c8-8336f86bdfec/ucr_participation_1960_2018.csv"
file <- paste0(getwd(),"/US-data/temp/ucr_participation.csv")


# Download Files 
download.file(links, file)


# Loading Files & Data Wrangling
temp <- read_csv(file = paste0(getwd(),"/US-data/temp/ucr_participation.csv"),
                 col_types = cols(.default = "c")) %>%
  left_join(states) %>%
  select(-state_name, -state_code, -jurisdiction_type) %>%
  rename("nibrs_participating_agencies_pct" = "nibrs_participating_agencies_1",
         "state" = "state_abbr",
         "year" = "data_year") %>%
  mutate(year = as.Date(paste0(year, "-01-01")))
  


# Delete temp data
unlink(paste0(getwd(),"/US-data/temp/ucr_participation.csv"))


# Save data
write_csv(temp, path = "./US-data/raw/US-ucr-participation.csv")




################################################################################################################



#### End of File ####