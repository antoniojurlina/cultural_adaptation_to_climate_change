#------- Libraries -------- 
library(tidyverse)
library(here)
library(readxl)
library(sf)
library(acs)

#------- Directory and APIs -------- 
paste0(here(), "/data") %>% setwd()

api.key.install("58554798ecec58533e024fe1066f394c68f10d3c")
#acs.tables.install()

#------- Functions -------- 
display_missing <- function(data) {
  missing <- sapply(names(data), 
                    function(x) which(is.na(data[[x]])))
  missing <- unique(unlist(missing[sapply(missing,length)>0]))
  missing <- sort(c(missing, missing+1, missing+2, missing-1, missing-2))
  return(missing)
}

find_missing <- function(data) {
  missing <- sapply(names(data), 
                    function(x) which(is.na(data[[x]])))
  missing <- missing[sapply(missing,length)>0]
  missing <- Reduce(intersect, missing)
  return(missing)
}

clean_fips <- function(data) {
  data %>%
    unite("fips", 
          c(STATE_FIPS_CODE, COUNTY_CODE), 
          sep="", 
          remove=TRUE) %>%
    mutate(fips = str_pad(fips, 
                          width=5,
                          side="left",
                          pad="0"))
}

`%notin%` <- Negate(`%in%`)

#------------------ ACS VARIABLES ----------------------------------------------
#------------------------------------------------------------------------------#
#------- POPULATION -------- 
population <- read_xls("acs_population.xls", skip=2)

non_contiguous <- c("AK", "HI", "PR", "US")

population <- population %>%
  select("fips" = "FIPStxt", 
         "state" = "State", 
         "county" = "Area_Name", 
         "population_2012" = "POP_ESTIMATE_2012", 
         "population_2017" = "POP_ESTIMATE_2017") %>%
  filter(!str_detect(fips, "000$"),
         state %notin% non_contiguous) %>%
  pivot_longer(4:5, 
               names_to="year",
               values_to="population") %>%
  mutate(county = str_remove_all(county, " County"),
         year = as.numeric(str_remove_all(year, "population_")))

#------- EDUCATION -------- 
variables <- c("STATE_FIPS_CODE", 
               "COUNTY_CODE",
               "high_school",
               "GED_or_alternative",
               "some_college_under_one_year",
               "some_college_over_one_year",
               "associates",
               "bachelors",
               "masters",
               "professional",
               "doctorate")

education <- acs.fetch(endyear = 2017, 
                       geography = geo.make(state = "*",
                                            county = "*"),
                       table.number = "B15003",
                       dataset = "acs",
                       span = "5",
                       variable = c("B15003_017", "B15003_018", "B15003_019",
                                    "B15003_020", "B15003_021", "B15003_022", 
                                    "B15003_023", "B15003_024", "B15003_025"))

education_2017 <- bind_cols(education@geography$state,
                            education@geography$county,
                            education@estimate %>% as_tibble())

names(education_2017) <- variables

education_2017 <- clean_fips(education_2017) %>%
  mutate(year = 2017)

education <- acs.fetch(endyear = 2012, 
                       geography = geo.make(state = "*",
                                            county = "*"),
                       table.number = "B15003",
                       dataset = "acs",
                       span = "5",
                       variable = c("B15003_017", "B15003_018", "B15003_019",
                                    "B15003_020", "B15003_021", "B15003_022", 
                                    "B15003_023", "B15003_024", "B15003_025"))

education_2012 <- bind_cols(education@geography$state,
                            education@geography$county,
                            education@estimate %>% as_tibble())

names(education_2012) <- variables

education_2012 <- clean_fips(education_2012) %>%
  mutate(year = 2012)

#------- COMBINED -------- 
entire_dataset <- population %>%
  left_join(bind_rows(education_2012, education_2017),
            by=c("fips", "year"))

entire_dataset <- entire_dataset %>%
  mutate(high_school_or_GED = high_school + 
                              GED_or_alternative,
         some_college_or_associates = some_college_under_one_year + 
                                      some_college_over_one_year +
                                      associates,
         bachelors_or_higher = bachelors +
                               masters +
                               professional +
                               doctorate) %>%
  select(fips, state, county, year, population,
         high_school_or_GED, some_college_or_associates,
         bachelors_or_higher) %>%
  mutate(high_school_or_GED = high_school_or_GED/population,
         some_college_or_associates = some_college_or_associates/population,
         bachelors_or_higher = bachelors_or_higher/population)

#------- REMOVE TEMPORARY DATA FRAMES -------- 
rm(education_2012, education_2017, education, population)

#------------------ COUNTY TO LAND GRANT DISTANCE ------------------------------
#------------------------------------------------------------------------------#
#------- GETTING THE DATA -------- 
universities <- read_xlsx("universities.xlsx") %>%
  filter(type == 1862)
universities <- st_as_sf(universities, coords=c("long", "lat"), crs = 4326) 
universities <- st_transform(universities, 26919)

counties <- st_read("Counties/cb_2018_us_county_500k.shp") %>% 
  select(STATEFP,
         COUNTYFP,
         fips = GEOID, 
         geometry) %>% 
  right_join(entire_dataset %>% 
               select(fips, county),
             by="fips") %>%
  group_by(fips) %>% 
  filter(row_number() == 1) %>%
  arrange(fips)

counties <- st_transform(counties, 26919)

#------- CALCULATING DISTANCES -------- 
county_centroids <- st_centroid(counties)

distances <- bind_cols(st_set_geometry(county_centroids, NULL),
                       st_distance(county_centroids, universities) %>% 
                         as_tibble())

names(distances) <- st_set_geometry(universities, NULL) %>% 
  select(university) %>% 
  unlist() %>%
  c("STATEFP", "COUNTYFP", "fips", "county", .)

distances <- distances %>%
  pivot_longer(cols=5:62, names_to="university", values_to="distance") %>%
  left_join(st_set_geometry(universities, NULL) %>%
              select(university, fips),
            by = "university",
            suffix=c("_county", "_college")) %>%
  mutate(distance = as.numeric(distance),
         fips_college = str_trunc(fips_college, 2, ellipsis=""))

output1 <- distances %>%
  group_by(fips_county) %>%
  mutate(closest_overall = min(distance)) %>%
  ungroup() %>% 
  filter(distance==closest_overall) %>%
  arrange(fips_county)

output2 <- map(unique(distances$STATEFP), function(x) {
  distances %>%
    filter(STATEFP == x,
           fips_college == x) %>%
    group_by(county) %>%
    mutate(closest_in_state = min(distance)) %>%
    ungroup() %>% 
    filter(closest_in_state == distance)
}) %>%
  bind_rows()

land_grant_data <- output1 %>%
  select(fips = fips_county, university, closest_overall) %>%
  full_join(output2 %>%
              select(fips = fips_county, university, closest_in_state),
            by="fips",
            suffix=c("_overall", "_in_state"))

land_grant_data <- land_grant_data[!duplicated(land_grant_data), ]

land_grant_data <- land_grant_data %>%
  left_join(st_set_geometry(universities, NULL) %>% 
              select(fips) %>%
              mutate(university_presence = fips),
            by = "fips") %>%
  mutate(university_presence = ifelse(fips==university_presence, 1, 0),
         university_presence = as.numeric(university_presence),
         university_presence = ifelse(is.na(university_presence), 0, 1))

land_grant_data <- land_grant_data %>%
  mutate(closest_in_state = round(closest_in_state * 0.00062137, 2),
         closest_overall = round(closest_overall * 0.00062137, 2))

#------- COMBINED -------- 
entire_dataset <- entire_dataset %>% 
  left_join(land_grant_data %>%
              select(fips, 
                     closest_land_grant_overall = closest_overall, 
                     closest_land_grant_in_state = closest_in_state, 
                     land_grant_presence = university_presence),
            by="fips")
  
#------- REMOVE TEMPORARY DATA FRAMES -------- 
rm(output1, output2, universities, 
   counties, distances, county_centroids)

#------------------ NASS VARIABLES ---------------------------------------------
#------------------------------------------------------------------------------#
#------- IMPORTING CENSUS DATA -------- 
# census_2012_link <- "https://www.nass.usda.gov/Publications/AgCensus/2012/Online_Resources/Census_Data_Query_Tool/2012_cdqt_data.txt.gz"
# census_2017_link <- "https://www.nass.usda.gov/Publications/AgCensus/2017/Online_Resources/Census_Data_Query_Tool/2017_cdqt_data.txt.gz"
census_2012 <- read.delim("nass_census_2012.txt", header = TRUE)
census_2017 <- read.delim("nass_census_2017.txt", header = TRUE)
land_practices <- "nass_census_2012_practices.xlsx" 

#------- COUNTY FARM DATA -------- 
variables <- c("FARM OPERATIONS - NUMBER OF OPERATIONS",
               "FARM OPERATIONS - ACRES OPERATED",
               "FARM OPERATIONS - AREA OPERATED, MEASURED IN ACRES / OPERATION, MEDIAN")
level_key <- c("FARM OPERATIONS - NUMBER OF OPERATIONS" = "N_OF_OPERATIONS",
               "FARM OPERATIONS - ACRES OPERATED" = "ACRES_OPERATED",
               "FARM OPERATIONS - AREA OPERATED, MEASURED IN ACRES / OPERATION, MEDIAN" = "AVERAGE_FARM_SIZE")

county_farms_2017 <- census_2017 %>%
  filter(CENSUS_TABLE == "1",
         CENSUS_CHAPTER == "2",
         SHORT_DESC %in% variables,
         AGG_LEVEL_DESC == "COUNTY",
         DOMAINCAT_DESC == "") %>%
  select(STATE_FIPS_CODE, COUNTY_CODE, SHORT_DESC, VALUE) %>%
  mutate(SHORT_DESC = recode_factor(SHORT_DESC, !!!level_key),
         VALUE = str_remove_all(VALUE, "\\,")) %>%
  pivot_wider(names_from=SHORT_DESC, 
              values_from=VALUE)

county_farms_2012 <- census_2012 %>%
  filter(CENSUS_TABLE == "1",
         CENSUS_CHAPTER == "2",
         SHORT_DESC %in% variables,
         AGG_LEVEL_DESC == "COUNTY",
         DOMAINCAT_DESC == "") %>%
  select(STATE_FIPS_CODE, COUNTY_CODE, SHORT_DESC, VALUE) %>%
  mutate(SHORT_DESC = recode_factor(SHORT_DESC, !!!level_key),
         VALUE = str_remove_all(VALUE, "\\,")) %>%
  pivot_wider(names_from=SHORT_DESC, 
              values_from=VALUE)

county_farms <- county_farms_2012 %>%
  full_join(county_farms_2017, 
            by = c("STATE_FIPS_CODE", 
                   "COUNTY_CODE"),
            suffix = c(".2017", ".2012")) %>% 
  pivot_longer(cols = 3:8, 
               names_to = "VARIABLE",
               values_to = "VALUE") %>% 
  separate(col="VARIABLE", 
           into=c("VARIABLE", "YEAR"),
           sep="\\.") %>% 
  pivot_wider(names_from=VARIABLE,
              values_from=VALUE) %>%
  mutate(YEAR = as.numeric(YEAR)) %>%
  arrange(STATE_FIPS_CODE, COUNTY_CODE, YEAR)

#county_farms[display_missing(county_farms), ] %>% View()
county_farms <- county_farms[-find_missing(county_farms), ]
county_farms <- clean_fips(county_farms)

#------- CONSERVATION PROGRAM PARTICIPATION -------- 
variables <- c("GOVT PROGRAMS, FEDERAL, CONSERVATION & WETLANDS - RECEIPTS, MEASURED IN $ / OPERATION",
               "GOVT PROGRAMS, FEDERAL, CONSERVATION & WETLANDS - OPERATIONS WITH RECEIPTS")
level_key <- c("GOVT PROGRAMS, FEDERAL, CONSERVATION & WETLANDS - RECEIPTS, MEASURED IN $ / OPERATION" = "CONS_PROG_RECEIPTS",
               "GOVT PROGRAMS, FEDERAL, CONSERVATION & WETLANDS - OPERATIONS WITH RECEIPTS" = "CONS_PROG_OPERATIONS")

cons_prog_particip_2017 <- census_2017 %>%
  filter(SHORT_DESC %in% variables,
         AGG_LEVEL_DESC == "COUNTY") %>% 
  select(STATE_FIPS_CODE, COUNTY_CODE, SHORT_DESC, VALUE) %>%
  mutate(SHORT_DESC = recode_factor(SHORT_DESC, !!!level_key),
         VALUE = str_remove_all(VALUE, "\\,")) %>%
  pivot_wider(names_from=SHORT_DESC, 
              values_from=VALUE)

cons_prog_particip_2012 <- census_2012 %>%
  filter(SHORT_DESC %in% variables,
         AGG_LEVEL_DESC == "COUNTY") %>% 
  select(STATE_FIPS_CODE, COUNTY_CODE, SHORT_DESC, VALUE) %>%
  mutate(SHORT_DESC = recode_factor(SHORT_DESC, !!!level_key),
         VALUE = str_remove_all(VALUE, "\\,")) %>%
  pivot_wider(names_from=SHORT_DESC, 
              values_from=VALUE)

cons_prog_particip <- cons_prog_particip_2017 %>%
  full_join(cons_prog_particip_2012, 
            by = c("STATE_FIPS_CODE", 
                   "COUNTY_CODE"),
            suffix = c(".2017", ".2012")) %>% 
  pivot_longer(cols = 3:6, 
               names_to = "VARIABLE",
               values_to = "VALUE") %>% 
  separate(col="VARIABLE", 
           into=c("VARIABLE", "YEAR"),
           sep="\\.") %>% 
  mutate(YEAR = as.numeric(YEAR)) %>% 
  pivot_wider(names_from=VARIABLE,
              values_from=VALUE) %>% 
  arrange(STATE_FIPS_CODE, COUNTY_CODE, YEAR)

#cons_prog_particip[display_missing(cons_prog_particip), ] %>% View()
cons_prog_particip <- cons_prog_particip[-find_missing(cons_prog_particip), ]
cons_prog_particip <- clean_fips(cons_prog_particip)

#------- FEDERAL PROGRAM PARTICIPATION -------- 
variables <- c("GOVT PROGRAMS, FEDERAL, (EXCL CONSERVATION & WETLANDS) - RECEIPTS, MEASURED IN $ / OPERATION",
               "GOVT PROGRAMS, FEDERAL, (EXCL CONSERVATION & WETLANDS) - OPERATIONS WITH RECEIPTS")
level_key <- c("GOVT PROGRAMS, FEDERAL, (EXCL CONSERVATION & WETLANDS) - RECEIPTS, MEASURED IN $ / OPERATION" = "FED_PROG_RECEIPTS",
               "GOVT PROGRAMS, FEDERAL, (EXCL CONSERVATION & WETLANDS) - OPERATIONS WITH RECEIPTS" = "FED_PROG_OPERATIONS")

fed_prog_particip_2017 <- census_2017 %>%
  filter(SHORT_DESC %in% variables,
         AGG_LEVEL_DESC == "COUNTY") %>% 
  select(STATE_FIPS_CODE, COUNTY_CODE, SHORT_DESC, VALUE) %>%
  mutate(SHORT_DESC = recode_factor(SHORT_DESC, !!!level_key),
         VALUE = str_remove_all(VALUE, "\\,")) %>%
  pivot_wider(names_from=SHORT_DESC, 
              values_from=VALUE)

fed_prog_particip_2012 <- census_2012 %>%
  filter(SHORT_DESC %in% variables,
         AGG_LEVEL_DESC == "COUNTY") %>% 
  select(STATE_FIPS_CODE, COUNTY_CODE, SHORT_DESC, VALUE) %>%
  mutate(SHORT_DESC = recode_factor(SHORT_DESC, !!!level_key),
         VALUE = str_remove_all(VALUE, "\\,")) %>%
  pivot_wider(names_from=SHORT_DESC, 
              values_from=VALUE)

fed_prog_particip <- fed_prog_particip_2017 %>%
  full_join(fed_prog_particip_2012, 
            by = c("STATE_FIPS_CODE", 
                   "COUNTY_CODE"),
            suffix = c(".2017", ".2012")) %>% 
  pivot_longer(cols = 3:6, 
               names_to = "VARIABLE",
               values_to = "VALUE") %>% 
  separate(col="VARIABLE", 
           into=c("VARIABLE", "YEAR"),
           sep="\\.") %>% 
  mutate(YEAR = as.numeric(YEAR)) %>% 
  pivot_wider(names_from=VARIABLE,
              values_from=VALUE) %>% 
  arrange(STATE_FIPS_CODE, COUNTY_CODE, YEAR)

#fed_prog_particip[display_missing(fed_prog_particip), ] %>% View()
fed_prog_particip <- fed_prog_particip[-find_missing(fed_prog_particip), ]
fed_prog_particip <- clean_fips(fed_prog_particip)

#------- PERCENT ORGANIC -------- 
level_key <- c("ORGANIC STATUS: (NOP USDA CERTIFIED)" = "ORGANIC_CERTIFIED",
               "ORGANIC STATUS: (NOP USDA EXEMPT)" = "ORGANIC_EXEMPT")

organic_2017 <- census_2017 %>%
  filter(CENSUS_TABLE == "42",
         SHORT_DESC %in% c("FARM OPERATIONS, ORGANIC - NUMBER OF OPERATIONS"),
         AGG_LEVEL_DESC == "COUNTY") %>% 
  select(STATE_FIPS_CODE, COUNTY_CODE, DOMAINCAT_DESC, VALUE) %>% 
  mutate(DOMAINCAT_DESC = recode_factor(DOMAINCAT_DESC, !!!level_key),
         VALUE = str_remove_all(VALUE, "\\,")) %>%
  pivot_wider(names_from = DOMAINCAT_DESC,
              values_from = VALUE) %>%
  mutate(ORGANIC_CERTIFIED = ifelse(is.na(ORGANIC_CERTIFIED), 
                                    "0", ORGANIC_CERTIFIED),
         ORGANIC_EXEMPT = ifelse(is.na(ORGANIC_EXEMPT), 
                                 "0", ORGANIC_EXEMPT)) %>%
  mutate(ORGANIC = as.numeric(ORGANIC_CERTIFIED) +
                   as.numeric(ORGANIC_EXEMPT)) %>%
  select(-ORGANIC_CERTIFIED, -ORGANIC_EXEMPT)

organic_2012 <- census_2012 %>%
  filter(CENSUS_TABLE == "42",
         SHORT_DESC %in% c("FARM OPERATIONS, ORGANIC - NUMBER OF OPERATIONS"),
         AGG_LEVEL_DESC == "COUNTY") %>% 
  select(STATE_FIPS_CODE, COUNTY_CODE, DOMAINCAT_DESC, VALUE) %>% 
  mutate(DOMAINCAT_DESC = recode_factor(DOMAINCAT_DESC, !!!level_key),
         VALUE = str_remove_all(VALUE, "\\,")) %>%
  pivot_wider(names_from = DOMAINCAT_DESC,
              values_from = VALUE) %>%
  mutate(ORGANIC_CERTIFIED = ifelse(is.na(ORGANIC_CERTIFIED), 
                                    "0", ORGANIC_CERTIFIED),
         ORGANIC_EXEMPT = ifelse(is.na(ORGANIC_EXEMPT), 
                                 "0", ORGANIC_EXEMPT)) %>%
  mutate(ORGANIC = as.numeric(ORGANIC_CERTIFIED) +
           as.numeric(ORGANIC_EXEMPT)) %>%
  select(-ORGANIC_CERTIFIED, -ORGANIC_EXEMPT)

organic <- organic_2017 %>%
  full_join(organic_2012, 
            by = c("STATE_FIPS_CODE", 
                   "COUNTY_CODE"),
            suffix = c(".2017", ".2012")) %>%
  pivot_longer(cols = 3:4, 
               names_to = "VARIABLE",
               values_to = "VALUE") %>% 
  separate(col="VARIABLE", 
           into=c("VARIABLE", "YEAR"),
           sep="\\.") %>% 
  mutate(YEAR = as.numeric(YEAR)) %>% 
  pivot_wider(names_from=VARIABLE,
              values_from=VALUE) %>% 
  arrange(STATE_FIPS_CODE, COUNTY_CODE, YEAR)

#organic[display_missing(organic), ] %>% View()
organic <- organic[-find_missing(organic), ]
organic <- clean_fips(organic)

#------- PERCENT CROP INSURANCE PARTICIPATION -------- 
crop_insurance_2017 <- census_2017 %>%
  filter(CENSUS_TABLE == "8",
         SHORT_DESC %in% c("AG LAND, CROP INSURANCE - NUMBER OF OPERATIONS"),
         AGG_LEVEL_DESC == "COUNTY") %>%
  select(STATE_FIPS_CODE, COUNTY_CODE, VALUE) %>%
  mutate(VALUE = str_remove_all(VALUE, "\\,"))

crop_insurance_2012 <- census_2012 %>%
  filter(CENSUS_TABLE == "8",
         SHORT_DESC %in% c("AG LAND, CROP INSURANCE - NUMBER OF OPERATIONS"),
         AGG_LEVEL_DESC == "COUNTY") %>%
  select(STATE_FIPS_CODE, COUNTY_CODE, VALUE) %>%
  mutate(VALUE = str_remove_all(VALUE, "\\,"))

crop_insurance <- crop_insurance_2017 %>%
  full_join(crop_insurance_2012, 
            by = c("STATE_FIPS_CODE", 
                   "COUNTY_CODE"),
            suffix = c(".2017", ".2012")) %>%
  pivot_longer(cols = 3:4, 
               names_to = "YEAR",
               values_to = "CROP_INSURANCE_OPERATIONS") %>% 
  mutate(YEAR = as.numeric(str_remove(YEAR, "VALUE."))) %>%
  arrange(STATE_FIPS_CODE, COUNTY_CODE, YEAR)

#crop_insurance[display_missing(crop_insurance), ] %>% View()
crop_insurance <- crop_insurance[-find_missing(crop_insurance), ]
crop_insurance <- clean_fips(crop_insurance)

#------- TOTAL OPERATING EXPENSES -------- 
operating_expenses_2017 <- census_2017 %>%
  filter(CENSUS_TABLE == "1",
         SHORT_DESC %in% c("EXPENSE TOTALS, OPERATING - EXPENSE, MEASURED IN $ / OPERATION"),
         AGG_LEVEL_DESC == "COUNTY") %>%
  select(STATE_FIPS_CODE, COUNTY_CODE, VALUE) %>%
  mutate(VALUE = str_remove_all(VALUE, "\\,"))

operating_expenses_2012 <- census_2012 %>%
  filter(CENSUS_TABLE == "1",
         SHORT_DESC %in% c("EXPENSE TOTALS, OPERATING - EXPENSE, MEASURED IN $ / OPERATION"),
         AGG_LEVEL_DESC == "COUNTY") %>%
  select(STATE_FIPS_CODE, COUNTY_CODE, VALUE) %>%
  mutate(VALUE = str_remove_all(VALUE, "\\,"))

operating_expenses <- operating_expenses_2017 %>%
  full_join(operating_expenses_2012,
            by = c("STATE_FIPS_CODE", 
                   "COUNTY_CODE"),
            suffix = c(".2017", ".2012")) %>%
  pivot_longer(cols = 3:4, 
               names_to = "YEAR",
               values_to = "TOTAL_OPERATING_EXPENSES") %>% 
  mutate(YEAR = as.numeric(str_remove(YEAR, "VALUE."))) %>%
  arrange(STATE_FIPS_CODE, COUNTY_CODE, YEAR)

#operating_expenses[display_missing(operating_expenses), ] %>% View()
operating_expenses <- operating_expenses[-find_missing(operating_expenses), ]
operating_expenses <- clean_fips(operating_expenses)

#------- NET FARM INCOME -------- 
net_income_2017 <- census_2017 %>%
  filter(CENSUS_TABLE == "1",
         SHORT_DESC %in% c("INCOME, NET CASH FARM, OF OPERATIONS - NET INCOME, MEASURED IN $ / OPERATION"),
         AGG_LEVEL_DESC == "COUNTY") %>%
  select(STATE_FIPS_CODE, COUNTY_CODE, VALUE) %>%
  mutate(VALUE = str_remove_all(VALUE, "\\,"))

net_income_2012 <- census_2012 %>%
  filter(CENSUS_TABLE == "1",
         SHORT_DESC %in% c("INCOME, NET CASH FARM, OF OPERATIONS - NET INCOME, MEASURED IN $ / OPERATION"),
         AGG_LEVEL_DESC == "COUNTY") %>%
  select(STATE_FIPS_CODE, COUNTY_CODE, VALUE) %>%
  mutate(VALUE = str_remove_all(VALUE, "\\,"))

net_income <- net_income_2017 %>%
  full_join(net_income_2012,
            by = c("STATE_FIPS_CODE", 
                   "COUNTY_CODE"),
            suffix = c(".2017", ".2012")) %>%
  pivot_longer(cols = 3:4, 
               names_to = "YEAR",
               values_to = "NET_INCOME") %>% 
  mutate(YEAR = as.numeric(str_remove(YEAR, "VALUE."))) %>%
  arrange(STATE_FIPS_CODE, COUNTY_CODE, YEAR)

#net_income[display_missing(net_income), ] %>% View()
net_income <- net_income[-find_missing(net_income), ]
net_income <- clean_fips(net_income)

#------- LABOR COSTS -------- 
variables <- c("LABOR, HIRED - EXPENSE, MEASURED IN $",
               "LABOR, CONTRACT - EXPENSE, MEASURED IN $")
level_key <- c("LABOR, HIRED - EXPENSE, MEASURED IN $" = "LABOR_HIRED",
               "LABOR, CONTRACT - EXPENSE, MEASURED IN $" = "LABOR_CONTRACT")

labor_costs_2017 <- census_2017 %>%
  filter(CENSUS_TABLE == "3",
         SHORT_DESC %in% variables,
         AGG_LEVEL_DESC == "COUNTY") %>% 
  select(STATE_FIPS_CODE, COUNTY_CODE, SHORT_DESC, VALUE) %>%
  mutate(SHORT_DESC = recode_factor(SHORT_DESC, !!!level_key),
         VALUE = str_remove_all(VALUE, "\\,")) %>%
  pivot_wider(names_from=SHORT_DESC, 
              values_from=VALUE) %>%
  mutate(LABOR_HIRED = ifelse(is.na(LABOR_HIRED), 
                              "0", LABOR_HIRED),
         LABOR_CONTRACT = ifelse(is.na(LABOR_CONTRACT), 
                                 "0", LABOR_CONTRACT))

labor_costs_2012 <- census_2012 %>%
  filter(CENSUS_TABLE == "3",
         SHORT_DESC %in% variables,
         AGG_LEVEL_DESC == "COUNTY") %>% 
  select(STATE_FIPS_CODE, COUNTY_CODE, SHORT_DESC, VALUE) %>%
  mutate(SHORT_DESC = recode_factor(SHORT_DESC, !!!level_key),
         VALUE = str_remove_all(VALUE, "\\,")) %>%
  pivot_wider(names_from=SHORT_DESC, 
              values_from=VALUE) %>%
  mutate(LABOR_HIRED = ifelse(is.na(LABOR_HIRED), 
                              "0", LABOR_HIRED),
         LABOR_CONTRACT = ifelse(is.na(LABOR_CONTRACT), 
                                 "0", LABOR_CONTRACT))

labor_costs <- labor_costs_2017 %>%
  full_join(labor_costs_2012,
            by = c("STATE_FIPS_CODE", 
                   "COUNTY_CODE"),
            suffix = c(".2017", ".2012")) %>%
  pivot_longer(cols = 3:6, 
               names_to = "VARIABLE",
               values_to = "VALUE") %>% 
  separate(col="VARIABLE", 
           into=c("VARIABLE", "YEAR"),
           sep="\\.") %>% 
  mutate(YEAR = as.numeric(YEAR)) %>% 
  pivot_wider(names_from=VARIABLE,
              values_from=VALUE) %>% 
  arrange(STATE_FIPS_CODE, COUNTY_CODE, YEAR)

#labor_costs[display_missing(labor_costs), ] %>% View()
labor_costs <- labor_costs[-find_missing(labor_costs), ]
labor_costs <- clean_fips(labor_costs)

#------- FERTILIZER AND CHEMICAL TOTALS -------- 
variables <- c("CHEMICAL TOTALS - EXPENSE, MEASURED IN $",
               "FERTILIZER TOTALS, INCL LIME & SOIL CONDITIONERS - EXPENSE, MEASURED IN $")
level_key <- c("CHEMICAL TOTALS - EXPENSE, MEASURED IN $" = "CHEMICAL",
               "FERTILIZER TOTALS, INCL LIME & SOIL CONDITIONERS - EXPENSE, MEASURED IN $" = "FERTILIZER")

fertilizer_chem_cost_2017 <- census_2017 %>%
  filter(CENSUS_TABLE == "3",
         SHORT_DESC %in% variables,
         AGG_LEVEL_DESC == "COUNTY") %>% 
  select(STATE_FIPS_CODE, COUNTY_CODE, SHORT_DESC, VALUE) %>%
  mutate(SHORT_DESC = recode_factor(SHORT_DESC, !!!level_key),
         VALUE = str_remove_all(VALUE, "\\,")) %>%
  pivot_wider(names_from=SHORT_DESC, 
              values_from=VALUE)

fertilizer_chem_cost_2012 <- census_2012 %>%
  filter(CENSUS_TABLE == "3",
         SHORT_DESC %in% variables,
         AGG_LEVEL_DESC == "COUNTY") %>% 
  select(STATE_FIPS_CODE, COUNTY_CODE, SHORT_DESC, VALUE) %>%
  mutate(SHORT_DESC = recode_factor(SHORT_DESC, !!!level_key),
         VALUE = str_remove_all(VALUE, "\\,")) %>%
  pivot_wider(names_from=SHORT_DESC, 
              values_from=VALUE)

fertilizer_chem_cost <- fertilizer_chem_cost_2017 %>%
  full_join(fertilizer_chem_cost_2012,
            by = c("STATE_FIPS_CODE", 
                   "COUNTY_CODE"),
            suffix = c(".2017", ".2012")) %>%
  pivot_longer(cols = 3:6, 
               names_to = "VARIABLE",
               values_to = "VALUE") %>% 
  separate(col="VARIABLE", 
           into=c("VARIABLE", "YEAR"),
           sep="\\.") %>% 
  mutate(YEAR = as.numeric(YEAR)) %>% 
  pivot_wider(names_from=VARIABLE,
              values_from=VALUE) %>% 
  arrange(STATE_FIPS_CODE, COUNTY_CODE, YEAR)

#fertilizer_chem_cost[find_missing(fertilizer_chem_cost), ] %>% View()
fertilizer_chem_cost <- fertilizer_chem_cost[-find_missing(fertilizer_chem_cost), ]
fertilizer_chem_cost <- clean_fips(fertilizer_chem_cost)

#------- PERCENT GRAZING LAND -------- 
variables <- c("AG LAND, CROPLAND, PASTURED ONLY - ACRES", 
               "AG LAND, PASTURELAND, (EXCL CROPLAND & WOODLAND) - ACRES")
level_key <- c("AG LAND, CROPLAND, PASTURED ONLY - ACRES" = "GRAZING1", 
               "AG LAND, PASTURELAND, (EXCL CROPLAND & WOODLAND) - ACRES" = "GRAZING2")

grazing_2017 <- census_2017 %>%
  filter(CENSUS_TABLE == "8",
         CENSUS_CHAPTER == "2",
         SHORT_DESC %in% variables,
         AGG_LEVEL_DESC == "COUNTY") %>% 
  select(STATE_FIPS_CODE, COUNTY_CODE, SHORT_DESC, VALUE) %>%
  mutate(SHORT_DESC = recode_factor(SHORT_DESC, !!!level_key),
         VALUE = str_remove_all(VALUE, "\\,")) %>% 
  pivot_wider(names_from=SHORT_DESC, 
              values_from=VALUE) %>% 
  mutate(GRAZING1 = ifelse(is.na(GRAZING1), "0", GRAZING1),
         GRAZING2 = ifelse(is.na(GRAZING2), "0", GRAZING2))

grazing_2012 <- census_2012 %>%
  filter(CENSUS_TABLE == "8",
         CENSUS_CHAPTER == "2",
         SHORT_DESC %in% variables,
         AGG_LEVEL_DESC == "COUNTY") %>% 
  select(STATE_FIPS_CODE, COUNTY_CODE, SHORT_DESC, VALUE) %>%
  mutate(SHORT_DESC = recode_factor(SHORT_DESC, !!!level_key),
         VALUE = str_remove_all(VALUE, "\\,")) %>% 
  pivot_wider(names_from=SHORT_DESC, 
              values_from=VALUE) %>% 
  mutate(GRAZING1 = ifelse(is.na(GRAZING1), "0", GRAZING1),
         GRAZING2 = ifelse(is.na(GRAZING2), "0", GRAZING2))

grazing <- grazing_2017 %>%
  full_join(grazing_2012,
            by = c("STATE_FIPS_CODE", 
                   "COUNTY_CODE"),
            suffix = c(".2017", ".2012")) %>%
  pivot_longer(cols = 3:6, 
               names_to = "VARIABLE",
               values_to = "VALUE") %>% 
  separate(col="VARIABLE", 
           into=c("VARIABLE", "YEAR"),
           sep="\\.") %>% 
  mutate(YEAR = as.numeric(YEAR)) %>% 
  pivot_wider(names_from=VARIABLE,
              values_from=VALUE) %>% 
  arrange(STATE_FIPS_CODE, COUNTY_CODE, YEAR)

#grazing[find_missing(grazing), ] %>% View()
grazing <- grazing[-find_missing(grazing), ]
grazing <- clean_fips(grazing)

#------- PRACTICES -------- 
sheets <- setdiff(excel_sheets(path=land_practices), 
                  c("US", "ALASKA", "HAWAII"))

practices_2012 <- lapply(sheets, function(x) 
  read_excel(path=land_practices, sheet=x))

practices_2012 <- lapply(practices_2012, function(x) {
    x["DataValue"] <- lapply(x["DataValue"], as.character); x
  }
)

practices_2012 <- bind_rows(practices_2012)

practices_2012 <- practices_2012 %>%
  mutate(CountyFips = str_pad(CountyFips, 
                        width=3,
                        side="left",
                        pad="0")) %>%
  filter(CountyFips != "000") %>%
  unite("fips", 
        c(StateFips, CountyFips), 
        sep="", 
        remove=TRUE) %>%
  mutate(fips = str_pad(fips, 
                        width=5,
                        side="left",
                        pad="0")) %>%
  select(-StateCountyName)

variables <- c("Land on which no-till practices were used, Farms, 2012",
               "Land on which no-till practices were used, Acres, 2012",
               "Land on which no-till practices were used, Acres, Avg Per Farm, 2012",
               "Land planted to a cover crop (excluding CRP), Farms, 2012",
               "Land planted to a cover crop (excluding CRP), Acres, 2012",
               "Land planted to a cover crop (excluding CRP), Acres, Avg Per Farm, 2012",
               "Land drained by tile, Farms, 2012",
               "Land drained by tile, Acres, 2012",
               "Land drained by tile, Acres, Avg Per Farm, 2012")
level_key <- c("Land on which no-till practices were used, Farms, 2012" = "NO_TILL_OPERATIONS",
               "Land on which no-till practices were used, Acres, 2012" = "NO_TILL_ACRES",
               "Land on which no-till practices were used, Acres, Avg Per Farm, 2012" = "NO_TILL_ACRES_PER_OPERATION",
               "Land planted to a cover crop (excluding CRP), Farms, 2012" = "COVER_CROP_OPERATIONS",
               "Land planted to a cover crop (excluding CRP), Acres, 2012" = "COVER_CROP_ACRES",
               "Land planted to a cover crop (excluding CRP), Acres, Avg Per Farm, 2012" = "COVER_CROP_ACRES_PER_OPERATION",
               "Land drained by tile, Farms, 2012" = "TILE_DRAINAGE_OPERATIONS",
               "Land drained by tile, Acres, 2012" = "TILE_DRAINAGE_ACRES",
               "Land drained by tile, Acres, Avg Per Farm, 2012" = "TILE_DRAINAGE_ACRES_PER_OPERATION")

practices_2012 <- practices_2012 %>%
  filter(DataItem %in% variables) %>%
  mutate(DataItem = recode_factor(DataItem, !!!level_key)) %>%
  pivot_wider(names_from=DataItem,
              values_from=DataValue)
  
variables <- c("PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - NUMBER OF OPERATIONS",
               "PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - ACRES",
               "PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - AREA, MEASURED IN ACRES / OPERATION",
               "PRACTICES, LAND USE, CROPLAND, COVER CROP PLANTED, (EXCL CRP) - NUMBER OF OPERATIONS",
               "PRACTICES, LAND USE, CROPLAND, COVER CROP PLANTED, (EXCL CRP) - ACRES",
               "PRACTICES, LAND USE, CROPLAND, COVER CROP PLANTED, (EXCL CRP) - AREA, MEASURED IN ACRES / OPERATION",
               "PRACTICES, LAND USE, DRAINED BY TILE - NUMBER OF OPERATIONS",
               "PRACTICES, LAND USE, DRAINED BY TILE - ACRES",
               "PRACTICES, LAND USE, DRAINED BY TILE - AREA, MEASURED IN ACRES / OPERATION")
level_key <- c("PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - NUMBER OF OPERATIONS" = "NO_TILL_OPERATIONS",
               "PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - ACRES" = "NO_TILL_ACRES",
               "PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - AREA, MEASURED IN ACRES / OPERATION" = "NO_TILL_ACRES_PER_OPERATION",
               "PRACTICES, LAND USE, CROPLAND, COVER CROP PLANTED, (EXCL CRP) - NUMBER OF OPERATIONS" = "COVER_CROP_OPERATIONS",
               "PRACTICES, LAND USE, CROPLAND, COVER CROP PLANTED, (EXCL CRP) - ACRES" = "COVER_CROP_ACRES",
               "PRACTICES, LAND USE, CROPLAND, COVER CROP PLANTED, (EXCL CRP) - AREA, MEASURED IN ACRES / OPERATION" = "COVER_CROP_ACRES_PER_OPERATION",
               "PRACTICES, LAND USE, DRAINED BY TILE - NUMBER OF OPERATIONS" = "TILE_DRAINAGE_OPERATIONS",
               "PRACTICES, LAND USE, DRAINED BY TILE - ACRES" = "TILE_DRAINAGE_ACRES",
               "PRACTICES, LAND USE, DRAINED BY TILE - AREA, MEASURED IN ACRES / OPERATION" = "TILE_DRAINAGE_ACRES_PER_OPERATION")

practices_2017 <- census_2017 %>%
  filter(CENSUS_TABLE == "41",
         SHORT_DESC %in% variables,
         AGG_LEVEL_DESC == "COUNTY") %>% 
  select(STATE_FIPS_CODE, COUNTY_CODE, SHORT_DESC, VALUE) %>%
  mutate(SHORT_DESC = recode_factor(SHORT_DESC, !!!level_key),
         VALUE = str_remove_all(VALUE, "\\,")) %>%
  pivot_wider(names_from=SHORT_DESC, 
              values_from=VALUE)

practices_2017 <- clean_fips(practices_2017)

practices <- practices_2017 %>%
  full_join(practices_2012,
            by = "fips",
            suffix = c(".2017", ".2012")) %>%
  pivot_longer(cols = 2:19, 
               names_to = "VARIABLE",
               values_to = "VALUE") %>% 
  separate(col="VARIABLE", 
           into=c("VARIABLE", "YEAR"),
           sep="\\.") %>% 
  mutate(YEAR = as.numeric(YEAR)) %>% 
  pivot_wider(names_from=VARIABLE,
              values_from=VALUE) %>% 
  arrange(fips, YEAR)

#practices[find_missing(practices), ] %>% View()
practices <- practices[-find_missing(practices), ]

#------- FEMALE PRODUCERS -------- 
female_producers_2017 <- census_2017 %>%
  filter(CENSUS_TABLE == "47",
         CENSUS_CHAPTER == "2",
         SHORT_DESC %in% c("PRODUCERS, PRINCIPAL, FEMALE - NUMBER OF OPERATIONS"),
         AGG_LEVEL_DESC == "COUNTY") %>%
  select(STATE_FIPS_CODE, COUNTY_CODE, VALUE) %>%
  mutate(VALUE = str_remove_all(VALUE, "\\,"))

female_producers_2012 <- census_2012 %>%
  filter(CENSUS_TABLE == "47",
         CENSUS_CHAPTER == "2",
         SHORT_DESC %in% c("OPERATORS, PRINCIPAL, FEMALE - NUMBER OF OPERATIONS"),
         AGG_LEVEL_DESC == "COUNTY") %>%
  select(STATE_FIPS_CODE, COUNTY_CODE, VALUE) %>%
  mutate(VALUE = str_remove_all(VALUE, "\\,"))

female_producers <- female_producers_2017 %>%
  full_join(female_producers_2012,
            by = c("STATE_FIPS_CODE", 
                   "COUNTY_CODE"),
            suffix = c(".2017", ".2012")) %>%
  pivot_longer(cols = 3:4, 
               names_to = "YEAR",
               values_to = "FEMALE_PRODUCERS") %>% 
  mutate(YEAR = as.numeric(str_remove(YEAR, "VALUE."))) %>%
  arrange(STATE_FIPS_CODE, COUNTY_CODE, YEAR)

#female_producers[display_missing(female_producers), ] %>% View()
female_producers <- female_producers[-find_missing(female_producers), ]
female_producers <- clean_fips(female_producers)

#------- NON-WHITE PRODUCERS -------- 
variables <- c("PRODUCERS, PRINCIPAL, HISPANIC - NUMBER OF OPERATIONS", 
               "PRODUCERS, PRINCIPAL, AMERICAN INDIAN OR ALASKA NATIVE - NUMBER OF OPERATIONS",
               "PRODUCERS, PRINCIPAL, ASIAN - NUMBER OF OPERATIONS",
               "PRODUCERS, PRINCIPAL, BLACK OR AFRICAN AMERICAN - NUMBER OF OPERATIONS",
               "PRODUCERS, PRINCIPAL, NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER - NUMBER OF OPERATIONS",
               "PRODUCERS, PRINCIPAL, MULTI-RACE - NUMBER OF OPERATIONS")
level_key <- c("PRODUCERS, PRINCIPAL, HISPANIC - NUMBER OF OPERATIONS" = "HISPANIC_PRODUCERS", 
               "PRODUCERS, PRINCIPAL, AMERICAN INDIAN OR ALASKA NATIVE - NUMBER OF OPERATIONS" = "AMERICAN_INDIAN_ALASKA_NATIVE_PRODUCERS",
               "PRODUCERS, PRINCIPAL, ASIAN - NUMBER OF OPERATIONS" = "ASIAN_PRODUCERS",
               "PRODUCERS, PRINCIPAL, BLACK OR AFRICAN AMERICAN - NUMBER OF OPERATIONS" = "AFRICAN_AMERICAN_PRODUCERS",
               "PRODUCERS, PRINCIPAL, NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER - NUMBER OF OPERATIONS" = "HAWAIIAN_PACIFIC_ISLANDER_PRODUCERS",
               "PRODUCERS, PRINCIPAL, MULTI-RACE - NUMBER OF OPERATIONS" = "MULTI_RACE_PRODUCERS")

non_white_2017 <- census_2017 %>%
  filter(CENSUS_TABLE %in% c("48", "49", "50", "51", "52", "54"),
         CENSUS_CHAPTER == "2",
         SHORT_DESC %in% variables,
         AGG_LEVEL_DESC == "COUNTY") %>%
  select(STATE_FIPS_CODE, COUNTY_CODE, SHORT_DESC, VALUE) %>%
  mutate(SHORT_DESC = recode_factor(SHORT_DESC, !!!level_key),
         VALUE = str_remove_all(VALUE, "\\,")) %>% 
  pivot_wider(names_from=SHORT_DESC, 
              values_from=VALUE) %>% 
  mutate(HISPANIC_PRODUCERS = ifelse(is.na(HISPANIC_PRODUCERS), 
                                     "0", HISPANIC_PRODUCERS),
         AMERICAN_INDIAN_ALASKA_NATIVE_PRODUCERS = ifelse(is.na(AMERICAN_INDIAN_ALASKA_NATIVE_PRODUCERS), 
                                                          "0", AMERICAN_INDIAN_ALASKA_NATIVE_PRODUCERS),
         ASIAN_PRODUCERS = ifelse(is.na(ASIAN_PRODUCERS), 
                                     "0", ASIAN_PRODUCERS),
         AFRICAN_AMERICAN_PRODUCERS = ifelse(is.na(AFRICAN_AMERICAN_PRODUCERS), 
                                     "0", AFRICAN_AMERICAN_PRODUCERS),
         HAWAIIAN_PACIFIC_ISLANDER_PRODUCERS = ifelse(is.na(HAWAIIAN_PACIFIC_ISLANDER_PRODUCERS), 
                                     "0", HAWAIIAN_PACIFIC_ISLANDER_PRODUCERS),
         MULTI_RACE_PRODUCERS = ifelse(is.na(MULTI_RACE_PRODUCERS), 
                                     "0", MULTI_RACE_PRODUCERS)) %>%
  mutate(NON_WHITE = as.numeric(HISPANIC_PRODUCERS) +
                     as.numeric(AMERICAN_INDIAN_ALASKA_NATIVE_PRODUCERS)+
                     as.numeric(ASIAN_PRODUCERS) +
                     as.numeric(AFRICAN_AMERICAN_PRODUCERS) +
                     as.numeric(HAWAIIAN_PACIFIC_ISLANDER_PRODUCERS) +
                     as.numeric(MULTI_RACE_PRODUCERS)) %>%
  select(-HISPANIC_PRODUCERS, -AMERICAN_INDIAN_ALASKA_NATIVE_PRODUCERS,
         -ASIAN_PRODUCERS, -AFRICAN_AMERICAN_PRODUCERS,
         -HAWAIIAN_PACIFIC_ISLANDER_PRODUCERS, -MULTI_RACE_PRODUCERS)

variables <- c("OPERATORS, PRINCIPAL, HISPANIC - NUMBER OF OPERATIONS", 
               "OPERATORS, PRINCIPAL, AMERICAN INDIAN OR ALASKA NATIVE - NUMBER OF OPERATIONS",
               "OPERATORS, PRINCIPAL, ASIAN - NUMBER OF OPERATIONS",
               "OPERATORS, PRINCIPAL, BLACK OR AFRICAN AMERICAN - NUMBER OF OPERATIONS",
               "OPERATORS, PRINCIPAL, NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER - NUMBER OF OPERATIONS",
               "OPERATORS, PRINCIPAL, MULTI-RACE - NUMBER OF OPERATIONS")
level_key <- c("OPERATORS, PRINCIPAL, HISPANIC - NUMBER OF OPERATIONS" = "HISPANIC_PRODUCERS", 
               "OPERATORS, PRINCIPAL, AMERICAN INDIAN OR ALASKA NATIVE - NUMBER OF OPERATIONS" = "AMERICAN_INDIAN_ALASKA_NATIVE_PRODUCERS",
               "OPERATORS, PRINCIPAL, ASIAN - NUMBER OF OPERATIONS" = "ASIAN_PRODUCERS",
               "OPERATORS, PRINCIPAL, BLACK OR AFRICAN AMERICAN - NUMBER OF OPERATIONS" = "AFRICAN_AMERICAN_PRODUCERS",
               "OPERATORS, PRINCIPAL, NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER - NUMBER OF OPERATIONS" = "HAWAIIAN_PACIFIC_ISLANDER_PRODUCERS",
               "OPERATORS, PRINCIPAL, MULTI-RACE - NUMBER OF OPERATIONS" = "MULTI_RACE_PRODUCERS")

non_white_2012 <- census_2012 %>%
  filter(CENSUS_TABLE %in% c("49", "50", "51", "52", "53", "55"),
         CENSUS_CHAPTER == "2",
         SHORT_DESC %in% variables,
         AGG_LEVEL_DESC == "COUNTY") %>%
  select(STATE_FIPS_CODE, COUNTY_CODE, SHORT_DESC, VALUE) %>%
  mutate(SHORT_DESC = recode_factor(SHORT_DESC, !!!level_key),
         VALUE = str_remove_all(VALUE, "\\,")) %>% 
  pivot_wider(names_from=SHORT_DESC, 
              values_from=VALUE) %>% 
  mutate(HISPANIC_PRODUCERS = ifelse(is.na(HISPANIC_PRODUCERS), 
                                     "0", HISPANIC_PRODUCERS),
         AMERICAN_INDIAN_ALASKA_NATIVE_PRODUCERS = ifelse(is.na(AMERICAN_INDIAN_ALASKA_NATIVE_PRODUCERS), 
                                                          "0", AMERICAN_INDIAN_ALASKA_NATIVE_PRODUCERS),
         ASIAN_PRODUCERS = ifelse(is.na(ASIAN_PRODUCERS), 
                                  "0", ASIAN_PRODUCERS),
         AFRICAN_AMERICAN_PRODUCERS = ifelse(is.na(AFRICAN_AMERICAN_PRODUCERS), 
                                             "0", AFRICAN_AMERICAN_PRODUCERS),
         HAWAIIAN_PACIFIC_ISLANDER_PRODUCERS = ifelse(is.na(HAWAIIAN_PACIFIC_ISLANDER_PRODUCERS), 
                                                      "0", HAWAIIAN_PACIFIC_ISLANDER_PRODUCERS),
         MULTI_RACE_PRODUCERS = ifelse(is.na(MULTI_RACE_PRODUCERS), 
                                       "0", MULTI_RACE_PRODUCERS)) %>%
  mutate(NON_WHITE = as.numeric(HISPANIC_PRODUCERS) +
           as.numeric(AMERICAN_INDIAN_ALASKA_NATIVE_PRODUCERS)+
           as.numeric(ASIAN_PRODUCERS) +
           as.numeric(AFRICAN_AMERICAN_PRODUCERS) +
           as.numeric(HAWAIIAN_PACIFIC_ISLANDER_PRODUCERS) +
           as.numeric(MULTI_RACE_PRODUCERS)) %>%
  select(-HISPANIC_PRODUCERS, -AMERICAN_INDIAN_ALASKA_NATIVE_PRODUCERS,
         -ASIAN_PRODUCERS, -AFRICAN_AMERICAN_PRODUCERS,
         -HAWAIIAN_PACIFIC_ISLANDER_PRODUCERS, -MULTI_RACE_PRODUCERS)

non_white <- non_white_2017 %>%
  full_join(non_white_2012,
            by = c("STATE_FIPS_CODE", 
                   "COUNTY_CODE"),
            suffix = c(".2017", ".2012")) %>% 
  pivot_longer(cols = 3:4, 
               names_to = "VARIABLE",
               values_to = "VALUE") %>% 
  separate(col="VARIABLE", 
           into=c("VARIABLE", "YEAR"),
           sep="\\.") %>% 
  mutate(YEAR = as.numeric(YEAR)) %>% 
  pivot_wider(names_from=VARIABLE,
              values_from=VALUE) %>% 
  arrange(STATE_FIPS_CODE, COUNTY_CODE, YEAR)

#non_white[display_missing(non_white), ] %>% View()
non_white <- non_white[-find_missing(non_white), ]
non_white <- clean_fips(non_white)

#------- BEGINNNING/NEW PRODUCERS -------- 
variables <- c("PRODUCERS, PRINCIPAL, YEARS ON ANY OPERATION, LT 6 YEARS - NUMBER OF PRODUCERS", 
               "PRODUCERS, PRINCIPAL, YEARS ON ANY OPERATION, 6 TO 10 YEARS - NUMBER OF PRODUCERS")
level_key <- c("PRODUCERS, PRINCIPAL, YEARS ON ANY OPERATION, LT 6 YEARS - NUMBER OF PRODUCERS" = "LESS_THAN_10_a", 
               "PRODUCERS, PRINCIPAL, YEARS ON ANY OPERATION, 6 TO 10 YEARS - NUMBER OF PRODUCERS" = "LESS_THAN_10_b")

new_producers_2017 <- census_2017 %>%
  filter(CENSUS_TABLE == "45",
         CENSUS_CHAPTER == "2",
         SHORT_DESC %in% variables,
         AGG_LEVEL_DESC == "COUNTY") %>%
  select(STATE_FIPS_CODE, COUNTY_CODE, SHORT_DESC, VALUE) %>%
  mutate(SHORT_DESC = recode_factor(SHORT_DESC, !!!level_key)) %>% 
  pivot_wider(names_from=SHORT_DESC, 
              values_from=VALUE) %>% 
  mutate(LESS_THAN_10_a = ifelse(is.na(LESS_THAN_10_a), "0", LESS_THAN_10_a),
         LESS_THAN_10_b = ifelse(is.na(LESS_THAN_10_b), "0", LESS_THAN_10_b)) %>%
  mutate(LESS_THAN_10_a = as.numeric(str_remove_all(LESS_THAN_10_a, "\\,")),
         LESS_THAN_10_b = as.numeric(str_remove_all(LESS_THAN_10_b, "\\,"))) %>%
  mutate(NEW_PRODUCERS = LESS_THAN_10_a + LESS_THAN_10_b) %>%
  select(-LESS_THAN_10_a, -LESS_THAN_10_b)

variables <- c("OPERATORS, PRINCIPAL, YEARS ON ANY OPERATION, LT 3 YEARS - NUMBER OF OPERATORS", 
               "OPERATORS, PRINCIPAL, YEARS ON ANY OPERATION, 3 TO 4 YEARS - NUMBER OF OPERATORS",
               "OPERATORS, PRINCIPAL, YEARS ON ANY OPERATION, 5 TO 9 YEARS - NUMBER OF OPERATORS")
level_key <- c("OPERATORS, PRINCIPAL, YEARS ON ANY OPERATION, LT 3 YEARS - NUMBER OF OPERATORS" = "LESS_THAN_10_a", 
               "OPERATORS, PRINCIPAL, YEARS ON ANY OPERATION, 3 TO 4 YEARS - NUMBER OF OPERATORS" = "LESS_THAN_10_b",
               "OPERATORS, PRINCIPAL, YEARS ON ANY OPERATION, 5 TO 9 YEARS - NUMBER OF OPERATORS" = "LESS_THAN_10_c")

new_producers_2012 <- census_2012 %>%
  filter(CENSUS_TABLE == "45",
         CENSUS_CHAPTER == "2",
         SHORT_DESC %in% variables,
         AGG_LEVEL_DESC == "COUNTY") %>%
  select(STATE_FIPS_CODE, COUNTY_CODE, SHORT_DESC, VALUE) %>%
  mutate(SHORT_DESC = recode_factor(SHORT_DESC, !!!level_key)) %>% 
  pivot_wider(names_from=SHORT_DESC, 
              values_from=VALUE) %>% 
  mutate(LESS_THAN_10_a = ifelse(is.na(LESS_THAN_10_a), "0", LESS_THAN_10_a),
         LESS_THAN_10_b = ifelse(is.na(LESS_THAN_10_b), "0", LESS_THAN_10_b),
         LESS_THAN_10_c = ifelse(is.na(LESS_THAN_10_c), "0", LESS_THAN_10_c)) %>%
  mutate(LESS_THAN_10_a = as.numeric(str_remove_all(LESS_THAN_10_a, "\\,")),
         LESS_THAN_10_b = as.numeric(str_remove_all(LESS_THAN_10_b, "\\,")),
         LESS_THAN_10_c = as.numeric(str_remove_all(LESS_THAN_10_c, "\\,"))) %>%
  mutate(NEW_PRODUCERS = LESS_THAN_10_a + LESS_THAN_10_b + LESS_THAN_10_c) %>%
  select(-LESS_THAN_10_a, -LESS_THAN_10_b, -LESS_THAN_10_c)

new_producers <- new_producers_2017 %>%
  full_join(new_producers_2012,
            by = c("STATE_FIPS_CODE", 
                   "COUNTY_CODE"),
            suffix = c(".2017", ".2012")) %>%
  pivot_longer(cols = 3:4, 
               names_to = "VARIABLE",
               values_to = "VALUE") %>% 
  separate(col="VARIABLE", 
           into=c("VARIABLE", "YEAR"),
           sep="\\.") %>% 
  mutate(YEAR = as.numeric(YEAR)) %>% 
  pivot_wider(names_from=VARIABLE,
              values_from=VALUE) %>% 
  arrange(STATE_FIPS_CODE, COUNTY_CODE, YEAR)

#new_producers[display_missing(new_producers), ] %>% View()
new_producers <- new_producers[-find_missing(new_producers), ]
new_producers <- clean_fips(new_producers)

#------- AGE BRACKETS -------- 
variables <- c("PRODUCERS, PRINCIPAL, AGE LT 25 - NUMBER OF PRODUCERS",
               "PRODUCERS, PRINCIPAL, AGE 25 TO 34 - NUMBER OF PRODUCERS",
               "PRODUCERS, PRINCIPAL, AGE 35 TO 44 - NUMBER OF PRODUCERS",
               "PRODUCERS, PRINCIPAL, AGE 45 TO 54 - NUMBER OF PRODUCERS",
               "PRODUCERS, PRINCIPAL, AGE 55 TO 64 - NUMBER OF PRODUCERS",
               "PRODUCERS, PRINCIPAL, AGE 65 TO 74 - NUMBER OF PRODUCERS",
               "PRODUCERS, PRINCIPAL, AGE GE 75 - NUMBER OF PRODUCERS")
level_key <- c("PRODUCERS, PRINCIPAL, AGE LT 25 - NUMBER OF PRODUCERS" = "UNDER_25",
               "PRODUCERS, PRINCIPAL, AGE 25 TO 34 - NUMBER OF PRODUCERS" = "25_TO_34",
               "PRODUCERS, PRINCIPAL, AGE 35 TO 44 - NUMBER OF PRODUCERS" = "35_TO_44",
               "PRODUCERS, PRINCIPAL, AGE 45 TO 54 - NUMBER OF PRODUCERS" = "45_TO_54",
               "PRODUCERS, PRINCIPAL, AGE 55 TO 64 - NUMBER OF PRODUCERS" = "55_TO_64",
               "PRODUCERS, PRINCIPAL, AGE 65 TO 74 - NUMBER OF PRODUCERS" = "65_TO_74",
               "PRODUCERS, PRINCIPAL, AGE GE 75 - NUMBER OF PRODUCERS" = "OVER_75")

age_brackets_2017 <- census_2017 %>%
  filter(CENSUS_TABLE == "45",
         CENSUS_CHAPTER == "2",
         SHORT_DESC %in% variables,
         AGG_LEVEL_DESC == "COUNTY") %>%
  select(STATE_FIPS_CODE, COUNTY_CODE, SHORT_DESC, VALUE) %>%
  mutate(SHORT_DESC = recode_factor(SHORT_DESC, !!!level_key)) %>% 
  pivot_wider(names_from=SHORT_DESC, 
              values_from=VALUE) %>% 
  mutate(`UNDER_25` = ifelse(is.na(`UNDER_25`), "0", `UNDER_25`),
         `25_TO_34` = ifelse(is.na(`25_TO_34`), "0", `25_TO_34`),
         `35_TO_44` = ifelse(is.na(`35_TO_44`), "0", `35_TO_44`),
         `45_TO_54` = ifelse(is.na(`45_TO_54`), "0", `45_TO_54`),
         `55_TO_64` = ifelse(is.na(`55_TO_64`), "0", `55_TO_64`),
         `65_TO_74` = ifelse(is.na(`65_TO_74`), "0", `65_TO_74`),
         `OVER_75` = ifelse(is.na(`OVER_75`), "0", `OVER_75`)) %>%
  mutate(`UNDER_25` = as.numeric(str_remove_all((`UNDER_25`), "\\,")),
         `25_TO_34` = as.numeric(str_remove_all((`25_TO_34`), "\\,")),
         `35_TO_44` = as.numeric(str_remove_all((`35_TO_44`), "\\,")),
         `45_TO_54` = as.numeric(str_remove_all((`45_TO_54`), "\\,")),
         `55_TO_64` = as.numeric(str_remove_all((`55_TO_64`), "\\,")),
         `65_TO_74` = as.numeric(str_remove_all((`65_TO_74`), "\\,")),
         `OVER_75` = as.numeric(str_remove_all((`OVER_75`), "\\,"))) %>%
  mutate(`OVER_65` = `65_TO_74` + `OVER_75`) %>%
  select(-`65_TO_74`, -`OVER_75`)

variables <- c("OPERATORS, PRINCIPAL, AGE LT 25 - NUMBER OF OPERATORS",
               "OPERATORS, PRINCIPAL, AGE 25 TO 34 - NUMBER OF OPERATORS",
               "OPERATORS, PRINCIPAL, AGE 35 TO 44 - NUMBER OF OPERATORS",
               "OPERATORS, PRINCIPAL, AGE 45 TO 54 - NUMBER OF OPERATORS",
               "OPERATORS, PRINCIPAL, AGE 55 TO 59 - NUMBER OF OPERATORS",
               "OPERATORS, PRINCIPAL, AGE 60 TO 64 - NUMBER OF OPERATORS",
               "OPERATORS, PRINCIPAL, AGE 65 TO 69 - NUMBER OF OPERATORS",
               "OPERATORS, PRINCIPAL, AGE GE 70 - NUMBER OF OPERATORS")
level_key <- c("OPERATORS, PRINCIPAL, AGE LT 25 - NUMBER OF OPERATORS" = "UNDER_25",
               "OPERATORS, PRINCIPAL, AGE 25 TO 34 - NUMBER OF OPERATORS" = "25_TO_34",
               "OPERATORS, PRINCIPAL, AGE 35 TO 44 - NUMBER OF OPERATORS" = "35_TO_44",
               "OPERATORS, PRINCIPAL, AGE 45 TO 54 - NUMBER OF OPERATORS" = "45_TO_54",
               "OPERATORS, PRINCIPAL, AGE 55 TO 59 - NUMBER OF OPERATORS" = "55_TO_59",
               "OPERATORS, PRINCIPAL, AGE 60 TO 64 - NUMBER OF OPERATORS" = "60_TO_64",
               "OPERATORS, PRINCIPAL, AGE 65 TO 69 - NUMBER OF OPERATORS" = "65_TO_69",
               "OPERATORS, PRINCIPAL, AGE GE 70 - NUMBER OF OPERATORS" = "OVER_70")

age_brackets_2012 <- census_2012 %>%
  filter(CENSUS_TABLE == "45",
         CENSUS_CHAPTER == "2",
         SHORT_DESC %in% variables,
         AGG_LEVEL_DESC == "COUNTY") %>%
  select(STATE_FIPS_CODE, COUNTY_CODE, SHORT_DESC, VALUE) %>%
  mutate(SHORT_DESC = recode_factor(SHORT_DESC, !!!level_key)) %>% 
  pivot_wider(names_from=SHORT_DESC, 
              values_from=VALUE) %>% 
  mutate(`UNDER_25` = ifelse(is.na(`UNDER_25`), "0", `UNDER_25`),
         `25_TO_34` = ifelse(is.na(`25_TO_34`), "0", `25_TO_34`),
         `35_TO_44` = ifelse(is.na(`35_TO_44`), "0", `35_TO_44`),
         `45_TO_54` = ifelse(is.na(`45_TO_54`), "0", `45_TO_54`),
         `55_TO_59` = ifelse(is.na(`55_TO_59`), "0", `55_TO_59`),
         `60_TO_64` = ifelse(is.na(`60_TO_64`), "0", `60_TO_64`),
         `65_TO_69` = ifelse(is.na(`65_TO_69`), "0", `65_TO_69`),
         `OVER_70` = ifelse(is.na(`OVER_70`), "0", `OVER_70`)) %>%
  mutate(`UNDER_25` = as.numeric(str_remove_all((`UNDER_25`), "\\,")),
         `25_TO_34` = as.numeric(str_remove_all((`25_TO_34`), "\\,")),
         `35_TO_44` = as.numeric(str_remove_all((`35_TO_44`), "\\,")),
         `45_TO_54` = as.numeric(str_remove_all((`45_TO_54`), "\\,")),
         `55_TO_59` = as.numeric(str_remove_all((`55_TO_59`), "\\,")),
         `60_TO_64` = as.numeric(str_remove_all((`60_TO_64`), "\\,")),
         `65_TO_69` = as.numeric(str_remove_all((`65_TO_69`), "\\,")),
         `OVER_70` = as.numeric(str_remove_all((`OVER_70`), "\\,"))) %>%
  mutate(`55_TO_64` = `55_TO_59` + `60_TO_64`,
         `OVER_65` = `65_TO_69` + `OVER_70`) %>%
  select(-`55_TO_59`, -`60_TO_64`, -`65_TO_69`, -`OVER_70`)

age_brackets <- age_brackets_2017 %>%
  full_join(age_brackets_2012,
            by = c("STATE_FIPS_CODE", 
                   "COUNTY_CODE"),
            suffix = c(".2017", ".2012")) %>% 
  pivot_longer(cols = 3:14, 
               names_to = "VARIABLE",
               values_to = "VALUE") %>% 
  separate(col="VARIABLE", 
           into=c("VARIABLE", "YEAR"),
           sep="\\.") %>% 
  mutate(YEAR = as.numeric(YEAR)) %>% 
  pivot_wider(names_from=VARIABLE,
              values_from=VALUE) %>% 
  arrange(STATE_FIPS_CODE, COUNTY_CODE, YEAR)

#age_brackets[display_missing(age_brackets), ] %>% View()
age_brackets <- age_brackets[-find_missing(age_brackets), ]
age_brackets <- clean_fips(age_brackets)

#------- REMOVE TEMPORARY DATA FRAMES -------- 
rm(county_farms_2012, county_farms_2017,
   cons_prog_particip_2012, cons_prog_particip_2017,
   fed_prog_particip_2012, fed_prog_particip_2017,
   organic_2012, organic_2017,
   crop_insurance_2012, crop_insurance_2017,
   operating_expenses_2012, operating_expenses_2017,
   net_income_2012, net_income_2017,
   labor_costs_2012, labor_costs_2017,
   fertilizer_chem_cost_2012, fertilizer_chem_cost_2017,
   grazing_2012, grazing_2017,
   practices_2012, practices_2017,
   female_producers_2012, female_producers_2017,
   non_white_2012, non_white_2017,
   new_producers_2012, new_producers_2017,
   age_brackets_2012, age_brackets_2017)

#------- COMBINE IT ALL TOGETHER -------- 
entire_dataset <- entire_dataset %>%
  left_join(county_farms, 
            by = c("fips", "year" = "YEAR")) %>%
  left_join(cons_prog_particip,
            by = c("fips", "year" = "YEAR")) %>%
  left_join(fed_prog_particip,
            by = c("fips", "year" = "YEAR")) %>%
  left_join(organic,
            by = c("fips", "year" = "YEAR")) %>%
  left_join(crop_insurance,
            by = c("fips", "year" = "YEAR")) %>%
  left_join(operating_expenses,
            by = c("fips", "year" = "YEAR")) %>%
  left_join(net_income,
            by = c("fips", "year" = "YEAR")) %>%
  left_join(labor_costs,
            by = c("fips", "year" = "YEAR")) %>%
  left_join(fertilizer_chem_cost,
            by = c("fips", "year" = "YEAR")) %>%
  left_join(grazing,
            by = c("fips", "year" = "YEAR")) %>%
  left_join(practices,
            by = c("fips", "year" = "YEAR")) %>%
  left_join(female_producers,
            by = c("fips", "year" = "YEAR")) %>%
  left_join(non_white,
            by = c("fips", "year" = "YEAR")) %>%
  left_join(new_producers,
            by = c("fips", "year" = "YEAR")) %>%
  left_join(age_brackets,
            by = c("fips", "year" = "YEAR"))

names(entire_dataset) <- str_to_lower(names(entire_dataset))

#------- REMOVE TEMPORARY DATA FRAMES -------- 
rm(census_2012, census_2017,
   county_farms, cons_prog_particip, fed_prog_particip,
   organic, crop_insurance, operating_expenses, net_income,
   labor_costs, fertilizer_chem_cost, grazing, female_producers,
   non_white, new_producers, age_brackets, practices, land_grant_data)



#------------------ EXPLORING MISSING DATA --------------------------------------
#------------------------------------------------------------------------------#
variables_to_check <- setdiff(names(entire_dataset),
                              c("fips", "county", "state", "year", "population"))

missing_data <- map(variables_to_check, function(x) {
  data <- unlist(entire_dataset[x])
  tibble(  
    "total_obs" = nrow(entire_dataset),
    "NAs_introduced" = which(is.na(data)) %>% length(),
    "non_numeric_total" = which(str_detect(data, "\\D")) %>% length(),
    "negatives" = which(str_detect(data, "[-]")) %>% length(),
    "decimals" = which(str_detect(data, "[.]")) %>% length(),
    "(D)" = which(str_detect(data, "[(][D][)]")) %>% length(),
    "(Z)" = which(str_detect(data, "[(][Z][)]")) %>% length(),
    "(H)" = which(str_detect(data, "[(][H][)]")) %>% length(),
    "(IC)" = which(str_detect(data, "[(][I][C][)]")) %>% length(),
    "(L)" = which(str_detect(data, "[(][L][)]")) %>% length(),
    "(NA)" = which(str_detect(data, "[(][N][A][)]"))%>% length(),
    "(X)" = which(str_detect(data, "[(][X][)]")) %>% length()
  )
})

names(missing_data) <- variables_to_check
missing_data <- bind_rows(missing_data, .id="variable") %>%
  mutate(non_numeric_total = non_numeric_total - negatives - decimals,
         missing_total = NAs_introduced + non_numeric_total) %>%
  select(variable, total_obs, missing_total, NAs_introduced,
         non_numeric_total, `(D)`, `(Z)`, `(H)`, `(IC)`,
         `(L)`, `(NA)`, `(X)`)

entire_dataset <- entire_dataset %>%
  mutate(n_of_operations = as.numeric(n_of_operations),
         cons_prog_operations = as.numeric(cons_prog_operations),
         fed_prog_operations = as.numeric(fed_prog_operations),
         organic = as.numeric(organic),
         crop_insurance_operations = as.numeric(crop_insurance_operations),
         tile_drainage_operations = as.numeric(tile_drainage_operations),
         no_till_operations = as.numeric(no_till_operations),
         cover_crop_operations = as.numeric(cover_crop_operations),
         female_producers = as.numeric(female_producers))


#------------------ NRCS AND EXTENSION EMPLOYMENT ------------------------------
#------------------------------------------------------------------------------#
#------- NUMBER OF NRCS EMPLOYEES PER COUNTY -------- 
nrcs_employees <- read_csv("nrcs_table_onlydotgovs.csv")
zipcodes <- read_csv("uszips.csv") %>%
  transmute(Zipcode = zip, 
            fips = as.character(county_fips))%>%
  mutate(fips = str_pad(fips, 
                        width=5,
                        side="left",
                        pad="0"))

nrcs_employees <- nrcs_employees %>%
  left_join(zipcodes, by=c("Zipcode")) %>%
  filter(!is.na(fips)) %>%
  group_by(fips) %>%
  summarize(nrcs_employees = n_distinct(Name))

#------- REMOVE TEMPORARY DATA FRAMES -------- 
rm(zipcodes)
