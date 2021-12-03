# --- Amy Jiravisitcul. 3 Dec 2021 ----
rm(list = ls())
library(knitr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
install.packages("readxl") # CRAN version
library(readxl)
install.packages("stringr")
library(stringr)
install.packages('segregation')
library(segregation)
getwd()
setwd("/Users/amyjiravisitcul/Downloads/States from Fall 2021/")

# --- ENROLLMENT 2021 ----
# Downloaded from http://www.p12.nysed.gov/irs/statistics/enroll-n-staff/home.html, posted 9/16/2021
# File URL http://www.p12.nysed.gov/irs/statistics/enroll-n-staff/PublicSchool2021Race.xlsx

enrl <- read_excel("raw data/NY_PublicSchool2021Race.xlsx")
names(enrl) <- tolower(names(enrl))
enrl <- enrl[,1:11]

full_entities <- enrl %>% 
  filter(county == "ERIE"|
           county == "MONROE"|
           county == "KINGS"|
           county == "QUEENS"|
           county == "NEW YORK"|
           county == "BRONX"|
           county == "RICHMOND")%>% 
  mutate(district = case_when(str_detect(`district name`, "NYC GEOG") ~ # re-format NYC public schools
                                paste0("NYC GEOG DISTRICT #", substr(`state location id`, 3, 4)),
                              county == "ERIE" &`school type`=="CHARTER" ~ "BUFFALO", # include charter demographics
                              county == "MONROE" & `school type`=="CHARTER" ~ "ROCHESTER", # in district counts
                              county == "KINGS" ~ paste0("NYC GEOG DISTRICT #",
                                                         substr(`state location id`, 3, 4)),
                              county == "QUEENS" ~ paste0("NYC GEOG DISTRICT #",
                                                          substr(`state location id`, 3, 4)),
                              county == "BRONX" ~ paste0("NYC GEOG DISTRICT #",
                                                          substr(`state location id`, 3, 4)),
                              county == "RICHMOND" ~ paste0("NYC GEOG DISTRICT #",
                                                          substr(`state location id`, 3, 4)),
                              county == "NEW YORK" ~ paste0("NYC GEOG DISTRICT #",
                                                            substr(`state location id`,3,4)),
                              TRUE ~ `district name`),
         id_code = `state location id`,
         school = `location name`,
         type = `school type`,
         subgroup = `subgroup name`,
         total = `pk12 total`) %>% 
  select(id_code, county, district, school, type,subgroup, total)
full_entities <- full_entities %>% 
  spread(key = subgroup, value = total) %>% # change from long to wide
  mutate_all(~replace(., is.na(.), 0)) %>% # change to zero if data is suppressed to NA
  mutate(amind = as.numeric(`American Indian/Alaska Native`),
         asian = as.numeric(`Asian/Pacific Islander`),
         black = as.numeric(`Black`),
         hisp = as.numeric(`Hispanic`),
         mult = as.numeric(`Multiracial`),
         white = as.numeric(`White`),
         total = amind + asian + black + hisp + mult + white) %>% 
  select(id_code,county, district, school, type, total, amind, asian, black, hisp, mult, white)

cumulative <- full_entities

full_entities <- full_entities %>% 
  mutate(amind = amind/total,
         asian = asian/total,
         black = black/total,
         hisp = hisp/total,
         mult = mult/total,
         white = white/total) %>% 
  select(id_code,county, district, school, type, total, amind, asian, black, hisp, mult, white)

# ----- Members with percentages of racial subgroups ------
memb_enrl <- full_entities %>% 
  filter(str_detect(school,"GENESEE")|
           str_detect(school, "ELMWOOD VILLAGE")|
           school == "ACADEMY OF THE CITY CHARTER SCHOOL"|
           str_detect(school,"PROSPECT CHARTER")|
           str_detect(school,"CENTRAL QUEENS ACADEMY")|
           school == "COMMUNITY ROOTS CHARTER SCHOOL"|
           school == "COMPASS CHARTER SCHOOL"|
           str_detect(school,"HEBREW LANGUAGE")|
           str_detect(school,"HELLENIC CLASSICAL")|
           str_detect(school,"INTERNATIONAL CHARTER")|
           str_detect(school,"FRENCH-AMER")|
           school == "BROOKLYN URBAN GARDEN CHARTER SCHOOL"|
           str_detect(school, "SUCCESS ACAD")|
           school == "LEEP DUAL LANGUAGE ACADEMY CHARTER")

# ----- Local segregation multigroup -----
locseg <- enrl %>% 
  filter(county == "ERIE"|
           county == "MONROE"|
           county == "KINGS"|
           county == "QUEENS"|
           county == "NEW YORK"|
           county == "BRONX"|
           county == "RICHMOND")%>% 
  mutate(district = case_when(str_detect(`district name`, "NYC GEOG") ~ # re-format NYC public schools
                                paste0("NYC GEOG DISTRICT #", substr(`state location id`, 3, 4)),
                              county == "ERIE" &`school type`=="CHARTER" ~ "BUFFALO", # include charter demographics
                              county == "MONROE" & `school type`=="CHARTER" ~ "ROCHESTER", # in district counts
                              county == "KINGS" ~ paste0("NYC GEOG DISTRICT #",
                                                         substr(`state location id`, 3, 4)),
                              county == "QUEENS" ~ paste0("NYC GEOG DISTRICT #",
                                                          substr(`state location id`, 3, 4)),
                              county == "BRONX" ~ paste0("NYC GEOG DISTRICT #",
                                                         substr(`state location id`, 3, 4)),
                              county == "RICHMOND" ~ paste0("NYC GEOG DISTRICT #",
                                                            substr(`state location id`, 3, 4)),
                              county == "NEW YORK" ~ paste0("NYC GEOG DISTRICT #",
                                                            substr(`state location id`,3,4)),
                              TRUE ~ `district name`),
         id_code = `state location id`,
         school = `location name`,
         type = `school type`,
         subgroup = `subgroup name`,
         total = `pk12 total`) %>% 
  group_by(county) %>% 
  select(id_code, county, district, school, type,subgroup, total)



#--- Cumulative race by county and district -----
cumulative_county <- cumulative %>% 
  filter(county == "ERIE"|
           county == "MONROE"|
           county == "KINGS"|
           county == "QUEENS"|
           county == "NEW YORK"|
           county == "BRONX"|
           county == "RICHMOND") %>% 
  group_by(county) %>% 
  summarize(county_total = sum(total),
            county_amind = sum(amind)/county_total,
            county_asian = sum(asian)/county_total,
            county_black = sum(black)/county_total,
            county_hisp = sum(hisp)/county_total,
            county_white = sum(white)/county_total,
            county_mult = sum(mult)/county_total)

memb_enrl %>% distinct(district) %>% 
  arrange(district) %>% 
  mutate(district = substr(district,18,21)) %>% View()
cumulative_district <- cumulative %>% 
  filter(district == "BUFFALO"|
           district == "ROCHESTER"|
           str_detect(district, "#02")|str_detect(district, "#03")|str_detect(district, "#04")|
           str_detect(district, "#05")|str_detect(district, "#06")|str_detect(district, "#07")|
           str_detect(district, "#08")|str_detect(district, "#09")|str_detect(district, "#13")|
           str_detect(district, "#14")|str_detect(district, "#15")|str_detect(district, "#16")|
           str_detect(district, "#17")|str_detect(district, "#21")|str_detect(district, "#22")|
           str_detect(district, "#24")|str_detect(district, "#27")|str_detect(district, "#29")|
           str_detect(district, "#30")|str_detect(district, "#31")|str_detect(district, "#32")) %>% 
  group_by(district) %>% 
  summarize(district_total = sum(total),
            district_amind = sum(amind)/district_total,
            district_asian = sum(asian)/district_total,
            district_black = sum(black)/district_total,
            district_hisp = sum(hisp)/district_total,
            district_white = sum(white)/district_total,
            district_mult = sum(mult)/district_total)

# Erie County and Buffalo
erie_locseg <- locseg %>% 
  filter(county == "ERIE") %>% 
  mutual_local("subgroup","id_code", weight = "total", wide = TRUE) %>% 
  arrange(ls) # sort from most to least representative of Erie County. Days Park ranks 44 out of 214 public and charter schools. Hertel ranks 77 out of 214.
memb_locseg <- erie_locseg %>% 
  filter(id_code == "140600860896"| # beds codes for EVCS Days Park and Hertel
           id_code == "140600861105") %>%
  mutate(school = case_when(id_code == "140600860896" ~ "ELMWOOD VILLAGE CHARTER DAYS PARK",
                            id_code == "140600861105" ~ "ELMWOOD VILLAGE CHARTER - HERTEL",
                            TRUE ~ "school"),
         county = "ERIE",
         district = "BUFFALO",
         ls_county = ls,
         p_county = p) %>% 
  select(id_code, school, district, county, ls_county, p_county)

bcsd_locseg <- locseg %>% 
  filter(district == "BUFFALO") %>% 
  mutual_local("subgroup","id_code",weight = "total", wide = TRUE) %>% 
  arrange(ls)# sort from most to least representative of Buffalo district. Days Park ranks 63 out of 79 public and charter schools. Hertel ranks 20 out of 79

temp <- bcsd_locseg %>% 
  filter(id_code == "140600860896"| # beds codes for EVCS Days Park and Hertel
           id_code == "140600861105") %>%
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(id_code, ls_district, p_district)
memb_locseg <- merge(memb_locseg,temp,by="id_code")

# Monroe County and Rochester
monroe_locseg <- locseg %>% 
  filter(county == "MONROE") %>% 
  mutual_local("subgroup","id_code", weight = "total", wide = TRUE) %>% 
  arrange(ls) # sort from most to least representative of Monroe County. GCCS ranks 6 out of 175 public and charter schools
monroe_locseg <- monroe_locseg %>% 
  filter(id_code == "261600860826") %>% #beds code for Genesee Community Charter
  mutate(school = "GENESEE COMM CHARTER SCHOOL",
         county = "MONROE",
         district = "ROCHESTER",
         ls_county = ls,
         p_county = p) %>% 
  select(id_code, school, district, county, ls_county, p_county)

rcsd_locseg <- locseg %>% 
  filter(district == "ROCHESTER") %>% 
  mutual_local("subgroup","id_code",weight = "total", wide = TRUE) %>% 
  arrange(ls)# sort from most to least representative of Rochester City schools. GCCS ranks 60 out of 60 public and charter schools

temp <- rcsd_locseg %>% 
  filter(id_code == "261600860826") %>% # beds code for Genesee Community Charter
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(id_code, ls_district, p_district)
temp <- merge(temp,monroe_locseg,by="id_code")
temp <- temp %>% 
  select(id_code, school, district, county, ls_county, p_county, ls_district, p_district)
memb_locseg <- rbind(memb_locseg,temp)

# --------Kings County, https://data.nysed.gov/profile.php?county=33 ------
# Brooklyn: NYC Districts 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 32
kings_locseg <- locseg %>% 
  filter(county == "KINGS") %>% 
  mutual_local("subgroup","school", weight = "total", wide = TRUE) %>% 
  arrange(ls) # sort from most to least representative of Kings County
temp <- kings_locseg %>% 
  filter(str_detect(school,"PROSPECT CHARTER")| # Brooklyn members
           school == "COMMUNITY ROOTS CHARTER SCHOOL"|
           school == "COMPASS CHARTER SCHOOL"|
           str_detect(school,"HEBREW LANGUAGE")|
           str_detect(school,"HELLENIC CLASSICAL")|
           str_detect(school,"INTERNATIONAL CHARTER")|
           school == "BROOKLYN URBAN GARDEN CHARTER SCHOOL"|
           str_detect(school, "SUCCESS ACAD")|
           school == "LEEP DUAL LANGUAGE ACADEMY CHARTER") %>% 
  mutate(ls_county = ls,
         p_county = p) %>% 
  select(school, ls_county, p_county)
z<- locseg %>% 
  select(school,district,id_code, county)
temp <- merge(temp,z,by="school")
temp <- unique(temp) #remove duplicate rows

d13_locseg <- locseg %>% 
  filter(district == "NYC GEOG DISTRICT #13") %>% 
  mutual_local("subgroup","school",weight = "total", wide = TRUE) %>% 
  arrange(ls)

d13 <- d13_locseg %>% 
  filter(str_detect(school,"PROSPECT CHARTER")| # Brooklyn members
           school == "COMMUNITY ROOTS CHARTER SCHOOL"|
           school == "COMPASS CHARTER SCHOOL"|
           str_detect(school,"HEBREW LANGUAGE")|
           str_detect(school,"HELLENIC CLASSICAL")|
           str_detect(school,"INTERNATIONAL CHARTER")|
           school == "BROOKLYN URBAN GARDEN CHARTER SCHOOL"|
           str_detect(school, "SUCCESS ACAD")|
           school == "LEEP DUAL LANGUAGE ACADEMY CHARTER") %>% #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
d13 <- merge(temp,d13,by="school")
d13 <- d13 %>% 
  select(id_code, school, district, county, ls_county, p_county, ls_district, p_district)

d14_locseg <- locseg %>% 
  filter(district == "NYC GEOG DISTRICT #14") %>% 
  mutual_local("subgroup","school",weight = "total", wide = TRUE) %>% 
  arrange(ls)

d14 <- d14_locseg %>% 
  filter(str_detect(school,"PROSPECT CHARTER")| # Brooklyn members
           school == "COMMUNITY ROOTS CHARTER SCHOOL"|
           school == "COMPASS CHARTER SCHOOL"|
           str_detect(school,"HEBREW LANGUAGE")|
           str_detect(school,"HELLENIC CLASSICAL")|
           str_detect(school,"INTERNATIONAL CHARTER")|
           school == "BROOKLYN URBAN GARDEN CHARTER SCHOOL"|
           str_detect(school, "SUCCESS ACAD")|
           school == "LEEP DUAL LANGUAGE ACADEMY CHARTER") %>% #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
d14 <- merge(temp,d14,by="school")
d14 <- d14 %>% 
  select(id_code, school, district, county, ls_county, p_county, ls_district, p_district)


d15_locseg <- locseg %>% 
  filter(district == "NYC GEOG DISTRICT #15") %>% 
  mutual_local("subgroup","school",weight = "total", wide = TRUE) %>% 
  arrange(ls)

d15 <- d15_locseg %>% 
  filter(str_detect(school,"PROSPECT CHARTER")| # Brooklyn members
           school == "COMMUNITY ROOTS CHARTER SCHOOL"|
           school == "COMPASS CHARTER SCHOOL"|
           str_detect(school,"HEBREW LANGUAGE")|
           str_detect(school,"HELLENIC CLASSICAL")|
           str_detect(school,"INTERNATIONAL CHARTER")|
           school == "BROOKLYN URBAN GARDEN CHARTER SCHOOL"|
           str_detect(school, "SUCCESS ACAD")|
           school == "LEEP DUAL LANGUAGE ACADEMY CHARTER") %>% #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
d15 <- merge(temp,d15,by="school")
d15 <- d15 %>% 
  select(id_code, school, district, county, ls_county, p_county, ls_district, p_district)

d16_locseg <- locseg %>% 
  filter(district == "NYC GEOG DISTRICT #16") %>% 
  mutual_local("subgroup","school",weight = "total", wide = TRUE) %>% 
  arrange(ls)

d16 <- d16_locseg %>% 
  filter(str_detect(school,"PROSPECT CHARTER")| # Brooklyn members
           school == "COMMUNITY ROOTS CHARTER SCHOOL"|
           school == "COMPASS CHARTER SCHOOL"|
           str_detect(school,"HEBREW LANGUAGE")|
           str_detect(school,"HELLENIC CLASSICAL")|
           str_detect(school,"INTERNATIONAL CHARTER")|
           school == "BROOKLYN URBAN GARDEN CHARTER SCHOOL"|
           str_detect(school, "SUCCESS ACAD")|
           school == "LEEP DUAL LANGUAGE ACADEMY CHARTER") %>% #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
d16 <- merge(temp,d16,by="school")
d16 <- d16 %>% 
  select(id_code, school, district, county, ls_county, p_county, ls_district, p_district)


d17_locseg <- locseg %>% 
  filter(district == "NYC GEOG DISTRICT #17") %>% 
  mutual_local("subgroup","school",weight = "total", wide = TRUE) %>% 
  arrange(ls)

d17 <- d17_locseg %>% 
  filter(str_detect(school,"PROSPECT CHARTER")| # Brooklyn members
           school == "COMMUNITY ROOTS CHARTER SCHOOL"|
           school == "COMPASS CHARTER SCHOOL"|
           str_detect(school,"HEBREW LANGUAGE")|
           str_detect(school,"HELLENIC CLASSICAL")|
           str_detect(school,"INTERNATIONAL CHARTER")|
           school == "BROOKLYN URBAN GARDEN CHARTER SCHOOL"|
           str_detect(school, "SUCCESS ACAD")|
           school == "LEEP DUAL LANGUAGE ACADEMY CHARTER") %>% #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
d17 <- merge(temp,d17,by="school")
d17 <- d17 %>% 
  select(id_code, school, district, county, ls_county, p_county, ls_district, p_district)


d21_locseg <- locseg %>% 
  filter(district == "NYC GEOG DISTRICT #21") %>% 
  mutual_local("subgroup","school",weight = "total", wide = TRUE) %>% 
  arrange(ls)

d21 <- d21_locseg %>% 
  filter(str_detect(school,"PROSPECT CHARTER")| # Brooklyn members
           school == "COMMUNITY ROOTS CHARTER SCHOOL"|
           school == "COMPASS CHARTER SCHOOL"|
           str_detect(school,"HEBREW LANGUAGE")|
           str_detect(school,"HELLENIC CLASSICAL")|
           str_detect(school,"INTERNATIONAL CHARTER")|
           school == "BROOKLYN URBAN GARDEN CHARTER SCHOOL"|
           str_detect(school, "SUCCESS ACAD")|
           school == "LEEP DUAL LANGUAGE ACADEMY CHARTER") %>% #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
d21 <- merge(temp,d21,by="school")
d21 <- d21 %>% 
  select(id_code, school, district, county, ls_county, p_county, ls_district, p_district)

d22_locseg <- locseg %>% 
  filter(district == "NYC GEOG DISTRICT #22") %>% 
  mutual_local("subgroup","school",weight = "total", wide = TRUE) %>% 
  arrange(ls)

d22 <- d22_locseg %>% 
  filter(str_detect(school,"PROSPECT CHARTER")| # Brooklyn members
           school == "COMMUNITY ROOTS CHARTER SCHOOL"|
           school == "COMPASS CHARTER SCHOOL"|
           str_detect(school,"HEBREW LANGUAGE")|
           str_detect(school,"HELLENIC CLASSICAL")|
           str_detect(school,"INTERNATIONAL CHARTER")|
           school == "BROOKLYN URBAN GARDEN CHARTER SCHOOL"|
           str_detect(school, "SUCCESS ACAD")|
           school == "LEEP DUAL LANGUAGE ACADEMY CHARTER") %>% #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
d22 <- merge(temp,d22,by="school")
d22 <- d22 %>% 
  select(id_code, school, district, county, ls_county, p_county, ls_district, p_district)

d32_locseg <- locseg %>% 
  filter(district == "NYC GEOG DISTRICT #32") %>% 
  mutual_local("subgroup","school",weight = "total", wide = TRUE) %>% 
  arrange(ls)

d32 <- d32_locseg %>% 
  filter(str_detect(school,"PROSPECT CHARTER")| # Brooklyn members
           school == "COMMUNITY ROOTS CHARTER SCHOOL"|
           school == "COMPASS CHARTER SCHOOL"|
           str_detect(school,"HEBREW LANGUAGE")|
           str_detect(school,"HELLENIC CLASSICAL")|
           str_detect(school,"INTERNATIONAL CHARTER")|
           school == "BROOKLYN URBAN GARDEN CHARTER SCHOOL"|
           str_detect(school, "SUCCESS ACAD")|
           school == "LEEP DUAL LANGUAGE ACADEMY CHARTER") %>% #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
d32 <- merge(temp,d32,by="school")
d32 <- d32 %>% 
  select(id_code, school, district, county, ls_county, p_county, ls_district, p_district)

unique(temp$district)
temp <- rbind(d13, d14, d15, d16, d17, 
              d21, d22, d32)
memb_locseg <- rbind(memb_locseg,temp)

# --------New York County, https://data.nysed.gov/profile.php?county=31 ------
# Manhattan: NYC Districts 1, 2, 3, 4, 5, 6
newyork_locseg <- locseg %>% 
  filter(county == "NEW YORK") %>% 
  mutual_local("subgroup","school", weight = "total", wide = TRUE) %>% 
  arrange(ls) # sort from most to least representative of New York County
temp <- newyork_locseg %>% 
  filter(str_detect(school,"HEBREW LANGUAGE")| # Manhattan based members
           str_detect(school,"FRENCH-AMER")|
           str_detect(school, "SUCCESS ACAD")) %>% 
  mutate(ls_county = ls,
         p_county = p) %>% 
  select(school, ls_county, p_county)
z<- locseg %>% 
  select(school,district,id_code, county)
temp <- merge(temp,z,by="school")
temp <- unique(temp) #remove duplicate rows

d02_locseg <- locseg %>% 
  filter(district == "NYC GEOG DISTRICT #02") %>% 
  mutual_local("subgroup","school",weight = "total", wide = TRUE) %>% 
  arrange(ls)
d02 <- d02_locseg %>% 
  filter(str_detect(school,"HEBREW LANGUAGE")| # Manhattan based members
           str_detect(school,"FRENCH-AMER")|
           str_detect(school, "SUCCESS ACAD")) %>%  #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
d02 <- merge(temp,d02,by="school")
d02 <- d02 %>% 
  select(id_code, school, district, county, ls_county, p_county, ls_district, p_district)

d03_locseg <- locseg %>% 
  filter(district == "NYC GEOG DISTRICT #03") %>% 
  mutual_local("subgroup","school",weight = "total", wide = TRUE) %>% 
  arrange(ls)
d03 <- d03_locseg %>% 
  filter(str_detect(school,"HEBREW LANGUAGE")| # Manhattan based members
           str_detect(school,"FRENCH-AMER")|
           str_detect(school, "SUCCESS ACAD")) %>%  #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
d03 <- merge(temp,d03,by="school")
d03 <- d03 %>% 
  select(id_code, school, district, county, ls_county, p_county, ls_district, p_district)

d04_locseg <- locseg %>% 
  filter(district == "NYC GEOG DISTRICT #04") %>% 
  mutual_local("subgroup","school",weight = "total", wide = TRUE) %>% 
  arrange(ls)
d04 <- d04_locseg %>% 
  filter(str_detect(school,"HEBREW LANGUAGE")| # Manhattan based members
           str_detect(school,"FRENCH-AMER")|
           str_detect(school, "SUCCESS ACAD")) %>%  #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
d04 <- merge(temp,d04,by="school")
d04 <- d04 %>% 
  select(id_code, school, district, county, ls_county, p_county, ls_district, p_district)

d05_locseg <- locseg %>% 
  filter(district == "NYC GEOG DISTRICT #05") %>% 
  mutual_local("subgroup","school",weight = "total", wide = TRUE) %>% 
  arrange(ls)
d05 <- d05_locseg %>% 
  filter(str_detect(school,"HEBREW LANGUAGE")| # Manhattan based members
           str_detect(school,"FRENCH-AMER")|
           str_detect(school, "SUCCESS ACAD")) %>%  #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
d05 <- merge(temp,d05,by="school")
d05 <- d05 %>% 
  select(id_code, school, district, county, ls_county, p_county, ls_district, p_district)

d06_locseg <- locseg %>% 
  filter(district == "NYC GEOG DISTRICT #06") %>% 
  mutual_local("subgroup","school",weight = "total", wide = TRUE) %>% 
  arrange(ls)
d06 <- d06_locseg %>% 
  filter(str_detect(school,"HEBREW LANGUAGE")| # Manhattan based members
           str_detect(school,"FRENCH-AMER")|
           str_detect(school, "SUCCESS ACAD")) %>%  #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
d06 <- merge(temp,d06,by="school")
d06 <- d06 %>% 
  select(id_code, school, district, county, ls_county, p_county, ls_district, p_district)

unique(temp$district)
temp <- rbind(d02, d03, d04, d05, d06)
memb_locseg <- rbind(memb_locseg,temp)

# --------Bronx County, https://data.nysed.gov/profile.php?county=32 ------
# Bronx: NYC Districts 7, 8, 9, 10, 11, 12
bronx_locseg <- locseg %>% 
  filter(county == "BRONX") %>% 
  mutual_local("subgroup","school", weight = "total", wide = TRUE) %>% 
  arrange(ls) # sort from most to least representative of Bronx County
temp <- bronx_locseg %>% 
  filter(str_detect(school, "SUCCESS ACAD")) %>% 
  mutate(ls_county = ls,
         p_county = p) %>% 
  select(school, ls_county, p_county)
z<- locseg %>% 
  select(school,district,id_code, county)
temp <- merge(temp,z,by="school")
temp <- unique(temp) #remove duplicate rows

d07_locseg <- locseg %>% 
  filter(district == "NYC GEOG DISTRICT #07") %>% 
  mutual_local("subgroup","school",weight = "total", wide = TRUE) %>% 
  arrange(ls)
d07 <- d07_locseg %>% 
  filter(str_detect(school, "SUCCESS ACAD")) %>%  #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
d07 <- merge(temp,d07,by="school")
d07 <- d07 %>% 
  select(id_code, school, district, county, ls_county, p_county, ls_district, p_district)

d08_locseg <- locseg %>% 
  filter(district == "NYC GEOG DISTRICT #08") %>% 
  mutual_local("subgroup","school",weight = "total", wide = TRUE) %>% 
  arrange(ls)
d08 <- d08_locseg %>% 
  filter(str_detect(school, "SUCCESS ACAD")) %>%  #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
d08 <- merge(temp,d08,by="school")
d08 <- d08 %>% 
  select(id_code, school, district, county, ls_county, p_county, ls_district, p_district)

d09_locseg <- locseg %>% 
  filter(district == "NYC GEOG DISTRICT #09") %>% 
  mutual_local("subgroup","school",weight = "total", wide = TRUE) %>% 
  arrange(ls)
d09 <- d09_locseg %>% 
  filter(str_detect(school, "SUCCESS ACAD")) %>%  #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
d09 <- merge(temp,d09,by="school")
d09 <- d09 %>% 
  select(id_code, school, district, county, ls_county, p_county, ls_district, p_district)

unique(temp$district)
temp <- rbind(d07, d08, d09)
memb_locseg <- rbind(memb_locseg,temp)


# --------Queens County, https://data.nysed.gov/profile.php?county=34 ------
# Queens: NYC Districts 24, 25, 26, 27, 28, 29,30
queens_locseg <- locseg %>% 
  filter(county == "QUEENS") %>% 
  mutual_local("subgroup","school", weight = "total", wide = TRUE) %>% 
  arrange(ls) # sort from most to least representative of Queens County
temp <- queens_locseg %>% 
  filter(str_detect(school, "SUCCESS ACAD")|
           str_detect(school, "CENTRAL QUEENS ACADEMY")|
           str_detect(school, "ACADEMY OF THE CITY")) %>% 
  mutate(ls_county = ls,
         p_county = p) %>% 
  select(school, ls_county, p_county)
z<- locseg %>% 
  select(school,district,id_code, county)
temp <- merge(temp,z,by="school")
temp <- unique(temp) #remove duplicate rows


d24_locseg <- locseg %>% 
  filter(district == "NYC GEOG DISTRICT #24") %>% 
  mutual_local("subgroup","school",weight = "total", wide = TRUE) %>% 
  arrange(ls)
d24 <- d24_locseg %>% 
  filter(str_detect(school, "SUCCESS ACAD")|
           str_detect(school, "CENTRAL QUEENS ACADEMY")|
           str_detect(school, "ACADEMY OF THE CITY")) %>%  #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
d24 <- merge(temp,d24,by="school")
d24 <- d24 %>% 
  select(id_code, school, district, county, ls_county, p_county, ls_district, p_district)

d27_locseg <- locseg %>% 
  filter(district == "NYC GEOG DISTRICT #27") %>% 
  mutual_local("subgroup","school",weight = "total", wide = TRUE) %>% 
  arrange(ls)
d27 <- d27_locseg %>% 
  filter(str_detect(school, "SUCCESS ACAD")|
           str_detect(school, "CENTRAL QUEENS ACADEMY")|
           str_detect(school, "ACADEMY OF THE CITY")) %>%  #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
d27 <- merge(temp,d27,by="school")
d27 <- d27 %>% 
  select(id_code, school, district, county, ls_county, p_county, ls_district, p_district)

d29_locseg <- locseg %>% 
  filter(district == "NYC GEOG DISTRICT #29") %>% 
  mutual_local("subgroup","school",weight = "total", wide = TRUE) %>% 
  arrange(ls)
d29 <- d29_locseg %>% 
  filter(str_detect(school, "SUCCESS ACAD")|
           str_detect(school, "CENTRAL QUEENS ACADEMY")|
           str_detect(school, "ACADEMY OF THE CITY")) %>%  #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
d29 <- merge(temp,d29,by="school")
d29 <- d29 %>% 
  select(id_code, school, district, county, ls_county, p_county, ls_district, p_district)

d30_locseg <- locseg %>% 
  filter(district == "NYC GEOG DISTRICT #30") %>% 
  mutual_local("subgroup","school",weight = "total", wide = TRUE) %>% 
  arrange(ls)
d30 <- d30_locseg %>% 
  filter(str_detect(school, "SUCCESS ACAD")|
           str_detect(school, "CENTRAL QUEENS ACADEMY")|
           str_detect(school, "ACADEMY OF THE CITY")) %>%  #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
d30 <- merge(temp,d30,by="school")
d30 <- d30 %>% 
  select(id_code, school, district, county, ls_county, p_county, ls_district, p_district)

unique(temp$district)
temp <- rbind(d24, d27, d29, d30)
memb_locseg <- rbind(memb_locseg,temp)

# --------Richmond County, https://data.nysed.gov/profile.php?county=35 ------
# Staten Island: NYC Districts 31
staten_locseg <- locseg %>% 
  filter(county == "RICHMOND") %>% 
  mutual_local("subgroup","school", weight = "total", wide = TRUE) %>% 
  arrange(ls) # sort from most to least representative of Queens County
temp <- staten_locseg %>% 
  filter(str_detect(school, "HELLENIC")) %>% 
  mutate(ls_county = ls,
         p_county = p) %>% 
  select(school, ls_county, p_county)
z<- locseg %>% 
  select(school,district,id_code, county)
temp <- merge(temp,z,by="school")
temp <- unique(temp) #remove duplicate rows


d31_locseg <- locseg %>% 
  filter(district == "NYC GEOG DISTRICT #31") %>% 
  mutual_local("subgroup","school",weight = "total", wide = TRUE) %>% 
  arrange(ls)
d31 <- d31_locseg %>% 
  filter(str_detect(school, "HELLENIC")) %>%  #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
d31 <- merge(temp,d31,by="school")
d31 <- d31 %>% 
  select(id_code, school, district, county, ls_county, p_county, ls_district, p_district)


memb_locseg <- rbind(memb_locseg,d31)
write.csv(memb_locseg, file = file.path('output data/ny_locseg.csv'),row.names = FALSE)

#--- ELL, SPED, ECON DIS ----
# Downloaded from http://www.p12.nysed.gov/irs/statistics/enroll-n-staff/home.html, posted 9/16/2021
# File URLs http://www.p12.nysed.gov/irs/statistics/enroll-n-staff/PublicSchool2021EconDisadv.xlsx
# http://www.p12.nysed.gov/irs/statistics/enroll-n-staff/PublicSchool2021LEP.xlsx
# http://www.p12.nysed.gov/irs/statistics/enroll-n-staff/PublicSchool2021SWD.xlsx

econdis <- read_excel("raw data/NY_PublicSchool2021EconDisadv.xlsx")
names(econdis) <- tolower(names(econdis))
econdis <- econdis[,1:11]

econ_entities <- econdis %>% 
  filter(county == "ERIE"|
           county == "MONROE"|
           county == "KINGS"|
           county == "QUEENS"|
           county == "NEW YORK"|
           county == "BRONX"|
           county == "RICHMOND")%>% 
  mutate(id_code = `state location id`,
         subgroup = `subgroup name`,
         econdis = `pk12 total`) %>% 
  filter(subgroup == "Economically Disadvantaged") %>% 
  select(id_code,econdis)
memb_enrl <- merge(memb_enrl,econ_entities,by=c("id_code"))
memb_enrl <- memb_enrl %>% 
  mutate(econdis = as.numeric(econdis)/total)

ell <- read_excel("raw data/NY_PublicSchool2021LEP.xlsx")
names(ell) <- tolower(names(ell))
ell <- ell[,1:11]

ell_entities <- ell %>% 
  filter(county == "ERIE"|
           county == "MONROE"|
           county == "KINGS"|
           county == "QUEENS"|
           county == "NEW YORK"|
           county == "BRONX"|
           county == "RICHMOND")%>% 
  mutate(id_code = `state location id`,
         subgroup = `subgroup name`,
         ell = `pk12 total`) %>% 
  filter(subgroup == "Not English Language Learner") %>% 
  select(id_code, ell)
z <- merge(memb_enrl,ell_entities,by=c("id_code"))
memb_enrl <- z %>% 
  mutate(ell = (total - as.numeric(ell))/total)

swd <- read_excel("raw data/NY_PublicSchool2021SWD.xlsx")
names(swd) <- tolower(names(swd))
swd <- swd[,1:11]

swd_entities <- swd %>% 
  filter(county == "ERIE"|
           county == "MONROE"|
           county == "KINGS"|
           county == "QUEENS"|
           county == "NEW YORK"|
           county == "BRONX"|
           county == "RICHMOND")%>% 
  mutate(id_code = `state location id`,
         subgroup = `subgroup name`,
         swd = `pk12 total`) %>% 
  filter(subgroup == "Students with Disabilities") %>% 
  select(id_code, swd)
memb_enrl <- merge(memb_enrl,swd_entities,by=c("id_code"))
memb_enrl <- memb_enrl %>% 
  mutate(swd = as.numeric(swd)/total)

full_entities <- merge(econ_entities,full_entities,by=c("id_code"))
full_entities <- merge(swd_entities,full_entities,by=c("id_code"))
full_entities <- merge(ell_entities,full_entities,by=c("id_code"))

# - Cumulative ELL SPED ECONDIS by county and district ----
full_entities <- full_entities %>% 
  mutate(swd = as.numeric(swd)/total,
         ell = (total - as.numeric(ell))/total,
         econdis = as.numeric(econdis)/total) %>% na.omit() 
  select(id_code, county, district, school, type, total, amind, asian, black, hisp, mult, white, econdis,ell, swd)

a<- ell %>% 
  filter(county == "ERIE"|
           county == "MONROE"|
           county == "KINGS"|
           county == "QUEENS"|
           county == "NEW YORK"|
           county == "BRONX"|
           county == "RICHMOND",
         `subgroup name` == "Not English Language Learner")%>% 
  mutate(id_code = `state location id`,
         ell = as.numeric(`pk12 total`)) %>%
  group_by(county) %>% 
  summarize(county_ell = sum(ell))

b <- swd %>% 
  filter(county == "ERIE"|
           county == "MONROE"|
           county == "KINGS"|
           county == "QUEENS"|
           county == "NEW YORK"|
           county == "BRONX"|
           county == "RICHMOND",
         `subgroup name` == "Students with Disabilities")%>% 
  mutate(swd = as.numeric(`pk12 total`)) %>% na.omit() %>% 
  group_by(county) %>%
  summarize(county_swd = sum(swd))

c <- econdis %>% 
  filter(county == "ERIE"|
           county == "MONROE"|
           county == "KINGS"|
           county == "QUEENS"|
           county == "NEW YORK"|
           county == "BRONX"|
           county == "RICHMOND",
         `subgroup name` == "Economically Disadvantaged")%>%
  mutate(econdis = as.numeric(`pk12 total`)) %>% na.omit() %>% 
  group_by(county) %>% 
  summarize(county_econ = sum(econdis))

c<- merge(c,a,by="county")
c<- merge(c,b,by="county")
cumulative_county <- merge(cumulative_county,c,by="county")
cumulative_county <- cumulative_county %>% 
  mutate(county_econ = county_econ/county_total,
         county_ell = (county_total - county_ell)/county_total,
         county_swd = county_swd/county_total)

memb_enrl <- merge(memb_enrl,cumulative_county,by="county")

h <- full_entities %>% select(id_code,county,district)
ell_entities <- merge(h, ell_entities,by="id_code")
a <- ell_entities %>% 
  filter(county == "ERIE"|
           county == "MONROE"|
           county == "KINGS"|
           county == "QUEENS"|
           county == "NEW YORK"|
           county == "BRONX"|
           county == "RICHMOND")%>% 
  mutate(ell = as.numeric(ell)) %>% na.omit() %>% 
  group_by(district) %>% 
  summarize(district_ell = sum(ell))

swd_entities <- merge(h, swd_entities,by="id_code")
b <- swd_entities %>% 
  filter(county == "ERIE"|
           county == "MONROE"|
           county == "KINGS"|
           county == "QUEENS"|
           county == "NEW YORK"|
           county == "BRONX"|
           county == "RICHMOND")%>% 
  mutate(swd = as.numeric(swd)) %>% na.omit() %>% 
  group_by(district) %>% 
  summarize(district_swd = sum(swd))

econ_entities <- merge(h, econ_entities,by="id_code")
c <- econ_entities %>% 
  filter(county == "ERIE"|
           county == "MONROE"|
           county == "KINGS"|
           county == "QUEENS"|
           county == "NEW YORK"|
           county == "BRONX"|
           county == "RICHMOND")%>%
  mutate(econdis = as.numeric(econdis)) %>% na.omit() %>% 
  group_by(district) %>% 
  summarize(district_econ = sum(econdis))

c<- merge(c,a,by="district")
c<- merge(c,b,by="district")
cumulative_district <- merge(cumulative_district,c,by="district")
cumulative_district <- cumulative_district %>% 
  mutate(district_econ = district_econ/district_total,
         district_ell = (district_total - district_ell)/district_total,
         district_swd = district_swd/district_total)
  
memb_enrl <- merge(memb_enrl,cumulative_district,by="district")

write.csv(memb_enrl, file = file.path('output data/ny_enrl.csv'),row.names =FALSE)

# --- academic data from 2021 ----
# Data from https://data.nysed.gov/files/assessment/20-21/3-8-2020-21.zip 
nysed <- read_excel("raw data/NY_3-8-2020-21/3-8_ELA_AND_MATH_RESEARCHER_FILE_2021.xlsx")
View(nysed)
names(nysed) <- tolower(names(nysed))
str(nysed)
names(nysed)[3] <- "school"
names(nysed)[2] <- "id_code"
nysed$school <- as.factor(nysed$school)
levels(nysed$school)[981:982] <- c("ELMWOOD VILLAGE CHARTER SCHOOL","ELMWOOD VILLAGE CHARTER - HERTEL")
# change to match the other naming convention for EVCS

entities <- full_entities %>% 
  select(id_code, district, county, type)

nysed <- merge(nysed, entities, by="id_code") # includes school, district, and county for all schools in counties that include DCSC members
a <- nysed %>% 
  mutate(subgroup = as.factor(subgroup_name),
         n_tested = as.numeric(total_tested),
         n_untested = as.numeric(total_not_tested),
         level_1 = as.numeric(l1_count),
         level_2 = as.numeric(l2_count),
         level_3 = as.numeric(l3_count),
         level_4 = as.numeric(l4_count),
         mean_scale_score = as.numeric(mean_scale_score)) %>% 
  select(school,id_code,county,district,type, item_desc,subgroup_code, subgroup,n_tested,n_untested,
        level_1,level_2,level_3,level_4, mean_scale_score) %>%
  arrange(subgroup_code,subgroup)
groups <- a[7:8]
groups <- distinct(groups)
View(groups) # Match codes to specific subgroups, so I don't have to type them all out by full subgroup name
nysed <- a %>% 
  filter(subgroup_code=="01"|
           subgroup_code== "04"|
           subgroup_code== "05"|
           subgroup_code== "06"|
           subgroup_code== "07"|
           subgroup_code== "08"|
           subgroup_code== "09"|
           subgroup_code== "15"|
           subgroup_code== "16" # Relevant subgroups to look at differential performance
         )  %>% # only include rows with tested students
  separate(item_desc, c("drop","grade","subject"),sep=" ") %>% # separate columns for grade and subject
  mutate(c_lev34 = level_3 + level_4,
         p_lev34 = c_lev34/n_tested) %>% 
  select(school,id_code,county,district,type, grade, subject,subgroup_code, subgroup,n_tested,n_untested,
         mean_scale_score,c_lev34,p_lev34)

library(plyr)
nysed <- ddply(nysed, .(school, subject, subgroup),summarize,
               n_tested = sum(n_tested), # sum tested numbers across all grades
               n_untested = sum(n_untested), 
               min_grade = min(grade), # make columns for the grade range
               max_grade = max(grade),
               c_lev34 = sum(c_lev34)) # total number of students testing into levels 3 and 4
  
# ???

nysed_memb <- nysed %>% 
  mutate(p_lev34 = c_lev34/n_tested) %>% 
  filter(str_detect(school,"GENESEE")|
           str_detect(school, "ELMWOOD VILLAGE")|
           school == "ACADEMY OF THE CITY CHARTER SCHOOL"|
           str_detect(school,"PROSPECT CHARTER")|
           str_detect(school,"CENTRAL QUEENS ACADEMY")|
           school == "COMMUNITY ROOTS CHARTER SCHOOL"|
           school == "COMPASS CHARTER SCHOOL"|
           str_detect(school,"HEBREW LANGUAGE")|
           str_detect(school,"HELLENIC CLASSICAL")|
           str_detect(school,"INTERNATIONAL CHARTER")|
           str_detect(school,"FRENCH-AMER")|
           school == "BROOKLYN URBAN GARDEN CHARTER SCHOOL"|
           str_detect(school, "SUCCESS ACAD")|
           school == "LEEP DUAL LANGUAGE ACADEMY CHARTER") %>% 
  select(school, subject, subgroup, n_tested, min_grade, max_grade, c_lev34) %>% 
  spread(key = subgroup, value = n_tested) # change from long to wide
  
str(nysed)

c <- ddply(nys_memb, .(school_name),summarize, # summarize by just the school
           n_students = sum(n_tested), # column for total students tested across all grades
           min_grade = min(grade), # make columns for the grade range
           max_grade = max(grade),
           z_wgt = weighted.mean(z_score, n_tested)) # calculate z score based on n_tested of both subjects
c$subject <- "both math and ELA"
nys_memb <- rbind(a,c) %>% arrange(school_name, subject)
nysed<- nysed %>% 
  mutate(school_mean = as.numeric(mean_scale_score.x),
         nys_mean = as.numeric(mean_scale_score.y),
         z_score = (school_mean - nys_mean)/sd) %>% 
  select(school_name,grade,subject,n_tested,school_mean,nys_mean,sd,z_score) %>%
  arrange(school_name,grade,subject) 

x <- ddply(nysed, .(school, subject), summarize, # summarize by school and subject
           n_students = sum(n_tested), # column for total students tested across all grades
           min_grade = min(grade), # make columns for the grade range
           max_grade = max(grade),
           z_wgt = weighted.mean(z_score,n_tested))  # calculate z score based on n_tested of each subject
str(nysed)

  mutate(n_tested = total_tested,
         c_lev34 = as.numeric(l3_count)+as.numeric(l4_count),
         p_lev34 = as.numeric(`l3-l4_pct`),
         subject = as.factor(item_subject_area)) %>% 
  select(school_name, boces_name, subject, n_tested, #remove item_desc in order to sum up all grades
         c_lev34, p_lev34)
levels(nys_acad$subject) <- c("ela","math")
