# --- Amy Jiravisitcul. 26 Jan 2022 ----
rm(list = ls()) # clear working environment
setwd("~/Downloads/States from Fall 2021/")
install.packages("tidyverse") # install tidyverse
library(tidyverse)
install.packages("readxl") # CRAN version
library(readxl)
install.packages("ggplot2")
library(ggplot2)
install.packages("data.table")
library(data.table)
install.packages('segregation')
library(segregation)
getwd()
# data import from https://apps.dese.mo.gov/MCDS/FileDownloadWebHandler.ashx?filename=e3b40c5a-cc9bBuilding%20Demographic%20Data%202006%20to%20Current.xlsx

# ---- enrollment ----
enrl <- read_excel("raw data/MO_Building Demographic Data 2006 to Current.xlsx", sheet = "Sheet1")
names(enrl) <- tolower(names(enrl))
names(enrl)[c(2:5,8,10:29,32)] <- c("ctydist_code","district", "school_code","school",
                                 "total","c_frpl","p_frpl","c_asian","p_asian","c_api","p_api",
                                 "c_black","p_black","c_hisp","p_hisp","c_amind","p_amind","c_mult",
                                 "p_mult","c_pi","p_pi","c_white","p_white","c_ell","p_ell","c_swd")
levels(as.factor(enrl$year))
str(enrl)
mo_enrl <- enrl %>% 
  filter(year == "2021",
         district == "ST. LOUIS CITY"|
           district == "KANSAS CITY 33"|
           school == "CITY GARDEN MONTESSORI SCHOOL"|
           district == "CROSSROADS CHARTER SCHOOLS"|
           school == "LAFAYETTE PREPARATORY ACADEMY"|
           district == "ACADEMIE LAFAYETTE"|
           district == "CITIZENS OF THE WORLD CHARTER"|
           school == "KAIROS ACADEMIES",
         as.numeric(total)>0) %>% 
  mutate(total = as.numeric(total),
         c_amind = case_when(is.na(as.numeric(c_amind)) ~ 0, # asterisks for data suppression. 
                             TRUE ~ as.numeric(c_amind)), # replace with zeros
         c_pi = case_when(is.na(as.numeric(c_pi)) ~ 0, # asterisks for data suppression. 
                          TRUE ~ as.numeric(c_pi)), # replace with zeros
         c_asiandrop = case_when(is.na(as.numeric(c_asian)) ~ 0, # asterisks for data suppression. 
                             TRUE ~ as.numeric(c_asian)), # replace with zeros
         c_asian = c_pi + c_asiandrop,
         c_black = case_when(is.na(as.numeric(c_black)) ~ 0, # asterisks for data suppression. 
                             TRUE ~ as.numeric(c_black)), # replace with zeros,
         c_hisp = case_when(is.na(as.numeric(c_hisp)) ~ 0, # asterisks for data suppression. 
                            TRUE ~ as.numeric(c_hisp)), # replace with zeros,
         c_white = case_when(is.na(as.numeric(c_white)) ~ 0, # asterisks for data suppression. 
                             TRUE ~ as.numeric(c_white)), # replace with zeros,
         c_mult = case_when(is.na(as.numeric(c_mult)) ~ 0, # asterisks for data suppression. 
                            TRUE ~ as.numeric(c_mult)), # replace with zeros,
         c_frpl = case_when(is.na(as.numeric(c_frpl)) ~ 0, # asterisks for data suppression. 
                            TRUE ~ as.numeric(c_frpl)), # replace with zeros,
         c_ell = case_when(is.na(as.numeric(c_ell)) ~ 0, # asterisks for data suppression. 
                           TRUE ~ as.numeric(c_ell)), # replace with zeros,
         c_swd = case_when(is.na(as.numeric(c_swd)) ~ 0, # asterisks for data suppression. 
                           TRUE ~ as.numeric(c_swd)), # replace with zeros,
         p_amind = c_amind/total,
         p_asian = c_asian/total,
         p_black = c_black/total,
         p_hisp = c_hisp/total,
         p_white = c_white/total,
         p_mult = c_mult/total,
         p_frpl = c_frpl/total,
         p_ell = c_ell/total,
         p_swd = c_swd/total) %>% 
  select(district, school, total, p_amind, p_asian, p_black, p_hisp, p_white, p_mult, p_frpl, p_ell, p_swd,
         c_amind, c_asian, c_black, c_hisp, c_white, c_mult, c_frpl, c_ell, c_swd) %>% 
  arrange(school)

# Map for reference: https://mogov.maps.arcgis.com/apps/webappviewer/index.html?id=42394a81012c4737a705d8e943b6fda5
mo_enrl[3:5,1] <- "KANSAS CITY 33" # Add comparison districts based on location
mo_enrl[(mo_enrl$school == "CITY GARDEN MONTESSORI SCHOOL"),1] <- "ST. LOUIS CITY"
mo_enrl[str_detect(mo_enrl$school, "CROSSROADS"),1] <- "KANSAS CITY 33"
mo_enrl[(mo_enrl$school == "KAIROS ACADEMIES"),1] <- "ST. LOUIS CITY"
mo_enrl[(mo_enrl$school == "LAFAYETTE PREPARATORY ACADEMY"),1] <- "ST. LOUIS CITY"
mo_enrl[(mo_enrl$school == "MIDDLE SCHOOL CAMPUS"),1] <- "KANSAS CITY 33"
mo_enrl[(mo_enrl$school == "PRIMARY GRADES CAMPUS"),1] <- "KANSAS CITY 33"

memb_enrl <- mo_enrl %>% 
  filter(school == "CITY GARDEN MONTESSORI SCHOOL"|
           school == "LAFAYETTE PREPARATORY ACADEMY"|
           school == "PRIMARY GRADES CAMPUS"|
           school == "MIDDLE SCHOOL CAMPUS"|
           school == "LAFAYETTE PREPARATORY ACADEMY"|
           school == "KAIROS ACADEMIES"|
           str_detect(school, "ACADEMIE LAFAYETTE")|
           str_detect(school, "Academie Lafayette")|
           str_detect(school,"CROSSROADS")) %>% 
  arrange(school)


write.csv(memb_enrl,file = file.path('output data/mo_enrl.csv'),row.names = FALSE)

# --- DISTRICT-LEVEL SEGREGATION ------
names(mo_enrl)
locseg <- mo_enrl %>% 
  select(school, district, c_amind, c_asian, c_black, c_hisp, c_white,c_mult) %>% 
  gather(subgroup, n, c_amind:c_mult, factor_key=TRUE) # change from wide to long

memb_enrl$school
kc <- locseg %>% 
  filter(district == "KANSAS CITY 33") %>% 
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls)
memb_locseg <- kc %>% 
  filter(school == "CITY GARDEN MONTESSORI SCHOOL"|
           school == "LAFAYETTE PREPARATORY ACADEMY"|
           str_detect(school, "CROSSROADS")|
           str_detect(school, "ACADEMIE LAFAYETTE")|
           str_detect(school,"Academie Lafayette")|
           school == "KAIROS ACADEMIES"|
           school == "MIDDLE SCHOOL CAMPUS"|
           school == "PRIMARY GRADES CAMPUS") %>% 
  mutate(ls_district = ls,
         p_district = p,
         district = "KANSAS CITY 33") %>% 
  select(school, district, ls_district, p_district)

stl <- locseg %>% 
  filter(district == "ST. LOUIS CITY") %>% 
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls)
g <- stl %>% 
  filter(school == "CITY GARDEN MONTESSORI SCHOOL"|
           school == "LAFAYETTE PREPARATORY ACADEMY"|
           str_detect(school, "CROSSROADS")|
           str_detect(school, "ACADEMIE LAFAYETTE")|
           str_detect(school,"Academie Lafayette")|
           school == "KAIROS ACADEMIES"|
           school == "MIDDLE SCHOOL CAMPUS"|
           school == "PRIMARY GRADES CAMPUS") %>% 
  mutate(ls_district = ls,
         p_district = p,
         district = "ST. LOUIS CITY") %>% 
  select(school, district, ls_district, p_district)
memb_locseg <- rbind(g, memb_locseg) %>%arrange(school)

# ------  COUNTY-LEVEL SEGREGATION ------ 
# KC: Jackson County -----
## Includes: Blue Springs R-IV, Center 58, For Osage R-I, Grain Valley R-V, Grandview C-4, Hickman Mills C-1,
## Independence 30, Lee's Summit R-VII, Lone Jack C-6, Oak Grove R-VI, Raytown C-2
jacksoncounty <- enrl %>% 
  filter(year == "2021",
         str_detect(district, "BLUE SPRINGS")|
           str_detect(district, "CENTER 58")|
           str_detect(district, "FORT OSAGE")|
           str_detect(district, "GRAIN VALLEY")|
           str_detect(district, "GRANDVIEW")|
           str_detect(district, "HICKMAN")|
           str_detect(district, "INDEPENDENCE")|
           str_detect(district, "LEE'S")|
           str_detect(district, "LONE")|
           str_detect(district, "OAK GROVE")|
           str_detect(district, "RAYTOWN")|
           str_detect(district,"KANSAS CITY 33")|
           str_detect(school, "ACADEMIE LAFAYETTE")|
           str_detect(school,"Academie Lafayette")|
           school == "MIDDLE SCHOOL CAMPUS"|
           school == "PRIMARY GRADES CAMPUS"|
           school == "CROSSROADS - CENTRAL STREET"|
           school == "CROSSROADS - QUALITY HILL"|
           school == "CROSSROADS PREPARATORY ACADEMY",
         as.numeric(total)>0) %>% 
  mutate(total = as.numeric(total),
         c_amind = case_when(is.na(as.numeric(c_amind)) ~ 0, # asterisks for data suppression. 
                             TRUE ~ as.numeric(c_amind)), # replace with zeros
         c_pi = case_when(is.na(as.numeric(c_pi)) ~ 0, # asterisks for data suppression. 
                          TRUE ~ as.numeric(c_pi)), # replace with zeros
         c_asiandrop = case_when(is.na(as.numeric(c_asian)) ~ 0, # asterisks for data suppression. 
                                 TRUE ~ as.numeric(c_asian)), # replace with zeros
         c_asian = c_pi + c_asiandrop,
         c_black = case_when(is.na(as.numeric(c_black)) ~ 0, # asterisks for data suppression. 
                             TRUE ~ as.numeric(c_black)), # replace with zeros,
         c_hisp = case_when(is.na(as.numeric(c_hisp)) ~ 0, # asterisks for data suppression. 
                            TRUE ~ as.numeric(c_hisp)), # replace with zeros,
         c_white = case_when(is.na(as.numeric(c_white)) ~ 0, # asterisks for data suppression. 
                             TRUE ~ as.numeric(c_white)), # replace with zeros,
         c_mult = case_when(is.na(as.numeric(c_mult)) ~ 0, # asterisks for data suppression. 
                            TRUE ~ as.numeric(c_mult)), # replace with zeros,
         c_frpl = case_when(is.na(as.numeric(c_frpl)) ~ 0, # asterisks for data suppression. 
                            TRUE ~ as.numeric(c_frpl)), # replace with zeros,
         c_ell = case_when(is.na(as.numeric(c_ell)) ~ 0, # asterisks for data suppression. 
                           TRUE ~ as.numeric(c_ell)), # replace with zeros,
         c_swd = case_when(is.na(as.numeric(c_swd)) ~ 0, # asterisks for data suppression. 
                           TRUE ~ as.numeric(c_swd)), # replace with zeros,
         p_amind = c_amind/total,
         p_asian = c_asian/total,
         p_black = c_black/total,
         p_hisp = c_hisp/total,
         p_white = c_white/total,
         p_mult = c_mult/total,
         p_frpl = c_frpl/total,
         p_ell = c_ell/total,
         p_swd = c_swd/total) %>% 
  select(district, school, total, p_amind, p_asian, p_black, p_hisp, p_white, p_mult, p_frpl, p_ell, p_swd,
         c_amind, c_asian, c_black, c_hisp, c_white, c_mult, c_frpl, c_ell, c_swd) %>% 
  arrange(school)

jacksoncounty[3:5,1] <- "KANSAS CITY 33" # Add comparison districts based on location
jacksoncounty[str_detect(jacksoncounty$school, "CROSSROADS"),1] <- "KANSAS CITY 33"
jacksoncounty[(jacksoncounty$school == "MIDDLE SCHOOL CAMPUS"),1] <- "KANSAS CITY 33"
jacksoncounty[(jacksoncounty$school == "PRIMARY GRADES CAMPUS"),1] <- "KANSAS CITY 33"

jacksoncounty <- jacksoncounty %>% 
  select(school, district, c_amind, c_asian, c_black, c_hisp, c_white,c_mult) %>% 
  gather(subgroup, n, c_amind:c_mult, factor_key=TRUE) %>% # change from wide to long 
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls)
cty_locseg <- jacksoncounty %>% 
  filter(school == "CITY GARDEN MONTESSORI SCHOOL"|
           school == "LAFAYETTE PREPARATORY ACADEMY"|
           str_detect(school, "CROSSROADS")|
           str_detect(school, "ACADEMIE LAFAYETTE")|
           str_detect(school,"Academie Lafayette")|
           school == "KAIROS ACADEMIES"|
           school == "MIDDLE SCHOOL CAMPUS"|
           school == "PRIMARY GRADES CAMPUS") %>% 
  mutate(ls_county = ls,
         p_county = p,
         county = "JACKSON COUNTY") %>% 
  select(school, county, ls_county, p_county)

# STL County ------
## Includes: Affton 101, Bayless, Brentwood, Clayton, Ferguson-Florissant R-II, Hancock Place, Hazelwood,
## Jennings, Kirkwood R-VII, Ladue, Lindbergh Schools, Maplewood-Richmond Heights, Mehlville R-IX, 
## Normandy Schools Collaborative, Parkway C-2, Pattonville R-III, Ritenour, Riverview Gardens, 
## Rockwood R-VI, University City, Valley Park, Webster Groves, St. Louis City
# Based on map https://data-stlcogis.opendata.arcgis.com/documents/stlcogis::st-louis-county-school-districts/explore 
stlcounty <- enrl %>% 
  filter(year == "2021",
         str_detect(district, "AFFTON 101")|
           str_detect(district, "BAYLESS")|
           str_detect(district, "BRENTWOOD")|
           str_detect(district, "CLAYTON")|
           str_detect(district, "FERGUSON")|
           str_detect(district, "HANCOCK PLACE")|
           str_detect(district, "HAZELWOOD")|
           str_detect(district, "JENNINGS")|
           str_detect(district, "KIRKWOOD")|
           str_detect(district, "LADUE")|
           str_detect(district, "LINDBERGH SCHOOLS")|
           str_detect(district,"MAPLEWOOD-RICHMOND HEIGHTS")|
           str_detect(district, "MEHLVILLE R-IX")|
           str_detect(district, "NORMANDY SCHOOLS COLLABORATIVE")|
           str_detect(district,"PARKWAY C-2")|
           str_detect(district, "PATTONVILLE")|
           str_detect(district, "RITENOUR")|
           str_detect(district, "RIVERVIEW GARDENS")|
           str_detect(district, "ROCKWOOD")|
           str_detect(district, "UNIVERSITY CITY")|
           str_detect(district, "VALLEY PARK")|
           str_detect(district, "WEBSTER")|
           str_detect(district, "ST. LOUIS CITY")|
           school == "CITY GARDEN MONTESSORI SCHOOL"|
           school == "LAFAYETTE PREPARATORY ACADEMY"|
           school == "KAIROS ACADEMIES",
         as.numeric(total)>0) %>% 
  mutate(total = as.numeric(total),
         c_amind = case_when(is.na(as.numeric(c_amind)) ~ 0, # asterisks for data suppression. 
                             TRUE ~ as.numeric(c_amind)), # replace with zeros
         c_pi = case_when(is.na(as.numeric(c_pi)) ~ 0, # asterisks for data suppression. 
                          TRUE ~ as.numeric(c_pi)), # replace with zeros
         c_asiandrop = case_when(is.na(as.numeric(c_asian)) ~ 0, # asterisks for data suppression. 
                                 TRUE ~ as.numeric(c_asian)), # replace with zeros
         c_asian = c_pi + c_asiandrop,
         c_black = case_when(is.na(as.numeric(c_black)) ~ 0, # asterisks for data suppression. 
                             TRUE ~ as.numeric(c_black)), # replace with zeros,
         c_hisp = case_when(is.na(as.numeric(c_hisp)) ~ 0, # asterisks for data suppression. 
                            TRUE ~ as.numeric(c_hisp)), # replace with zeros,
         c_white = case_when(is.na(as.numeric(c_white)) ~ 0, # asterisks for data suppression. 
                             TRUE ~ as.numeric(c_white)), # replace with zeros,
         c_mult = case_when(is.na(as.numeric(c_mult)) ~ 0, # asterisks for data suppression. 
                            TRUE ~ as.numeric(c_mult)), # replace with zeros,
         c_frpl = case_when(is.na(as.numeric(c_frpl)) ~ 0, # asterisks for data suppression. 
                            TRUE ~ as.numeric(c_frpl)), # replace with zeros,
         c_ell = case_when(is.na(as.numeric(c_ell)) ~ 0, # asterisks for data suppression. 
                           TRUE ~ as.numeric(c_ell)), # replace with zeros,
         c_swd = case_when(is.na(as.numeric(c_swd)) ~ 0, # asterisks for data suppression. 
                           TRUE ~ as.numeric(c_swd)), # replace with zeros,
         p_amind = c_amind/total,
         p_asian = c_asian/total,
         p_black = c_black/total,
         p_hisp = c_hisp/total,
         p_white = c_white/total,
         p_mult = c_mult/total,
         p_frpl = c_frpl/total,
         p_ell = c_ell/total,
         p_swd = c_swd/total) %>% 
  select(district, school, total, p_amind, p_asian, p_black, p_hisp, p_white, p_mult, p_frpl, p_ell, p_swd,
         c_amind, c_asian, c_black, c_hisp, c_white, c_mult, c_frpl, c_ell, c_swd) %>% 
  arrange(school)

# Add comparison districts based on location
stlcounty[(stlcounty$school == "CITY GARDEN MONTESSORI SCHOOL"),1] <- "ST. LOUIS CITY"
stlcounty[(stlcounty$school == "KAIROS ACADEMIES"),1] <- "ST. LOUIS CITY"
stlcounty[(stlcounty$school == "LAFAYETTE PREPARATORY ACADEMY"),1] <- "ST. LOUIS CITY"

stlcounty <- stlcounty %>% 
  select(school, district, c_amind, c_asian, c_black, c_hisp, c_white,c_mult) %>% 
  gather(subgroup, n, c_amind:c_mult, factor_key=TRUE) %>% # change from wide to long 
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls)
y <- stlcounty %>% 
  filter(school == "CITY GARDEN MONTESSORI SCHOOL"|
           school == "LAFAYETTE PREPARATORY ACADEMY"|
           str_detect(school, "CROSSROADS")|
           str_detect(school, "ACADEMIE LAFAYETTE")|
           str_detect(school,"Academie Lafayette")|
           school == "KAIROS ACADEMIES"|
           school == "MIDDLE SCHOOL CAMPUS"|
           school == "PRIMARY GRADES CAMPUS") %>% 
  mutate(ls_county = ls,
         p_county = p,
         county = "ST. LOUIS COUNTY") %>% 
  select(school, county, ls_county, p_county) 
cty_locseg <- rbind(y,cty_locseg)
memb_locseg <- merge(cty_locseg,memb_locseg,by="school")

# totals and percentages for district, county-----
jacksoncounty <- enrl %>% 
  filter(year == "2021",
         str_detect(district, "BLUE SPRINGS")|
           str_detect(district, "CENTER 58")|
           str_detect(district, "FORT OSAGE")|
           str_detect(district, "GRAIN VALLEY")|
           str_detect(district, "GRANDVIEW")|
           str_detect(district, "HICKMAN")|
           str_detect(district, "INDEPENDENCE")|
           str_detect(district, "LEE'S")|
           str_detect(district, "LONE")|
           str_detect(district, "OAK GROVE")|
           str_detect(district, "RAYTOWN")|
           str_detect(district,"KANSAS CITY 33")|
           str_detect(school, "ACADEMIE LAFAYETTE")|
           str_detect(school,"Academie Lafayette")|
           school == "MIDDLE SCHOOL CAMPUS"|
           school == "PRIMARY GRADES CAMPUS"|
           school == "CROSSROADS - CENTRAL STREET"|
           school == "CROSSROADS - QUALITY HILL"|
           school == "CROSSROADS PREPARATORY ACADEMY",
         as.numeric(total)>0) %>% 
  mutate(total = as.numeric(total),
         c_amind = case_when(is.na(as.numeric(c_amind)) ~ 0, # asterisks for data suppression. 
                             TRUE ~ as.numeric(c_amind)), # replace with zeros
         c_pi = case_when(is.na(as.numeric(c_pi)) ~ 0, # asterisks for data suppression. 
                          TRUE ~ as.numeric(c_pi)), # replace with zeros
         c_asiandrop = case_when(is.na(as.numeric(c_asian)) ~ 0, # asterisks for data suppression. 
                                 TRUE ~ as.numeric(c_asian)), # replace with zeros
         c_asian = c_pi + c_asiandrop,
         c_black = case_when(is.na(as.numeric(c_black)) ~ 0, # asterisks for data suppression. 
                             TRUE ~ as.numeric(c_black)), # replace with zeros,
         c_hisp = case_when(is.na(as.numeric(c_hisp)) ~ 0, # asterisks for data suppression. 
                            TRUE ~ as.numeric(c_hisp)), # replace with zeros,
         c_white = case_when(is.na(as.numeric(c_white)) ~ 0, # asterisks for data suppression. 
                             TRUE ~ as.numeric(c_white)), # replace with zeros,
         c_mult = case_when(is.na(as.numeric(c_mult)) ~ 0, # asterisks for data suppression. 
                            TRUE ~ as.numeric(c_mult)), # replace with zeros,
         c_frpl = case_when(is.na(as.numeric(c_frpl)) ~ 0, # asterisks for data suppression. 
                            TRUE ~ as.numeric(c_frpl)), # replace with zeros,
         c_ell = case_when(is.na(as.numeric(c_ell)) ~ 0, # asterisks for data suppression. 
                           TRUE ~ as.numeric(c_ell)), # replace with zeros,
         c_swd = case_when(is.na(as.numeric(c_swd)) ~ 0, # asterisks for data suppression. 
                           TRUE ~ as.numeric(c_swd)), # replace with zeros,
         p_amind = c_amind/total,
         p_asian = c_asian/total,
         p_black = c_black/total,
         p_hisp = c_hisp/total,
         p_white = c_white/total,
         p_mult = c_mult/total,
         p_frpl = c_frpl/total,
         p_ell = c_ell/total,
         p_swd = c_swd/total) %>% 
  select(district, school, total, p_amind, p_asian, p_black, p_hisp, p_white, p_mult, p_frpl, p_ell, p_swd,
         c_amind, c_asian, c_black, c_hisp, c_white, c_mult, c_frpl, c_ell, c_swd) %>% 
  arrange(school)

jacksoncounty[3:5,1] <- "KANSAS CITY 33" # Add comparison districts based on location
jacksoncounty[str_detect(jacksoncounty$school, "CROSSROADS"),1] <- "KANSAS CITY 33"
jacksoncounty[(jacksoncounty$school == "MIDDLE SCHOOL CAMPUS"),1] <- "KANSAS CITY 33"
jacksoncounty[(jacksoncounty$school == "PRIMARY GRADES CAMPUS"),1] <- "KANSAS CITY 33"

kc_dist <- jacksoncounty %>% 
  filter(district == "KANSAS CITY 33") %>% 
  mutate(dist_total = sum(total),
         dist_amind = sum(c_amind)/dist_total,
         dist_asian = sum(c_asian)/dist_total,
         dist_black = sum(c_black)/dist_total,
         dist_hisp = sum(c_hisp)/dist_total,
         dist_white = sum(c_white)/dist_total,
         dist_mult = sum(c_mult)/dist_total,
         dist_frpl = sum(c_frpl)/dist_total,
         dist_ell = sum(c_ell)/dist_total,
         dist_swd = sum(c_swd)/dist_total) %>% 
  select(district, dist_total:dist_swd) %>% 
  unique()

stlcounty <- enrl %>% 
  filter(year == "2021",
         str_detect(district, "AFFTON 101")|
           str_detect(district, "BAYLESS")|
           str_detect(district, "BRENTWOOD")|
           str_detect(district, "CLAYTON")|
           str_detect(district, "FERGUSON")|
           str_detect(district, "HANCOCK PLACE")|
           str_detect(district, "HAZELWOOD")|
           str_detect(district, "JENNINGS")|
           str_detect(district, "KIRKWOOD")|
           str_detect(district, "LADUE")|
           str_detect(district, "LINDBERGH SCHOOLS")|
           str_detect(district,"MAPLEWOOD-RICHMOND HEIGHTS")|
           str_detect(district, "MEHLVILLE R-IX")|
           str_detect(district, "NORMANDY SCHOOLS COLLABORATIVE")|
           str_detect(district,"PARKWAY C-2")|
           str_detect(district, "PATTONVILLE")|
           str_detect(district, "RITENOUR")|
           str_detect(district, "RIVERVIEW GARDENS")|
           str_detect(district, "ROCKWOOD")|
           str_detect(district, "UNIVERSITY CITY")|
           str_detect(district, "VALLEY PARK")|
           str_detect(district, "WEBSTER")|
           str_detect(district, "ST. LOUIS CITY")|
           school == "CITY GARDEN MONTESSORI SCHOOL"|
           school == "LAFAYETTE PREPARATORY ACADEMY"|
           school == "KAIROS ACADEMIES",
         as.numeric(total)>0) %>% 
  mutate(total = as.numeric(total),
         c_amind = case_when(is.na(as.numeric(c_amind)) ~ 0, # asterisks for data suppression. 
                             TRUE ~ as.numeric(c_amind)), # replace with zeros
         c_pi = case_when(is.na(as.numeric(c_pi)) ~ 0, # asterisks for data suppression. 
                          TRUE ~ as.numeric(c_pi)), # replace with zeros
         c_asiandrop = case_when(is.na(as.numeric(c_asian)) ~ 0, # asterisks for data suppression. 
                                 TRUE ~ as.numeric(c_asian)), # replace with zeros
         c_asian = c_pi + c_asiandrop,
         c_black = case_when(is.na(as.numeric(c_black)) ~ 0, # asterisks for data suppression. 
                             TRUE ~ as.numeric(c_black)), # replace with zeros,
         c_hisp = case_when(is.na(as.numeric(c_hisp)) ~ 0, # asterisks for data suppression. 
                            TRUE ~ as.numeric(c_hisp)), # replace with zeros,
         c_white = case_when(is.na(as.numeric(c_white)) ~ 0, # asterisks for data suppression. 
                             TRUE ~ as.numeric(c_white)), # replace with zeros,
         c_mult = case_when(is.na(as.numeric(c_mult)) ~ 0, # asterisks for data suppression. 
                            TRUE ~ as.numeric(c_mult)), # replace with zeros,
         c_frpl = case_when(is.na(as.numeric(c_frpl)) ~ 0, # asterisks for data suppression. 
                            TRUE ~ as.numeric(c_frpl)), # replace with zeros,
         c_ell = case_when(is.na(as.numeric(c_ell)) ~ 0, # asterisks for data suppression. 
                           TRUE ~ as.numeric(c_ell)), # replace with zeros,
         c_swd = case_when(is.na(as.numeric(c_swd)) ~ 0, # asterisks for data suppression. 
                           TRUE ~ as.numeric(c_swd)), # replace with zeros,
         p_amind = c_amind/total,
         p_asian = c_asian/total,
         p_black = c_black/total,
         p_hisp = c_hisp/total,
         p_white = c_white/total,
         p_mult = c_mult/total,
         p_frpl = c_frpl/total,
         p_ell = c_ell/total,
         p_swd = c_swd/total) %>% 
  select(district, school, total, p_amind, p_asian, p_black, p_hisp, p_white, p_mult, p_frpl, p_ell, p_swd,
         c_amind, c_asian, c_black, c_hisp, c_white, c_mult, c_frpl, c_ell, c_swd) %>% 
  arrange(school)

# Add comparison districts based on location
stlcounty[(stlcounty$school == "CITY GARDEN MONTESSORI SCHOOL"),1] <- "ST. LOUIS CITY"
stlcounty[(stlcounty$school == "KAIROS ACADEMIES"),1] <- "ST. LOUIS CITY"
stlcounty[(stlcounty$school == "LAFAYETTE PREPARATORY ACADEMY"),1] <- "ST. LOUIS CITY"

stl_dist <- stlcounty %>% 
  filter(district == "ST. LOUIS CITY") %>% 
  mutate(dist_total = sum(total),
         dist_amind = sum(c_amind)/dist_total,
         dist_asian = sum(c_asian)/dist_total,
         dist_black = sum(c_black)/dist_total,
         dist_hisp = sum(c_hisp)/dist_total,
         dist_white = sum(c_white)/dist_total,
         dist_mult = sum(c_mult)/dist_total,
         dist_frpl = sum(c_frpl)/dist_total,
         dist_ell = sum(c_ell)/dist_total,
         dist_swd = sum(c_swd)/dist_total) %>% 
  select(district, dist_total:dist_swd) %>% 
  unique()

dist_aggregates <- rbind(kc_dist,stl_dist)

memb_locseg <- merge(memb_locseg,dist_aggregates,by="district")

cty_aggregate <- jacksoncounty %>%
  mutate(county = "JACKSON COUNTY",
         cty_total = sum(total),
         cty_amind = sum(c_amind)/cty_total,
         cty_asian = sum(c_asian)/cty_total,
         cty_black = sum(c_black)/cty_total,
         cty_hisp = sum(c_hisp)/cty_total,
         cty_white = sum(c_white)/cty_total,
         cty_mult = sum(c_mult)/cty_total,
         cty_frpl = sum(c_frpl)/cty_total,
         cty_ell = sum(c_ell)/cty_total,
         cty_swd = sum(c_swd)/cty_total) %>% 
  select(county, cty_total:cty_swd) %>% 
  unique()

z <- stlcounty %>% 
  mutate(county = "ST. LOUIS COUNTY",
         cty_total = sum(total),
         cty_amind = sum(c_amind)/cty_total,
         cty_asian = sum(c_asian)/cty_total,
         cty_black = sum(c_black)/cty_total,
         cty_hisp = sum(c_hisp)/cty_total,
         cty_white = sum(c_white)/cty_total,
         cty_mult = sum(c_mult)/cty_total,
         cty_frpl = sum(c_frpl)/cty_total,
         cty_ell = sum(c_ell)/cty_total,
         cty_swd = sum(c_swd)/cty_total) %>% 
  select(county, cty_total:cty_swd) %>% 
  unique()

cty_aggregate <- rbind(cty_aggregate,z)

memb_locseg <- merge(memb_locseg,cty_aggregate,by="county")
write.csv(memb_locseg, file = file.path("output data/mo_locseg.csv"),row.names = FALSE)
# ----- END -------