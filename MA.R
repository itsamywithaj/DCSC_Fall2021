# --- Amy Jiravisitcul. 15 Feb 2022 ----
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

# ----- ENROLLMENT BY RACE --------
# data from http://profiles.doe.mass.edu/statereport/enrollmentbyracegender.aspx
enrl <- read_excel("raw data/MA_enrollmentbyracegender.xlsx")
names(enrl) <- tolower(enrl[1,])
enrl <- enrl[-1,]
enrl <- enrl %>% 
  separate(`school name`,c("district","school")," - ",extra = "merge") %>% 
  mutate(district = case_when(school == "Benjamin Banneker Charter Public School" ~ "Cambridge",
                            school == "Boston Collegiate Charter School" ~ "Boston",
                            TRUE ~ district))
str(enrl)
enrl <- enrl %>% 
  mutate(amind = .01 * as.numeric(`native american`),
         asian = .01 * (as.numeric(asian)+as.numeric(`native hawaiian, pacific islander`)),
         black = .01 * as.numeric(`african american`),
         hispanic = .01 * as.numeric(hispanic),
         white = .01 * as.numeric(white),
         multiple = .01 * as.numeric(`multi-race, non-hispanic`)) %>% 
  select(district, school, amind, asian, black, hispanic, white, multiple)

# - need the other data set for total student enrollment calculations by school
sub <- read_excel("raw data/MA_selectedpopulations.xlsx")
names(sub) <- tolower(sub[1,])
sub <- sub[-1,]
sub <- sub[,c(1,5,7,17,18)]
names(sub)[c(2:5)] <- c("ell","swd","n_ecdis","p_ecdis")

str(sub)
sub <- sub %>% 
  separate(`school name`,c("district","school")," - ",extra = "merge") %>%
  mutate(total = round(as.numeric(n_ecdis)/(.01* as.numeric(p_ecdis))),
         ell = as.numeric(ell)/total,
         swd = as.numeric(swd)/total,
         ecdis = as.numeric(n_ecdis)/total) %>% 
  select(school, ell, swd, ecdis, total)
View(sub)

enrl <- merge(enrl,sub,by="school")

memb_enrl <- enrl %>% 
  filter(str_detect(school, "Banneker")|
           str_detect(school,"Boston Collegiate"))
# ----- LOCAL SEGREGATION BY DISTRICT -------
camb <- enrl %>% 
  gather(race, n, amind:multiple) %>%
  select(school, district, race, n) %>%
  filter(district == "Cambridge") %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  mutate(ls_dist = ls) %>% 
  filter(str_detect(school,"Benjamin Banneker")) %>% 
  select(school, ls_dist)

boston <- enrl %>% 
  gather(race, n, amind:multiple) %>% 
  select(school, district, race, n) %>% 
  filter(district == "Boston") %>% 
  mutual_local("race","school",weight = "n", wide = TRUE) %>% 
  mutate(ls_dist = ls) %>% 
  filter(str_detect(school, "Boston Collegiate")) %>% 
  select(school, ls_dist)

a <- rbind(boston, camb)
memb_enrl<- merge(memb_enrl,a)

# --- DISTRICT AGGREGATE BY RACE, ECON DISADVANTAGE, SWD, ELL -----
# data from http://profiles.doe.mass.edu/statereport/selectedpopulations.aspx

agg <- enrl %>% 
  mutate(amind = amind * total,
         asian = asian * total,
         black = black * total,
         hispanic = hispanic * total,
         white = white * total,
         multiple = multiple * total,
         ecdis = ecdis * total,
         ell = ell * total,
         swd = swd * total) %>% 
  group_by(district) %>% 
  summarize(dist_total = sum(total),
            dist_amind = sum(amind)/dist_total,
            dist_asian = sum(asian)/dist_total,
            dist_black = sum(black)/dist_total,
            dist_hispanic = sum(hispanic)/dist_total,
            dist_white = sum(white)/dist_total,
            dist_multiple = sum(multiple)/dist_total,
            dist_ecdis = sum(ecdis)/dist_total,
            dist_ell = sum(ell)/dist_total,
            dist_swd = sum(swd)/dist_total) %>% 
  filter(district == "Boston"|
           district == "Cambridge")

memb_enrl <- merge(memb_enrl,agg) 

# ---- COUNTY AGGREGATE BY RACE, ECON DISADVANTAGE, SWD, ELL -----
# https://www.k12academics.com/national-directories/school-district/Massachusetts/Middlesex
# https://massgis.maps.arcgis.com/apps/webappviewer/index.html?id=5b652451857f41ecabc5aa96ef53641c

cty_agg <- enrl %>% 
  filter(str_detect(district,"Acton")| # School districts listed in Middlesex County
           str_detect(district, "Arlington")|
           str_detect(district, "Ashland")|
           str_detect(district,"Ayer")|
           district == "Bedford"|
           str_detect(district, "Belmont")|
           str_detect(district,"Billerica")|
           str_detect(district, "Boxborough")|
           str_detect(district, "Burlington")|
           str_detect(district, "Cambridge")|
           str_detect(district,"Carlisle")|
           str_detect(district, "Chelmsford")|
           str_detect(district, "Concord")|
           str_detect(district, "Dracut")|
           str_detect(district, "Everett")|
           str_detect(district, "Framingham")|
           str_detect(district, "Groton")|
           str_detect(district, "Holliston")|
           str_detect(district, "Hopkinton")|
           str_detect(district, "Hudson")|
           str_detect(district, "Lexington")|
           str_detect(district, "Lincoln")|
           str_detect(district, "Littleton")|
           str_detect(district, "Lowell")|
           str_detect(district, "Malden")|
           str_detect(district, "Marlborough")|
           str_detect(district, "Maynard")|
           str_detect(district, "Medford")|
           str_detect(district, "Melrose")|
           str_detect(district, "Minuteman")|
           str_detect(district, "Natick")|
           str_detect(district, "Newton")|
           str_detect(district, "North Middlesex")|
           str_detect(district, "Reading")|
           str_detect(district, "Shirley")|
           str_detect(district, "Somerville")|
           str_detect(district, "Stoneham")|
           str_detect(district, "Sudbury")|
           str_detect(district, "Tewksbury")|
           str_detect(district, "Tyngsborough")|
           str_detect(district, "Wakefield")|
           str_detect(district, "Waltham")|
           str_detect(district, "Watertown")|
           str_detect(district, "Wayland")|
           str_detect(district, "Westford")|
           str_detect(district, "Weston")|
           str_detect(district, "Wilmington")|
           str_detect(district, "Winchester")|
           str_detect(district, "Woburn")) %>% 
  mutate(amind = amind * total,
         asian = asian * total,
         black = black * total,
         hispanic = hispanic * total,
         white = white * total,
         multiple = multiple * total,
         ecdis = ecdis * total,
         ell = ell * total,
         swd = swd * total)
cty_locseg <- cty_agg %>% 
  select(school, amind:multiple) %>% 
  gather(race, n, amind:multiple) %>% 
  mutual_local("race", "school", weight = "n", wide = TRUE) %>% 
  filter(str_detect(school, "Banneker")) %>% 
  mutate(ls_cty = ls,
         county = "Middlesex") %>% select(school, county, ls_cty)
cty_agg <- cty_agg %>% 
  mutate(county = "Middlesex") %>%
  group_by(county) %>% 
  summarize(cty_total = sum(total),
            cty_amind = sum(amind)/cty_total,
            cty_asian = sum(asian)/cty_total,
            cty_black = sum(black)/cty_total,
            cty_hisp = sum(hispanic)/cty_total,
            cty_white = sum(white)/cty_total,
            cty_mult = sum(multiple)/cty_total,
            cty_ecdis = sum(ecdis)/cty_total,
            cty_ell = sum(ell)/cty_total,
            cty_swd = sum(swd)/cty_total)
cty_locseg <- merge(cty_locseg, cty_agg)

# https://www.k12academics.com/national-directories/school-district/Massachusetts/Suffolk
suffolk_agg <- enrl %>% 
  filter(str_detect(district,"Boston")| # School districts listed in Suffolk County
           str_detect(district, "Chelsea")|
           str_detect(district, "Revere")|
           str_detect(district,"Winthrop")) %>% 
  mutate(amind = amind * total,
         asian = asian * total,
         black = black * total,
         hispanic = hispanic * total,
         white = white * total,
         multiple = multiple * total,
         ecdis = ecdis * total,
         ell = ell * total,
         swd = swd * total)
suffolk_locseg <- suffolk_agg %>% 
  select(school, amind:multiple) %>% 
  gather(race, n, amind:multiple) %>% 
  mutual_local("race", "school", weight = "n", wide = TRUE) %>% 
  filter(str_detect(school, "Boston Collegiate")) %>% 
  mutate(ls_cty = ls,
         county = "Suffolk") %>% select(school, county, ls_cty)
suffolk_agg <- suffolk_agg %>% 
  mutate(county = "Suffolk") %>%
  group_by(county) %>% 
  summarize(cty_total = sum(total),
            cty_amind = sum(amind)/cty_total,
            cty_asian = sum(asian)/cty_total,
            cty_black = sum(black)/cty_total,
            cty_hisp = sum(hispanic)/cty_total,
            cty_white = sum(white)/cty_total,
            cty_mult = sum(multiple)/cty_total,
            cty_ecdis = sum(ecdis)/cty_total,
            cty_ell = sum(ell)/cty_total,
            cty_swd = sum(swd)/cty_total)
suffolk_locseg <- merge(suffolk_locseg, suffolk_agg)
cty_locseg <- rbind(suffolk_locseg,cty_locseg)
memb_enrl <- merge(memb_enrl,cty_locseg)

write.csv(memb_enrl, file = file.path('output data/ma_enrl.csv'),row.names = FALSE)
