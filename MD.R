# --- Amy Jiravisitcul. 16 Feb 2022 ----
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
# Data downloaded from https://reportcard.msde.maryland.gov/DataDownloads/FileDownload/408
enrl <- read.csv('raw data/MD_Enrollment_2021/Enrollment_By_Race_2021.csv')
names(enrl) <- tolower(names(enrl))
enrl <- enrl %>% 
  mutate(enrolled.count = as.numeric(enrolled.count)) # change format from character to number

agg <- enrl %>% 
  filter(school.name == "All Baltimore City Schools"|
           school.name == "All Baltimore County Schools") %>% 
  spread(race, enrolled.count)
names(agg)[c(3:5,7:14)] <- c("district","school_code","school","total","amind",
                             "asian","black","hisp","nhpi","mult","white")
agg <- agg %>% # percentages by race subgroup of both district and county
  mutate(total = as.numeric(total),
         amind = as.numeric(amind)/total,
         asian = (as.numeric(asian) + as.numeric(nhpi))/total,
         black = as.numeric(black)/total,
         hisp = as.numeric(hisp)/total,
         white = as.numeric(white)/total,
         mult = as.numeric(mult)/total) %>% 
  select(lss, district, school, total:hisp, white, mult)

wide_enrl <- enrl %>% 
  spread(race, enrolled.count) # Reshape to data set with 1,419 rows; 1 for each school
names(wide_enrl)[c(3:5,7:14)] <- c("district","school_code","school","total","amind",
                                   "asian","black","hisp","nhpi","mult","white")
wide_enrl <- wide_enrl %>% 
  filter(lss == "03"| # only include Baltimore City and County
           lss == "30") %>% 
  mutate(total = as.numeric(total),
         amind_p = case_when(lss == "03" ~ agg$amind[1], # estimate where data is suppressed
                             lss == "30" ~ agg$amind[2]),
         amind = case_when(as.numeric(amind)>0 ~ as.numeric(amind),
                           (amind == "*"|is.na(amind)) ~ round(total * amind_p)),
         asian_p = case_when(lss == "03" ~ agg$asian[1],
                             lss == "30" ~ agg$asian[2]),
         asian = case_when(as.numeric(asian)>0 ~ as.numeric(asian),
                           (asian == "*"|is.na(asian)) ~ round(total * asian_p)),
         black_p = case_when(lss == "03" ~ agg$black[1],
                             lss == "30" ~ agg$black[2]),
         black = case_when(as.numeric(black)>0 ~ as.numeric(black),
                           (black == "*"|is.na(black)) ~ round(total * black_p)),
         hisp_p = case_when(lss == "03" ~ agg$hisp[1],
                             lss == "30" ~ agg$hisp[2]),
         hisp = case_when(as.numeric(hisp)>0 ~ as.numeric(hisp),
                           (hisp == "*"|is.na(hisp)) ~ round(total * hisp_p)),
         white_p = case_when(lss == "03" ~ agg$white[1],
                             lss == "30" ~ agg$white[2]),
         white = case_when(as.numeric(white)>0 ~ as.numeric(white),
                           (white == "*"|is.na(white)) ~ round(total * white_p)),
         mult_p = case_when(lss == "03" ~ agg$mult[1],
                             lss == "30" ~ agg$mult[2]),
         mult = case_when(as.numeric(mult)>0 ~ as.numeric(mult),
                           (mult == "*"|is.na(mult)) ~ round(total * mult_p))) %>% 
  select(lss, district, school, total, amind, asian, black, hisp,white, mult) %>% na.omit()

memb_enrl <- wide_enrl %>% 
  filter(str_detect(school, "City Neighbors")|
           str_detect(school, "Montessori")) %>% 
  mutate(county = "Baltimore County",
         amind = amind/total,
         asian = asian/total,
         black = black/total,
         hisp = hisp/total,
         white = white/total,
         mult = mult/total)

a <- agg %>% 
  filter(lss == "30") %>% 
  mutate(dist_total = total,
         dist_amind = amind,
         dist_asian = asian,
         dist_black = black,
         dist_hisp = hisp,
         dist_white = white,
         dist_mult = mult) %>% 
  select(district, dist_total:dist_mult)

memb_enrl <- merge(memb_enrl,a)

b <- agg %>% 
  filter(lss == "03") %>% 
  mutate(county = "Baltimore County",
         cty_total = total,
         cty_amind = amind,
         cty_asian = asian,
         cty_black = black,
         cty_hisp = hisp,
         cty_white = white,
         cty_mult = mult) %>% 
  select(county,cty_total:cty_mult)

memb_enrl <- merge(memb_enrl,b)

# ---- LOCAL RACIAL SEGREGATION BY DISTRICT AND COUNTY ------
locseg_cty <- wide_enrl %>% 
  select(-total)  %>% 
  filter(lss == "03"|
           str_detect(school, "City Neighbors")|
           str_detect(school, "Montessori"),
         school != "All Baltimore County Schools") %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  filter(str_detect(school, "City Neighbors")|
           str_detect(school, "Montessori")) %>% 
  mutate(ls_cty = ls) %>% 
  select(school, ls_cty)
memb_enrl <- merge(memb_enrl,locseg_cty)

locseg_dist <- wide_enrl %>% 
  select(-total) %>% 
  filter(lss == "30",
         school != "All Baltimore City Schools") %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  filter(str_detect(school, "City Neighbors")|
           str_detect(school, "Montessori")) %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist)

memb_enrl <- merge(memb_enrl, locseg_dist)

# ------ FRPL, SWD, ELL ------
subgroups <- read.csv('raw data/MD_2021_Special_Services.csv')
names(subgroups) <- tolower(names(subgroups))
names(subgroups)[c(2:5,25,11,15,27)] <- c("lss","district","school_code","school","ecdis",
                                          "ell","swd","total")
subgroups <- subgroups %>% 
  select(lss, district, school.type, school_code, school, ecdis, ell, swd) %>% 
  filter(school.type == "All"|
           school.type == "High",
         str_detect(school, "All Baltimore")|
         str_detect(school, "City Neighbors")|
           str_detect(school, "Baltimore Montessori")) %>% 
  mutate(ecdis = as.numeric(ecdis)*.01,
         ell= as.numeric(ell) * .01,
         swd = as.numeric (swd) * .01)

memb_sub <- subgroups %>% 
  filter(str_detect(school, "City Neighbors")|
           str_detect(school, "Montessori")) %>% 
  mutate(ell = 0) %>% 
  select(school, ecdis, ell, swd)

memb_enrl <- merge(memb_enrl, memb_sub)

dist_sub <- subgroups %>% 
  filter(school.type=="All",
         school == "All Baltimore City Schools") %>%
  mutate(dist_ecdis = ecdis,
         dist_ell = ell,
         dist_swd = swd) %>% 
  select(district, dist_ecdis, dist_ell, dist_swd)

memb_enrl <- merge(memb_enrl, dist_sub)

cty_sub <- subgroups %>% 
  filter(school.type=="All",
         school == "All Baltimore County Schools") %>%
  mutate(cty_ecdis = ecdis,
         cty_ell = ell,
         cty_swd = swd,
         county = "Baltimore County") %>% 
  select(county, cty_ecdis, cty_ell, cty_swd)

memb_enrl <- merge(memb_enrl, cty_sub)

names(memb_enrl)
memb_enrl <- memb_enrl %>% 
  select(school, total:mult, ecdis:swd, ls_dist, ls_cty, district, dist_total:dist_mult,
         dist_ecdis:dist_swd, county, cty_total:cty_mult, cty_ecdis:cty_swd)

write.csv(memb_enrl, file = file.path('output data/md_enrl.csv'), row.names = FALSE)
