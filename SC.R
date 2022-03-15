# --- Amy Jiravisitcul. 28 Feb 2022 ----
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

# ----- ENROLLMENT BY RACE w/ LOCAL SEG NUMBERS --------
# Data downloaded from https://ed.sc.gov/data/other/student-counts/active-student-headcounts/
# 45-day head count from 11/2021 as a stand-in for 2020-2021 data
# includes Liberty STEAM, member founded in 2020
enrl <- read_excel('raw data/SC_School Headcount by Gender, Ethnicity and Pupils in Poverty.xlsx')
names(enrl) <- tolower(enrl[5,]) # rename columns
names(enrl)[5:15] <- tolower(enrl[6,5:15])
enrl <- enrl[-c(1:6,1228:1231),-7] # remove empty rows and duplicate "missing" column and statewide total row
names(enrl)
str(enrl) 
enrl[(enrl$school == "Lead Academy"), 2] <- "Greenville 01" # Rename to comparison districts
enrl[(str_detect(enrl$school,"Liberty STEAM")),2] <- "Sumter 01"

wide_enrl <- enrl %>% 
  mutate(total = as.numeric(`total # actively enrolled students`),
         amind = as.numeric(`american indian`)/total,
         asian = (as.numeric(asian)+as.numeric(`hawaiian or other pacific islander`))/total,
         black = as.numeric(`black or african-american`)/total,
         hisp = as.numeric(`hispanic or latino`)/total,
         white = as.numeric(white)/total,
         mult = as.numeric(`two or more races`)/total,
         frpl = as.numeric(`pupils in poverty`)/total) %>% # stand-in for FRPL
  select(`school id`,district, school, total, amind,asian,black,hisp,white, mult, frpl)

memb_enrl <- wide_enrl %>% 
  filter(str_detect(school, "Lead Academy")|
           str_detect(school, "Liberty STEAM"))

long_enrl <- enrl %>%
  select(`school id`, district, school, `black or african-american`:white) %>% 
  gather(race, n, `black or african-american`:white) %>% 
  mutate(n = as.numeric(n)) 

locseg <- long_enrl %>% 
  filter(district == "Greenville 01") %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  filter(school == "Lead Academy") %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist)

locseg_ <- long_enrl %>% 
  filter(district == "Sumter 01") %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  filter(str_detect(school, "Liberty STEAM")) %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist)

memb_enrl <- merge(memb_enrl,rbind(locseg, locseg_))

# ----- DISTRICT AGGREGATES RACE ------
agg <- long_enrl %>% 
  spread(race,n) %>% 
  group_by(district) %>% 
  summarize(amind = sum(`american indian`),
            asian = sum(asian) + sum(`hawaiian or other pacific islander`),
            black = sum(`black or african-american`),
            hisp = sum(`hispanic or latino`),
            white = sum(white),
            mult = sum(`two or more races`)) %>% 
  mutate(dist_total = amind + asian + black + hisp + white + mult,
         dist_amind = amind/dist_total,
         dist_asian = asian/dist_total,
         dist_black = black/dist_total,
         dist_hisp = hisp/dist_total,
         dist_white = white/dist_total,
         dist_mult = mult/dist_total) %>% 
  select(district, dist_total:dist_mult) %>% 
  filter(str_detect(district, "Greenville")|
           str_detect(district, "Sumter"))

memb_enrl <- merge(memb_enrl, agg)

agg_frpl <- enrl %>% 
  mutate(frpl = as.numeric(`pupils in poverty`)) %>% 
  select(district, frpl) %>% 
  group_by(district) %>% 
  summarize(dist_frpl = sum(frpl)) %>% 
  filter(str_detect(district, "Greenville")|
         str_detect(district, "Sumter"))

memb_enrl <- memb_enrl %>% 
  merge(agg_frpl) %>% 
  mutate(dist_frpl = dist_frpl/dist_total)

# ---- EL FROM REPORT CARD DATA FILE -------
# Data downloaded from https://screportcards.ed.sc.gov/files/2021//data-files/
ell <- read_excel('raw data/SC_ReportCardData_forResearchers2021.XLSx',
                  sheet = '5.EnglishLearners')
names(ell) <- tolower(names(ell))
str(ell)
ell <- ell %>% 
  select(districtnm,schoolid,schooltypecd,schoolnm,numelsubgroup) %>% 
  filter(str_detect(schoolnm, "Lead Academy")|
           str_detect(schoolnm,"School District Of Greenville")|
           str_detect(schoolnm,"Sumter Schools"))%>%
  mutate(ell = as.numeric(numelsubgroup)) %>% 
  group_by(schoolnm) %>% 
  summarize(ell = sum(ell))

agg_ell <- ell %>% 
  mutate(district = schoolnm,
         dist_ell = ell) %>% 
  filter(str_detect(district, "Lead")==FALSE) %>% 
  select(district, dist_ell)

agg_ell[,1] <- c("Sumter 01", # match naming convention from prior dataset
                 "Greenville 01")       
memb_enrl <- memb_enrl %>% 
  merge(agg_ell) %>% 
  mutate(dist_ell = dist_ell/dist_total)

ell[4,1] <- memb_enrl[2,2] # Add Liberty back into the dataset for merging later, even though their ELL data is unavailable
memb_ell <- ell %>% 
  mutate(school = schoolnm,
         ell = ell) %>% 
  filter(str_detect(school, "Lead")|
           str_detect(school, "Liberty")) %>% 
  select(school, ell)
memb_ell[1,1] <- memb_enrl[1,2] # match naming convention from prior dataset for merging

memb_enrl <- memb_enrl %>% 
  merge(memb_ell) %>% 
  mutate(ell = ell/total)

# ---- SPECIAL EDUCATION -------
# Data downloaded from https://ed.sc.gov/districts-schools/special-education-services/data-and-technology-d-t/data-collection-and-reporting/sc-data-collection-history/idea-child-count-data/2020-2021-child-count-data/

# District summary report for ages 5 to 21

subs <- read.csv('raw data/SC_2020-2021 - Age 5 to 21 - District Demographic Summary.csv')
swd <- subs[1:96,] # take the top rows of raw count by gender
names(swd) <- tolower(swd[8,])
names(swd)[1] <- "district"
swd <- swd[-(1:9),-c(3,5:35)]
str(swd)
swd <- swd %>% 
  mutate(dist_swd = case_when(male == "*" ~ 0,
                              TRUE ~ as.numeric(male)) +
           case_when(female == "*" ~ 0,
                     TRUE ~ as.numeric(female))) %>% 
  select(district, dist_swd) %>% 
  filter(district == "Greenville"|
           district == "Sumter")
swd[,1] <- c("Greenville 01", # match naming convention from prior dataset
             "Sumter 01")

memb_enrl <- memb_enrl %>% 
  merge(swd) %>% 
  mutate(dist_swd = dist_swd/dist_total)

# school-level special ed from the report card file
sped <- read_excel('raw data/SC_ReportCardData_forResearchers2021.XLSx',
                   sheet = "2c.Participation")
names(sped) <- tolower(names(sped))
sped <- sped[,c(2:5,29)]

memb_sped <- sped %>% 
  filter(str_detect(schoolnm, "Lead Academy")|
           schoolid=="2301999"|
           str_detect(schoolnm,"Sumter Schools"))%>%
  mutate(sped = as.numeric(totaln_disabled)) %>% 
  group_by(districtnm) %>% 
  summarize(sped = sum(sped)) %>% 
  mutate(school = districtnm) %>% 
  select(school, sped)
memb_sped[1,1] <- "Lead Academy"
memb_sped[4,1] <- memb_enrl[2,2] # match for merging. Liberty STEAM special ed data not reported
memb_enrl <- memb_sped %>% 
  merge(memb_enrl) %>% 
  mutate(sped = sped/total) %>% 
  select(school, total:frpl,ell,sped,ls_dist, district, dist_total:dist_ell,dist_swd)

write.csv(memb_enrl, file = file.path('output data/sc_enrl.csv'),row.names = FALSE)
