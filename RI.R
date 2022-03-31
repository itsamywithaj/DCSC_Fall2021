# --- Amy Jiravisitcul. 30 Mar 2022 ----
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

# ---- 
# data definitions http://www.eride.ri.gov/doc/DataCollections/EnrollmentCensusCollection.pdf
# asp file downloaded and resaved as .xls to be accessible https://www.eride.ri.gov/reports/reports.asp

# --- BVP - ALL SCHOOLS - OCTOBER ENROLLMENT - 2020-2021 ------
bvp <- read_excel('raw data/RI_report_excel_new.xlsx')
str(bvp)
bvp <- bvp[14,]
names(bvp) <- tolower(names(bvp))
bvp <- bvp %>% 
  mutate(total = as.numeric(total),
         amind = 0, asian = (as.numeric(`asian pacific (male)`)+ as.numeric(`asian pacific (female)`))/total,
         black = (as.numeric(`black (female)`)+as.numeric(`black (male)`))/total,
         hisp = (as.numeric(`hispanic (female)`)+as.numeric(`hispanic (male)`))/total,
         white = (as.numeric(`white (female)`)+ as.numeric(`white (male)`))/total,
         mult = (as.numeric(`multi-race (female)`)+as.numeric(`multi-race (male)`))/total,
         frl = (as.numeric(`frl (female)`)+as.numeric(`frl (male)`))/total,
         lep = (as.numeric(`lep (male)`)+as.numeric(`lep (female)`))/total,
         swd = (as.numeric(`iep (male)`)+ as.numeric(`iep (female)`))/total) %>% 
  select(total:swd)

# --- DOWNLOAD STATISTICAL TABLE - SCHOOL REPORT - OCT ENROLLMENT - 2020-2021 ------
enrl <- read_excel('raw data/RI_download.xlsx')
names(enrl) <- tolower(names(enrl))
enrl <- enrl %>% 
  mutate(district = as.factor(district),
         school = as.factor(school),
         total = as.numeric(total),
         amind = as.numeric(gsub("[^0-9.-]",0,`native american (female)`)) +
           as.numeric(gsub("[^0-9.-]",0,`native american (male)`)),
         asian = as.numeric(gsub("[^0-9.-]",0,`asian pacific (female)`)) + 
           as.numeric(gsub("[^0-9.-]",0,`asian pacific (male)`)),
         black = as.numeric(gsub("[^0-9.-]",0,`black (female)`)) + 
           as.numeric(gsub("[^0-9.-]",0,`black (male)`)),
         hisp = as.numeric(gsub("[^0-9.-]",0,`hispanic (female)`)) + 
           as.numeric(gsub("[^0-9.-]",0,`hispanic (male)`)),
         white = as.numeric(gsub("[^0-9.-]",0,`white (female)`)) + 
           as.numeric(gsub("[^0-9.-]",0,`white (male)`)),
         mult = as.numeric(gsub("[^0-9.-]",0,`multi-race (female)`)) + 
           as.numeric(gsub("[^0-9.-]",0,`multi-race (male)`)),
         frl = as.numeric(gsub("[^0-9.-]",0,`frl (female)`)) + 
           as.numeric(gsub("[^0-9.-]",0,`frl (male)`)),
         lep = as.numeric(gsub("[^0-9.-]",0,`lep (female)`)) + 
           as.numeric(gsub("[^0-9.-]",0,`lep (male)`)),
         swd = as.numeric(gsub("[^0-9.-]",0,`iep (female)`)) + 
           as.numeric(gsub("[^0-9.-]",0,`iep (male)`))) %>%
  select(district, school, total, amind:swd)

bvp_7 <- enrl %>% 
  filter(str_detect(school, "Blackstone Valley Prep")) %>% 
  arrange(school)

bvp_7$district = c("Cumberland", # Add comparison districts based on geographic proximity
                   "Cumberland",
                   "Cumberland",
                   "Cumberland",
                   "Cumberland",
                   "Central Falls",
                   "Lincoln")

enrl <- enrl %>% 
  filter(str_detect(school, "Blackstone Valley Prep")==FALSE) %>% 
  rbind(bvp_7)

# ------ DISTRICT AND STATE-WIDE REPRESENTATION ------

cumberland <- enrl %>% 
  filter(district == "Cumberland") %>% 
  select(district, school, amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  mutate(ls_dist = ls) %>% 
  filter(str_detect(school, "Blackstone Valley Prep")) %>% 
  select(school, ls_dist)

lincoln <- enrl %>% 
  filter(district == "Lincoln") %>% 
  select(district, school, amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  mutate(ls_dist = ls) %>% 
  filter(str_detect(school, "Blackstone Valley Prep")) %>% 
  select(school, ls_dist)

central_falls <- enrl %>% 
  filter(district == "Central Falls") %>% 
  select(district, school, amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE) %>% 
  mutate(ls_dist = ls) %>%
  filter(str_detect(school, "Blackstone Valley Prep")) %>% 
  select(school, ls_dist)

memb_enrl <- enrl %>% 
  merge(rbind(cumberland, central_falls, lincoln)) %>% 
  mutate(state = "Rhode Island")

ls_ri <- enrl %>% 
  select(school, amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE) %>% 
  mutate(ls_ri = ls) %>%
  filter(str_detect(school, "Blackstone Valley Prep")) %>% 
  select(school, ls_ri)

memb_enrl <- memb_enrl %>% merge(ls_ri)

# aggregates by state and district
memb_enrl <- enrl %>% 
  group_by(district) %>% 
  summarize(dist_total = sum(total),
            dist_amind = sum(amind)/dist_total,
            dist_asian = sum(asian)/dist_total,
            dist_black = sum(black)/dist_total,
            dist_hisp = sum(hisp)/dist_total,
            dist_white = sum(white)/dist_total,
            dist_mult = sum(mult)/dist_total,
            dist_frl = sum(frl)/dist_total,
            dist_lep = sum(lep)/dist_total,
            dist_swd = sum(swd)/dist_total) %>% 
  filter(district == "Central Falls"|
           district == "Cumberland"|
           district == "Lincoln") %>% 
  merge(memb_enrl)

memb_enrl <- enrl %>% 
  mutate(state = "Rhode Island") %>% 
  group_by(state) %>% 
  summarize(ri_total = sum(total),
            ri_amind = sum(amind)/ri_total,
            ri_asian = sum(asian)/ri_total,
            ri_black = sum(black)/ri_total,
            ri_hisp = sum(hisp)/ri_total,
            ri_white = sum(white)/ri_total,
            ri_mult = sum(mult)/ri_total,
            ri_frl = sum(frl)/ri_total,
            ri_lep = sum(lep)/ri_total,
            ri_swd = sum(swd)/ri_total) %>% 
  merge(memb_enrl)
names(memb_enrl)
memb_enrl <- memb_enrl %>% 
  select(school,total:ls_ri, district:dist_swd, state, ri_total:ri_swd)

write.csv(memb_enrl, file = file.path('output data/ri_enrl.csv'), row.names = FALSE)
