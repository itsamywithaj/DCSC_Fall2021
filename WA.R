# --- Amy Jiravisitcul. 09 Feb 2022 ----
rm(list = ls()) # clear working environment
setwd("~/Downloads/States from Fall 2021/")
install.packages("tidyverse") # install tidyverse
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("data.table")
library(data.table)
install.packages('segregation')
library(segregation)
getwd()

# data files from https://data.wa.gov/dataset/Report-Card-Enrollment-2020-21-School-Year/nvpc-yr7b

# ----- RACE AND ETHNICITY, SPED, ELL, FRPL ENROLLMENT -----
enrl <- read.csv('raw data/WA_Report_Card_Enrollment_2020-21_School_Year.csv')
names(enrl) <- tolower(names(enrl))
str(enrl)
levels(as.factor(enrl$gradelevel))
enrl <- enrl %>% 
  mutate(school = schoolname,
         total = all.students,
         n_amind = american.indian..alaskan.native,
         n_asian = asian + native.hawaiian..other.pacific.islander,
         n_black = black..african.american,
         n_hisp = hispanic..latino.of.any.race.s.,
         n_white = white,
         n_mult = two.or.more.races,
         n_frpl = low.income,
         n_ell = english.language.learners,
         n_swd = students.with.disabilities,
         amind = n_amind/total,
         asian = n_asian/total,
         black = n_black/total,
         hisp = n_hisp/total,
         white = n_white/total,
         mult = n_mult/total,
         frpl = n_frpl/total,
         ell = n_ell/total,
         swd = n_swd/total, # Comparison districts by proximity:
         district = case_when(school == "Summit Public School: Atlas" 
                              ~ "Seattle School District No. 1",
                              school == "Summit Public School: Olympus" 
                              ~ "Tacoma School District",
                              school == "Summit Public School: Sierra"
                              ~ "Seattle School District No. 1",
                              TRUE ~ districtname)) %>% 
  filter(gradelevel == "AllGrades",
         organizationlevel == "School") %>% 
  select(county, district, school,total, n_amind, n_asian, n_black, n_hisp, n_white, n_mult,
         n_frpl, n_ell, n_swd, amind, asian, black, hisp, white, mult, frpl, ell, swd)

memb_enrl <- enrl %>% 
  filter(str_detect(school, "Summit Public")) %>% 
  select(county:total,amind:swd)


# ---- LOCAL SEG BY DISTRICT ----
seattle <- enrl %>% 
  filter(district == "Seattle School District No. 1") %>% 
  select(district, school, n_amind:n_mult)

locseg_seattle <- seattle %>% 
  gather(race,n, n_amind:n_mult) %>% #reshape to long format
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  filter(str_detect(school,"Summit Public")) %>% 
  mutate(ls_dist = ls) %>% select(school, ls_dist)

locseg_tacoma <- enrl %>% 
  filter(district == "Tacoma School District") %>% 
  select(district, school, n_amind:n_mult) %>% 
  gather(race, n, n_amind:n_mult) %>% #reshape to long format
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  filter(str_detect(school, "Summit Public")) %>% 
  mutate(ls_dist = ls) %>% select(school, ls_dist)
locseg <- rbind(locseg_tacoma,locseg_seattle)

# ----- LOCAL SEG BY COUNTY -----
unique(memb_enrl$county)
locseg_king <- enrl %>% 
  filter(county == "King") %>% 
  select(county, school, n_amind:n_mult) %>% 
  gather(race, n, n_amind:n_mult) %>% # reshape to long format
  mutual_local("race","school",weight = "n", wide = TRUE) %>% 
  filter(str_detect(school, "Summit Public")) %>% 
  mutate(ls_cty = ls) %>% select(school, ls_cty)

locseg_pierce <- enrl %>% 
  filter(county == "Pierce") %>% 
  select(county, school, n_amind:n_mult) %>% 
  gather(race, n, n_amind:n_mult) %>% # reshape to long format
  mutual_local("race","school",weight = "n", wide = TRUE) %>% 
  filter(str_detect(school, "Summit Public")) %>% 
  mutate(ls_cty = ls) %>% select(school, ls_cty)

locseg <- merge(locseg,rbind(locseg_king,locseg_pierce))
memb_enrl <- merge(memb_enrl,locseg)

# ----- AGGREGATE RACE, SPED, ELL, FRPL NUMBERS BY DISTRICT AND COUNTY ------
agg_dist <- enrl %>% 
  group_by(district) %>% 
  summarize(total = sum(total),
            amind = sum(n_amind),
            asian = sum(n_asian),
            black = sum(n_black),
            hisp = sum(n_hisp),
            white = sum(n_white),
            mult = sum(n_mult),
            frpl = sum(n_frpl),
            ell = sum(n_ell),
            swd = sum(n_swd)) %>% 
  filter(district == "Seattle School District No. 1"|
           district == "Tacoma School District") %>% 
  mutate(dist_total = total,
         dist_amind = amind/total,
         dist_asian = asian/total,
         dist_black = black/total,
         dist_hisp = hisp/total,
         dist_white = white/total,
         dist_mult = mult/total,
         dist_frpl = frpl/total,
         dist_ell = ell/total,
         dist_swd = swd/total) %>% 
  select(district, dist_total:dist_swd)

agg_county <- enrl %>% 
  group_by(county) %>% 
  summarize(total = sum(total),
            amind = sum(n_amind),
            asian = sum(n_asian),
            black = sum(n_black),
            hisp = sum(n_hisp),
            white = sum(n_white),
            mult = sum(n_mult),
            frpl = sum(n_frpl),
            ell = sum(n_ell),
            swd = sum(n_swd)) %>% 
  filter(county == "King"|
           county == "Pierce") %>% 
  mutate(cty_total = total,
         cty_amind = amind/total,
         cty_asian = asian/total,
         cty_black = black/total,
         cty_hisp = hisp/total,
         cty_white = white/total,
         cty_mult = mult/total,
         cty_frpl = frpl/total,
         cty_ell = ell/total,
         cty_swd = swd/total) %>% 
  select(county, cty_total:cty_swd)
w <- merge(merge(memb_enrl,agg_dist),agg_county)
write.csv(w,file = file.path("output data/wa_enrl.csv"),row.names = FALSE)
