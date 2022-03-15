# --- Amy Jiravisitcul. 7 Mar 2022 ----
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
enrl <- read_excel('raw data/IN_school-enrollment-ethnicity-and-free-reduced-price-meal-status-2006-22.xlsx',
           sheet = "2021")
names(enrl) <- tolower(names(enrl))
str(enrl)

# https://inview.doe.in.gov/schools/1091451122/population
# https://inview.doe.in.gov/schools/1096505724/population
memb_enrl <- data.frame(school = c("Riverside High School",
                                 "Herron High School"),
                        schid = NA,
                        district = "Indianapolis Public Schools",
                        total = c(437,951),
                        amind = c(0, .002),
                        asian = c(.016 ,.015),
                        black = c(.586, .262),
                        hisp = c(.169, .111),
                        white = c(.181, .532),
                        mult = c(.048, .078),
                        ecdis = c(.661, .401),
                        ell = c(.076, .021),
                        swd = c(.192, .135))

n_memb <- memb_enrl %>% 
  mutate(n_amind = amind * total,
         n_asian = asian * total,
         n_black = black * total,
         n_hisp = hisp * total,
         n_white = white * total,
         n_mult = mult * total,
         n_ecdis = ecdis * total,
         n_ell = ell * total,
         n_swd = swd * total) %>% 
  select(school, schid, district, total, n_amind:n_swd)

enrl[is.na(enrl)] = 0
enrl <- enrl %>% 
  mutate(total = `total enrollment`,
         n_amind = `american indian`,
         n_asian = asian + `native hawaiian or other pacific islander`,
         n_black = black,
         n_hisp = hispanic,
         n_white = white,
         n_mult = multiracial,
         n_ecdis = `free/reduced price meals`,
         school = `schl name`,
         schid = `schl id`,
         district = `corp name`) %>% 
  select(school, schid, district, total:n_ecdis)

enrl <- rbind(enrl,n_memb[,1:11])

indy_dist <- enrl %>% 
  group_by(district) %>% 
  summarize(dist_total = sum(total),
            dist_amind = sum(n_amind)/dist_total,
            dist_asian = sum(n_asian)/dist_total,
            dist_black = sum(n_black)/dist_total,
            dist_hisp = sum(n_hisp)/dist_total,
            dist_white = sum(n_white)/dist_total,
            dist_mult = sum(n_mult)/dist_total,
            dist_ecdis = sum(n_ecdis)/dist_total) %>% 
  filter(str_detect(district, "Indianapolis Public"))


locseg <- enrl %>% 
  filter(str_detect(district, "Indianapolis Public")) %>% 
  select(school, n_amind:n_mult) %>% 
  gather(race,n,n_amind:n_mult) %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  mutate(ls_dist = ls) %>% 
  filter(school == "Riverside High School"|
           school == "Herron High School") %>% 
  select(school, ls_dist)

memb_enrl <- merge(locseg, memb_enrl)

# SWD AND ENGLISH LEARNER -------
subgroups <- read_excel('raw data/IN_school-enrollment-ell-special-education-2006-21.xlsx')
names(subgroups) <- tolower(names(subgroups))
subgroups <- subgroups %>% 
  mutate(n_ell = `ell n`,
         n_swd = `special education n`,
         school = `school name`,
         schid = `schl id`,
         district = `corp name`) %>% 
  select(school, schid, district, n_ell, n_swd)
subgroups <- rbind(subgroups,n_memb[,c(1:3,12:13)])

indy_dist <- subgroups %>% 
  group_by(district) %>% 
  summarize(dist_ell = sum(n_ell),
            dist_swd = sum(n_swd)) %>% 
  filter(str_detect(district,"Indianapolis Public")) %>% 
  merge(indy_dist) %>% 
  mutate(dist_ell = dist_ell/dist_total,
         dist_swd = dist_swd/dist_total)

memb_enrl <- merge(indy_dist, memb_enrl, by="district")

# COUNTY LEVELS ------
# https://www.k12academics.com/national-directories/school-district/Indiana/Marion
marion <- enrl %>% 
  filter(district == "Beech Grove City Schools"|
           str_detect(district, "Franklin Township")|
           str_detect(district, "Indianapolis Public")|
           str_detect(district, "MSD Decatur")|
           str_detect(district, "MSD Lawrence")|
           str_detect(district, "Perry Township")|
           str_detect(district, "MSD Pike")|
           str_detect(district, "MSD Warren")|
           str_detect(district, "MSD Washington")|
           str_detect(district, "MSD Wayne")|
           district == "School Town of Speedway") %>% 
  mutate(county = "Marion County")

marion_agg <- marion %>% 
  group_by(county) %>% 
  summarize(cty_total = sum(total),
            cty_amind = sum(n_amind)/cty_total,
            cty_asian = sum(n_asian)/cty_total,
            cty_black = sum(n_black)/cty_total,
            cty_hisp = sum(n_hisp)/cty_total,
            cty_white = sum(n_white)/cty_total,
            cty_mult = sum(n_mult)/cty_total,
            cty_ecdis = sum(n_ecdis)/cty_total)

memb_enrl <- marion %>% 
  select(school, n_amind:n_mult) %>% 
  gather(race,n,n_amind:n_mult) %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  mutate(ls_cty = ls) %>% 
  filter(school == "Riverside High School"|
           school == "Herron High School") %>% 
  select(school, ls_cty) %>% 
  mutate(county = "Marion County")%>% merge(memb_enrl)

marion_agg <- subgroups %>% 
  filter(district == "Beech Grove City Schools"|
           str_detect(district, "Franklin Township")|
           str_detect(district, "Indianapolis Public")|
           str_detect(district, "MSD Decatur")|
           str_detect(district, "MSD Lawrence")|
           str_detect(district, "Perry Township")|
           str_detect(district, "MSD Pike")|
           str_detect(district, "MSD Warren")|
           str_detect(district, "MSD Washington")|
           str_detect(district, "MSD Wayne")|
           district == "School Town of Speedway") %>% 
  mutate(county = "Marion County") %>% 
  group_by(county) %>% 
  summarize(cty_ell = sum(n_ell),
            cty_swd = sum(n_swd)) %>% 
  merge(marion_agg, by="county") %>% 
  mutate(cty_ell = cty_ell/cty_total,
         cty_swd = cty_swd/cty_total)

memb_enrl <- merge(memb_enrl, marion_agg, by = "county") %>% 
  select(school, total:swd,ls_dist, ls_cty, district,dist_total:dist_ecdis, dist_ell, dist_swd,
         county, cty_total:cty_ecdis, cty_ell, cty_swd)

write.csv(memb_enrl,file = file.path('output data/in_enrl.csv'), row.names = FALSE)
