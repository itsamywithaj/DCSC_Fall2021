# --- Amy Jiravisitcul. 14 Mar 2022 ----
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

# ----- ENROLLMENT BY RACE AND SUBGROUP --------
# https://www.nj.gov/education/doedata/enr/index.shtml
# Downloaded from zip file https://www.nj.gov/education/doedata/enr/enr21/enrollment_2021.zip

enrl <- read_excel('raw data/NJ_enrollment_2021.xlsx', sheet = "School")
documentation <- names(enrl)[1]
documentation
names(enrl) <- tolower(enrl[2,])
enrl <- enrl[-(1:2),]
str(enrl)
enrl <- enrl %>% 
  mutate(county = as.factor(`county name`),
         district = as.factor(`district name`),
         school = `school name`,
         total = as.numeric(`total enrollment`),
         amind = as.numeric(gsub("[^0-9.-]", "", `native american`)),
         asian = as.numeric(gsub("[^0-9.-]","",asian)) +
           as.numeric(gsub("[^0-9.-]","",`hawaiian native`)),
         black = as.numeric(gsub("[^0-9.-]","",black)),
         hisp = as.numeric(gsub("[^0-9.-]","", hispanic)),
         white = as.numeric(white),
         mult = as.numeric(gsub("[^0-9.-]","", `two or more races`)),
         frpl = (as.numeric(gsub("[^0-9.-]","",`%free lunch`)) +
           as.numeric(gsub("[^0-9.-]","",`%reduced lunch`))) *.01 * total,
         ell = as.numeric(gsub("[^0-9.-]","",`%english learners`)) * .01 * total) %>% 
  select(county:amind, asian, black,hisp, white, mult:ell)

levels(enrl$county)
levels(enrl$district)

memb_enrl <- enrl %>%
  filter(school == "Hoboken Charter School") %>% 
  mutate(county = levels(enrl$county)[10], # Rename to match comparison counties and districts
         district = levels(enrl$district)[255])

enrl <- enrl %>% 
  filter(school != "Hoboken Charter School") %>%
  rbind(memb_enrl)

dist_ls <- enrl %>% 
  filter(district == "Hoboken Public School District") %>% 
  select(school,amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race", "school", weight = "n", wide = TRUE) %>% 
  filter(school == "Hoboken Charter School") %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist)
memb_enrl <- memb_enrl %>% 
  merge(dist_ls, by = "school")

dist_agg <- enrl %>% 
  filter(district == "Hoboken Public School District") %>% 
  group_by(district) %>% 
  summarize(dist_total = sum(total),
            dist_amind = sum(amind)/dist_total,
            dist_asian = sum(asian)/dist_total,
            dist_black = sum(black)/dist_total,
            dist_hisp = sum(hisp)/dist_total,
            dist_white = sum(white)/dist_total,
            dist_mult = sum(mult)/dist_total,
            dist_frpl = sum(frpl)/dist_total,
            dist_ell = sum(ell)/dist_total)
memb_enrl <- memb_enrl %>% 
  merge(dist_agg)

cty_ls <- enrl %>% 
  filter(county == "Hudson") %>% 
  select(school,amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race", "school", weight = "n", wide = TRUE) %>% 
  filter(school == "Hoboken Charter School") %>% 
  mutate(ls_cty = ls) %>% 
  select(school, ls_cty)
memb_enrl <- memb_enrl %>% 
  merge(cty_ls)

cty_agg <- enrl %>% 
  filter(county == "Hudson") %>% 
  group_by(county) %>% 
  summarize(cty_total = sum(total),
            cty_amind = sum(amind)/cty_total,
            cty_asian = sum(asian)/cty_total,
            cty_black = sum(black)/cty_total,
            cty_hisp = sum(hisp)/cty_total,
            cty_white = sum(white)/cty_total,
            cty_mult = sum(mult)/cty_total,
            cty_frpl = sum(frpl)/cty_total,
            cty_ell = sum(ell)/cty_total)
memb_enrl <- memb_enrl %>% 
  merge(cty_agg) %>% 
  mutate(amind = amind/total,
         asian = asian/total,
         black = black/total,
         hisp = hisp/total,
         white = white/total,
         mult = mult/total,
         frpl = frpl/total,
         ell = ell/total)

# ---- SPECIAL EDUCATION? ----- 
