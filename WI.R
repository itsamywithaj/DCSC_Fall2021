# --- Amy Jiravisitcul. 31 Mar 2022 ----
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

# Data downloaded from https://dpi.wi.gov/wisedash/download-files/type?field_wisedash_upload_type_value=Enrollment
# File link https://dpi.wi.gov/sites/default/files/wise/downloads/enrollment_certified_2020-21.zip

# --- FULL SCHOOL-LEVEL ENROLLMENT FILE ------
enrl <- read.csv('raw data/WI_enrollment_certified_2020-21/enrollment_certified_2020-21.csv')
str(enrl)
names(enrl) <- tolower(names(enrl))
levels(as.factor(enrl$county))
enrl <- enrl %>% 
  mutate(district = case_when(district_name == "Milestone Democratic School" ~ "Madison Metropolitan",
                              TRUE ~ district_name), # Change to comparison district
         school = school_name,
         n = as.numeric(student_count),
         group = group_by_value) %>% 
  filter(group_by == "All Students"|
           group_by == "Disability Status"|
           group_by == "Economic Status"|
           group_by == "EL Status"|
           group_by == "RACE_ALT")%>% 
  select(county,district:group)
levels(as.factor(dane_agg$group))

# ---- FILTERED TO ONLY WISCONSIN SCHOOLS IN THE SAME COUNTY AS MILESTONE ------
dane_cty <- enrl %>% 
  filter(county == "Dane",
         group != "[Data Suppressed]",
         group != "Unknown",
         school != "[Districtwide]") %>% 
  spread(group,n)%>% 
  mutate(total = `All Students`,
         amind = `Amer Indian`,
         asian = Asian + `Pacific Isle`,
         black = Black,
         hisp = Hispanic,
         white = White,
         mult = `Two or More`,
         ecdis = `Econ Disadv`,
         el = EL,
         swd = SwD) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  select(county, district, school, total:swd)

# ---- COUNTY- AND DISTRICT- AGGREGATES BY SUBGROUP ----
dane_agg <- dane_cty %>% 
  group_by(county) %>% 
  summarize(cty_total = sum(total),
            cty_amind = sum(amind)/sum(total),
            cty_asian = sum(asian)/sum(total),
            cty_black = sum(black)/sum(total),
            cty_hisp = sum(hisp)/sum(total),
            cty_white = sum(white)/sum(total),
            cty_mult = sum(mult)/sum(total),
            cty_ecdis = sum(ecdis)/sum(total),
            cty_el = sum(el)/sum(total),
            cty_swd = sum(swd)/sum(total)) 
memb_enrl <- dane_cty %>% 
  filter(str_detect(school,"Milestone")) %>% 
  mutate(amind = amind/total,
         asian = asian/total,
         black = black/total,
         hisp = hisp/total,
         white = white/total,
         mult = mult/total,
         ecdis = ecdis/total,
         el = el/total,
         swd = swd/total) %>% 
  merge(dane_agg)

mad_agg <- dane_cty %>% 
  filter(district == "Madison Metropolitan") %>% 
  group_by(district) %>% 
  summarize(dist_total = sum(total),
            dist_amind = sum(amind)/sum(total),
            dist_asian = sum(asian)/sum(total),
            dist_black = sum(black)/sum(total),
            dist_hisp = sum(hisp)/sum(total),
            dist_white = sum(white)/sum(total),
            dist_mult = sum(mult)/sum(total),
            dist_ecdis = sum(ecdis)/sum(total),
            dist_el = sum(el)/sum(total),
            dist_swd = sum(swd)/sum(total))
memb_enrl <- memb_enrl %>% merge(mad_agg)

# ----- LOCAL SEGREGATION MEASURES ------
ls_mad <- enrl %>% 
  filter(group == "Amer Indian"|
           group == "Asian"|
           group == "Black"|
           group == "Hispanic"|
           group == "Pacific Isle"|
           group == "Two or More"|
           group == "White",
         district == "Madison Metropolitan",
         school != "[Districtwide]") %>% 
  mutual_local("group", "school", weight = "n", wide = TRUE) %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist)

ls_dane <- enrl %>% 
  filter(group == "Amer Indian"|
           group == "Asian"|
           group == "Black"|
           group == "Hispanic"|
           group == "Pacific Isle"|
           group == "Two or More"|
           group == "White",
         county == "Dane",
         school != "[Districtwide]") %>% 
  mutual_local("group", "school", weight = "n", wide = TRUE) %>% 
  mutate(ls_cty = ls) %>% 
  select(school, ls_cty)

memb_enrl <- memb_enrl %>% 
  merge(ls_mad) %>% 
  merge(ls_dane) %>% 
  select(school, total:swd, ls_dist, ls_cty, district, dist_total:dist_swd,
         county, cty_total:cty_swd)

write.csv(memb_enrl, file = file.path('output data/wi_enrl.csv'), row.names = FALSE)
