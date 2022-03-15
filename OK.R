# --- Amy Jiravisitcul. 6 Mar 2022 ----
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

# Data downloaded from https://sde.ok.gov/sites/default/files/documents/files/GG_bySITE_2A_EthGen-FY20-21_Public%20.xlsx

# ------ RACE ENROLLMENT SCHOOL-LEVEL ----------
enrl <- read_excel('raw data/OK_02_SchoolSiteTotals_with_EthnicityandGender_12-14-2021 172240_ (1).xlsx',
                   sheet = "bySITE  Public")
names(enrl) <- tolower(enrl[1,])
enrl <- enrl[-1,]
str(enrl)
levels(as.factor(enrl$grade))
enrl <- enrl %>% 
  mutate(hisp = as.numeric(`hispaniclatino female`)+
           as.numeric(`hispaniclatino male`)+
           as.numeric(`hispaniclatino gender not disclosed`),
         amind = as.numeric(`american indian(non-hispanic) male`)+
           as.numeric(`american indian(non-hispanic) female`)+
           as.numeric(`american indian(non-hispanic) gender not disclosed`),
         asian = as.numeric(`asian(non-hispanic) male`)+
           as.numeric(`asian(non-hispanic) female`)+ 
           as.numeric(`asian(non-hispanic) gender not disclosed`)+
           as.numeric(`hawaiian or pacific islander(non-hispanic) female`)+
           as.numeric(`hawaiian or pacific islander(non-hispanic) male`)+
           as.numeric(`hawaiian or pacific islander(non-hispanic) gender not disclosed`),
         black = as.numeric(`black(non-hispanic) male`)+ 
           as.numeric(`black(non-hispanic) female`)+ 
           as.numeric(`black(non-hispanic) gender not disclosed`),
         white = as.numeric(`white(non-hispanic) gender not disclosed`)+
           as.numeric(`white(non-hispanic) male`)+
           as.numeric(`white(non-hispanic) female`),
         mult = as.numeric(`two or more races(non-hispanic) male`)+
           as.numeric(`two or more races(non-hispanic) gender not disclosed`)+
           as.numeric(`two or more races(non-hispanic) female`),
         total = amind + asian + black + hisp + white + mult,
         school = `school site`) %>% 
  select(county, district, school, total, amind, asian, black, hisp, white, mult)
DT <- data.table(enrl) # sum across all grades
enrl <- DT[, lapply(.SD, sum), by=list(county, district, school)]
enrl[(str_detect(school,"TULSA SC")),2] <- "TULSA"

memb_enrl <- enrl %>% 
  filter(str_detect(school,"TULSA SC")) %>% 
  mutate(amind = amind/total,
         asian = asian/total,
         black = black/total,
         hisp = hisp/total,
         white = white/total,
         mult = mult/total) %>% na.omit()

locseg_dist <- enrl %>% 
  gather(race, n, amind:mult) %>% 
  filter(district == "TULSA") %>% 
  mutual_local("race","school", weight = "n", wide = TRUE)

memb_enrl <- locseg_dist %>% 
  filter(str_detect(school, "TULSA SC")) %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist) %>% 
  merge(memb_enrl)

locseg_cty <- enrl %>% 
  gather(race, n, amind:mult) %>% 
  filter(county == "TULSA") %>% 
  mutual_local("race","school", weight = "n",wide = TRUE)

memb_enrl <- locseg_cty %>% 
  filter(str_detect(school, "TULSA SC")) %>% 
  mutate(ls_cty = ls) %>% 
  select(school, ls_cty) %>% 
  merge(memb_enrl, by="school")

# ---- AGGREGATE NUMBERS FOR COUNTY AND DISTRICT -----
DT <- enrl %>% 
  select(-county, -school)
district <- DT[, lapply(.SD, sum), by=list(district)] %>% 
  filter(district == "TULSA") %>% 
  mutate(dist_total = total,
         dist_amind = amind/total,
         dist_asian = asian/total,
         dist_black = black/total,
         dist_hisp = hisp/total,
         dist_white = white/total,
         dist_mult = mult/total) %>% 
  select(district,dist_total:dist_mult)
memb_enrl <- merge(memb_enrl, district, by="district")

DT <- enrl %>% 
  select(-district, -school)
county <- DT[, lapply(.SD, sum), by=list(county)] %>% 
  filter(county == "TULSA") %>% 
  mutate(cty_total = total,
         cty_amind = amind/total,
         cty_asian = asian/total,
         cty_black = black/total,
         cty_hisp = hisp/total,
         cty_white = white/total,
         cty_mult = mult/total) %>% 
  select(county, cty_total:cty_mult)
memb_enrl <- merge(memb_enrl, county, by= "county")

# ---- SPECIAL EDUCATION ------
# Data downloaded from https://sde.ok.gov/sites/default/files/documents/files/FY%202021%20Public%20Reporting%20Protected.xlsx

swd <- read_excel('raw data/OK_FY 2021 Public Reporting Unprotected.xlsx')
names(swd) <- tolower(names(swd))
str(swd)
d_swd <- swd %>% 
  select(`district name`, `grand total`) %>% 
  filter(`district name` == "Tulsa"|
           `district name` == "Tulsa Charter: Schl Arts/Sci.")
memb_enrl <- memb_enrl %>% 
  mutate(swd = as.numeric(d_swd[2,2])/523, # sum of HS and MS total = 523
         dist_swd = as.numeric(d_swd[1,2])/dist_total)

all_districts <- enrl %>% 
  filter(county == "TULSA") %>% 
  select(district) %>% unique() %>%
  mutate(district = str_to_title(district))
all_districts

c_swd <- swd %>% 
  select(`district name`, `grand total`) %>% 
  filter(`district name` == "Keystone"|
           `district name` == "Tulsa"|
           `district name` == "Tulsa Charter: Kipp Tulsa"|
           `district name` == "Tulsa Legacy Charter"|
           `district name` == "Tulsa Charter: Collegiate Hall"|
           `district name` == "Tulsa Charter: College Bound"|
           `district name` == "Tulsa Charter: Honor Academy"|
           `district name` == "Deborah Brown Charter"|
           `district name` == "Dove Schools of Tulsa"|
           `district name` == "Sankofa Middle Schl Charter"|
           `district name` == "Sand Springs"|
           `district name` == "Broken Arrow"|
           `district name` == "Bixby"|
           `district name` == "Jenks"|
           `district name` == "Collinsville"|
           `district name` == "Skiatook"|
           `district name` == "Sperry"|
           `district name` == "Union"|
           `district name` == "Berryhill"|
           `district name` == "Owasso"|
           `district name` == "Glenpool"|
           `district name` == "Liberty")
memb_enrl$cty_swd = sum(c_swd$`grand total`)
memb_enrl <- memb_enrl %>% 
  mutate(cty_swd = cty_swd/cty_total)

# ----- FRPL ELIGIBILITY ------
frpl <- read_excel('raw data/OK_Community Eligibility Proxy for Districts and Sites 2021__0.xlsx',
                   sheet = "LEA-wide Notification Report")
names(frpl) <- tolower(frpl[5,])
frpl <- frpl[-(1:6),]
names(frpl)
frpl <- frpl %>% 
  mutate(district = `lea name`,
         frpl = as.numeric(`\r\ndistrict-wide identified student percentage (isp)`))%>% 
  select(district, frpl)

totals <- enrl %>% 
  select(school, district, total)
dist_frpl <- frpl %>% 
  filter(district == "TULSA") %>% 
  mutate(dist_frpl = frpl) %>% 
  select(district,dist_frpl)

memb_enrl <- memb_enrl %>% 
  merge(dist_frpl, by= "district")

cty_frpl <- frpl %>%
  mutate(district = str_to_title(district)) %>% 
  filter(district == "Keystone"|
           district == "Tulsa"|
           district == "Tulsa Charter: Kipp Tulsa"|
           district == "Tulsa Legacy Charter"|
           district == "Tulsa Charter: Collegiate Hall"|
           district == "Tulsa Charter: College Bound"|
           district == "Tulsa Charter: Honor Academy"|
           district == "Deborah Brown Charter"|
           district == "Dove Schools of Tulsa"|
           district == "Sankofa Middle Schl Charter"|
           district == "Sand Springs"|
           district == "Broken Arrow"|
           district == "Bixby"|
           district == "Jenks"|
           district == "Collinsville"|
           district == "Skiatook"|
           district == "Sperry"|
           district == "Union"|
           district == "Berryhill"|
           district == "Owasso"|
           district == "Glenpool"|
           district == "Liberty") %>% 
  mutate(district = str_to_upper(district))

totals <- enrl %>% 
  group_by(district) %>% 
  summarize(dist_total = sum(total))

cty_frpl<- cty_frpl %>% 
  merge(totals, by="district") %>%
  mutate(cty_frpl = dist_total * frpl,
         county = "TULSA") %>% 
  select(county,district,cty_frpl) %>% 
  group_by(county) %>% 
  summarize(cty_frpl = sum(cty_frpl))

memb_enrl <- memb_enrl %>% 
  merge(cty_frpl, by = "county") %>% 
  mutate(cty_frpl = cty_frpl/cty_total)

memb_enrl <- memb_enrl %>% 
  mutate(frpl = frpl$frpl[(str_detect(frpl$district,"SCHL ARTS"))]) %>% 
  select(school, total:mult, frpl, swd, ls_dist, ls_cty, district, dist_total:dist_mult,
         dist_frpl,dist_swd, county, cty_total:cty_mult,cty_frpl,cty_swd)

write.csv(memb_enrl,file = file.path('output data/ok_enrl.csv'), row.names = FALSE)
