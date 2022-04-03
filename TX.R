# --- Amy Jiravisitcul. 2 Apr 2022 ----
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

# Data downloaded https://rptsvr1.tea.texas.gov/perfreport/tapr/2021/xplore/DownloadSelData.html
# --- FULL SCHOOL-LEVEL ENROLLMENT FILE ------
enrl <- read.csv('raw data/TX_CSTUD.csv')
names(enrl) <- tolower(names(enrl))
str(enrl)
# variable documentation https://rptsvr1.tea.texas.gov/perfreport/tapr/2021/xplore/cstud.html
enrl <- enrl %>% 
  mutate(district = case_when(campname == "MAGNOLIA MONTESSORI FOR ALL" ~ "AUSTIN ISD",
                              campname == "THE GATHERING PLACE" ~ "NORTHSIDE ISD",
                              TRUE ~ distname),
         school = campname,
         total = cpntallc,
         amind = cpntindc,
         asian = cpntasic + cpntpcic,
         black = cpntblac,
         hisp = cpnthisc,
         white = cpntwhic,
         mult = cpnttwoc,
         ecdis = cpntecoc,
         el = cpntlepc,
         swd = cpntspec) %>% 
  select(district:swd)

aisd <- enrl %>% 
  filter(district == "AUSTIN ISD") %>% 
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

nisd <- enrl %>% 
  filter(district == "NORTHSIDE ISD") %>% 
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

memb_enrl <- enrl %>% 
  filter(str_detect(school, "MAGNOLIA MONT")|
           str_detect(school, "GATHERING PL")) %>% 
  mutate(amind = amind/total,
         asian = asian/total,
         black = black/total,
         hisp = hisp/total,
         white = white/total,
         mult = mult/total,
         ecdis = ecdis/total,
         el = el/total,
         swd = swd/total) %>% 
  merge(rbind(aisd,nisd))

aisd_ls <- enrl %>% 
  filter(district == "AUSTIN ISD") %>% 
  select(school, amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist)

nisd_ls <- enrl %>% 
  filter(district == "NORTHSIDE ISD") %>% 
  select(school, amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist)

memb_enrl <- memb_enrl %>% 
  merge(rbind(aisd_ls, nisd_ls))

# county level
# Bexar County includes San Antonio https://www.bexar.org/QuickLinks.aspx?CID=38
memb_enrl$county <- c("Travis County","Bexar County")
bexar <- enrl %>% 
  filter(district == "ALAMO HEIGHTS ISD"|
           district == "BASIS TEXAS"|
           district == "BEXAR COUNTY ACADEMY"|
           district == "BROOKS ACADEMIES OF TEXAS"|
           district == "BOERNE ISD"|
           district == "COMPASS ROSE ACADEMY"|
           district == "EAST CENTRAL ISD"|
           district == "EDGEWOOD ISD"|
           district == "ELEANOR KOLITZ HEBREW LANGUAGE ACA"|
           district == "FT SAM HOUSTON ISD"|
           district == "GEORGE GERVIN ACADEMY"|
           district == "GREAT HEARTS TEXAS"|
           district == "HARLANDALE ISD"|
           district == "HARMONY SCIENCE ACAD (SAN ANTONIO)"|
           district == "HENRY FORD ACADEMY ALAMEDA SCHOOL"|
           district == "HERITAGE ACADEMY"|
           district == "INSPIRE ACADEMIES"|
           district == "JUBILEE ACADEMIES"|
           district == "JUDSON ISD"|
           district == "LACKLAND ISD"|
           district == "LIGHTHOUSE PUBLIC SCHOOLS"|
           district == "NEW FRONTIERS PUBLIC SCHOOLS INC"|
           district == "NORTH EAST ISD"|
           district == "NORTHSIDE ISD"|
           district == "POR VIDA ACADEMY"|
           district == "POSITIVE SOLUTIONS CHARTER SCHOOL"|
           district == "PROMESA ACADEMY CHARTER SCHOOL"|
           district == "RANDOLPH FIELD ISD"|
           district == "SAN ANTONIO ISD"|
           district == "SAN ANTONIO PREPARATORY CHARTER SC"|
           district == "SCHOOL OF EXCELLENCE IN EDUCATION"|
           district == "SCHOOL OF SCIENCE AND TECHNOLOGY"|
           district == "SCHOOL OF SCIENCE AND TECHNOLOGY D"|
           district == "SOMERSET ISD"|
           district == "SOUTH SAN ANTONIO ISD"|
           district == "SOUTHSIDE ISD"|
           district == "SOUTHWEST ISD"|
           district == "SOUTHWEST PREPARATORY SCHOOL")
bexar_ls <- bexar %>% 
  select(school, amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  mutate(ls_cty = ls) %>% 
  select(school, ls_cty)

bexar_agg <- bexar %>% 
  mutate(county = "Bexar County") %>% 
  group_by(county) %>% 
  summarize(cty_total = sum(total),
            cty_amind = sum(amind)/sum(total),
            cty_asian = sum(asian)/sum(total),
            cty_black= sum(black)/sum(total),
            cty_hisp = sum(hisp)/sum(total),
            cty_white = sum(white)/sum(total),
            cty_mult = sum(mult)/sum(total),
            cty_ecdis = sum(ecdis)/sum(total),
            cty_el = sum(el)/sum(total),
            cty_swd = sum(swd)/sum(total))

# Travis County's districts https://rptsvr1.tea.texas.gov/cgi/sas/broker
travis <- enrl %>% 
  filter(district == "AUSTIN ISD"|
           district == "AUSTIN ACHIEVE PUBLIC SCHOOLS"|
           district == "AUSTIN DISCOVERY"|
           district == "CEDARS INTERNATIONAL ACADEMY"|
           district == "CHAPARRAL STAR ACADEMY"|
           district == "DEL VALLE ISD"|
           district == "EANES ISD"|
           district == "HARMONY SCIENCE ACADEMY (AUSTIN)"|
           district == "KIPP TEXAS PUBLIC SCHOOLS"|
           district == "LAGO VISTA ISD"|
           district == "LAKE TRAVIS ISD"|
           district == "MANOR ISD"|
           district == "NYOS CHARTER SCHOOL"|
           district == "PFLUGERVILLE ISD"|
           district == "PROMESA PUBLIC SCHOOLS"|
           district == "TEXAS EMPOWERMENT ACADEMY"|
           district == "THE EXCEL CENTER (FOR ADULTS)"|
           district == "UNIVERSITY OF TEXAS AT AUSTIN H S"|
           district == "UNIVERSITY OF TEXAS ELEMENTARY CHA"|
           district == "UNIVERSITY OF TEXAS UNIVERSITY CHA"|
           district == "VALOR PUBLIC SCHOOLS"|
           district == "WAYSIDE SCHOOLS")

travis_agg <- travis %>% 
  mutate(county = "Travis County") %>% 
  group_by(county) %>% 
  summarize(cty_total = sum(total),
            cty_amind = sum(amind)/sum(total),
            cty_asian = sum(asian)/sum(total),
            cty_black= sum(black)/sum(total),
            cty_hisp = sum(hisp)/sum(total),
            cty_white = sum(white)/sum(total),
            cty_mult = sum(mult)/sum(total),
            cty_ecdis = sum(ecdis)/sum(total),
            cty_el = sum(el)/sum(total),
            cty_swd = sum(swd)/sum(total))

travis_ls <- travis %>% 
  select(school, amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  mutate(ls_cty = ls) %>% 
  select(school, ls_cty)

memb_enrl <- memb_enrl %>% 
  merge(rbind(travis_agg,bexar_agg)) %>% 
  merge(rbind(travis_ls, bexar_ls)) %>% 
  select(school, total:swd, ls_dist, ls_cty, district, dist_total:dist_swd,
         county, cty_total:cty_swd)

write.csv(memb_enrl, file = file.path('output data/tx_enrl.csv'), row.names = FALSE)
