# --- Amy Jiravisitcul. 09 Feb 2022 ----
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

#--- ENROLLMENT BY RACE AND ETHNICITY -----
# data imported from https://www.education.pa.gov/DataAndReporting/Enrollment/Pages/PublicSchEnrReports.aspx
# PIMS from 4/20/2021. Only LEA-level available for race. Oct 1 from 2020-2021

enrl <- read_excel("raw data/PA_Enrollment Public Schools 2020-21.xlsx", sheet = "LEA and Race")
names(enrl) <- tolower(enrl[4,]) # change column names
enrl <- enrl[-c(1:4),] # remove rows with no data
enrl <- enrl %>% 
  mutate(lea = as.factor(`lea name`),
         lea_type = as.factor(`lea type`),
         race = as.factor(race),
         n = as.numeric(total)) %>% 
  select(lea, lea_type, county, race, n)
levels(enrl$race) <- c("amind", "asian", "black","hisp","mult","nhpi","white")
enrl <- enrl[-(5461:5463),]
enrl_wide <- enrl %>% 
  spread(race, n) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(asian = asian + nhpi,
         total = amind + asian + black + hisp + mult + white,
         amind = amind/total,
         asian = asian/ total,
         black = black/total,
         hisp = hisp/total,
         white = white/ total,
         mult = mult/total) %>% 
  select(lea, lea_type, county, total, amind, asian, black, hisp, white, mult)
memb_enrl <- enrl_wide %>% 
  filter(lea == "Vida CS"|
           str_detect(lea, "Hebrew")|
           str_detect(lea,"Memphis"))

# ---- LOCAL SEGREGATION INDEX BY COUNTY ------
memb_enrl$county
locseg <- enrl %>% 
  filter(county == "Philadelphia"|
           county == "Adams") %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

phil_ls <- locseg %>% 
  filter(county == "Philadelphia") %>% 
  mutual_local("race","lea", weight = "n", wide = TRUE) %>% 
  arrange(ls) %>% 
  filter(str_detect(lea, "Hebrew")|
           str_detect(lea, "Memphis")) %>% 
  mutate(ls_cty = ls) %>% 
  select(lea, ls_cty)

adams_ls <- locseg %>% 
  filter(county == "Adams") %>% 
  mutual_local("race","lea",weight = "n",wide = TRUE) %>% 
  arrange(ls) %>% 
  filter(str_detect(lea,"Vida")) %>% 
  mutate(ls_cty = ls) %>% 
  select(lea, ls_cty)

memb_enrl <- merge(memb_enrl,rbind(adams_ls,phil_ls))

# ----- AGGREGATES FOR SCHOOL DISTRICTS AND COUNTIES -----
agg <- read_excel("raw data/PA_Enrollment Public Schools 2020-21.xlsx", sheet = "LEA and Race")
names(agg) <- tolower(agg[4,]) # change column names
agg <- agg[-c(1:4),] # remove rows with no data
agg <- agg %>% 
  mutate(lea = as.factor(`lea name`),
         lea_type = as.factor(`lea type`),
         race = as.factor(race),
         n = as.numeric(total)) %>% 
  select(lea, lea_type, county, race, n)
levels(agg$race) <- c("amind", "asian", "black","hisp","mult","nhpi","white")
agg <- agg[-(5461:5463),]

agg <- agg %>% 
  spread(race, n) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(asian = asian + nhpi)

agg <- agg %>% 
  group_by(county) %>% 
  (cty_amind = sum(amind),
            cty_asian = sum(asian),
            cty_black = sum(black),
            cty_hisp = sum(hisp),
            cty_white = sum(white),
            cty_mult = sum(mult)) %>% 
  mutate(cty_total = cty_amind + cty_asian + cty_black + cty_hisp + cty_white +
           cty_mult,
         cty_amind = cty_amind/cty_total,
         cty_asian = cty_asian/cty_total,
         cty_black = cty_black/cty_total,
         cty_hisp = cty_hisp/cty_total,
         cty_white = cty_white/cty_total,
         cty_mult = cty_mult/cty_total)%>% 
  filter(county == "Philadelphia"|
           county == "Adams")
memb_enrl <- merge(memb_enrl,agg)

dist <- enrl %>% 
  filter(lea_type == "SD") %>% 
  spread(race,n) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  filter(lea == "Philadelphia City SD"|
           lea == "Gettysburg Area SD") %>% 
  mutate(asian = asian + nhpi,
         dist_total = asian + amind + black +hisp + white + mult,
         total = dist_total,
         dist_asian = asian/total,
         dist_amind = amind/total,
         dist_black = black/total,
         dist_hisp = hisp/total,
         dist_white = white/total,
         dist_mult = mult/total,
         dist = lea) %>% 
  select(dist, county, dist_total, dist_amind, dist_asian, 
         dist_black, dist_hisp, dist_white, dist_mult)

memb_enrl <- merge(memb_enrl,dist,by="county")

# ------- EL STUDENTS -----
# Data from https://www.education.pa.gov/DataAndReporting/EnglishLearners/Pages/default.aspx
el <- read_excel("raw data/PA_2020-2021 EL Students by LEA and School.xlsx", 
                 sheet = "LEP Students_1")
names(el) <- tolower(el[5,])
el <- el[-(1:5),]
dist_el <- el %>% 
  filter(`lea name`=="Philadelphia City SD"| # comparison districts
           `lea name` == "Gettysburg Area SD") %>% 
  mutate(dist = `lea name`,
         n_el = as.numeric(`el students`)) %>% 
  select(dist, n_el)
dist_el$total = c(2862,124111) #from agg data
dist_el <- dist_el %>% 
  mutate(d_el = n_el/total) %>% 
  select(dist, d_el)

memb_enrl <- merge(memb_enrl, dist_el)

z <- enrl %>% # add county to the EL data set
  select(lea,county) %>% unique()
el$lea = el$`lea name`
el <- merge(el, z)
cty_el <- el %>% 
  mutate(n_el = as.numeric(`el students`)) %>% 
  group_by(county) %>% 
  (cty_el = sum(n_el)) %>% 
  select(county, cty_el) %>% 
  filter(county == "Adams"|
           county == "Philadelphia")

cty_el$total = c(14446,192662) # from agg data
cty_el <- cty_el %>% 
  mutate(cty_el = cty_el/total) %>% 
  select(county, cty_el)
memb_el <- el %>% 
  filter(str_detect(lea, "Hebrew")|
           str_detect(lea, "Memphis")|
           str_detect(lea, "Vida")) %>% 
  mutate(n_el = as.numeric(`el students`)) %>% 
  select(lea, n_el)

memb_enrl <- merge(memb_enrl,cty_el)
memb_enrl <- merge(memb_enrl, memb_el)
memb_enrl <- memb_enrl %>% 
  mutate(el = n_el/total) %>% 
  select(-n_el)

# ----- SPECIAL EDUCATION ENROLLMENT ----
# Available in PDFs for LEAs at https://penndata.hbg.psu.edu/Public-Reporting/Data-at-a-Glance

sped <- data.frame(lea = c("Vida CS",
                           "Memphis Street Academy CS @ JP Jones",
                           "Philadelphia Hebrew Public CS"), # match the ordering of memb_enrl
                   sped = c(0.19,0.2,0.122),
                   dist_sped = c(0.147,.165,.165),
                   cty_sped = 0.18) # PA-wide. county-level unavailable
memb_enrl <- merge(memb_enrl,sped)

# ---- FRPL ------
# Data downloaded from https://www.education.pa.gov/Teachers%20-%20Administrators/Food-Nutrition/reports/Pages/National-School-Lunch-Program-Reports.aspx with YTD Excel file

frpl <- read_excel('raw data/PA_2020-2021 SY-YTD Monthly Eligibility Report.xls', sheet = "YTD")
View(frpl)
names(frpl) <- tolower(frpl[4,])
str(frpl)
frpl <- frpl[-c(1:4),] # Remove rows with no data
frpl <- frpl %>% 
  mutate(county = as.factor(`county\r\nname`),
         district = as.factor(`school district/sponsor`),
         school = as.factor(`site name`),
         meals = as.numeric(`nslp total\r\nmeals served`),
         days = as.numeric(`lunch\r\noperating days`),
         n_frpl = meals/days) %>% 
  select(county,district, school, meals, days, n_frpl)

memb_frpl <- frpl %>% 
  filter(school == "Vida Charter School") %>% # only Vida in this data set
  mutate(lea = "Vida CS", # match earlier naming convention
         frpl = n_frpl/211,
         dummy = "dummy") %>% # total number from memb_enrl
  select(lea, frpl, dummy)

agg_frpl <- frpl %>% 
  group_by(district) %>% 
  (n = sum(n_frpl)) %>% 
  filter(str_detect(district,"GETTYSBURG AREA")) %>% 
  mutate(d_frpl = n/2862,
         dummy = "dummy") %>% # total number from memb_enrl$dist_total
  select(district, d_frpl, dummy)
c <- frpl %>% 
  group_by(county) %>% 
  (n = sum(n_frpl)) %>% 
  filter(str_detect(county,"ADAMS")) %>% 
  mutate(cty_frpl = n/14446) %>%  # total number from memb_enrl$cty_total
  select(county, cty_frpl)
agg_frpl <- merge(agg_frpl,c)
memb_frpl <- merge(agg_frpl,memb_frpl)

frpl_2 <- read_excel('raw data/PA_2020-2021 Building Data Report.xlsx',
                     sheet = "CEP Sites Free Claiming %") # other dataset which includes Hebrew and Memphis
names(frpl_2) <- tolower(frpl_2[2,])
frpl_2 <- frpl_2[-(1:2),]

frpl_2 <- frpl_2 %>% 
  mutate(district = `sponsor name`,
         school = `site name`,
         n_frpl = as.numeric(`eligible students`),
         frpl = .01* as.numeric(`identified student %`)) %>% 
  select(district, school, n_frpl, frpl)

memb_frpl_2 <- frpl_2 %>% 
  filter(str_detect(school, "Hebrew Public")|
           str_detect(school,"MEMPHIS")) %>% 
  mutate(lea = school,
         dummy = "dummy") %>% 
  select(dummy,lea, frpl)

agg_frpl_2 <- frpl_2 %>% 
  filter(str_detect(district, "SCHOOL DISTRICT OF PHILADELPHIA")) %>% 
  group_by(district) %>% 
  (n = sum(n_frpl)) %>% 
  mutate(dummy = "dummy",
         d_frpl = n/124111) %>%  # figure from memb_enrl$dist_total
  select(dummy, district, d_frpl)
memb_frpl_2 <- merge(memb_frpl_2,agg_frpl_2)
memb_frpl_2$county = "PHILADELPHIA"


h <- enrl_wide %>% # Which schools are included in this county based on the other dataset?
  filter(county =="Philadelphia") %>% 
  mutate(lea = toupper(lea)) %>% 
  select(lea)

cty_f <- frpl_2 %>%  # a rough approximation
  filter(str_detect(h,district)|
           str_detect(district,"MASTERY CHARTER")|
           str_detect(district,"ARCHDIOCESE")) 
cty_f <- cty_f %>% 
  (n = sum(n_frpl)) %>% 
  mutate(dummy = "dummy",
         cty_frpl = n/192662) %>% 
  select(dummy,cty_frpl)
memb_frpl_2 <- merge(memb_frpl_2,cty_f) %>% 
  mutate(district = "Philadelphia") %>% 
  select(dummy,county, district, lea,frpl,d_frpl,cty_frpl)

memb_frpl <- rbind(memb_frpl_2,memb_frpl) %>% select(lea,frpl,d_frpl,cty_frpl)
memb_frpl[2,1] <- "Memphis Street Academy CS @ JP Jones"

memb_enrl <- merge(memb_frpl,memb_enrl)
# ---- PREP TO COPY/PASTE INTO MEMBER LOG -----
sort(names(memb_enrl))
memb_enrl <- memb_enrl %>% 
  select(lea, total, amind, asian, black, hisp, white, mult, frpl, el, sped,ls_cty,
         dist, dist_total, dist_amind, dist_asian, dist_black, dist_hisp, dist_white, dist_mult,
         d_frpl, d_el, dist_sped, county, cty_total, cty_amind, cty_asian, cty_black, cty_hisp,
         cty_white, cty_mult, cty_frpl, cty_el, cty_sped)
write.csv(memb_enrl, file = file.path('output data/pa_enrl.csv'),row.names = FALSE)
