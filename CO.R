# --- Amy Jiravisitcul. 03 Feb 2022 ----
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

# data all from https://www.cde.state.co.us/cdereval/pupilcurrent
# ----- racial demographics -----
# https://www.cde.state.co.us/cdereval/2021-2022schoolmembershipethnicityracegender Last updated 1/3/2022
race_enrl <- read_excel("raw data/CO_2021-22_Membership_Race_Gender_byGradeSchool.xlsx",sheet = "Data")
str(race_enrl)
names(race_enrl) <- tolower(race_enrl[2,]) 
names(race_enrl)[c(6,8,10,12,14,16,18)] <- c("amind_",
                                             "asian_",
                                             "black_",
                                             "hisp_",
                                             "white_",
                                             "nhpi_",
                                             "mult_") #rename columns where male and female separate

race_enrl <- race_enrl[-c(1:3),] # Remove rows with no data

race_enrl <- race_enrl %>% 
  filter(is.na(`grade level`)) %>% # keep only rows with aggregates of all grade levels
  mutate(c_amind = as.numeric(`american indian or alaskan native`)+
           as.numeric(amind_),
         c_asian = as.numeric(`asian`)+as.numeric(asian_)+
           as.numeric(`native hawaiian or other pacific islander`)+
           as.numeric(nhpi_),
         c_black = as.numeric(`black or african american`)+as.numeric(black_),
         c_hisp = as.numeric(`hispanic or latino`)+as.numeric(hisp_),
         c_white = as.numeric(white)+as.numeric(white_),
         c_mult = as.numeric(`two or more races`)+as.numeric(mult_),
         total = as.numeric(`pk-12 total`),
         amind = c_amind/total,
         asian = c_asian/total,
         black = c_black/total,
         hisp = c_hisp/total,
         white = c_white/total,
         mult = c_mult/total) %>%
  select(`org. code`,`organization name`,`school code`,`school name`,
         total, amind, asian, black, hisp, white, mult,
         c_amind,c_asian,c_black,c_hisp,c_white,c_mult)

memb_enrl <- race_enrl %>% 
  filter(str_detect(`school name`,"DSST")|
           `school name` == "Downtown Denver Expeditionary School"|
           `school name` == "Aurora Science & Tech Middle School") %>% 
  arrange(`school code`) %>% 
  mutate(school = `school name`)

# Entities: https://www.cde.state.co.us/cdeedserv/coloradoschooldistrictswithcountiesmap
# ---- LOCAL SEGREGATION BY DISTRICT -----
unique(memb_enrl$`org. code`)
entities <- race_enrl %>% 
  filter(`org. code`=="0180"|
           `org. code`=="0880")
locseg <- entities %>% 
  gather(subgroup, n, c_amind:c_mult, factor_key=TRUE) %>%  # change from wide to long
  mutate(school = `school name`) %>% 
  arrange(school) %>% 
  select(school, subgroup, n, `org. code`) %>% 
  na.omit() # remove the TOTAL row for each district
denver <- locseg %>% 
  filter(`org. code`=="0880") %>% 
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls)
denver <- denver %>% 
  filter(str_detect(school,"DSST")|
           school == "Downtown Denver Expeditionary School")
aur <- locseg %>% 
  filter(`org. code` == "0180") %>% 
  mutual_local("subgroup","school",weight = "n",wide = TRUE) %>% 
  arrange(ls) %>% 
  filter(str_detect(school,"Science & Tech"))

denver <- rbind(denver, aur)

memb_enrl <- merge(memb_enrl, denver,by="school") %>%
  select(school,`org. code`, `organization name`,total, amind:mult, ls)
agg <- entities %>% 
  filter(is.na(`school name`))
agg <- agg[,-c(2:4,12:17)]
names(agg)[2:8]<- c("dist_total",
                    "dist_amind",
                    "dist_asian",
                    "dist_black",
                    "dist_hisp",
                    "dist_white",
                    "dist_mult")
memb_enrl <- merge(memb_enrl,agg)


#----- FRPL ----
# FRL downloaded from https://www.cde.state.co.us/cdereval/2021-2022schoolk-12frl
frpl <- read_excel("raw data/CO_2021-22_K12_FRL_bySchool - Suppressed.xlsx", sheet = "Data")
names(frpl) <- tolower(frpl[2,])
frpl <- frpl[-c(1:2),]
str(frpl)
frpl <- frpl %>% 
  mutate(county = as.factor(`county name`),
         district = as.factor(`district name`),
         total = as.numeric(`k-12 count`),
         c_frpl = as.numeric(`free and reduced count`),
         frpl = c_frpl/total,
         school = `school name`) %>% 
  select(county, `county code`, district, `district code`, school,total, c_frpl, frpl)
memb_frpl <- frpl %>% 
  filter(school == "Downtown Denver Expeditionary School"|
           school == "Aurora Science & Tech Middle School"|
           str_detect(school, "DSST")) %>% 
  arrange(school) %>% 
  select(school, frpl)
county <- frpl %>% 
  select(county, district, `district code`) %>% unique() %>%
  filter(county == "DENVER"|
           county == "ARAPAHOE") # 6 districts associated with Aurora's ARAPAHOE county
unique(county$`district code`)
    
library(plyr)
detach(package:plyr)    
library(dplyr)
agg_frpl <- frpl %>% 
  group_by(`district code`) %>% 
  summarize(dist_c_frpl = sum(c_frpl,na.rm = TRUE),
            dist_total = sum(total,na.rm = TRUE),
            dist_frpl = dist_c_frpl/dist_total) %>% 
  filter(`district code`=="0180"|
           `district code`=="0880") %>% 
  select(`district code`,dist_frpl)
h <- frpl %>% group_by(county) %>% 
  summarize(county_c_frpl = sum(c_frpl,na.rm = TRUE),
            county_total = sum(total, na.rm = TRUE),
            county_frpl = county_c_frpl/county_total) %>%
  filter(county == "DENVER"|
           county == "ARAPAHOE")

agg$county <- c("ARAPAHOE","DENVER")
names(agg)[1] <- "district code"
agg <- merge(agg,agg_frpl)
agg <- merge(agg,h)

# ---- LOCAL SEGREGATION BY COUNTY ------
unique(county$`district code`)
entities <- race_enrl %>% 
    filter(`org. code`=="0120"| # Use all Arapahoe County's districts for broader comparison
             `org. code`=="0123"|
             `org. code`=="0130"|
             `org. code`=="0140"|
             `org. code`=="0170"|
             `org. code`=="0180"|
             `org. code`=="0190"|
             `org. code`=="0880") # Plus Denver

locseg <- entities %>% 
  gather(subgroup, n, c_amind:c_mult, factor_key=TRUE) %>%  # change from wide to long
  mutate(school = `school name`) %>% 
  arrange(school) %>% 
  select(school, subgroup, n, `org. code`) %>% 
  na.omit() # remove the TOTAL row for each district

broad <- locseg %>% 
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls)
cty_locseg <- broad %>% 
  filter(str_detect(school,"DSST")|
           school == "Downtown Denver Expeditionary School"|
           str_detect(school,"Science & Tech")) %>% 
  mutate(ls_cty = ls,
         p_cty = p) %>% 
  select(school, ls_cty, p_cty)
g <- entities %>% 
  filter(str_detect(`organization name`,"TOTAL")) %>% 
  summarize(cty_total = sum(total),
            cty_amind = sum(c_amind)/cty_total,
            cty_asian = sum(c_asian)/cty_total,
            cty_black = sum(c_black)/cty_total,
            cty_hisp = sum(c_hisp)/cty_total,
            cty_white = sum(c_white)/cty_total,
            cty_mult = sum(c_mult)/cty_total)
g$dummy = "dummy"
agg$dummy = "dummy"
agg <- merge(agg,g)
cty_locseg$dummy = "dummy"
cty_locseg <- merge(agg,cty_locseg)

memb_enrl <- merge(memb_enrl,cty_locseg)

# ---- ELL AND SPECIAL EDUCATION -------
ellsped <- read_excel("raw data/CO_2021-22_IPST_bySchool - Suppressed.xlsx",sheet = "Data")
names(ellsped) <- tolower(ellsped[1,])
ellsped <- ellsped[-1,c(1:9)]
str(ellsped)
ellsped <- ellsped %>% 
  mutate(district = as.factor(`district name`),
         total = as.numeric(`pk-12 pupil membership`),
         c_el = as.numeric(`el count`),
         el = c_el/total,
         school = `school name`,
         c_sped = as.numeric(`sped count`),
         sped = c_sped/total) %>% 
  select(`district code`, district, school, total, c_el, el, c_sped, sped) %>% 
  mutate_if(is.numeric, replace_na, 0)

memb_ellsped <- ellsped %>% 
  filter(school == "Downtown Denver Expeditionary School"|
           school == "Aurora Science & Tech Middle School"|
           str_detect(school, "DSST")) %>% 
  arrange(school) %>% 
  select(school, el, sped)

agg_ellsped <- ellsped %>% 
  group_by(`district code`) %>% 
  summarize(dist_c_el = sum(c_el,na.rm = TRUE),
            dist_total = sum(total),
            dist_el = dist_c_el/dist_total,
            dist_c_sped = sum(c_sped,na.rm = TRUE),
            dist_sped = dist_c_sped/dist_total) %>% 
  filter(`district code`=="0180"|
           `district code`=="0880") %>% 
  select(`district code`,dist_el,dist_sped)

h <- ellsped %>% 
  group_by(`district code`) %>% 
  summarize(dist_c_el = sum(c_el,na.rm = TRUE),
            dist_total = sum(total),
            dist_el = dist_c_el/dist_total,
            dist_c_sped = sum(c_sped,na.rm = TRUE),
            dist_sped = dist_c_sped/dist_total) %>%
  filter(`district code`=="0120"| # Use all Arapahoe County's districts for broader comparison
           `district code`=="0123"|
           `district code`=="0130"|
           `district code`=="0140"|
           `district code`=="0170"|
           `district code`=="0180"|
           `district code`=="0190"|
           `district code`=="0880") # Plus Denver
h <- h %>% 
  mutate(county = case_when(`district code` == "0880" ~ "DENVER",
                            `district code` != "0880" ~ "ARAPAHOE"))
h <- h %>% 
  group_by(county) %>% 
  summarize(cty_c_el = sum(dist_c_el,na.rm = TRUE),
            cty_total = sum(dist_total),
            cty_el = cty_c_el/cty_total,
            cty_c_sped = sum(dist_c_sped,na.rm = TRUE),
            cty_sped = cty_c_sped/cty_total)
h$`district code` <- c("0180","0880")
agg_ellsped <- merge(agg_ellsped,h,by="district code")
agg_ellsped <- agg_ellsped[,c(1:3,7,9)]

memb_enrl <- merge(memb_enrl,agg_ellsped)
memb_enrl <- merge(memb_enrl,memb_ellsped)
memb_enrl <- merge(memb_enrl, memb_frpl)
names(memb_enrl)
memb_enrl <- memb_enrl %>% 
  select(school, total, amind:mult, frpl, el, sped,
         ls, ls_cty, `organization name`,dist_total, dist_amind, dist_asian, 
         dist_black, dist_hisp, dist_white, dist_mult, dist_frpl, dist_el, dist_sped,
         county, cty_total, cty_amind, cty_asian, cty_black, cty_hisp,
         cty_white, cty_mult, county_frpl, cty_el, cty_sped)

write.csv(memb_enrl,file = file.path('output data/co_enrl.csv'),row.names = FALSE)
# -- END -----
