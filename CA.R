# --- Amy Jiravisitcul. 2 Dec 2021 ----
rm(list = ls())
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

# --- ENROLLMENT BY RACE 2020-2021 ----
# Data Downloaded from https://www.cde.ca.gov/ds/ad/enrolldowndata.asp
# Open TXT file and save as CSV
enrl <- read.csv("raw data/CA_filesenr.csv")
names(enrl) <- tolower(names(enrl))
str(enrl)
enrl <- enrl %>% 
  select(school, county, district, gender, ethnic, enr_total)
enrl$ethnic <- as.factor(enrl$ethnic)
enrl$gender <- as.factor(enrl$gender)

levels(enrl$ethnic) # codes listed on https://www.cde.ca.gov/ds/ad/fsenr.asp
levels(enrl$ethnic) <- c("unreported","amind","asian","pi","filipino","hisp","black","white","mult")
levels(enrl$ethnic)[4:5] <- "asian" # code to match categories in other states

enrl <- enrl %>% # reshape data to have a column for each ethnic category
  distinct(school, district, gender, ethnic, .keep_all = TRUE) %>% 
  spread(key = ethnic,
         value = enr_total,
         fill = 0) %>% 
  mutate(total = unreported + amind + asian + hisp + black + white + mult)
levels(enrl$gender) <- c("F","F","F") # combine genders to sum it up
enrl <- enrl %>% 
  select(school, county, district, unreported, amind, asian, hisp, black, white, mult, total)

DT <- data.table(enrl)
enrl <- DT[, lapply(.SD, sum), by=list(school, district, county)] # Summing the rows for all genders
enrl <- enrl %>% # from counts to percentages
  mutate(p_white = white / total,
         p_amind = amind / total,
         p_asian = asian / total,
         p_black = black / total,
         p_hisp = hisp / total,
         p_mult = mult / total,
         p_unreported = unreported / total, # manually fix comparison districts for OCS, TLC, HTH, Yu Ming, Urban Montessori
         district = case_when(school=="Odyssey Charter" ~ "Pasadena Unified", 
                              school == "Tomorrow's Leadership Collaborative (TLC) Charter" ~ "Orange Unified",
                              school == "High Tech Elementary Chula Vista" ~ "Chula Vista Elementary",
                              school == "High Tech Elementary Mesa" ~ "San Diego Unified",
                              school == "High Tech Elementary North County" ~ "San Marcos Unified",
                              school == "High Tech High Chula Vista" ~ "Sweetwater Union High",
                              school == "High Tech High Mesa" ~ "San Diego Unified",
                              school == "High Tech High North County" ~ "San Marcos Unified",
                              school == "High Tech Middle Chula Vista" ~ "Chula Vista Elementary",
                              school == "High Tech Middle Mesa" ~ "San Diego Unified",
                              school == "High Tech Middle North County" ~ "San Marcos Unified",
                              str_detect(school, "Summit Public") == TRUE ~ "Sunnyvale",
                              school == "Yu Ming Charter"|school =="Urban Montessori Charter" ~ "Oakland Unified",
                              TRUE ~ district))

# --- Filter to DCSC members ----
memb_enrl <- enrl %>% 
  filter(str_detect(school, "Citizens of the World Charter School") == TRUE|
           school == "The City" |
           school == "City Language Immersion Charter"|
           school == "Gateway High"|
           school == "Gateway Middle"|
           school == "Growth Public"|
           school == "Gateway High"|
           school == "Gateway Middle"|
           str_detect(school, "High Tech") == TRUE|
           school == "Larchmont Charter"|
           school == "Odyssey Charter"|
           str_detect(school, "Summit Public") == TRUE|
           school == "Tomorrow's Leadership Collaborative (TLC) Charter"|
           school == "Urban Montessori Charter"|
           school == "Valley Charter Elementary" |
           school == "Valley Charter Middle"|
           school == "Yu Ming Charter") %>% 
  arrange(district, school) %>% 
  select(school, county, district, total, p_amind, p_asian, p_black, 
         p_hisp, p_white, p_mult, p_unreported)

# Percentages for districts and counties... 

write.csv(memb_enrl,file = file.path("output data/ca_enrl.csv"),row.names = FALSE)

## --- local segregation multigroup measure ----
unique(memb_enrl$county) # list of counties in which we have members

locseg <- enrl %>% 
  filter(county == "Los Angeles"|
           county == "San Francisco"|
           county == "Sacramento"|
           county == "San Diego"|
           county == "Contra Costa"|
           county == "Santa Clara"|
           county == "San Mateo"|
           county == "Orange"|
           county == "Alameda")%>%
  select(school, district, county, unreported, amind, asian, hisp, black, white, mult) %>% 
  gather(subgroup, n, unreported:mult, factor_key=TRUE) # change from wide to long

# - Los Angeles County-----
la_locseg <- locseg %>% 
  filter(county == "Los Angeles") %>% 
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls) # sort from most to least representative of LA County
memb_locseg <- la_locseg %>% 
  filter(str_detect(school, "Citizens of the World Charter School") == TRUE|
           school == "The City" |
           school == "City Language Immersion Charter"|
           school == "Gateway High"|
           school == "Gateway Middle"|
           school == "Growth Public"|
           school == "Gateway High"|
           school == "Gateway Middle"|
           str_detect(school, "High Tech") == TRUE|
           school == "Larchmont Charter"|
           school == "Odyssey Charter"|
           str_detect(school, "Summit Public") == TRUE|
           school == "Tomorrow's Leadership Collaborative (TLC) Charter"|
           school == "Urban Montessori Charter"|
           school == "Valley Charter Elementary" |
           school == "Valley Charter Middle"|
           school == "Yu Ming Charter")  %>%
  mutate(county = "Los Angeles",
         ls_county = ls,
         p_county = p) %>% 
  select(school, county, ls_county, p_county)
g <- memb_enrl %>% 
  select(school, district)
memb_locseg <- merge(memb_locseg,g,by="school") # add district column back in

unique(memb_locseg$district)
launified <- locseg %>% 
  filter(district == "Los Angeles Unified") %>% 
  mutual_local("subgroup","school",weight = "n", wide = TRUE) %>% 
  arrange(ls)
launified <- launified %>% 
  filter(str_detect(school, "Citizens of the World Charter School") == TRUE|
           school == "The City" |
           school == "City Language Immersion Charter"|
           school == "Gateway High"|
           school == "Gateway Middle"|
           school == "Growth Public"|
           school == "Gateway High"|
           school == "Gateway Middle"|
           str_detect(school, "High Tech") == TRUE|
           school == "Larchmont Charter"|
           school == "Odyssey Charter"|
           str_detect(school, "Summit Public") == TRUE|
           school == "Tomorrow's Leadership Collaborative (TLC) Charter"|
           school == "Urban Montessori Charter"|
           school == "Valley Charter Elementary" |
           school == "Valley Charter Middle"|
           school == "Yu Ming Charter")  %>%  #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)

unique(memb_locseg$district)
pas_locseg <- locseg %>% 
  filter(district == "Pasadena Unified") %>% 
  mutual_local("subgroup","school",weight = "n", wide = TRUE) %>% 
  arrange(ls)
pas_locseg <- pas_locseg %>% 
  filter(school == "Odyssey Charter")  %>%  #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
temp <- rbind(launified, pas_locseg)
memb_locseg <- merge(memb_locseg,temp, by="school")

# San Francisco County----
sf_locseg <- locseg %>% 
  filter(county == "San Francisco") %>% 
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls) # sort from most to least representative of SF County
sf_locseg <- sf_locseg %>% 
  filter(str_detect(school, "Citizens of the World Charter School") == TRUE|
           school == "The City" |
           school == "City Language Immersion Charter"|
           school == "Gateway High"|
           school == "Gateway Middle"|
           school == "Growth Public"|
           school == "Gateway High"|
           school == "Gateway Middle"|
           str_detect(school, "High Tech") == TRUE|
           school == "Larchmont Charter"|
           school == "Odyssey Charter"|
           str_detect(school, "Summit Public") == TRUE|
           school == "Tomorrow's Leadership Collaborative (TLC) Charter"|
           school == "Urban Montessori Charter"|
           school == "Valley Charter Elementary" |
           school == "Valley Charter Middle"|
           school == "Yu Ming Charter")  %>%
  mutate(county = "San Francisco",
         district = "San Francisco Unified", # all the Gateway schools are in SF Unified
         ls_county = ls,
         p_county = p) %>% 
  select(school, district, county, ls_county, p_county)

sfusd <- locseg %>% 
  filter(district == "San Francisco Unified") %>% 
  mutual_local("subgroup","school",weight = "n", wide = TRUE) %>% 
  arrange(ls)
sfusd <- sfusd %>% 
  filter(school == "Gateway High"|
           school == "Gateway Middle")  %>%  #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
temp <- merge(sfusd,sf_locseg,by="school")
memb_locseg <- rbind(memb_locseg,temp)

# Sacramento County -----
sac_locseg <- locseg %>% 
  filter(county == "Sacramento") %>% 
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls) # sort from most to least representative of Sacramento County
sac_locseg <- sac_locseg %>% 
  filter(str_detect(school, "Citizens of the World Charter School") == TRUE|
           school == "The City" |
           school == "City Language Immersion Charter"|
           school == "Gateway High"|
           school == "Gateway Middle"|
           school == "Growth Public"|
           school == "Gateway High"|
           school == "Gateway Middle"|
           str_detect(school, "High Tech") == TRUE|
           school == "Larchmont Charter"|
           school == "Odyssey Charter"|
           str_detect(school, "Summit Public") == TRUE|
           school == "Tomorrow's Leadership Collaborative (TLC) Charter"|
           school == "Urban Montessori Charter"|
           school == "Valley Charter Elementary" |
           school == "Valley Charter Middle"|
           school == "Yu Ming Charter")  %>%
  mutate(county = "Sacramento",
         district = "Sacramento City Unified", # Growth PS
         ls_county = ls,
         p_county = p) %>% 
  select(school, district, county, ls_county, p_county)

sac_u <- locseg %>% 
  filter(district == "Sacramento City Unified") %>% 
  mutual_local("subgroup","school",weight = "n", wide = TRUE) %>% 
  arrange(ls)
sac_u <- sac_u %>% 
  filter(school == "Growth Public")  %>%  #
  mutate(ls_district = ls,
         p_district = p) %>% 
  select(school, ls_district, p_district)
temp <- merge(sac_u,sac_locseg,by="school")
memb_locseg <- rbind(memb_locseg,temp)

# San Diego County -----
sd_locseg <- locseg %>% 
  filter(county == "San Diego") %>% 
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls) # sort from most to least representative of San Diego County
sd_locseg <- sd_locseg %>% 
  filter(str_detect(school, "Citizens of the World Charter School") == TRUE|
           school == "The City" |
           school == "City Language Immersion Charter"|
           school == "Gateway High"|
           school == "Gateway Middle"|
           school == "Growth Public"|
           school == "Gateway High"|
           school == "Gateway Middle"|
           str_detect(school, "High Tech") == TRUE|
           school == "Larchmont Charter"|
           school == "Odyssey Charter"|
           str_detect(school, "Summit Public") == TRUE|
           school == "Tomorrow's Leadership Collaborative (TLC) Charter"|
           school == "Urban Montessori Charter"|
           school == "Valley Charter Elementary" |
           school == "Valley Charter Middle"|
           school == "Yu Ming Charter")  %>%
  mutate(county = "San Diego",
         ls_county = ls,
         p_county = p) %>% 
  select(school, county, ls_county, p_county)
memb_enrl %>% filter(county == "San Diego") %>% select(district) %>% unique()


sd_u <- locseg %>% 
  filter(district == "San Diego Unified") %>% 
  mutual_local("subgroup","school",weight = "n", wide = TRUE) %>% 
  arrange(ls)
memb_enrl %>% filter(district == "San Diego Unified") %>% select(school)
sd_u <- sd_u %>% 
  filter(str_detect(school, "High Tech") == TRUE)  %>%  #
  mutate(district = "San Diego Unified",
         ls_district = ls,
         p_district = p) %>% 
  select(school, district, ls_district, p_district)
memb_enrl %>% filter(county == "San Diego") %>% select(district) %>% unique()

cve <- locseg %>% 
  filter(district == "Chula Vista Elementary") %>% 
  mutual_local("subgroup","school",weight = "n", wide = TRUE) %>% 
  arrange(ls)
cve <- cve %>% 
  filter(str_detect(school, "High Tech") == TRUE)  %>%  #
  mutate(district = "Chula Vista Elementary",
         ls_district = ls,
         p_district = p) %>% 
  select(school, district, ls_district, p_district)
memb_enrl %>% filter(county == "San Diego") %>% select(district) %>% unique()

sm_u <- locseg %>% 
  filter(district == "San Marcos Unified") %>% 
  mutual_local("subgroup","school",weight = "n", wide = TRUE) %>% 
  arrange(ls)
memb_enrl %>% filter(district == "San Marcos Unified") %>% select(school)
sm_u <- sm_u %>% 
  filter(str_detect(school, "High Tech") == TRUE)  %>%  #
  mutate(district = "San Marcos Unified",
         ls_district = ls,
         p_district = p) %>% 
  select(school, district, ls_district, p_district)
memb_enrl %>% filter(county == "San Diego") %>% select(district) %>% unique()

sweetwater <- locseg %>% 
  filter(district == "Sweetwater Union High") %>% 
  mutual_local("subgroup","school",weight = "n", wide = TRUE) %>% 
  arrange(ls)
memb_enrl %>% filter(district == "Sweetwater Union High") %>% select(school)
sweetwater <- sweetwater %>% 
  filter(str_detect(school, "High Tech") == TRUE)  %>%  #
  mutate(district = "Sweetwater Union High",
         ls_district = ls,
         p_district = p) %>% 
  select(school, district, ls_district, p_district)
memb_enrl %>% filter(county == "San Diego") %>% select(district) %>% unique()

temp <- rbind(sd_u,cve,sm_u,sweetwater)
temp <- merge(temp,sd_locseg,by="school")
memb_locseg <- rbind(memb_locseg,temp)

# Santa Clara County -----
sc_locseg <- locseg %>% 
  filter(county == "Santa Clara") %>% 
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls) # sort from most to least representative of Santa Clara County
sc_locseg <- sc_locseg %>% 
  filter(str_detect(school, "Citizens of the World Charter School") == TRUE|
           school == "The City" |
           school == "City Language Immersion Charter"|
           school == "Gateway High"|
           school == "Gateway Middle"|
           school == "Growth Public"|
           school == "Gateway High"|
           school == "Gateway Middle"|
           str_detect(school, "High Tech") == TRUE|
           school == "Larchmont Charter"|
           school == "Odyssey Charter"|
           str_detect(school, "Summit Public") == TRUE|
           school == "Tomorrow's Leadership Collaborative (TLC) Charter"|
           school == "Urban Montessori Charter"|
           school == "Valley Charter Elementary" |
           school == "Valley Charter Middle"|
           school == "Yu Ming Charter")  %>%
  mutate(county = "Santa Clara",
         ls_county = ls,
         p_county = p) %>% 
  select(school, county, ls_county, p_county)
memb_enrl %>% filter(county == "Santa Clara") %>% select(district) %>% unique()

sunnyvale <- locseg %>% 
  filter(district == "Sunnyvale") %>% 
  mutual_local("subgroup","school",weight = "n", wide = TRUE) %>% 
  arrange(ls)
memb_enrl %>% filter(district == "Sunnyvale", county == "Santa Clara") %>% select(school)
sc_sunnyvale <- sunnyvale %>% 
  filter(school == "Summit Public School: Denali"|
           school == "Summit Public School Tahoma")  %>%  #
  mutate(district = "Sunnyvale",
         ls_district = ls,
         p_district = p) %>% 
  select(school, district, ls_district, p_district)
memb_enrl %>% filter(county == "Santa Clara") %>% select(district) %>% unique()

temp <- merge(sc_sunnyvale,sc_locseg,by="school")
memb_locseg <- rbind(memb_locseg,temp)

# Orange County ----
orange_locseg <- locseg %>% 
  filter(county == "Orange") %>% 
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls) # sort from most to least representative of Orange County
orange_locseg <- orange_locseg %>% 
  filter(str_detect(school, "Citizens of the World Charter School") == TRUE|
           school == "The City" |
           school == "City Language Immersion Charter"|
           school == "Gateway High"|
           school == "Gateway Middle"|
           school == "Growth Public"|
           school == "Gateway High"|
           school == "Gateway Middle"|
           str_detect(school, "High Tech") == TRUE|
           school == "Larchmont Charter"|
           school == "Odyssey Charter"|
           str_detect(school, "Summit Public") == TRUE|
           school == "Tomorrow's Leadership Collaborative (TLC) Charter"|
           school == "Urban Montessori Charter"|
           school == "Valley Charter Elementary" |
           school == "Valley Charter Middle"|
           school == "Yu Ming Charter")  %>%
  mutate(county = "Orange",
         ls_county = ls,
         p_county = p) %>% 
  select(school, county, ls_county, p_county)
memb_enrl %>% filter(county == "Orange") %>% select(district) %>% unique()

orange <- locseg %>% 
  filter(district == "Orange Unified") %>% 
  mutual_local("subgroup","school",weight = "n", wide = TRUE) %>% 
  arrange(ls)
memb_enrl %>% filter(district == "Orange Unified", county == "Orange") %>% select(school)
orange <- orange %>% 
  filter(school == "Tomorrow's Leadership Collaborative (TLC) Charter")  %>%  #
  mutate(district = "Orange Unified",
         ls_district = ls,
         p_district = p) %>% 
  select(school, district, ls_district, p_district)

temp <- merge(orange,orange_locseg,by="school")
memb_locseg <- rbind(memb_locseg,temp)

# San Mateo County ----
sm_locseg <- locseg %>% 
  filter(county == "San Mateo") %>% 
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls) # sort from most to least representative of San Mateo County
sm_locseg <- sm_locseg %>% 
  filter(str_detect(school, "Citizens of the World Charter School") == TRUE|
           school == "The City" |
           school == "City Language Immersion Charter"|
           school == "Gateway High"|
           school == "Gateway Middle"|
           school == "Growth Public"|
           school == "Gateway High"|
           school == "Gateway Middle"|
           str_detect(school, "High Tech") == TRUE|
           school == "Larchmont Charter"|
           school == "Odyssey Charter"|
           str_detect(school, "Summit Public") == TRUE|
           school == "Tomorrow's Leadership Collaborative (TLC) Charter"|
           school == "Urban Montessori Charter"|
           school == "Valley Charter Elementary" |
           school == "Valley Charter Middle"|
           school == "Yu Ming Charter")  %>%
  mutate(county = "San Mateo",
         ls_county = ls,
         p_county = p) %>% 
  select(school, county, ls_county, p_county)
memb_enrl %>% filter(county == "San Mateo") %>% select(district) %>% unique()

sunnyvale <- locseg %>% 
  filter(district == "Sunnyvale") %>% 
  mutual_local("subgroup","school",weight = "n", wide = TRUE) %>% 
  arrange(ls)
memb_enrl %>% filter(district == "Sunnyvale", county == "San Mateo") %>% select(school)
sm_sunnyvale <- sunnyvale %>% 
  filter(school == "Summit Public School: Shasta")  %>%  #
  mutate(district = "Sunnyvale",
         ls_district = ls,
         p_district = p) %>% 
  select(school, district, ls_district, p_district)

temp <- merge(sm_sunnyvale,sm_locseg,by="school")
memb_locseg <- rbind(memb_locseg,temp)


# Contra Costa County ----
cc_locseg <- locseg %>% 
  filter(county == "Contra Costa") %>% 
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls) # sort from most to least representative of Contra Costa County
cc_locseg <- cc_locseg %>% 
  filter(str_detect(school, "Citizens of the World Charter School") == TRUE|
           school == "The City" |
           school == "City Language Immersion Charter"|
           school == "Gateway High"|
           school == "Gateway Middle"|
           school == "Growth Public"|
           school == "Gateway High"|
           school == "Gateway Middle"|
           str_detect(school, "High Tech") == TRUE|
           school == "Larchmont Charter"|
           school == "Odyssey Charter"|
           str_detect(school, "Summit Public") == TRUE|
           school == "Tomorrow's Leadership Collaborative (TLC) Charter"|
           school == "Urban Montessori Charter"|
           school == "Valley Charter Elementary" |
           school == "Valley Charter Middle"|
           school == "Yu Ming Charter")  %>%
  mutate(county = "Contra Costa",
         ls_county = ls,
         p_county = p) %>% 
  select(school, county, ls_county, p_county)
memb_enrl %>% filter(county == "Contra Costa") %>% select(district) %>% unique()

sunnyvale <- locseg %>% 
  filter(district == "Sunnyvale") %>% 
  mutual_local("subgroup","school",weight = "n", wide = TRUE) %>% 
  arrange(ls)
memb_enrl %>% filter(district == "Sunnyvale", county == "Contra Costa") %>% select(school)
cc_sunnyvale <- sunnyvale %>% 
  filter(school == "Summit Public School: Tamalpais")  %>%  #
  mutate(district = "Sunnyvale",
         ls_district = ls,
         p_district = p) %>% 
  select(school, district, ls_district, p_district)

temp <- merge(cc_sunnyvale,cc_locseg,by="school")
memb_locseg <- rbind(memb_locseg,temp)


# Alameda County -----
alam_locseg <- locseg %>% 
  filter(county == "Alameda") %>% 
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls) # sort from most to least representative of Alameda County
alam_locseg <- alam_locseg %>% 
  filter(str_detect(school, "Citizens of the World Charter School") == TRUE|
           school == "The City" |
           school == "City Language Immersion Charter"|
           school == "Gateway High"|
           school == "Gateway Middle"|
           school == "Growth Public"|
           school == "Gateway High"|
           school == "Gateway Middle"|
           str_detect(school, "High Tech") == TRUE|
           school == "Larchmont Charter"|
           school == "Odyssey Charter"|
           str_detect(school, "Summit Public") == TRUE|
           school == "Tomorrow's Leadership Collaborative (TLC) Charter"|
           school == "Urban Montessori Charter"|
           school == "Valley Charter Elementary" |
           school == "Valley Charter Middle"|
           school == "Yu Ming Charter")  %>%
  mutate(county = "Alameda",
         ls_county = ls,
         p_county = p) %>% 
  select(school, county, ls_county, p_county)
memb_enrl %>% filter(county == "Alameda") %>% select(district) %>% unique()

oausd <- locseg %>% 
  filter(district == "Oakland Unified") %>% 
  mutual_local("subgroup","school",weight = "n", wide = TRUE) %>% 
  arrange(ls)
memb_enrl %>% filter(district == "Oakland Unified", county == "Alameda") %>% select(school)

oausd <- oausd %>% 
  filter(school == "Urban Montessori Charter"|
           school == "Yu Ming Charter")  %>%  #
  mutate(district = "Oakland Unified",
         ls_district = ls,
         p_district = p) %>% 
  select(school, district, ls_district, p_district)

temp <- merge(oausd,alam_locseg,by="school")
memb_locseg <- rbind(memb_locseg,temp)
memb_locseg <- memb_locseg %>% 
  select(school, district, county, ls_county, p_county, ls_district, p_district)

write.csv(memb_locseg, file = file.path('output data/ca_locseg.csv'),row.names = FALSE)

# --- ENROLLMENT BY SWD, ELL, FRPL ----