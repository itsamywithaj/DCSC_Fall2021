# --- Amy Jiravisitcul. 14 Dec 2021 ----
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
enrl <- enrl %>% mutate("K" = as.integer(as.logical(kdgn)), # change grades to logical
                "1" = as.integer(as.logical(gr_1)),
                "2" = as.integer(as.logical(gr_2)),
                "3" = as.integer(as.logical(gr_3)),
                "4" = as.integer(as.logical(gr_4)),
                "5" = as.integer(as.logical(gr_5)),
                "6" = as.integer(as.logical(gr_6)),
                "7" = as.integer(as.logical(gr_7)),
                "8" = as.integer(as.logical(gr_8)),
                "9" = as.integer(as.logical(gr_1)),
                "10" = as.integer(as.logical(gr_10)),
                "11" = as.integer(as.logical(gr_11)),
                "12" = as.integer(as.logical(gr_12))) %>% 
  select(school, county, district, gender, ethnic, enr_total, K:"12")
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
  select(school, county, district, unreported, amind, asian, hisp, black, white, mult, total,K:"12")

DT <- data.table(enrl)
enrl <- DT[, lapply(.SD, sum), by=list(school, district, county)] # Summing the rows for all genders
enrl <- enrl %>% 
  gather(grade,dummy,K:"12",factor_key = TRUE) %>% 
  filter(dummy > 0) %>% 
  mutate(grade = as.character(grade))
str(enrl)
enrl <- enrl %>% 
  mutate(grade = case_when(is.na(as.numeric(grade)) ~ 0,
                           TRUE ~ as.numeric(grade))) %>%
  arrange(school, grade)

library(plyr)
a <- ddply(enrl, .(school, district, county, unreported,
                   amind, asian, hisp, black, white, mult, total), summarize,
           min_grade = min(grade), # make columns for the grade range
           max_grade = max(grade)) 
enrl <-a %>% 
  mutate(min_grade = as.factor(min_grade),
         max_grade = as.factor (max_grade))
levels(enrl$min_grade)[1] <- "K"
levels(enrl$max_grade)[1] <- "K"
cumulative <- enrl # student counts

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
                              school == "Summit Public School K2"|
                                school == "Summit Public School: Tamalpais" ~ "West Contra Costa Unified",
                              school == "Summit Public School: Denali" ~ "Sunnyvale",
                              school == "Summit Public School: Shasta" ~ "Jefferson Union High",
                              school == "Summit Public School: Tahoma" ~ "East Side Union High",
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
  select(school, county, district, min_grade, max_grade, total, p_amind, p_asian, p_black, 
         p_hisp, p_white, p_mult, p_unreported)


# Percentages for districts and counties
memb_enrl %>% select(county) %>% unique()
cumulative_county %>% select(county) %>% unique()
DT <- data.table(cumulative %>% select(county, unreported:total))
cumulative_county <- DT[, lapply(.SD, sum), by=list(county)] # Summing the rows for all counties
cumulative_county <- cumulative_county %>%
  filter(county == "San Diego"|
           county == "Los Angeles"|
           county == "Alameda"|
           county == "Orange"|
           county == "Sacramento"|
           county == "San Francisco"|
           county == "Contra Costa"|
           county == "Santa Clara"|
           county == "San Mateo") %>% 
  select(county, unreported:total)
cumulative_county <- cumulative_county %>% 
  mutate(county_total = total,
            county_amind = amind/county_total,
            county_asian = asian/county_total,
            county_black = black/county_total,
            county_hisp = hisp/county_total,
            county_white = white/county_total,
            county_mult = mult/county_total) %>% 
  select(county, county_total:county_mult)
memb_enrl <- merge(memb_enrl,cumulative_county,by="county")

memb_enrl %>% select(district) %>% unique()
DT <- data.table(cumulative %>% select(district, unreported:total))
cumulative_district <- DT[, lapply(.SD, sum), by=list(district)] # Summing the rows for all counties
cumulative_district <- cumulative_district %>%
  filter(district == "Oakland Unified"|
           district == "West Contra Costa Unified"|
           district == "Sunnyvale"|
           district == "Los Angeles Unified"|
           district == "Pasadena Unified"|
           district == "Orange Unified"|
           district == "Sacramento City Unified"|
           district == "Chula Vista Elementary"|
           district == "San Diego Unified"|
           district == "San Marcos Unified"|
           district == "Sweetwater Union High"|
           district == "San Francisco Unified"|
           district == "Jefferson Union High"|
           district == "East Side Union High") %>% 
  select(district, unreported:total)
cumulative_district <- cumulative_district %>% 
  mutate(district_total = total,
            district_amind = amind/district_total,
            district_asian = asian/district_total,
            district_black = black/district_total,
            district_hisp = hisp/district_total,
            district_white = white/district_total,
            district_mult = mult/district_total) %>% 
  select(district, district_total:district_mult)
memb_enrl <- merge(memb_enrl,cumulative_district,by="district")


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
  filter(school == "Summit Public School: Denali")  %>%  #
  mutate(district = "Sunnyvale",
         ls_district = ls,
         p_district = p) %>% 
  select(school, district, ls_district, p_district)
memb_enrl %>% filter(county == "Santa Clara") %>% select(district) %>% unique()

esuh <- locseg %>% 
  filter(district == "East Side Union High") %>% 
  mutual_local("subgroup","school",weight = "n", wide = TRUE) %>% 
  arrange(ls)
memb_enrl %>% filter(district == "East Side Union High", county == "Santa Clara") %>% select(school)
sc_esuh <- esuh %>% 
  filter(school =="Summit Public School: Tahoma") %>% 
  mutate(district = "East Side Union High",
         ls_district = ls,
         p_district = p) %>% 
  select(school, district, ls_district, p_district)

temp <- merge(sc_sunnyvale,sc_locseg,by="school")
temp2 <- merge(sc_esuh, sc_locseg, by="school")
memb_locseg <- rbind(memb_locseg,temp, temp2)

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

juh <- locseg %>% 
  filter(district == "Jefferson Union High") %>% 
  mutual_local("subgroup","school",weight = "n", wide = TRUE) %>% 
  arrange(ls)
memb_enrl %>% filter(district == "Jefferson Union High", county == "San Mateo") %>% select(school)
sm_juh <- juh %>% 
  filter(school == "Summit Public School: Shasta")  %>%  #
  mutate(district = "Jefferson Union High",
         ls_district = ls,
         p_district = p) %>% 
  select(school, district, ls_district, p_district)

temp <- merge(sm_juh,sm_locseg,by="school")
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

wccu <- locseg %>% 
  filter(district == "West Contra Costa Unified") %>% 
  mutual_local("subgroup","school",weight = "n", wide = TRUE) %>% 
  arrange(ls)
memb_enrl %>% filter(district == "West Contra Costa Unified", county == "Contra Costa") %>% select(school)
cc_wccu <- wccu %>% 
  filter(str_detect(school, "Summit Public"))  %>%  #
  mutate(district = "West Contra Costa Unified",
         ls_district = ls,
         p_district = p) %>% 
  select(school, district, ls_district, p_district)

temp <- merge(cc_wccu,cc_locseg,by="school")
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
# EL data file https://www.cde.ca.gov/ds/ad/fileselsch.asp  ----
# Data Reporting Office dro@cde.ca.gov, Last Reviewed: 29 April 2021
ell <- read.delim('raw data/CA_fileselsch.txt')
names(ell) <- tolower(names(ell))
library(dplyr)
detach(package:plyr)
school_ell <- ell %>%
  select(county, district, school, total_el) %>% 
  group_by(school) %>% 
  summarise(ell = sum(total_el))
county_ell <- ell %>% 
  select(county, total_el) %>% 
  group_by(county) %>% 
  summarize(ell_county = sum(total_el))
district_ell <- ell %>% 
  select(district, total_el) %>% 
  group_by(district) %>% 
  summarize(ell_dist = sum(total_el))

memb_ell <- school_ell %>% 
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
           school == "Yu Ming Charter")
z <- memb_enrl %>% 
  select(school, district, county, total, county_total, district_total)
memb_ell <- merge(z,memb_ell,by="school")
memb_ell <- merge(memb_ell,county_ell,by="county")
memb_ell <- merge(memb_ell,district_ell, by="district")
memb_ell <- memb_ell %>% 
  mutate(ell = ell/total,
         ell_dist = ell_dist/district_total,
         ell_county = ell_county/county_total) %>% 
  select(school, ell:ell_dist)
memb_enrl <- merge(memb_enrl,memb_ell,by="school")


# FRPM Data file at https://www.cde.ca.gov/ds/ad/filessp.asp ----
frpm <- read_excel('raw data/CA_frpm2021.xlsx', sheet = "FRPM School-Level Data ")
read_excel('raw data/CA_frpm2021.xlsx',sheet = "Data Field Descriptions") %>% View()
colnames(frpm) <- frpm[1,]
frpm <- frpm[-1,]
names(frpm) <- tolower(names(frpm))
names(frpm)[c(5:7, 21)] <- c("county","district","school","frpm") # select FRPM count
frpm$frpm <- as.numeric(frpm$frpm) # change to numeric
school_frpm <- frpm %>%
  select(county, district, school, frpm) %>% 
  group_by(school) %>% 
  summarise(frpm = sum(frpm))
county_frpm <- frpm %>% 
  select(county, frpm) %>% 
  group_by(county) %>% 
  summarize(frpm_county = sum(frpm))
district_frpm <- frpm %>% 
  select(district, frpm) %>% 
  group_by(district) %>% 
  summarize(frpm_dist = sum(frpm))
memb_frpm <- school_frpm %>% 
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
           school == "Yu Ming Charter")
z <- memb_enrl %>% 
  select(school, district, county, total, county_total, district_total)
memb_frpm <- merge(z,memb_frpm,by="school")
memb_frpm <- merge(memb_frpm,county_frpm,by="county")
memb_frpm <- merge(memb_frpm,district_frpm, by="district")
memb_frpm <- memb_frpm %>% 
  mutate(frpm = frpm/total,
         frpm_dist = frpm_dist/district_total,
         frpm_county = frpm_county/county_total) %>% 
  select(school, frpm:frpm_dist)
memb_enrl <- merge(memb_enrl,memb_frpm,by="school")

write.csv(memb_enrl, file = file.path('output data/ca_enrl.csv'),row.names = FALSE)

# SWD data -----
# https://www.kidsdata.org/topic/95/special-education/table#fmt=1146&loc=171,364,365,344,368,265,4,59,2,127&tf=124&sortType=asc
swd <- read.csv('raw data/Kidsdata-special-education-Special-Education-Enrollment.csv')
View(swd)
names(swd) <- tolower(names(swd))
swd <- swd %>% 
  filter(locationtype == "School District"|
           locationtype == "County",
         timeframe == "2019") %>% 
  spread(key = dataformat,
         value = data,
         fill = 0) %>%
  mutate(swd = as.numeric(gsub(",","",Percent))*.01) %>% na.omit() %>% # NAs from suppressed data
  select(locationtype, location, parentcounty, timeframe, swd)
swd <- swd %>% arrange(location)
sort(unique(memb_enrl$district))
swd_dist <- swd %>% 
  filter(locationtype=="School District",
         location == "Chula Vista Elementary"|
           location == "East Side Union High"|
           location == "Jefferson Union High"|
           location == "Los Angeles Unified"|
           location == "Oakland Unified"|
           location == "Orange Unified"|
           location == "Pasadena Unified"|
           location == "Sacramento City Unified"|
           location == "San Diego Unified"|
           location == "San Francisco Unified"|
           location == "San Marcos Unified"|
           location == "Sunnyvale"|
           location == "Sweetwater Union High"|
           location == "West Contra Costa Unified") %>% 
  mutate(district = location,
         swd_dist = swd) %>% 
  select(district, swd_dist)
sort(unique(memb_enrl$county))
swd_county <- swd %>% 
  filter(locationtype=="County") %>% 
  mutate(county = gsub(" County","",location),
         swd_county = swd) %>% 
  filter(county == "Alameda"|
           county == "Contra Costa"|
           county == "Los Angeles"|
           county == "Orange"|
           county == "Sacramento"|
           county == "San Diego"|
           county == "San Francisco"|
           county == "San Mateo"|
           county == "Santa Clara") %>% 
  select(county, swd_county)
memb_enrl <- merge(memb_enrl,swd_dist,by="district")
memb_enrl <- merge(memb_enrl, swd_county, by= "county")

write.csv(memb_enrl, file = file.path('output data/ca_enrl.csv'),row.names = FALSE)
