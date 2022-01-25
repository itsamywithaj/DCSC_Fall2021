# --- Amy Jiravisitcul. 21 Jan 2022 ----
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

# --- loading enrollment data ----
# https://osse.dc.gov/page/dc-school-report-card-resource-library 
# https://osse.dc.gov/sites/default/files/dc/sites/osse/page_content/attachments/2021%20DC%20School%20Report%20Card%20Aggregate%20Enrollment%20Metrics_0.xlsx updated 8/2021

enrl <- read_excel("raw data/DC_2021 DC School Report Card Aggregate Enrollment Metrics_0.xlsx", sheet = "Audit Population")
str(enrl)
names(enrl) <- tolower(names(enrl))
names(enrl) <- c("lea_code","lea_name","school_code","school","entity","ward","subgroup","grade","perc_enrolled","count","total","year")
sort(levels(as.factor(enrl$subgroup)))
summary(enrl$count)
enrl <- enrl %>% 
  mutate(entity = as.factor(entity),
         ward = as.factor(ward),
         grade = as.factor(grade),
         subgroup = as.factor(subgroup),
         perc_enrolled = .01* as.numeric(gsub("[^0-9.-]", "",perc_enrolled))) %>%  # remove non numeric characters
  filter(subgroup == "American Indian/Alaskan Native"|
           subgroup == "Asian"|
           subgroup == "Black/African-American"|
           subgroup == "Hispanic/Latino of any race"|
           subgroup == "Native Hawaiian/Other Pacific Islander"|
           subgroup == "White"|
           subgroup == "Two or more races"|
           subgroup == "At-Risk"|
           subgroup == "English Learners"|
           subgroup == "Students with Disabilities",
         grade == "All") %>% 
  mutate(count = case_when(is.na(as.numeric(count)) ~ perc_enrolled*total,
                           TRUE ~ as.numeric(count))) %>% 
  select(lea_code,lea_name,school_code,school, entity, ward, subgroup, total, perc_enrolled, count)
levels(as.factor(enrl$lea_name))
dc_full <- enrl %>% 
  filter(lea_name == "District of Columbia Public Schools",
         school == "All")
levels(enrl$subgroup) <- c("","amind","asian","at-risk","black","","ell",
                           "hispanic","","","hawpi","swd","multiple","white")
names(enrl)
dc_enrl <- enrl %>% 
  spread(key = subgroup,
         value = count,
         fill = 0) %>% 
  mutate(asian = asian + hawpi) %>% 
  select(lea_name, school, entity, ward, total, amind, asian, black, hispanic, white, multiple, `at-risk`, ell, swd)

# --- ENROLLMENT BY RACE BY SCHOOL -----
DT <- data.table(dc_enrl %>% select(school, amind:swd))
schools_enrl <- DT[, lapply(.SD, sum), by=list(school)] # Summing the rows for all schools
DTA <- unique(dc_enrl %>%
                filter(str_detect(ward,"All")==FALSE) %>%
                select(lea_name,school,entity,ward, total)) # every school's LEA, entity type, and ward
schools_enrl <- merge(schools_enrl,DTA,by="school")

memb_enrl <- schools_enrl %>% 
  filter(str_detect(school, "Capital City PCS")==TRUE|
           school == "District of Columbia International School"|
           str_detect(school,"E.L. Haynes PCS")==TRUE |
           str_detect(school,"Elsie Whitlow Stokes Community Freedom PCS")==TRUE |
           school == "Inspired Teaching Demonstration PCS"|
           str_detect(school,"Lee Montessori PCS")|
           str_detect(school, "Washington Latin PCS") == TRUE|
           school == "Social Justice PCS"|
           school == "Washington Yu Ying PCS"|
           str_detect(school, "Two Rivers PCS") == TRUE) %>%
  mutate(p_amind = amind/total,
         p_asian = asian/total,
         p_black = black/total,
         p_hispanic = hispanic/total,
         p_multiple = multiple/total,
         p_white = white/total,
         p_ell= ell/total,
         p_swd = swd/total,
         `p_at-risk` = `at-risk`/total)
write.csv(memb_enrl,file = file.path("output data/dc_enrl.csv"),row.names = FALSE)

# --- WARD-LEVEL PERCENTAGES BY RACE -----
DT <- data.table(schools_enrl %>% select(ward,total, amind:swd))
wards_enrl <- DT[, lapply(.SD, sum), by=list(ward)]
wards_enrl <- wards_enrl %>% 
  mutate(w_amind = amind/total,
         w_asian = asian/total,
         w_black = black/total,
         w_hispanic = hispanic/total,
         w_multiple = multiple/total,
         w_white = white/total,
         w_ell= ell/total,
         w_swd = swd/total,
         `w_at-risk` = `at-risk`/total,
         w_total = total) %>% arrange(ward) %>% 
  select(ward, w_total, w_amind, w_asian, w_black, w_hispanic, w_multiple, w_white,`w_at-risk`, w_ell, w_swd)

# ----- LOCAL SEGREGATION -----
dc_locseg <- schools_enrl %>% 
  select(school, entity, ward, amind, asian, black, hispanic, multiple, white) %>% 
  gather(subgroup, n, amind:white, factor_key=TRUE) # change from wide to long

unique(memb_enrl$ward)
w_1 <- dc_locseg %>% 
  filter(ward == "1") %>% 
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls)
memb_locseg <- w_1 %>% 
  filter(str_detect(school, "Capital City PCS")==TRUE|
           school == "District of Columbia International School"|
           str_detect(school,"E.L. Haynes PCS")==TRUE |
           str_detect(school,"Elsie Whitlow Stokes Community Freedom PCS")==TRUE |
           school == "Inspired Teaching Demonstration PCS"|
           str_detect(school,"Lee Montessori PCS")|
           str_detect(school, "Washington Latin PCS") == TRUE|
           school == "Social Justice PCS"|
           school == "Washington Yu Ying PCS"|
           str_detect(school, "Two Rivers PCS") == TRUE)  %>%
  mutate(ward = "1",
         ls_ward = ls,
         p_ward = p) %>% 
  select(school, ward, ls_ward, p_ward)

unique(memb_enrl$ward)
w_4 <- dc_locseg %>% 
  filter(ward == "4") %>% 
  mutual_local("subgroup","school",weight = "n", wide = TRUE) %>% 
  arrange(ls)
g <- w_4 %>% 
  filter(str_detect(school, "Capital City PCS")==TRUE|
           school == "District of Columbia International School"|
           str_detect(school,"E.L. Haynes PCS")==TRUE |
           str_detect(school,"Elsie Whitlow Stokes Community Freedom PCS")==TRUE |
           school == "Inspired Teaching Demonstration PCS"|
           str_detect(school,"Lee Montessori PCS")|
           str_detect(school, "Washington Latin PCS") == TRUE|
           school == "Social Justice PCS"|
           school == "Washington Yu Ying PCS"|
           str_detect(school, "Two Rivers PCS") == TRUE)  %>%
  mutate(ward = "4",
         ls_ward = ls,
         p_ward = p) %>% 
  select(school, ward, ls_ward, p_ward)
memb_locseg <- rbind(memb_locseg,g) # Add rows from Ward 4 to Ward 1

unique(memb_enrl$ward)
w_5 <- dc_locseg %>% 
  filter(ward == "5") %>% 
  mutual_local("subgroup","school",weight = "n", wide = TRUE) %>% 
  arrange(ls)
g <- w_5 %>% 
  filter(str_detect(school, "Capital City PCS")==TRUE|
           school == "District of Columbia International School"|
           str_detect(school,"E.L. Haynes PCS")==TRUE |
           str_detect(school,"Elsie Whitlow Stokes Community Freedom PCS")==TRUE |
           school == "Inspired Teaching Demonstration PCS"|
           str_detect(school,"Lee Montessori PCS")|
           str_detect(school, "Washington Latin PCS") == TRUE|
           school == "Social Justice PCS"|
           school == "Washington Yu Ying PCS"|
           str_detect(school, "Two Rivers PCS") == TRUE)  %>%
  mutate(ward = "5",
         ls_ward = ls,
         p_ward = p) %>% 
  select(school, ward, ls_ward, p_ward)
memb_locseg <- rbind(memb_locseg,g) # Add rows from Ward 5 to running list

unique(memb_enrl$ward)
w_6 <- dc_locseg %>% 
  filter(ward == "6") %>% 
  mutual_local("subgroup","school",weight = "n", wide = TRUE) %>% 
  arrange(ls)
g <- w_6 %>% 
  filter(str_detect(school, "Capital City PCS")==TRUE|
           school == "District of Columbia International School"|
           str_detect(school,"E.L. Haynes PCS")==TRUE |
           str_detect(school,"Elsie Whitlow Stokes Community Freedom PCS")==TRUE |
           school == "Inspired Teaching Demonstration PCS"|
           str_detect(school,"Lee Montessori PCS")|
           str_detect(school, "Washington Latin PCS") == TRUE|
           school == "Social Justice PCS"|
           school == "Washington Yu Ying PCS"|
           str_detect(school, "Two Rivers PCS") == TRUE)  %>%
  mutate(ward = "6",
         ls_ward = ls,
         p_ward = p) %>% 
  select(school, ward, ls_ward, p_ward)
memb_locseg <- rbind(memb_locseg,g) # Add rows from Ward 6 to running list

unique(memb_enrl$ward)
w_7 <- dc_locseg %>% 
  filter(ward == "7") %>% 
  mutual_local("subgroup","school",weight = "n", wide = TRUE) %>% 
  arrange(ls)
g <- w_7 %>% 
  filter(str_detect(school, "Capital City PCS")==TRUE|
           school == "District of Columbia International School"|
           str_detect(school,"E.L. Haynes PCS")==TRUE |
           str_detect(school,"Elsie Whitlow Stokes Community Freedom PCS")==TRUE |
           school == "Inspired Teaching Demonstration PCS"|
           str_detect(school,"Lee Montessori PCS")|
           str_detect(school, "Washington Latin PCS") == TRUE|
           school == "Social Justice PCS"|
           school == "Washington Yu Ying PCS"|
           str_detect(school, "Two Rivers PCS") == TRUE)  %>%
  mutate(ward = "7",
         ls_ward = ls,
         p_ward = p) %>% 
  select(school, ward, ls_ward, p_ward)
memb_locseg <- rbind(memb_locseg,g) # Add rows from Ward 7 to running list

unique(memb_enrl$ward)
w_8 <- dc_locseg %>% 
  filter(ward == "8") %>% 
  mutual_local("subgroup","school",weight = "n", wide = TRUE) %>% 
  arrange(ls)
g <- w_8 %>% 
  filter(str_detect(school, "Capital City PCS")==TRUE|
           school == "District of Columbia International School"|
           str_detect(school,"E.L. Haynes PCS")==TRUE |
           str_detect(school,"Elsie Whitlow Stokes Community Freedom PCS")==TRUE |
           school == "Inspired Teaching Demonstration PCS"|
           str_detect(school,"Lee Montessori PCS")|
           str_detect(school, "Washington Latin PCS") == TRUE|
           school == "Social Justice PCS"|
           school == "Washington Yu Ying PCS"|
           str_detect(school, "Two Rivers PCS") == TRUE)  %>%
  mutate(ward = "8",
         ls_ward = ls,
         p_ward = p) %>% 
  select(school, ward, ls_ward, p_ward)
memb_locseg <- rbind(memb_locseg,g) # Add rows from Ward 8 to running list
memb_locseg <- merge(wards_enrl,memb_locseg,by="ward") # merge to include the ward totals and ward race percentages
memb_enrl <- merge(memb_enrl,memb_locseg,by=c("school","ward"))


# ----- DC-WIDE SEGREGATION ----
dc_memb_ls <- dc_locseg %>% 
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls) %>% 
  filter(str_detect(school, "Capital City PCS")==TRUE|
           school == "District of Columbia International School"|
           str_detect(school,"E.L. Haynes PCS")==TRUE |
           str_detect(school,"Elsie Whitlow Stokes Community Freedom PCS")==TRUE |
           school == "Inspired Teaching Demonstration PCS"|
           str_detect(school,"Lee Montessori PCS")|
           str_detect(school, "Washington Latin PCS") == TRUE|
           school == "Social Justice PCS"|
           school == "Washington Yu Ying PCS"|
           str_detect(school, "Two Rivers PCS") == TRUE) %>% 
  mutate(ls_dc = ls,
         p_dc = p) %>% select(school, ls_dc,p_dc)
dc_race <- schools_enrl %>% 
  select(school, total, amind:swd)
dc_sum <- t(colSums(dc_race[,2:11], na.rm = FALSE, dims = 1))
dc_sum <- data.frame(dc_sum)
dc_sum <- dc_sum %>% 
  mutate(dc_amind = amind/total,
         dc_asian = asian/total,
         dc_black = black/total,
         dc_hispanic = hispanic/total,
         dc_white = white/total, 
         dc_multiple = multiple/total,
         `dc_at.risk` = `at.risk`/total,
         dc_ell = ell/total,
         dc_swd= swd/total,
         dc_total = total,
         dummy = "dummy") %>% 
  select(dummy,dc_total, dc_amind, dc_asian, dc_black, dc_hispanic,dc_white,dc_multiple,`dc_at.risk`,dc_ell, dc_swd)
memb_enrl <- memb_enrl %>% mutate(dummy = "dummy")
memb_enrl <- merge(memb_enrl,dc_sum,by="dummy")
memb_enrl <- memb_enrl %>% select(-dummy)
memb_enrl <- merge(memb_enrl, dc_memb_ls, by="school")
write.csv(memb_enrl,file = file.path("output data/dc_enrl.csv"),row.names = FALSE)
