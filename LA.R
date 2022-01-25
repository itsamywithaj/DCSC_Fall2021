# --- Amy Jiravisitcul. 25 Jan 2022 ----
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
# ------ ENROLLMENT BY RACE BY SCHOOL 2020-2021 ------
# import data from https://www.louisianabelieves.com/docs/default-source/data-management/feb-2021-multi-stats-(total-by-site-and-school-system).xlsx?sfvrsn=f4b16718_6
# Metro Statistical Area = Jefferson, Orleans, Plaquemines, St. Bernard, St. Charles, St. John the Baptist, St. Tammany, and St. James parishes

enrl <- read_excel("raw data/LA_feb-2021-multi-stats-(total-by-site-and-school-system).xlsx",
                   sheet = "Total by Site")
enrl <- enrl[-c(1,2,3,4),] # remove rows with no data
colnames(enrl) = enrl[1, ] # data from header row to column name
enrl <- enrl[-1,] # remove row with column names
names(enrl)<- tolower(names(enrl))
summary(enrl)
enrl <- enrl %>% 
  select(`school system name`,sitename, `total students`,amind, asian, black, hispanic, hawpi, white, multiple,
         `%lep`, `ed%`,sitecd,`charter type`, `parish code`)
names(enrl)[c(1:3,11,12)] <- c("district","school","total","lep","ed")
enrl <- enrl %>% 
  mutate(total = as.numeric(total),
         amind = as.numeric(amind),
         perc_amind = amind/total,
         asian = as.numeric(asian) + as.numeric(hawpi),
         perc_asian = asian/total,
         black = as.numeric(black),
         perc_black = black/total,
         hispanic = as.numeric(hispanic),
         perc_hispanic = hispanic/total,
         white = as.numeric(white),
         perc_white = white/total,
         multiple = as.numeric(multiple),
         perc_multiple = multiple/total,
         perc_lep = as.numeric(lep),
         perc_ed = as.numeric(ed)) %>% 
  select(district, school, total, perc_amind, perc_asian, perc_black, perc_hispanic, 
         perc_white, perc_multiple, perc_lep, perc_ed, amind, asian, black, hispanic, hawpi, white, multiple,
         sitecd, `charter type`,`parish code`)
memb <- enrl %>% 
  filter(school == "International High School of New Orleans"|
           school == "International School of Louisiana"|
           school == "Kenner Discovery Health Sciences Academy"|
           school == "Lycee Francais de la Nouvelle-Orleans"|
           school == "Morris Jeff Community School"|
           str_detect(school,"Edward Hynes Charter School")|
           school == "Bricolage Academy"|
           school == "Homer A. Plessy Community School")

# --- DISTRICT-LEVEL SEGREGATION (PARISH) ------
locseg <- enrl %>% 
  select(school, district, `parish code`, amind, asian, black, hispanic, white,multiple) %>% 
  gather(subgroup, n, amind:multiple, factor_key=TRUE) # change from wide to long

unique(memb$district)
jeff <- locseg %>% 
  filter(district == "Jefferson Parish") %>% 
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls)
memb_locseg <- jeff %>% 
  filter(school == "International High School of New Orleans"|
           school == "International School of Louisiana"|
           school == "Kenner Discovery Health Sciences Academy"|
           school == "Lycee Francais de la Nouvelle-Orleans"|
           school == "Morris Jeff Community School"|
           str_detect(school,"Edward Hynes Charter School")|
           school == "Bricolage Academy"|
           school == "Homer A. Plessy Community School") %>% 
  mutate(ls_district = ls,
         p_district = p,
         district = "Jefferson Parish") %>% 
  select(school, district, ls_district, p_district)

unique(memb$district)
type2 <- locseg %>% 
  filter(district == "Type 2 Charters") %>%
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls)
x <- type2 %>% 
  filter(school == "International High School of New Orleans"|
           school == "International School of Louisiana"|
           school == "Kenner Discovery Health Sciences Academy"|
           school == "Lycee Francais de la Nouvelle-Orleans"|
           school == "Morris Jeff Community School"|
           str_detect(school,"Edward Hynes Charter School")|
           school == "Bricolage Academy"|
           school == "Homer A. Plessy Community School") %>% 
  mutate(ls_district = ls,
         p_district = p,
         district = "Type 2 Charters") %>% 
  select(school, district, ls_district, p_district)
memb_locseg <- rbind(x,memb_locseg)

unique(memb$district)
orleans <- locseg %>% 
  filter(district == "Orleans Parish") %>%
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls)
x <- orleans %>% 
  filter(school == "International High School of New Orleans"|
           school == "International School of Louisiana"|
           school == "Kenner Discovery Health Sciences Academy"|
           school == "Lycee Francais de la Nouvelle-Orleans"|
           school == "Morris Jeff Community School"|
           str_detect(school,"Edward Hynes Charter School")|
           school == "Bricolage Academy"|
           school == "Homer A. Plessy Community School") %>% 
  mutate(ls_district = ls,
         p_district = p,
         district = "Orleans Parish") %>% 
  select(school, district, ls_district, p_district)
memb_locseg <- rbind(x,memb_locseg)

# ---- GREATER METRO NOLA ----
# Metro Statistical Area = Jefferson, Orleans, Plaquemines, St. Bernard, St. Charles, St. John the Baptist, St. Tammany, and St. James parishes + Type 2 Charters
unique(locseg$district)
metro <- locseg %>% 
  filter(district == "Orleans Parish"|
           district == "Jefferson Parish"|
           district == "Plaquemines Parish"|
           district == "St. Bernard Parish"|
           district == "St. Charles Parish"|
           district == "St. John the Baptist Parish"|
           district == "St. Tammany Parish"|
           district == "St. James Parish"|
           district == "Type 2 Charters") %>%
  mutual_local("subgroup","school", weight = "n", wide = TRUE) %>% 
  arrange(ls)
x <- metro %>% 
  filter(school == "International High School of New Orleans"|
           school == "International School of Louisiana"|
           school == "Kenner Discovery Health Sciences Academy"|
           school == "Lycee Francais de la Nouvelle-Orleans"|
           school == "Morris Jeff Community School"|
           str_detect(school,"Edward Hynes Charter School")|
           school == "Bricolage Academy"|
           school == "Homer A. Plessy Community School") %>% 
  mutate(ls_metro = ls,
         p_metro = p,
         metro = "NOLA metro") %>% 
  select(school, metro, ls_metro, p_metro)
memb <- merge(memb, merge(memb_locseg,x,by="school"),by="school")

write.csv(memb, file = file.path("output data/la_enrl.csv"), row.names = FALSE)

# ------ COMPARISON ENTITIES ------
# import data from https://www.louisianabelieves.com/docs/default-source/data-management/feb-2021-multi-stats-(total-by-site-and-school-system).xlsx?sfvrsn=f4b16718_6
# Metro Statistical Area = Jefferson, Orleans, Plaquemines, St. Bernard, St. Charles, St. John the Baptist, St. Tammany, and St. James parishes

enrl <- read_excel("raw data/LA_feb-2021-multi-stats-(total-by-site-and-school-system).xlsx",sheet = "Total by School System")
enrl <- enrl[-c(1,2,3,4),] # remove rows with no data
colnames(enrl) = enrl[1, ] # data from header row to column name
enrl <- enrl[-1,] # remove row with column names
names(enrl)<- tolower(names(enrl))
summary(enrl)
enrl <- enrl %>% 
  select(`school system name`, `total students`,amind, asian, black, hispanic, hawpi, white, multiple,
         `%lep`, `ed%`)
names(enrl)[c(1,2,10,11)] <- c("district","total","lep","ed")
metro_totals <- enrl %>% 
  filter(district == "Orleans Parish"|
           district == "Jefferson Parish"|
           district == "Plaquemines Parish"|
           district == "St. Bernard Parish"|
           district == "St. Charles Parish"|
           district == "St. John the Baptist Parish"|
           district == "St. Tammany Parish"|
           district == "St. James Parish"|
           district == "Type 2 Charters")
write.csv(metro_totals,file = file.path('output data/la_entities.csv'),row.names = FALSE)

# ---- SPECIAL EDUCATION ENROLLMENT ----
# Data from https://louisianabelieves.com/docs/default-source/academics/2021-feb-sped-rates-by-lea-site_public.xlsx?sfvrsn=6c9b6418_2

sped <- read_excel("raw data/LA_2021-feb-sped-rates-by-lea-site_public.xlsx",sheet = "Feb Total by Site")
sped <- sped[-c(1:5),] # remove rows with no data
colnames(sped) = sped[1, ] # data from header row to column name
sped <- sped[-1,] # remove row with column names
names(sped)<- tolower(names(sped))
summary(sped)
names(sped)[c(3,5:6)] <- c("district","school","swd")

sped <- sped %>% 
  mutate(swd = case_when(is.na(as.numeric(swd)) ~ .01 * as.numeric(gsub("[^0-9.-]", "", swd)),
                         TRUE ~ as.numeric(swd))) %>% 
  na.omit() %>% 
  select(district, school, swd)
memb_sped <- sped %>% 
  filter(school == "International High School of New Orleans"|
           school == "International School of Louisiana"|
           school == "Kenner Discovery Health Sciences Academy"|
           school == "Lycee Francais de la Nouvelle-Orleans"|
           school == "Morris Jeff Community School"|
           str_detect(school,"Edward Hynes Charter School")|
           school == "Bricolage Academy"|
           school == "Homer A. Plessy Community School")

comp_sped <- read_excel("raw data/LA_2021-feb-sped-rates-by-lea-site_public.xlsx",sheet = "Feb Total by LEA")
comp_sped <- comp_sped[-c(1:5),] # remove rows with no data
colnames(comp_sped) = comp_sped[1, ] # data from header row to column name
comp_sped <- comp_sped[-1,-c(6:7)] # remove row with column names and remove empty columns
names(comp_sped)<- tolower(names(comp_sped))
names(comp_sped)[c(3:4)] <- c("district","swd")

type2schools <- data.frame(charters = unique(type2$school))
View(type2schools)
unique(comp_sped$district)
comp_sped <- comp_sped %>% 
  filter(district == "Jefferson Parish"|
           district == "Orleans Parish"|
           district == "Plaquemines Parish"|
           district == "St. Bernard Parish"|
           district == "St. Charles Parish"|
           district == "St. John the Baptist Parish"|
           district == "St. Tammany Parish"|
           district == "St. James Parish"|
           district == "Type 2 Charters"|
           match(district,unique(type2$school))>0) %>% 
  mutate(swd = case_when(is.na(as.numeric(swd)) ~ .01 * as.numeric(gsub("[^0-9.-]", "", swd)),
                         TRUE ~ as.numeric(swd)))%>% 
  select(district, swd)
schools <- enrl %>% # go back to variable enrl in lines 15-29
  mutate(total = as.numeric(total),
         district = school) %>% 
  select(district, total)

systems <- enrl %>% # go back to variable enrl in lines 159-168
  mutate(total = as.numeric(total)) %>% 
  select(district, total)
z <- merge(comp_sped,schools, by="district")
y <- merge(comp_sped, systems,by="district")
comp_sped <- rbind(z,y)

write.csv(memb_sped, file = file.path('output data/la_sped_memb.csv'),row.names = FALSE)
write.csv(comp_sped, file = file.path('output data/la_entities_sped.csv'),row.names = FALSE)
#------ END -----
