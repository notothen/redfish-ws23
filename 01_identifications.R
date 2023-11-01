#### Script to explore redfish identification data 
#### Greenland Institute of Natural Resources (GINR)
## 03/04/2023
## H. Christiansen
## v1.2

#### load packages
library(here) # to shorten file paths
library(tidyverse) # for data wrangling
library(readxl) # to read excel files
library(RODBC) # to query MS Access database

#### query TA05 data
#####
### read IDs
id <- read_excel(here("redfish-ws23_non-gh/IDs_all.xlsx"))

### read genetic IDs
gen <- read_excel(here("data/morphometry_IDs.xlsx"))

### join
dta <- full_join(id, gen, join_by(ID == ID))

### query DataWarehouse
dw <- odbcConnectAccess2007(here("../43_catchability_comparison/data/DW-5.02.accdb"))

### load length data
l00 <- sqlFetch(dw, "DwIndividuallist")
l00 <- l00 %>% filter(Species %in% c("REG", "RED", "REB", "REV")) %>% 
  filter(Year == 2022) %>% filter(Ship == "TA")

### save files
write.table(l00, here("data/redfish_lengths_raw.csv"), sep = ",",
            col.names = T, row.names = F)

### close connection and remove intermediate files
close(dw)
rm(l00, id, gen)

### add length data from DataWarehouse to workshop data
lengths <- read.csv(here("data/redfish_lengths_raw.csv"))
head(lengths)

length(levels(as.factor(dta$TA_ID)))
lengths <- lengths %>% filter(IndividualNumber %in% dta$TA_ID) %>% select (IndividualNumber, Length)
dta <- full_join(dta, lengths, join_by(TA_ID == IndividualNumber))

### there are still a bunch of errors here...
# 255 fish were sampled for the workshop, but only 176 can be found in the database, so length of 79 fish is missing
# some fish were recorded wrongly during the workshop, so they cannot be traced back, e.g. "259", "1000", and "1977"
# and more...
# anyway, this how far I got so far

#####

### analyze matches between morph. and genet. ID
#####
### create new columns
# true species
dta <- dta %>% mutate(Species_gen = 
                         if_else(S_norvegicus_gen == 1, "S_norvegicus",
                                 if_else(S_mentella_gen == 1, "S_mentella",
                                         if_else(S_viviparus_gen == 1, "S_viviparus", NA), NA), NA))

# estimated species
dta <- dta %>% mutate(Species_mor =
                         if_else(S_norvegicus == 1, "S_norvegicus",
                                 if_else(S_mentella == 1, "S_mentella",
                                         if_else(S_viviparus == 1, "S_viviparus",
                                                 if_else(Other == 1, "Other", NA), NA), NA), NA))

# accuracy
dta <- dta %>%  mutate(Match = 
                          if_else(Species_gen == Species_mor, 1, 0))
dta %>% group_by(Person) %>% count(Match) %>% mutate(percent = n /sum(n) * 100)

# accuracy of AnRe
dta_AnRe <- dta %>% filter(Name == "AnRe")
# write table with her results
write.table(dta_AnRe, here("redfish-ws23_non-gh/data/out/redfish-ws_AnRe.csv"), sep = ",",
            col.names = T, row.names = F)

# accuracy of KK
dta_KK <- dta %>% filter(Name == "KK")
# write table with her results
write.table(dta_KK, here("redfish-ws23_non-gh/data/out/redfish-ws_KK.csv"), sep = ",",
            col.names = T, row.names = F)

# accuracy of HeCh
dta_HeCh <- dta %>% filter(Name == "HeCh")
# write table with her results
write.table(dta_HeCh, here("redfish-ws23_non-gh/data/out/redfish-ws_HeCh.csv"), sep = ",",
            col.names = T, row.names = F)

# overall accuracy
dta %>% count(Match) %>% mutate(percent = n / sum(n) *100)
# for REG only
dta %>% filter(Species_gen == "S_norvegicus") %>%  count(Match) %>% mutate(percent = n / sum(n) *100) # 54 %
# for REB only
dta %>% filter(Species_gen == "S_mentella") %>%  count(Match) %>% mutate(percent = n / sum(n) *100) # 83 %
# for REV only
dta %>% filter(Species_gen == "S_viviparus") %>%  count(Match) %>% mutate(percent = n / sum(n) *100)
# for large fish only
dta[!duplicated(dta$TA_ID), ] # 256 fish in total
dta_distinct %>% filter(is.na(Length)|Length > 17) # 185 fish remaining that are either > 17 or NA
dta %>% filter(is.na(Length)|Length > 17) %>%  count(Match) %>% mutate(percent = n / sum(n) *100) # 69 %
# accuracy is almost the same after excluding 71 small fish...
# for REG only
dta %>% filter(Species_gen == "S_norvegicus") %>%  filter(is.na(Length)|Length > 17) %>% count(Match) %>% mutate(percent = n / sum(n) *100) # 54 %
# for REB only
dta %>% filter(Species_gen == "S_mentella") %>%  filter(is.na(Length)|Length > 17) %>% count(Match) %>% mutate(percent = n / sum(n) *100) # 84 %

# summarise
# norvegicus
dta2 <- dta %>% filter(Species_gen == "S_norvegicus") %>% group_by(Person) %>% count(Match) %>% 
  mutate(percent = n / sum(n) * 100)
dta2 <- dta2 %>% mutate(Match = if_else(Match == 0, "incorrect", ifelse(Match == 1, "correct", NA), NA))
dta2

ggplot(dta2, aes(fill=Match, y= percent, x=Person)) + 
  geom_bar(position="dodge", stat="identity") + 
  scale_fill_manual(values=c("#009E73", "#D55E00", "#999999"))
ggsave(here("figures/REG_accuracy_by_person.pdf"), width = 7.5, height = 5, dpi = 300)
ggsave(here("figures/REG_accuracy_by_person.png"), width = 7.5, height = 5, dpi = 300)

# mentella
dta3 <- dta %>% filter(Species_gen == "S_mentella") %>% group_by(Person) %>% count(Match) %>% 
  mutate(percent = n / sum(n) * 100)
dta3 <- dta3 %>% mutate(Match = if_else(Match == 0, "incorrect", ifelse(Match == 1, "correct", NA), NA))
dta3

ggplot(dta3, aes(fill=Match, y= percent, x=Person)) + 
  geom_bar(position="dodge", stat="identity")+ 
  scale_fill_manual(values=c("#009E73", "#D55E00", "#999999"))
ggsave(here("figures/REB_accuracy_by_person.pdf"), width = 7.5, height = 5, dpi = 300)
ggsave(here("figures/REB_accuracy_by_person.png"), width = 7.5, height = 5, dpi = 300)


# all
dta4 <- dta %>% group_by(Person) %>% count(Match) %>% 
  mutate(percent = n / sum(n) * 100)
dta4 <- dta4 %>% mutate(Match = if_else(Match == 0, "incorrect", ifelse(Match == 1, "correct", NA), NA))
dta4

ggplot(dta4, aes(fill=Match, y= percent, x=Person)) + 
  geom_bar(position="dodge", stat="identity")+ 
  scale_fill_manual(values=c("#009E73", "#D55E00", "#999999"))
ggsave(here("figures/ALL_accuracy_by_person.pdf"), width = 7.5, height = 5, dpi = 300)
ggsave(here("figures/ALL_accuracy_by_person.png"), width = 7.5, height = 5, dpi = 300)


# by institute
dta4 <- dta %>% group_by(Institute) %>% count(Match) %>% 
  mutate(percent = n / sum(n) * 100)
dta4 <- dta4 %>% mutate(Match = if_else(Match == 0, "incorrect", ifelse(Match == 1, "correct", NA), NA))
dta4

ggplot(dta4, aes(fill=Match, y= percent, x=Institute)) + 
  geom_bar(position="dodge", stat="identity")+ 
  scale_fill_manual(values=c("#009E73", "#D55E00", "#999999"))
ggsave(here("figures/INST_accuracy_by_person.pdf"), width = 7.5, height = 5, dpi = 300)
ggsave(here("figures/INST_accuracy_by_person.png"), width = 7.5, height = 5, dpi = 300)


# by institute, mentella
dta4 <- dta %>% filter(Species_gen == "S_mentella")  %>% group_by(Institute) %>% count(Match) %>% 
  mutate(percent = n / sum(n) * 100)
dta4 <- dta4 %>% mutate(Match = if_else(Match == 0, "incorrect", ifelse(Match == 1, "correct", NA), NA))
dta4

ggplot(dta4, aes(fill=Match, y= percent, x=Institute)) + 
  geom_bar(position="dodge", stat="identity")+ 
  scale_fill_manual(values=c("#009E73", "#D55E00", "#999999"))
ggsave(here("figures/INST-REB_accuracy_by_person.pdf"), width = 7.5, height = 5, dpi = 300)
ggsave(here("figures/INST-REB_accuracy_by_person.png"), width = 7.5, height = 5, dpi = 300)


# by institute, norvegicus
dta4 <- dta %>% filter(Species_gen == "S_norvegicus")  %>% group_by(Institute) %>% count(Match) %>% 
  mutate(percent = n / sum(n) * 100)
dta4 <- dta4 %>% mutate(Match = if_else(Match == 0, "incorrect", ifelse(Match == 1, "correct", NA), NA))
dta4

ggplot(dta4, aes(fill=Match, y= percent, x=Institute)) + 
  geom_bar(position="dodge", stat="identity")+ 
  scale_fill_manual(values=c("#009E73", "#D55E00", "#999999"))
ggsave(here("figures/INST-REG_accuracy_by_person.pdf"), width = 7.5, height = 5, dpi = 300)
ggsave(here("figures/INST-REG_accuracy_by_person.png"), width = 7.5, height = 5, dpi = 300)

#####