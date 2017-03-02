#Preambule
#R Version 3.3.2
rm(list=ls())

options(scipen=999) # supressing scientific notation
par(mar=c(5.1,4.1,4.1,2.1)) 
par(mfrow=c(1,1))

#libraries used
library(tidyverse)
library(eeptools)
library(openxlsx)
library(stringr)

######## Comparing LN data and Bueno TSE data

#### Loading LN
load("~/Dropbox/LOCAL_ELECTIONS/LN_data/prefeitos_margin_a.Rda")
load("~/Dropbox/LOCAL_ELECTIONS/LN_data/prefeitos_margin_b.Rda")
#NB
load("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/final_data/electionsff_2000_2012.Rda")
electionstse <- electionsff

#Comparing number of muncipalities
head(prefeitos_a)
table(prefeitos_a$year)

prefeitos_a <- as_tibble(prefeitos_a)
prefeitos_a <- prefeitos_a %>% filter(year != 2016) #not in NB data

#Number of municipalities in NB 
Number_Mun <- electionstse %>% group_by(ANO_ELEICAO) %>% 
  distinct(SIGLA_UE) %>% count()
table(electionstse$DESC_SIT_TOT_TURNO)

#NB number of muns
Number_Mun
#Number of muns LN
table(prefeitos_a$year)

#Different number of municipalities in all year
#Difference by year
Number_Mun[,2] - as.numeric(table(prefeitos_a$year))

#Getting NB data in same format as LN data
electionstse1 <- electionstse %>% filter(vote_margin_share > 0) 

#Creating ids
prefeitos_a <- prefeitos_a %>% mutate(yearid = paste0(year, muni_code))
electionstse1 <- electionstse1 %>% mutate(yearid = paste0(ANO_ELEICAO, SIGLA_UE))

#Comparing vote margin in places that are in both datasets
common <- intersect(prefeitos_a$yearid, electionstse1$yearid)


#Getting vote margins for common municipalities
prefeitos_a2 <- prefeitos_a[prefeitos_a$yearid %in% common,]
electionstse2 <- electionstse1[electionstse1$yearid %in% common,]

#Two results for this nonregular election
dups <- electionstse2[duplicated(electionstse2$yearid),]

#Excluding it for now from both #to check in NB data later
prefeitos_a2 <- prefeitos_a2 %>% filter(yearid != 200873059)
electionstse2 <- electionstse2 %>% filter(yearid != 200873059)

#Summary stats
summary(prefeitos_a2$V3)
summary(electionstse2$vote_margin_share)
prefeitos_a2 <- arrange(prefeitos_a2, yearid)
electionstse2 <- arrange(electionstse2, yearid)
table(prefeitos_a2$V3==electionstse2$vote_margin_share)

#Merge data
electionstse3 <- electionstse2 %>% select(yearid, SIGLA_UE, ANO_ELEICAO, 
                                          vote_margin_share, VOTO_MUN_TOTAL, SIGLA_UF, DESCRICAO_UE)

both <- electionstse3 %>% left_join(prefeitos_a2, by = "yearid")
all.equal(both$V3, both$vote_margin_share)
elementwise.all.equal <- Vectorize(function(x, y) {isTRUE(all.equal(x, y))})
table(elementwise.all.equal(both$V3, both$vote_margin_share))
diffs_margin <- both[!elementwise.all.equal(both$V3, both$vote_margin_share),]
diffs_total <- both[!elementwise.all.equal(both$totalVotesMun, both$VOTO_MUN_TOTAL),]

table(diffs$ANO_ELEICAO)
dim(diffs)
dim(diffs_total)

#Looking at 2012 cases
temp <- diffs %>% filter(yearid == 201276511) #dados LN correto
temp <- diffs %>% filter(yearid == 201278255) #dados LN correto
temp <- diffs %>% filter(yearid == 201280551) #dados NB correto
temp <- diffs %>% filter(yearid == 201286177) #dados LN correto
temp <- diffs %>% filter(yearid == 201289001) #dados LN correto
temp <- diffs %>% filter(yearid == 201290190) #dados NB correto


#Looking at 2008 cases
temp <- diffs %>% filter(yearid == 200811355) #dados NB coreto
temp <- diffs %>% filter(yearid == 200811371) #dados NB correto (very close tp LN)
temp <- diffs %>% filter(yearid == 200821873) #dados NB correto
temp <- diffs %>% filter(yearid == 200821750) #dados NB correto
temp <- diffs %>% filter(yearid == 200821296) #dados NB correto
temp <- diffs %>% filter(yearid == 200815717) #dados NB correto

#Looking at 2004 cases




#Looking at 2000 cases

