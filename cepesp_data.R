###################################################################
############# Downloading, Organizing and Cleaning Electoral Data
#0. Download CEPESP repositorio data
#1. Combine and merge
#2. Clean and get it ready for use
###################################################################
###################################################################

#Preambule
#R Version 3.3.2
rm(list=ls())

options(scipen=999) # supressing scientific notation
par(mar=c(5.1,4.1,4.1,2.1)) 
par(mfrow=c(1,1))

#libraries used
library(tidyverse)
library(httr)

###################################################################
#1. Downloading Data Using CEPESP R's Interface
###################################################################

#TBD


###################################################################
#1. Combining
###################################################################
#Importing data
br <- locale("es", encoding = "latin1") 
round1 <- read_csv("~/Dropbox/LOCAL_ELECTIONS/cepesp_data/original_data/2000-2004-2008-2012-2016_Prefeito_Municipio_Candidato_1turno.csv",
                   col_types = cols(.default = col_character(), voto_total= col_number()), locale=br)
  
round2 <- read_csv("~/Dropbox/LOCAL_ELECTIONS/cepesp_data/original_data/2000-2004-2008-2012-2016_Prefeito_Municipio_Candidato_2turno.csv",
                   col_types = cols(.default = col_character(), voto_total= col_number()), locale=br) 

#Excluding 2016 at the moment
round1 <- round1 %>% filter(anoEleicao != 2016)
round2 <- round2 %>% filter(anoEleicao != 2016)

#creating municipality election year key and removing brancos/nulos
round1 <- round1 %>% mutate(mun_electionyear = paste0(SG_UE, anoEleicao)) %>% filter(cargo_cod!=0)
round2 <- round2 %>% mutate(mun_electionyear = paste0(SG_UE, anoEleicao)) %>% filter(cargo_cod!=0)

#Excluding places with runoff from first round 
#because I am interested in vote margin in the decisive round

e_round2 <- round1 %>% filter(resultado_des == "2º TURNO") %>% distinct(mun_electionyear)
round1v1 <- round1 %>% filter(!mun_electionyear %in% e_round2$mun_electionyear) 

#Showing issues with consistency using absolute number of votes (not just vote share)

#Number of unique municipalities per year
Number_Mun <- round1 %>% group_by(anoEleicao) %>% 
              distinct(SG_UE) %>% count()
#Varies the number of observations per year

round1v1 <- round1 %>% group_by(anoEleicao, SG_UE) %>%
          mutate(NUMBER_CANDIDATES = n()) 

round1v2 <- round1v1 %>% filter(NUMBER_CANDIDATES != 1) %>% group_by(SG_UE) %>% 
                mutate(rankvote = rank(voto_total)) %>%  #same as voto nominal for mayors
                 mutate(rankvoter = ifelse(rankvote == 1.5 & resultado_des == "ELEITO", 1,
                  ifelse(rankvote == 1.5 & resultado_des == "NÃO ELEITO", 2, rankvote)))
#check
table(round1v2$resultado_des, round1v2$rankvoter) 

#Getting candidate vote share
round1v2 <- round1v1 %>% filter(resultado_des %in% c("ELEITO","NÃO ELEITO")) %>% group_by(SG_UE, anoEleicao) %>% summarize(voto_mun_total = sum(voto_total))
#round1v2 <- round1v1 %>% group_by(SG_UE, anoEleicao) %>% summarize(voto_mun_total = sum(voto_total))

#Calculating vote shares and getting number of candidates per election cycle in each municipality
round1v3 <- round1v1 %>% filter(resultado_des %in% c("ELEITO","NÃO ELEITO")) %>% left_join(round1v2, by=c("SG_UE", "anoEleicao")) %>%
            mutate(voto_cand = voto_total/voto_mun_total) %>% group_by(SG_UE, anoEleicao) %>% mutate(ncandidates = n())

#round1v3 <- round1v1 %>% left_join(round1v2, by=c("SG_UE", "anoEleicao")) %>%
#            mutate(voto_cand = voto_total/voto_mun_total) %>% group_by(SG_UE, anoEleicao) %>% mutate(ncandidates = n())

#Eliminating places that only had one candidates and getting rank, 
#dealing with ties for winner and runner-up (older candidates have advantage)
#Fix runnerup tie (also using the age rule)
#table(is.na(round1v3$voto_cand)) #no NAs
round1v4 <- round1v3 %>% filter(ncandidates != 1) %>% mutate(rankvote = rank(-voto_cand)) %>% 
                         mutate(rankvoter = ifelse(rankvote == 1.5 & resultado_des == "ELEITO", 1,
                                ifelse(rankvote == 1.5 & resultado_des == "NÃO ELEITO", 2, 
                                ifelse(rankvote == 2.5 & titulo==117261490116, 2,
                                ifelse(rankvote == 2.5 & titulo==46038350132, 3, 
                                ifelse(rankvote == 2.5 & titulo==40299850663, 2,
                                ifelse(rankvote == 2.5 & titulo==57422710604, 3, rankvote)))))))
  
table(round1v4$resultado_des, round1v4$rankvoter)
#259 ranked second but elected
#15 ranked third but elected
#288 ranked first but notelected

#Errors
round1v5 <- round1v4 %>% mutate(first_notelected = ifelse(rankvoter == 1 & resultado_des == "NÃO ELEITO", 1, 0),
                                sec_third_elected = ifelse((rankvoter == 2 |rankvoter == 3)  & resultado_des == "ELEITO", 1, 0))
#Examining these cases
first_notelected <- round1v5 %>% filter(first_notelected==1)
View(first_notelected) #it seems like elected who were removed from office 
sec_third_elected <- round1v5 %>% filter(sec_third_elected==1)
View(sec_third_elected) #it seems like second place who were elected because largest number of votes did not become mayor 
example <- 85294 #in SG_UE in 2000, removed from office
tt <- round1v5 %>% filter(SG_UE==85294)
tt2 <- round1 %>% filter(SG_UE==85294)
View(tt2)
#eleicoes municipio augusto pestana 2012 #cepesp gives the resultado da urna (not accounting for changes because of anulacao, etc)

#Including runoffs (checking if these errors come from runoffs)
round2v2 <- round2 %>% filter(cargo_cod!=0, resultado_des %in% c("ELEITO","NÃO ELEITO", "ELEITO POR QUOCIENTE")) %>% group_by(SG_UE, anoEleicao) %>% summarize(voto_mun_total = sum(voto_total))

#Calculating vote shares and getting number of candidates per election cycle in each municipality
round2v3 <- round2 %>% filter(cargo_cod!=0, resultado_des %in% c("ELEITO","NÃO ELEITO", "ELEITO POR QUOCIENTE")) %>% left_join(round2v2, by=c("SG_UE", "anoEleicao")) %>%
  mutate(voto_cand = voto_total/voto_mun_total) %>% group_by(SG_UE, anoEleicao) %>% mutate(ncandidates = n())
table(round2v3$ncandidates) #error (there should be only 2 candidades -- not 1 or 3)
londrina <- round2v3 %>% filter(ncandidates==3) #terceiro turno em Londrina


#calculating vote margin and removing first_notelected and sec_third_elected


##Testing merging against known benchmarks

#Number of municipalities per election

#Number of winners
  
#Number of losers







 
  