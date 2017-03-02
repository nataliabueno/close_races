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
prefeitos_a <- prefeitos_a %>% filter(year != 2016)

#Number of municipalities in NB 
Number_Mun <- electionstse %>% group_by(ANO_ELEICAO) %>% 
  distinct(SIGLA_UE) %>% count()
table(electionstse$DESC_SIT_TOT_TURNO)
Number_Mun

#Comparing vote margin in places that are in both datasets



