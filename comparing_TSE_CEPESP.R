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

#### Loading TSE
load("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/final_data/electionsff_2000_2012.Rda")

electionstse <- electionsff

Number_Mun <- electionstse %>% group_by(ANO_ELEICAO) %>% 
              distinct(SIGLA_UE) %>% count()
table(electionstse$DESC_SIT_TOT_TURNO)
Number_Mun

electionst0 <- electionstse %>% select(ANO_ELEICAO, NUM_TURNO, NOME_CANDIDATO, 
												  SIGLA_UE, DESC_SIT_TOT_TURNO, SIGLA_UF, 
												  VOTO_MUN_CAND, VOTO_MUN_TOTAL,VOTO_CAND_SHARE,
												  vote_margin_share,
												  vote_margin_abs, SIGLA_PARTIDO) %>% 
												  filter(SIGLA_PARTIDO == "PT")
summary(electionst0)
dim(electionst0)
rm(electionsff)

#### Loading CEPESP
load("~/Dropbox/Bypassing_CPS/Bueno_CPS_2017/replication/raw_data/electionsffR&R.Rda")

electionsffcepesp <- electionsff

Number_Mun <- electionsffcepesp %>% group_by(anoEleicao) %>% 
              distinct(SG_UE) %>% count()
table(electionsffcepesp$resultado_des)
Number_Mun

electionsc0 <- electionsffcepesp %>% select(anoEleicao, turno, nome_Candidato, titulo,
												  SG_UE, resultado_des, sigla_UF, ibge, 
												  nome_Municipio, voto_nominal, voto_total,
												  totalvotes, vote_share, vote_margin_share,
												  vote_margin_abs, partido_sig) %>% 
												  filter(partido_sig == "PT")
summary(electionsc0)
dim(electionsc0)

######### Checking municipalities in both
electionsffcepesp$SG_UE <- as.character(electionsffcepesp$SG_UE)
electionstse$SIGLA_UE <- as.character(electionstse$SIGLA_UE)

electionsc0$SG_UE <- as.character(electionsc0$SG_UE) 
electionst0$SIGLA_UE <- as.character(electionst0$SIGLA_UE)
munsc <- unique(electionsc0$SG_UE)
munst <- unique(electionst0$SIGLA_UE)
#municipios que nao tem no cepesp
common <- union(munsc, munst)
differentc <- setdiff(munsc, munst)
differentt <- setdiff(munst, munsc)

#Why aren't these in TSE (differentc)?
not_tse_pt <- electionst0[electionst0$SIGLA_UE %in% differentc,]
yes_cepesp_pt <- electionsc0[electionsc0$SG_UE %in% differentc,]

tempt <- electionstse %>% filter(SIGLA_UE == "75477", ANO_ELEICAO == 2012) #tse is correct
tempc <- electionsffcepesp %>% filter(SG_UE == "75477", anoEleicao == 2012)

tempt <- electionstse %>% filter(SIGLA_UE == "68330", ANO_ELEICAO == 2012) #eleicao nao regular, tse correct
tempc <- electionsffcepesp %>% filter(SG_UE == "68330", anoEleicao == 2012) #

tempt <- electionstse %>% filter(SIGLA_UE == "58300", ANO_ELEICAO == 2008) #eleicao nao regular, tse correc
tempc <- electionsffcepesp %>% filter(SG_UE == "58300", anoEleicao == 2008) 

#Why aren't these in CEPESP
not_cepesp_pt <- electionsc0[electionsc0$SG_UE %in% differentt,]
yes_tse_pt <- electionst0[electionst0$SIGLA_UE %in% differentt,]

tempt <- electionstse %>% filter(SIGLA_UE == "52531", ANO_ELEICAO == 2012) #eleicao_suplementar
tempc <- electionsffcepesp %>% filter(SG_UE == "52531", anoEleicao == 2012)

tempt <- electionstse %>% filter(SIGLA_UE == "68330", ANO_ELEICAO == 2012) 
tempc <- electionsffcepesp %>% filter(SG_UE == "68330", anoEleicao == 2012)

tempt <- electionstse %>% filter(SIGLA_UE == "58300", ANO_ELEICAO == 2008) 
tempc <- electionsffcepesp %>% filter(SG_UE == "58300", anoEleicao == 2008) 


########### Later
load(paste0(dir, "replication/raw_data/electionsffR&R.Rda"))
elections_cepesp <- electionsff
elections_cepesp0 <- elections_cepesp %>% select(anoEleicao, turno, nome_Candidato, 
												  SG_UE, resultado_des, sigla_UF, ibge, 
												  nome_Municipio, voto_nominal, voto_total,
												  totalvotes, vote_share, vote_margin_share,
												  vote_margin_abs, partido_sig)
elections_cepesp0 <- as_tibble(elections_cepesp0)												  
elections_cepesp1 <- elections_cepesp0 %>% filter(partido_sig == "PT")
elections_cepesp2 <- elections_cepesp1[abs(elections_cepesp1$vote_margin_share) < 0.01,]												  
												  
load(paste0(dir, "replication/final_data/data_auxiliary/electionsff_2000_2012.Rda"))
elections_tse <- electionsff
elections_tse0 <- elections_tse %>% select(ANO_ELEICAO, NUM_TURNO, NOME_CANDIDATO, 
												  SIGLA_UE, DESC_SIT_TOT_TURNO, SIGLA_UF,  
												  DESCRICAO_UE, VOTO_MUN_CAND, VOTO_MUN_TOTAL,
												  VOTO_CAND_SHARE, vote_margin_share,
												  vote_margin_abs, SIGLA_PARTIDO)

elections_tse1 <- elections_tse0 %>% filter(SIGLA_PARTIDO == "PT")
elections_tse2 <- elections_tse1[abs(elections_tse1$vote_margin_share) < 0.01,]

summary(elections_tse2)
summary(elections_cepesp2)

temp <- elections_cepesp2 %>% filter(vote_margin_share == max(vote_margin_share))
temp$nome_Municipio #OK
temp <- elections_cepesp2 %>% filter(vote_margin_abs == min(vote_margin_abs))


temp0 <- elections_tse2 %>% filter(vote_margin_share == max(vote_margin_share))
max(temp0$vote_margin_share) #WHAT IS GOING ON WITH TSE DATA?
