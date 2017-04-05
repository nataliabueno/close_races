#Goal is to compare number of municipalities, total number of votes per municipality and 
# candidate's vote share in each municipality


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

##### Getting CEPESP data

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

#Binding municipalities with only decisive round
roundf <- bind_rows(round1v1, round2)  


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

tempt <- electionstse %>% filter(SIGLA_UE == "58300", ANO_ELEICAO == 2008) #eleicao nao regular, tse correct
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

#### FROM COMPARING NB and LN
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
diffs <- both[!elementwise.all.equal(both$V3, both$vote_margin_share),]
diffs_total <- both[!elementwise.all.equal(both$totalVotesMun, both$VOTO_MUN_TOTAL),]

table(diffs$ANO_ELEICAO)
dim(diffs)
dim(diffs_total)


