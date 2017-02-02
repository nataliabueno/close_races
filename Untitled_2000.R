#######Elections 2000
vot_2000 <- vot_2000_2016[[1]]
cand_2000 <- cand_2000_2016[[1]]

#To start with, unequal numbers of municipalities in 2000 elections
length(unique(cand_2000$SIGLA_UE))
length(unique(vot_2000$CODIGO_MUNICIPIO))

#Excluding places with runoff from first round 
#because I am interested in vote margin in the decisive round
e_round2 <- vot_2000 %>% filter(NUM_TURNO == 2) %>% distinct(CODIGO_MUNICIPIO)
#Excluding places where result changed after election (similar to eleicoes suplementares)
change <- cand_2000 %>% filter(DESCRICAO_CARGO == "PREFEITO") %>% 
          filter(DESC_SIT_TOT_TURNO=="RENÚNCIA/FALECIMENTO/CASSAÇÃO APÓS A ELEIÇÃO"|
           DESC_SIT_TOT_TURNO=="REGISTRO NEGADO APÓS A ELEIÇÃO" | 
           DESC_SIT_TOT_TURNO=="RENÚNCIA;FALECIMENTO;CASSAÇÃO APÓS A ELEIÇÃO" |
           DESC_SIT_TOT_TURNO=="RENÚNCIA/FALECIMENTO COM SUBSTITUIÇÃO" |
           DES_SITUACAO_CANDIDATURA == "HOMOLOGAÇÃO DE RENÚNCIA") %>% distinct(SIGLA_UE)
#errors_tse <- 31518 #For muncipality SIGLA_UE 31518: file vot_2004 does not have votes for winning candidate
vot_2000v1 <- vot_2000 %>% filter(!CODIGO_MUNICIPIO %in% e_round2$CODIGO_MUNICIPIO, !CODIGO_MUNICIPIO %in% change$SIGLA_UE,
                                  DESCRICAO_CARGO == "PREFEITO")  #primeiro turno, prefeito #removind candidatos that count twice
cand_2000v1 <- cand_2000 %>% filter(!SIGLA_UE %in% e_round2$CODIGO_MUNICIPIO, !SIGLA_UE %in% change$SIGLA_UE,
                                    DESCRICAO_CARGO == "PREFEITO", DESC_SIT_TOT_TURNO != "#NULO#", DESC_SIT_TOT_TURNO != "") #primeiro turno, prefeito

#Temp Unique ID  #not using CPF or TITULO DE ELEITOR BECAUSE OF MISSING DATA and MISSING DATA IN SEQUENCIAL CANDIDATO IN 2000
cand_2000v1$temp_id <- paste0(cand_2000v1$SIGLA_UE, cand_2000v1$SEQUENCIAL_CANDIDATO, cand_2000v1$SIGLA_UF)
#vot_2000v1$temp_id <- paste0(vot_2000v1$CODIGO_MUNICIPIO, vot_2000v1$SQ_CANDIDATO, vot_2000v1$SIGLA_UF)
stopifnot(length(unique(cand_2000v1$temp_id))==nrow(cand_2000v1))

#Making sure no municipality has a repeated candidate


#Selecting candidatos aptos to aggregate votes per municipality 
#(removing places that had a runoff vote from first rounds and removing eleicoes nao regulares)
#Not using apto ou nao apto because that is not available
#included UF to get unique ID
vot_2000v2 <- vot_2000v1 %>% group_by(CODIGO_MUNICIPIO, NOME_CANDIDATO, SIGLA_UF) %>% 
              summarize(VOTO_MUN_CAND = sum(TOTAL_VOTOS))
#Check
stopifnot(length(unique(vot_2000v2$CODIGO_MUNICIPIO))==length(unique(vot_2000v1$CODIGO_MUNICIPIO)))

#Getting municipality's total vote and candidate vote share 
vot_2000v3 <- vot_2000v2 %>% group_by(CODIGO_MUNICIPIO, SIGLA_UF) %>% summarize(VOTO_MUN_TOTAL = sum(VOTO_MUN_CAND)) %>% 
              left_join(vot_2000v2, by = c("CODIGO_MUNICIPIO", "SIGLA_UF")) %>% 
              mutate(VOTO_CAND_SHARE = VOTO_MUN_CAND/VOTO_MUN_TOTAL) %>% 
              group_by(CODIGO_MUNICIPIO, SIGLA_UF) %>% mutate(NUMBER_CANDIDATES = n()) 
#Check
stopifnot(min(vot_2000v3$NUMBER_CANDIDATES)==1)
stopifnot(vot_2000v3$VOTO_CAND_SHARE >= 0 & vot_2000v3$VOTO_CAND_SHARE <= 1)

#Merging with candidate information
#Making sure there are no NAs and keys are unique
nrow(cand_2000v1 %>% filter(is.na(SIGLA_UE)))==0
nrow(vot_2000v3 %>% filter(is.na(CODIGO_MUNICIPIO)))==0
nrow(cand_2000v1 %>% filter(is.na(NOME_CANDIDATO)))==0
nrow(vot_2000v3 %>% filter(is.na(NOME_CANDIDATO)))==0
nrow(cand_2000v1 %>% filter(is.na(SIGLA_UF)))==0
nrow(vot_2000v3 %>% filter(is.na(SIGLA_UF)))==0
#Creating unique ids (to check merge by three variables, two not enough), 
vot_2000v3$id_merge <- paste0(vot_2000v3$CODIGO_MUNICIPIO, vot_2000v3$NOME_CANDIDATO, vot_2000v3$SIGLA_UF)
cand_2000v1$id_merge <- paste0(cand_2000v1$SIGLA_UE, cand_2000v1$NOME_CANDIDATO, cand_2000v1$SIGLA_UF)
nrow(vot_2000v3) == length(unique(vot_2000v3$id_merge))
nrow(cand_2000v1) == length(unique(cand_2000v1$id_merge))
#dups <- cand_2000v1[duplicated(cand_2000v1$id_merge),]
#temp <- cand_2000v1 %>% filter(id_merge == dups$id_merge)
#Excluding duplicate candidate because of indeferido and sob judice
#cand_2000v1 <- cand_2000v1 %>% filter(NOME_CANDIDATO!="VALDOMIRO IZIDIO PEREIRA")
#Merging
cand_2000v2 <- cand_2000v1 %>% left_join(vot_2000v3, by=c("id_merge"))

#Debugging #which do not merge?
bugs <- anti_join(cand_2000v2, vot_2000v3, by=c("id_merge"))
summary(bugs$VOTO_CAND_SHARE) #all NA's
table(bugs$DESC_SIT_TOT_TURNO) #how can you have eleito is vote share == NA?
temp <- bugs %>% filter(DESC_SIT_TOT_TURNO == "ELEITO")
#Eliminating cases with NA vote share, eliminating places with only one candidate, and using age rule to break ties
cand_2000v3 <- cand_2000v2 %>% filter(!is.na(VOTO_CAND_SHARE)) 

cand_2000v4 <- cand_2000v3 %>% filter(NUMBER_CANDIDATES != 1) %>% group_by(SIGLA_UE, SIGLA_UF.x) %>% 
  mutate(rankvote = rank(-VOTO_CAND_SHARE)) %>% 
  mutate(rankvoter = ifelse(rankvote == 1.5 & DESC_SIT_TOT_TURNO == "ELEITO", 1,
                            ifelse(rankvote == 1.5 & DESC_SIT_TOT_TURNO == "NÃO ELEITO", 2, rankvote)))
#check
table(cand_2000v4$DESC_SIT_TOT_TURNO, cand_2000v4$rankvoter) 

first_place_not_elected <- cand_2000v4 %>% filter(rankvoter == 1, DESC_SIT_TOT_TURNO == "NÃO ELEITO")
#See in raw data:
ex1 <- cand_2000 %>% filter(NOME_CANDIDATO=="JOSÉ TORRENTE DIOGO DE FARIAS")
ex1v <- vot_2000 %>% filter(NOME_CANDIDATO=="JOSÉ TORRENTE DIOGO DE FARIAS")
mun1 <- cand_2000 %>% filter(SIGLA_UE == "66931", DESCRICAO_CARGO=="PREFEITO")

second_place_elected <-   cand_2000v4 %>% filter(rankvoter == 2, DESC_SIT_TOT_TURNO == "ELEITO")
#See in raw data:
ex2 <- cand_2000 %>% filter(NOME_CANDIDATO=="VALDIR BERNARDINO MARTINAZZO")
ex2v <- vot_2000 %>% filter(NOME_CANDIDATO=="VALDIR BERNARDINO MARTINAZZO")
mun2 <- cand_2000 %>% filter(SIGLA_UE == "79871", DESCRICAO_CARGO=="PREFEITO")
mun2v <- vot_2000 %>% filter(SIGLA_UE == "79871", DESCRICAO_CARGO=="PREFEITO")
city <- cand_2000v4 %>% filter(SIGLA_UE == "66931") #votos de homologacao de renunica count twice
#comparing with cepesp data
test <- round1 %>% filter(SG_UE=="66931", anoEleicao=="2000")



#######Calculating vote share for eleicoes regulares (places that had runoffs and nao eleicoes suplementares)

#Geting the right base
vot_2000_runoff <- vot_2000 %>% filter(SIGLA_UE %in% e_round2$SIGLA_UE & DESCRICAO_CARGO == "PREFEITO" 
                                       & NUM_TURNO==2)
cand_2000_runoff <- cand_2000 %>% filter(SIGLA_UE %in% e_round2$SIGLA_UE & NUM_TURNO == 2 &
                                           DESCRICAO_CARGO == "PREFEITO" & DESC_SIT_TOT_TURNO != "#NULO")
#Check
stopifnot(length(unique(cand_2000_runoff$NUM_TITULO_ELEITORAL_CANDIDATO))==nrow(cand_2000_runoff))

#Selecting candidatos a prefeito, aptos to aggregate votes per municipality
vot_2000_runoffv2 <- vot_2000_runoff %>% group_by(SIGLA_UE, SQ_CANDIDATO, SIGLA_UF) %>% summarize(VOTO_MUN_CAND = sum(TOTAL_VOTOS))
#Check
stopifnot(length(unique(vot_2000_runoffv2$SIGLA_UE))==length(unique(vot_2000_runoff$SIGLA_UE)))

#Getting municipality's total vote and candidate vote share and renaming to merge
vot_2000_runoffv3 <- vot_2000_runoffv2  %>% group_by(SIGLA_UE, SIGLA_UF) %>% 
  summarize(VOTO_MUN_TOTAL = sum(VOTO_MUN_CAND)) %>%
  left_join(vot_2000_runoffv2, by=c("SIGLA_UE", "SIGLA_UF")) %>% 
  mutate(VOTO_CAND_SHARE = VOTO_MUN_CAND/VOTO_MUN_TOTAL) %>% group_by(SIGLA_UE, SIGLA_UF) %>% 
  mutate(NUMBER_CANDIDATES = n()) %>% rename(SEQUENCIAL_CANDIDATO = SQ_CANDIDATO)
#Checks
table(vot_2000_runoffv3$NUMBER_CANDIDATES==2)
summary(vot_2000_runoffv3$VOTO_CAND_SHARE)

#Merging with candidate information 
#Making sure there are no NAs and keys are unique
nrow(cand_2000_runoff %>% filter(is.na(SEQUENCIAL_CANDIDATO)))==0
nrow(vot_2000_runoffv3 %>% filter(is.na(SEQUENCIAL_CANDIDATO)))==0
cand_2000_runoffv2 <- cand_2000_runoff %>% 
  left_join(vot_2000_runoffv3, by=c("SIGLA_UE", "SEQUENCIAL_CANDIDATO", "SIGLA_UF"))

#Debugging tactic
stopifnot(nrow(anti_join(cand_2000_runoff, vot_2000_runoffv3, by=c("SIGLA_UE", "SEQUENCIAL_CANDIDATO", "SIGLA_UF")))==0)

#Ranking votes (no ties, but creating rankvoter varaible to facilitate merge later on)
cand_2000_runoffv3 <- cand_2000_runoffv2 %>%  group_by(SIGLA_UE, SIGLA_UF) %>% 
  mutate(rankvote = rank(-VOTO_CAND_SHARE)) %>% 
  mutate(rankvoter = rankvote)
#check
table(cand_2000_runoffv3$rankvote, cand_2000_runoffv3$DESC_SIT_TOT_TURNO)

#Binding first round with runoff candidates
cand_2000v5 <- bind_rows(cand_2000v4, cand_2000_runoffv3)

####Vote margin (share and absolute)

#getting winner and runnerups
cand_2000v6 <- cand_2000v5 %>% filter(rankvoter== 1 | rankvoter== 2)
table(cand_2000v6$DESC_SIT_TOT_TURNO)

#Getting vote margins
muns <- unique(cand_2000v6$SIGLA_UE)
#Check
stopifnot(length(unique(muns))==length(muns))

electionsf_2000 <- NULL

for (i in 1:length(muns)){
  
  mun <- cand_2000v6[which(cand_2000v6$SIGLA_UE==muns[i]), ]
  
  vote_margin_share <- max(mun$VOTO_CAND_SHARE) - min(mun$VOTO_CAND_SHARE) 
  vote_margin_abs <- max(mun$VOTO_MUN_CAND) - min(mun$VOTO_MUN_CAND)
  winner <- mun %>% filter(DESC_SIT_TOT_TURNO == "ELEITO")
  runner_up <- mun %>% filter(DESC_SIT_TOT_TURNO == "NÃO ELEITO")
  
  margin_winner <- winner %>% mutate(vote_margin_share = vote_margin_share, vote_margin_abs = vote_margin_abs)
  margin_runner <- runner_up %>% mutate(vote_margin_share = -vote_margin_share, vote_margin_abs = -vote_margin_abs)
  
  electionsf_2000 <- bind_rows(electionsf_2000, margin_winner, margin_runner)
  print(i)
  
} 

#######Calculating vote share for eleicoes nao-regulares 

#only with places where results changed after elections
vot_2000_supv1 <- vot_2000 %>% filter(!SIGLA_UE %in% e_round2$SIGLA_UE, SIGLA_UE %in% change$SIGLA_UE,
                                      !SIGLA_UE %in% errors_tse,
                                      DESCRICAO_CARGO == "PREFEITO")  #primeiro turno, prefeito
cand_2000_supv1 <- cand_2000 %>% filter(!SIGLA_UE %in% e_round2$SIGLA_UE, SIGLA_UE %in% change$SIGLA_UE,
                                        !SIGLA_UE %in% errors_tse,
                                        DESCRICAO_CARGO == "PREFEITO", DESC_SIT_TOT_TURNO != "#NULO#") #primeiro turno, prefeito
#Check
stopifnot(length(unique(cand_2000_supv1$NUM_TITULO_ELEITORAL_CANDIDATO))==nrow(cand_2000_supv1))

#Selecting candidatos aptos to aggregate votes per municipality 
#(removing places that had a runoff vote from first rounds and removing eleicoes nao regulares)
#Not using apto ou nao apto because that is not available
#included UF to get unique ID
vot_2000_supv2 <- vot_2000_supv1 %>% group_by(SIGLA_UE, SQ_CANDIDATO, SIGLA_UF) %>% 
  summarize(VOTO_MUN_CAND = sum(TOTAL_VOTOS))
#Check
stopifnot(length(unique(vot_2000_supv2$SIGLA_UE))==length(unique(vot_2000_supv1$SIGLA_UE)))

#Getting municipality's total vote and candidate vote share and renaming to merge
vot_2000_supv3 <- vot_2000_supv2 %>% group_by(SIGLA_UE, SIGLA_UF) %>% summarize(VOTO_MUN_TOTAL = sum(VOTO_MUN_CAND)) %>% 
  left_join(vot_2000_supv2, by = c("SIGLA_UE", "SIGLA_UF")) %>% 
  mutate(VOTO_CAND_SHARE = VOTO_MUN_CAND/VOTO_MUN_TOTAL) %>% 
  group_by(SIGLA_UE, SIGLA_UF) %>% mutate(NUMBER_CANDIDATES = n()) %>% 
  rename(SEQUENCIAL_CANDIDATO = SQ_CANDIDATO)
#Check
stopifnot(vot_2000_supv3$VOTO_CAND_SHARE >= 0 & vot_2000_supv3$VOTO_CAND_SHARE <= 1)
stopifnot(min(vot_2000_supv3$NUMBER_CANDIDATES)==1)

#Merging with candidate information
#Making sure there are no NAs and keys are unique
nrow(cand_2000_supv1 %>% filter(is.na(SIGLA_UE)))==0
nrow(vot_2000_supv3 %>% filter(is.na(SIGLA_UE)))==0
nrow(cand_2000_supv1 %>% filter(is.na(SEQUENCIAL_CANDIDATO)))==0
nrow(vot_2000_supv3 %>% filter(is.na(SEQUENCIAL_CANDIDATO)))==0
nrow(cand_2000_supv1 %>% filter(is.na(SIGLA_UF)))==0
nrow(vot_2000_supv3 %>% filter(is.na(SIGLA_UF)))==0
#Creating unique ids (to check merge by three variables, two not enough), it's okay, they are unique #after checking once, no need to create it
#vot_2000v3$id_merge <- paste0(vot_2000v3$SIGLA_UE, vot_2000v3$SEQUENCIAL_CANDIDATO, vot_2000v3$SIGLA_UF)
#cand_2000v1$id_merge <- paste0(cand_2000v1$SIGLA_UE, cand_2000v1$SEQUENCIAL_CANDIDATO, cand_2000v1$SIGLA_UF)
#nrow(vot_2000v3) == length(unique(vot_2000v3$id_merge))
#row(cand_2000v1) == length(unique(cand_2000v1$id_merge))

#Merging
cand_2000_supv2 <- cand_2000_supv1 %>% left_join(vot_2000_supv3, by=c("SIGLA_UE", "SEQUENCIAL_CANDIDATO", "SIGLA_UF"))

#Debugging #which do not merge?
bugs <- anti_join(cand_2000_supv2, vot_2000_supv3, by=c("SIGLA_UE", "SEQUENCIAL_CANDIDATO", "SIGLA_UF"))
summary(bugs$VOTO_CAND_SHARE) #all NA's
#View(bugs %>% filter(DES_SITUACAO_CANDIDATURA=="DEFERIDO")) #situacao turno == Nao eleito (3), Why?
table(bugs$DESC_SIT_TOT_TURNO)

#Eliminating cases with NA vote share, eliminating places with only one candidate, and using age rule to break ties
cand_2000_supv3 <- cand_2000_supv2 %>% filter(!is.na(VOTO_CAND_SHARE)) 

cand_2000_supv4 <- cand_2000_supv3 %>% filter(NUMBER_CANDIDATES != 1) %>% group_by(SIGLA_UE, SIGLA_UF) %>% 
  mutate(rankvote = rank(-VOTO_CAND_SHARE)) %>% 
  mutate(rankvoter = ifelse(rankvote == 1.5 & DESC_SIT_TOT_TURNO == "ELEITO", 1,
                            ifelse(rankvote == 1.5 & DESC_SIT_TOT_TURNO == "NÃO ELEITO", 2, rankvote)))
cand_2000_supv5 <- cand_2000_supv4 %>% mutate(DESC_SIT_TOT_TURNO = ifelse(rankvote == 1 & 
                                                                            DESC_SIT_TOT_TURNO == "RENÚNCIA/FALECIMENTO COM SUBSTITUIÇÃO", "ELEITO",
                                                                          ifelse(rankvote == 2 & DESC_SIT_TOT_TURNO == "RENÚNCIA/FALECIMENTO COM SUBSTITUIÇÃO", 
                                                                                 "NÃO ELEITO", DESC_SIT_TOT_TURNO)))
table(cand_2000_supv5$DESC_SIT_TOT_TURNO, cand_2000_supv5$rankvoter) 

####Vote margin (share and absolute)

#getting winner and runnerups
cand_2000_supv6 <- cand_2000_supv5 %>% filter(rankvoter== 1 | rankvoter== 2)
table(cand_2000_supv6$DESC_SIT_TOT_TURNO) #half and half

#Getting vote margins
muns_sup <- unique(cand_2000_supv6$SIGLA_UE)
#Check
stopifnot(length(unique(muns_sup))==length(muns_sup))

electionsf_sup_2000 <- NULL

for (i in 1:length(muns_sup)){
  
  mun <- cand_2000_supv6[which(cand_2000_supv6$SIGLA_UE==muns_sup[i]), ]
  
  vote_margin_share <- max(mun$VOTO_CAND_SHARE) - min(mun$VOTO_CAND_SHARE) 
  vote_margin_abs <- max(mun$VOTO_MUN_CAND) - min(mun$VOTO_MUN_CAND)
  winner <- mun %>% filter(DESC_SIT_TOT_TURNO == "ELEITO")
  runner_up <- mun %>% filter(DESC_SIT_TOT_TURNO == "NÃO ELEITO")
  
  margin_winner <- winner %>% mutate(vote_margin_share = vote_margin_share, vote_margin_abs = vote_margin_abs)
  margin_runner <- runner_up %>% mutate(vote_margin_share = -vote_margin_share, vote_margin_abs = -vote_margin_abs)
  
  electionsf_sup_2000 <- bind_rows(electionsf_sup_2000, margin_winner, margin_runner)
  print(i)
  
} 

######### Binding elections (regulares and nao regulares)
electionsf_2000 <- electionsf_2000 %>% bind_cols(data_frame(TYPE_ELECTION = rep("regular", nrow(electionsf_2000))))  
electionsf_sup_2000 <- electionsf_sup_2000 %>% bind_cols(data_frame(TYPE_ELECTION = rep("nonregular", nrow(electionsf_sup_2000))))                                            

electionsff_2000 <- bind_rows(electionsf_2000, electionsf_sup_2000)

#Simple Smell tests
table(electionsff_2000$TYPE_ELECTION)
table(electionsff_2000$DESC_SIT_TOT_TURNO) #half and half
table(electionsff_2000$rankvoter) #half and half

######## TESTING IDENTIFIERS' INTEGRITY (length of codes, number of municipalities, number of elected mayors, number of nonelected)
#Municipality's identifier's 
str(electionsff_2000)
glimpse(electionsff_2000)
table(nchar(electionsff_2000$CPF_CANDIDATO)) #existem erros
table(nchar(electionsff_2000$NUM_TITULO_ELEITORAL_CANDIDATO)) #existem erros

save(electionsff_2000, file="~/Dropbox/LOCAL_ELECTIONS/repositorio_data/final_data/electionsff_2000.Rda")
