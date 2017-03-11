###################################################################
############# Downloading, Organizing and Cleaning Electoral Data
#0. Download TSE repositorio data #TO DO
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
library(eeptools)

###################################################################
#0. Downloading #TO DO
###################################################################


###################################################################
#1. Combining
# Reading candidate data
# Reading voting data
# Merging and Binding
###################################################################

ufs <- c("AC", "AL", "AP", "AM", "BA",     
         "CE", "ES", "GO", "MA", "MT", "MS",
         "MG", "PA", "PB", "PR", "PE", "PI", "RJ",
         "RN", "RS", "RO","RR","SC", "SP", "SE", "TO") #NO DF

labels_pre2012 <- c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "NUM_TURNO", "DESCRICAO_ELEICAO",
                    "SIGLA_UF", "SIGLA_UE", "DESCRICAO_UE", "CODIGO_CARGO", "DESCRICAO_CARGO",
                     "NOME_CANDIDATO", "SEQUENCIAL_CANDIDATO", "NUMERO_CANDIDATO", "CPF_CANDIDATO",
                     "NOME_URNA_CANDIDATO", "COD_SITUACAO_CANDIDATURA", "DES_SITUACAO_CANDIDATURA",
                    "NUMERO_PARTIDO", "SIGLA_PARTIDO", "NOME_PARTIDO", "CODIGO_LEGENDA", "SIGLA_LEGENDA",
                    "COMPOSICAO_LEGENDA", "NOME_COLIGACAO", "CODIGO_OCUPACAO", "DESCRICAO_OCUPACAO",
                    "DATA_NASCIMENTO", "NUM_TITULO_ELEITORAL_CANDIDATO", "IDADE_DATA_ELEICAO",
                    "CODIGO_SEXO", "DESCRICAO_SEXO", "COD_GRAU_INSTRUCAO", "DESCRICAO_GRAU_INSTRUCAO",
                    "CODIGO_ESTADO_CIVIL", "DESCRICAO_ESTADO_CIVIL", "CODIGO_NACIONALIDADE",
                    "DESCRICAO_NACIONALIDADE", "SIGLA_UF_NASCIMENTO", "CODIGO_MUNICIPIO_NASCIMENTO",
                    "NOME_MUNICIPIO_NASCIMENTO", "DESPESA_MAX_CAMPANHA", "COD_SIT_TOT_TURNO",
                    "DESC_SIT_TOT_TURNO")

#candidates 2000
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/consulta_cand/consulta_cand_2000/consulta_cand_2000_",ufs, ".txt"))
cand_2000 <- lapply(files, read.table, sep = ";", header = F, stringsAsFactors = F, fill = T, fileEncoding = "windows-1252") 
cand_2000 <- do.call("rbind", cand_2000)
names(cand_2000) <- labels_pre2012
cand_2000 <- as_tibble(cand_2000)

#candidates 2004
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/consulta_cand/consulta_cand_2004/consulta_cand_2004_",ufs, ".txt"))
cand_2004 <- lapply(files, read.table, sep = ";", header=F, stringsAsFactors = F, fill = T, fileEncoding = "windows-1252") 
cand_2004 <- do.call("rbind", cand_2004)
names(cand_2004) <- labels_pre2012
cand_2004 <- as_tibble(cand_2004)

#candidates 2008
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/consulta_cand/consulta_cand_2008/consulta_cand_2008_",ufs, ".txt"))
cand_2008 <- lapply(files, read.table, sep = ";", header = F, stringsAsFactors = F, fill = T, fileEncoding = "windows-1252") 
cand_2008 <- do.call("rbind", cand_2008)
names(cand_2008) <- labels_pre2012
cand_2008 <- as_tibble(cand_2008)

#candidates 2012
labels_2012 <- c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "NUM_TURNO", "DESCRICAO_ELEICAO",
                   "SIGLA_UF", "SIGLA_UE", "DESCRICAO_UE", "CODIGO_CARGO", "DESCRICAO_CARGO",
                   "NOME_CANDIDATO", "SEQUENCIAL_CANDIDATO", "NUMERO_CANDIDATO", "CPF_CANDIDATO",
                   "NOME_URNA_CANDIDATO", "COD_SITUACAO_CANDIDATURA", "DES_SITUACAO_CANDIDATURA",
                   "NUMERO_PARTIDO", "SIGLA_PARTIDO", "NOME_PARTIDO", "CODIGO_LEGENDA", "SIGLA_LEGENDA",
                   "COMPOSICAO_LEGENDA", "NOME_COLIGACAO", "CODIGO_OCUPACAO", "DESCRICAO_OCUPACAO",
                   "DATA_NASCIMENTO", "NUM_TITULO_ELEITORAL_CANDIDATO", "IDADE_DATA_ELEICAO",
                   "CODIGO_SEXO", "DESCRICAO_SEXO", "COD_GRAU_INSTRUCAO", "DESCRICAO_GRAU_INSTRUCAO",
                   "CODIGO_ESTADO_CIVIL", "DESCRICAO_ESTADO_CIVIL", "CODIGO_NACIONALIDADE",
                   "DESCRICAO_NACIONALIDADE", "SIGLA_UF_NASCIMENTO", "CODIGO_MUNICIPIO_NASCIMENTO",
                   "NOME_MUNICIPIO_NASCIMENTO", "DESPESA_MAX_CAMPANHA", "COD_SIT_TOT_TURNO",
                   "DESC_SIT_TOT_TURNO", "EMAIL_CANDIDATO")

files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/consulta_cand/consulta_cand_2012/consulta_cand_2012_",ufs, ".txt"))
cand_2012 <- lapply(files, read.table, sep = ";", header = F, stringsAsFactors = F, fill = T, fileEncoding = "windows-1252") 
cand_2012 <- do.call("rbind", cand_2012)
names(cand_2012) <- labels_2012
cand_2012 <- as_tibble(cand_2012)

#candidates 2016
labels_2016 <- c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "NUM_TURNO", "DESCRICAO_ELEICAO",
                  "SIGLA_UF", "SIGLA_UE", "DESCRICAO_UE", "CODIGO_CARGO", "DESCRICAO_CARGO",
                  "NOME_CANDIDATO", "SEQUENCIAL_CANDIDATO", "NUMERO_CANDIDATO", "CPF_CANDIDATO",
                  "NOME_URNA_CANDIDATO", "COD_SITUACAO_CANDIDATURA", "DES_SITUACAO_CANDIDATURA",
                  "NUMERO_PARTIDO", "SIGLA_PARTIDO", "NOME_PARTIDO", "CODIGO_LEGENDA", "SIGLA_LEGENDA",
                  "COMPOSICAO_LEGENDA", "NOME_COLIGACAO", "CODIGO_OCUPACAO", "DESCRICAO_OCUPACAO",
                  "DATA_NASCIMENTO", "NUM_TITULO_ELEITORAL_CANDIDATO", "IDADE_DATA_ELEICAO",
                  "CODIGO_SEXO", "DESCRICAO_SEXO", "COD_GRAU_INSTRUCAO", "DESCRICAO_GRAU_INSTRUCAO",
                  "CODIGO_ESTADO_CIVIL", "DESCRICAO_ESTADO_CIVIL", "CODIGO_COR_RACA", "DESCRICAO_COR_RACA",
                  "CODIGO_NACIONALIDADE", "DESCRICAO_NACIONALIDADE", "SIGLA_UF_NASCIMENTO", 
                  "CODIGO_MUNICIPIO_NASCIMENTO", "NOME_MUNICIPIO_NASCIMENTO", "DESPESA_MAX_CAMPANHA",
                  "COD_SIT_TOT_TURNO", "DESC_SIT_TOT_TURNO", "EMAIL_CANDIDATO")

files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/consulta_cand/consulta_cand_2016/consulta_cand_2016_",ufs, ".txt"))
cand_2016 <- lapply(files, read.table, sep = ";", header = F, stringsAsFactors = F, fill = T, fileEncoding = "latin1") 
cand_2016 <- do.call("rbind", cand_2016)
names(cand_2016) <- labels_2016
cand_2016 <- as_tibble(cand_2016)

cand_2000_2016 <- list(cand_2000, cand_2004, cand_2008, cand_2012, cand_2016)
save(cand_2000_2016, file = "~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/cand_2000_2016.RData")

#Voting data 2000
labels_pre2012 <- c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "NUM_TURNO", "DESCRICAO_ELEICAO",
                  "SIGLA_UF", "SIGLA_UE", "CODIGO_MUNICIPIO", "NOME_MUNICIPIO", "NUMERO_ZONA",
                  "CODIGO_CARGO", "NUMERO_CAND", "SQ_CANDIDATO", "NOME_CANDIDATO", "NOME_URNA_CANDIDATO",
                  "DESCRICAO_CARGO", "COD_SIT_CAND_SUPERIOR", "DESC_SIT_CAND_SUPERIOR", "CODIGO_SIT_CANDIDATO",
                  "DESC_SIT_CANDIDATO", "CODIGO_SIT_CAND_TOT", "DESC_SIT_CAND_TOT", "NUMERO_PARTIDO",
                  "SIGLA_PARTIDO", "NOME_PARTIDO", "SEQUENCIAL_LEGENDA", "NOME_COLIGACAO", "COMPOSICAO_LEGENDA",
                  "TOTAL_VOTOS")

files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/votacao_munzona/votacao_candidato_munzona_2000/votacao_candidato_munzona_2000_",ufs, ".txt"))
vot_2000 <- lapply(files, read.table, sep = ";", header = F, stringsAsFactors = F, fill = T, fileEncoding = "windows-1252") 
vot_2000 <- do.call("rbind", vot_2000)
names(vot_2000) <- labels_pre2012
vot_2000 <- as_tibble(vot_2000)

#Voting data 2004
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/votacao_munzona/votacao_candidato_munzona_2004/votacao_candidato_munzona_2004_",ufs, ".txt"))
vot_2004 <- lapply(files, read.table, sep = ";", header = F, stringsAsFactors = F, fill = T, fileEncoding = "windows-1252") 
vot_2004 <- do.call("rbind", vot_2004)
names(vot_2004) <- labels_pre2012
vot_2004 <- as_tibble(vot_2004)

#Voting data 2008
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/votacao_munzona/votacao_candidato_munzona_2008/votacao_candidato_munzona_2008_",ufs, ".txt"))
vot_2008 <- lapply(files, read.table, sep = ";", header = F, stringsAsFactors=F, fill = T, fileEncoding = "windows-1252") 
vot_2008 <- do.call("rbind", vot_2008)
names(vot_2008) <- labels_pre2012
vot_2008 <- as_tibble(vot_2008)

#voting data 2012
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/votacao_munzona/votacao_candidato_munzona_2012/votacao_candidato_munzona_2012_",ufs, ".txt"))
vot_2012 <- lapply(files, read.table, sep = ";", header = F, stringsAsFactors = F, fill = T, fileEncoding = "windows-1252") 
vot_2012 <- do.call("rbind", vot_2012)
names(vot_2012) <- labels_pre2012
vot_2012 <- as_tibble(vot_2012)

labels_2016 <- c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "NUM_TURNO", "DESCRICAO_ELEICAO",
                  "SIGLA_UF", "SIGLA_UE", "CODIGO_MUNICIPIO", "NOME_MUNICIPIO", "NUMERO_ZONA",
                  "CODIGO_CARGO", "NUMERO_CAND", "SQ_CANDIDATO", "NOME_CANDIDATO", "NOME_URNA_CANDIDATO",
                  "DESCRICAO_CARGO", "COD_SIT_CAND_SUPERIOR", "DESC_SIT_CAND_SUPERIOR", "CODIGO_SIT_CANDIDATO",
                  "DESC_SIT_CANDIDATO", "CODIGO_SIT_CAND_TOT", "DESC_SIT_CAND_TOT", "NUMERO_PARTIDO",
                  "SIGLA_PARTIDO", "NOME_PARTIDO", "SEQUENCIAL_LEGENDA", "NOME_COLIGACAO", "COMPOSICAO_LEGENDA",
                  "TOTAL_VOTOS", "TRANSITO")

#voting data 2016
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/votacao_munzona/votacao_candidato_munzona_2016/votacao_candidato_munzona_2016_",ufs, ".txt"))
vot_2016 <- lapply(files, read.table, sep=";", header=F, stringsAsFactors=F, fill = T, fileEncoding = "latin1") 
vot_2016 <- do.call("rbind", vot_2016)
names(vot_2016) <- labels_2016
vot_2016 <- as_tibble(vot_2016)

vot_2000_2016 <- list(vot_2000, vot_2004, vot_2008, vot_2012, vot_2016)
save(vot_2000_2016, file = "~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/vot_2000_2016.RData")

###################################################################
#2. Cleaning and getting it ready for analyse 
# The main goal here is getting vote margin
#2.1 Elections 2000 -----> OK
#2.2 Elections 2004 -----> FIX 
#2.3 Elections 2008 -----> OK
#2.4 Elections 2012 -----> OK
###################################################################

load("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/vot_2000_2016.RData")
load("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/cand_2000_2016.RData")

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
errors_tse <- c(24554, 27715) #For muncipality SIGLA_UE 24554:  winners listed there took office after winner was moved, and all 27715 candidates are sob judice
vot_2000v1 <- vot_2000 %>% filter(!CODIGO_MUNICIPIO %in% e_round2$CODIGO_MUNICIPIO, !CODIGO_MUNICIPIO %in% change$SIGLA_UE,
                                  !CODIGO_MUNICIPIO %in% errors_tse,
                                  DESCRICAO_CARGO == "PREFEITO")  #primeiro turno, prefeito #removind candidatos that count twice
cand_2000v1 <- cand_2000 %>% filter(!SIGLA_UE %in% e_round2$CODIGO_MUNICIPIO, !SIGLA_UE %in% change$SIGLA_UE,
                                    !SIGLA_UE %in% errors_tse,
                                    DESCRICAO_CARGO == "PREFEITO", DESC_SIT_TOT_TURNO != "#NULO#", DESC_SIT_TOT_TURNO != "") #primeiro turno, prefeito
change_vot <- vot_2000v1 %>% filter(DESC_SIT_CANDIDATO== "INDEFERIDO" |
                                      DESC_SIT_CANDIDATO== "INDEFERIDO POR IMPUGNAÇÃO" |
                                      DESC_SIT_CANDIDATO==  "CASSAÇÃO DO REGISTRO" |
                                      DESC_SIT_CANDIDATO==  "CANCELAMENTO" |
                                      DESC_SIT_CANDIDATO==  "FALECIDO") %>% distinct(CODIGO_MUNICIPIO)

vot_2000v1 <- vot_2000v1 %>% filter(!CODIGO_MUNICIPIO %in% change_vot$CODIGO_MUNICIPIO)
cand_2000v1 <- cand_2000v1 %>% filter(!SIGLA_UE %in% change_vot$CODIGO_MUNICIPIO)

#Temp Unique ID  #not using CPF or TITULO DE ELEITOR BECAUSE OF MISSING DATA and MISSING DATA IN SEQUENCIAL CANDIDATO IN 2000
cand_2000v1$temp_id <- paste0(cand_2000v1$SIGLA_UE, cand_2000v1$SEQUENCIAL_CANDIDATO, cand_2000v1$SIGLA_UF)
#vot_2000v1$temp_id <- paste0(vot_2000v1$CODIGO_MUNICIPIO, vot_2000v1$SQ_CANDIDATO, vot_2000v1$SIGLA_UF)
stopifnot(length(unique(cand_2000v1$temp_id))==nrow(cand_2000v1))

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

#######Calculating vote share for eleicoes regulares (places that had runoffs and nao eleicoes suplementares)

#Geting the right base
vot_2000_runoff <- vot_2000 %>% filter(CODIGO_MUNICIPIO %in% e_round2$CODIGO_MUNICIPIO & DESCRICAO_CARGO == "PREFEITO" 
                                       & NUM_TURNO==2)
cand_2000_runoff <- cand_2000 %>% filter(SIGLA_UE %in% e_round2$CODIGO_MUNICIPIO & NUM_TURNO == 2 &
                                           DESCRICAO_CARGO == "PREFEITO" & DESC_SIT_TOT_TURNO != "#NULO", DESC_SIT_TOT_TURNO != "")
#Check
stopifnot(length(unique(cand_2000_runoff$NUM_TITULO_ELEITORAL_CANDIDATO))==nrow(cand_2000_runoff))
#Generating temp_id
cand_2000_runoff$temp_id <- paste0(cand_2000_runoff$SIGLA_UE, cand_2000_runoff$SEQUENCIAL_CANDIDATO, cand_2000_runoff$SIGLA_UF)
stopifnot(length(unique(cand_2000_runoff$temp_id))==nrow(cand_2000_runoff))

#Selecting candidatos a prefeito, aptos to aggregate votes per municipality
vot_2000_runoffv2 <- vot_2000_runoff %>% group_by(CODIGO_MUNICIPIO, NOME_CANDIDATO, SIGLA_UF) %>% summarize(VOTO_MUN_CAND = sum(TOTAL_VOTOS))
#Check
stopifnot(length(unique(vot_2000_runoffv2$CODIGO_MUNICIPIO))==length(unique(vot_2000_runoff$CODIGO_MUNICIPIO)))

#Getting municipality's total vote and candidate vote share and renaming to merge
vot_2000_runoffv3 <- vot_2000_runoffv2  %>% group_by(CODIGO_MUNICIPIO, SIGLA_UF) %>% 
  summarize(VOTO_MUN_TOTAL = sum(VOTO_MUN_CAND)) %>%
  left_join(vot_2000_runoffv2, by=c("CODIGO_MUNICIPIO", "SIGLA_UF")) %>% 
  mutate(VOTO_CAND_SHARE = VOTO_MUN_CAND/VOTO_MUN_TOTAL) %>% group_by(CODIGO_MUNICIPIO, SIGLA_UF) %>% 
  mutate(NUMBER_CANDIDATES = n())
#Checks
table(vot_2000_runoffv3$NUMBER_CANDIDATES==2)
summary(vot_2000_runoffv3$VOTO_CAND_SHARE)

#Merging with candidate information 
#Making sure there are no NAs and keys are unique
nrow(cand_2000_runoff %>% filter(is.na(SIGLA_UE)))==0
nrow(vot_2000_runoffv3 %>% filter(is.na(CODIGO_MUNICIPIO)))==0
nrow(cand_2000_runoff %>% filter(is.na(NOME_CANDIDATO)))==0
nrow(vot_2000_runoffv3 %>% filter(is.na(NOME_CANDIDATO)))==0
nrow(cand_2000_runoff %>% filter(is.na(SIGLA_UF)))==0
nrow(vot_2000_runoffv3 %>% filter(is.na(SIGLA_UF)))==0
#Creating unique ids (to check merge by three variables, two not enough), 
vot_2000_runoffv3$id_merge <- paste0(vot_2000_runoffv3$CODIGO_MUNICIPIO, vot_2000_runoffv3$NOME_CANDIDATO, vot_2000_runoffv3$SIGLA_UF)
cand_2000_runoff$id_merge <- paste0(cand_2000_runoff$SIGLA_UE, cand_2000_runoff$NOME_CANDIDATO, cand_2000_runoff$SIGLA_UF)
nrow(vot_2000_runoffv3) == length(unique(vot_2000_runoffv3$id_merge))
nrow(cand_2000_runoff) == length(unique(cand_2000_runoff$id_merge))
#Merging
cand_2000_runoffv2 <- cand_2000_runoff %>% left_join(vot_2000_runoffv3, by=c("id_merge"))

#Debugging tactic
stopifnot(nrow(anti_join(cand_2000_runoff, vot_2000_runoffv3, by=c("id_merge")))==0)

#Ranking votes (no ties, but creating rankvoter varaible to facilitate merge later on)
cand_2000_runoffv3 <- cand_2000_runoffv2 %>%  group_by(SIGLA_UE, SIGLA_UF.x) %>% 
  mutate(rankvote = rank(-VOTO_CAND_SHARE)) %>% 
  mutate(rankvoter = rankvote)
#check
table(cand_2000_runoffv3$rankvote, cand_2000_runoffv3$DESC_SIT_TOT_TURNO)
cand_2000_runoffv3 <- cand_2000_runoffv3 %>% mutate(DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO == "ELEITO POR QUOCIENTE PARTIDÁRIO", "ELEITO", DESC_SIT_TOT_TURNO))

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
vot_2000_supv1 <- vot_2000 %>% filter(!CODIGO_MUNICIPIO %in% e_round2$CODIGO_MUNICIPIO, CODIGO_MUNICIPIO  %in% change$SIGLA_UE,
                                      #CODIGO_MUNICIPIO %in% change_vot$CODIGO_MUNICIPIO,
                                      DESCRICAO_CARGO == "PREFEITO")  #primeiro turno, prefeito
cand_2000_supv1 <- cand_2000 %>% filter(!SIGLA_UE %in% e_round2$CODIGO_MUNICIPIO, SIGLA_UE %in% change$SIGLA_UE,
                                        #SIGLA_UE %in% errors_tse, SIGLA_UE %in% change_vot$CODIGO_MUNICIPIO,
                                        DESCRICAO_CARGO == "PREFEITO", DESC_SIT_TOT_TURNO != "#NULO#", 
                                        DESC_SIT_TOT_TURNO != "") #primeiro turno, prefeito
#Check
cand_2000_supv1$temp_id <- paste0(cand_2000_supv1$SIGLA_UE, cand_2000_supv1$SEQUENCIAL_CANDIDATO, cand_2000_supv1$SIGLA_UF)
stopifnot(length(unique(cand_2000_supv1$temp_id))==nrow(cand_2000_supv1))

vot_2000_supv1 <- vot_2000_supv1 %>% filter(DESC_SIT_CANDIDATO != "HOMOLOGAÇÃO DE RENÚNCIA") 
#Excluding one case with unclear winner
vot_2000_supv1 <- vot_2000_supv1 %>% filter(CODIGO_MUNICIPIO != 66931) 
cand_2000_supv1 <- cand_2000_supv1 %>% filter(SIGLA_UE != 66931) 


#Selecting candidatos aptos to aggregate votes per municipality 
#(removing places that had a runoff vote from first rounds and removing eleicoes nao regulares)
#Not using apto ou nao apto because that is not available
#included UF to get unique ID
vot_2000_supv2 <- vot_2000_supv1 %>% group_by(CODIGO_MUNICIPIO, NOME_CANDIDATO, SIGLA_UF) %>% 
  summarize(VOTO_MUN_CAND = sum(TOTAL_VOTOS))
#Check
stopifnot(length(unique(vot_2000_supv2$CODIGO_MUNICIPIO))==length(unique(vot_2000_supv1$CODIGO_MUNICIPIO)))

#Getting municipality's total vote and candidate vote share 
vot_2000_supv3 <- vot_2000_supv2 %>% group_by(CODIGO_MUNICIPIO, SIGLA_UF) %>% summarize(VOTO_MUN_TOTAL = sum(VOTO_MUN_CAND)) %>% 
  left_join(vot_2000_supv2, by = c("CODIGO_MUNICIPIO", "SIGLA_UF")) %>% 
  mutate(VOTO_CAND_SHARE = VOTO_MUN_CAND/VOTO_MUN_TOTAL) %>% 
  group_by(CODIGO_MUNICIPIO, SIGLA_UF) %>% mutate(NUMBER_CANDIDATES = n()) 
#Check
stopifnot(min(vot_2000_supv3$NUMBER_CANDIDATES)==1)
stopifnot(vot_2000_supv3$VOTO_CAND_SHARE >= 0 & vot_2000_supv3$VOTO_CAND_SHARE <= 1)

#Merging with candidate information
#Making sure there are no NAs and keys are unique
nrow(cand_2000_supv1 %>% filter(is.na(SIGLA_UE)))==0
nrow(vot_2000_supv3 %>% filter(is.na(CODIGO_MUNICIPIO)))==0
nrow(cand_2000_supv1 %>% filter(is.na(NOME_CANDIDATO)))==0
nrow(vot_2000_supv3 %>% filter(is.na(NOME_CANDIDATO)))==0
nrow(cand_2000_supv1 %>% filter(is.na(SIGLA_UF)))==0
nrow(vot_2000_supv3 %>% filter(is.na(SIGLA_UF)))==0
#Creating unique ids (to check merge by three variables, two not enough), 
vot_2000_supv3$id_merge <- paste0(vot_2000_supv3$CODIGO_MUNICIPIO, vot_2000_supv3$NOME_CANDIDATO, vot_2000_supv3$SIGLA_UF)
cand_2000_supv1$id_merge <- paste0(cand_2000_supv1$SIGLA_UE, cand_2000_supv1$NOME_CANDIDATO, cand_2000_supv1$SIGLA_UF)
nrow(vot_2000_supv3) == length(unique(vot_2000_supv3$id_merge))
nrow(cand_2000_supv1) == length(unique(cand_2000_supv1$id_merge))
#Merging
cand_2000_supv2 <- cand_2000_supv1 %>% left_join(vot_2000_supv3, by=c("id_merge"))

#Debugging #which do not merge?
bugs <- anti_join(cand_2000_supv2, vot_2000_supv3, by=c("id_merge"))
summary(bugs$VOTO_CAND_SHARE) #all NA's
table(bugs$DESC_SIT_TOT_TURNO) #how can you have eleito is vote share == NA?
temp <- bugs %>% filter(DESC_SIT_TOT_TURNO == "ELEITO")
#Eliminating cases with NA vote share, eliminating places with only one candidate, and using age rule to break ties
cand_2000_supv3 <- cand_2000_supv2 %>% filter(!is.na(VOTO_CAND_SHARE)) 

cand_2000_supv4 <- cand_2000_supv3 %>% filter(NUMBER_CANDIDATES != 1) %>% group_by(SIGLA_UE, SIGLA_UF.x) %>% 
  mutate(rankvote = rank(-VOTO_CAND_SHARE)) %>% 
  mutate(rankvoter = ifelse(rankvote == 1.5 & DESC_SIT_TOT_TURNO == "ELEITO", 1,
                            ifelse(rankvote == 1.5 & DESC_SIT_TOT_TURNO == "NÃO ELEITO", 2, rankvote)))
#check
table(cand_2000_supv4$DESC_SIT_TOT_TURNO, cand_2000_supv4$rankvoter) 

####Vote margin (share and absolute)

#getting winner and runnerups
cand_2000_supv5 <- cand_2000_supv4 %>% filter(rankvoter== 1 | rankvoter== 2)
table(cand_2000_supv5$DESC_SIT_TOT_TURNO) #half and half

#Getting vote margins
muns_sup <- unique(cand_2000_supv5$SIGLA_UE)
#Check
stopifnot(length(unique(muns_sup))==length(muns_sup))

electionsf_sup_2000 <- NULL

for (i in 1:length(muns_sup)){
  
  mun <- cand_2000_supv5[which(cand_2000_supv5$SIGLA_UE==muns_sup[i]), ]
  
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
#Renaming and cleaning up for binding
electionsff_2000 <- electionsff_2000 %>% select(-c(NOME_CANDIDATO.y,SIGLA_UF.y, temp_id, id_merge, CODIGO_MUNICIPIO)) %>%
                    rename(SIGLA_UF = SIGLA_UF.x, NOME_CANDIDATO = NOME_CANDIDATO.x)

save(electionsff_2000, file="~/Dropbox/LOCAL_ELECTIONS/repositorio_data/final_data/electionsff_2000.Rda")

#######Elections 2004  
vot_2004 <- vot_2000_2016[[2]]
cand_2004 <- cand_2000_2016[[2]]

#To start with, equal numbers of municipalities in 2004 elections
length(unique(cand_2004$SIGLA_UE))
length(unique(vot_2004$SIGLA_UE))

#Excluding places with runoff from first round 
#because I am interested in vote margin in the decisive round
e_round2 <- vot_2004 %>% filter(NUM_TURNO == 2) %>% distinct(SIGLA_UE)
#Excluding places where result changed after election (similar to eleicoes suplementares)
change <- cand_2004 %>% filter(DESCRICAO_CARGO == "PREFEITO") %>% 
          filter(DESC_SIT_TOT_TURNO=="RENÚNCIA/FALECIMENTO/CASSAÇÃO APÓS A ELEIÇÃO"|
          DESC_SIT_TOT_TURNO=="REGISTRO NEGADO APÓS A ELEIÇÃO" |
          DESC_SIT_TOT_TURNO=="RENÚNCIA/FALECIMENTO COM SUBSTITUIÇÃO") %>% distinct(SIGLA_UE)
errors_tse <- 31518 #For muncipality SIGLA_UE 31518: file vot_2004 does not have votes for winning candidate
errors_tse_type1 <- vot_2004 %>% filter(DESC_SIT_CAND_TOT == "RENÚNCIA/FALECIMENTO COM SUBSTITUIÇÃO") %>% distinct(SIGLA_UE)    
#candidates who replaced candidates with "RENÚNCIA/FALECIMENTO COM SUBSTITUIÇÃO" are not in the
#candidates database only in the votacao_mun_zona database
  
vot_2004v1 <- vot_2004 %>% filter(!SIGLA_UE %in% e_round2$SIGLA_UE, !SIGLA_UE %in% change$SIGLA_UE,
                                  !SIGLA_UE %in% errors_tse, !SIGLA_UE %in% errors_tse_type1$SIGLA_UE,
                                  DESCRICAO_CARGO == "PREFEITO")  #primeiro turno, prefeito
cand_2004v1 <- cand_2004 %>% filter(!SIGLA_UE %in% e_round2$SIGLA_UE, !SIGLA_UE %in% change$SIGLA_UE,
                                    !SIGLA_UE %in% errors_tse, !SIGLA_UE %in% errors_tse, !SIGLA_UE %in% errors_tse_type1$SIGLA_UE,
                                    DESCRICAO_CARGO == "PREFEITO", DESC_SIT_TOT_TURNO != "#NULO#") #primeiro turno, prefeito
#Check
stopifnot(length(unique(cand_2004v1$NUM_TITULO_ELEITORAL_CANDIDATO))==nrow(cand_2004v1))

#Selecting candidatos aptos to aggregate votes per municipality 
#(removing places that had a runoff vote from first rounds and removing eleicoes nao regulares)
#Not using apto ou nao apto because that is not available
#included UF to get unique ID
vot_2004v2 <- vot_2004v1 %>% group_by(SIGLA_UE, SQ_CANDIDATO, SIGLA_UF) %>% 
              summarize(VOTO_MUN_CAND = sum(TOTAL_VOTOS))
#Check
stopifnot(length(unique(vot_2004v2$SIGLA_UE))==length(unique(vot_2004v1$SIGLA_UE)))

#Getting municipality's total vote and candidate vote share and renaming to merge
vot_2004v3 <- vot_2004v2 %>% group_by(SIGLA_UE, SIGLA_UF) %>% summarize(VOTO_MUN_TOTAL = sum(VOTO_MUN_CAND)) %>% 
              left_join(vot_2004v2, by = c("SIGLA_UE", "SIGLA_UF")) %>% 
              mutate(VOTO_CAND_SHARE = VOTO_MUN_CAND/VOTO_MUN_TOTAL) %>% 
              group_by(SIGLA_UE, SIGLA_UF) %>% mutate(NUMBER_CANDIDATES = n()) %>% 
              rename(SEQUENCIAL_CANDIDATO = SQ_CANDIDATO)
#Check
stopifnot(min(vot_2004v3$NUMBER_CANDIDATES)==1)
stopifnot(vot_2004v3$VOTO_CAND_SHARE >= 0 & vot_2004v3$VOTO_CAND_SHARE <= 1)

#Merging with candidate information
#Making sure there are no NAs and keys are unique
nrow(cand_2004v1 %>% filter(is.na(SIGLA_UE)))==0
nrow(vot_2004v3 %>% filter(is.na(SIGLA_UE)))==0
nrow(cand_2004v1 %>% filter(is.na(SEQUENCIAL_CANDIDATO)))==0
nrow(vot_2004v3 %>% filter(is.na(SEQUENCIAL_CANDIDATO)))==0
nrow(cand_2004v1 %>% filter(is.na(SIGLA_UF)))==0
nrow(vot_2004v3 %>% filter(is.na(SIGLA_UF)))==0
#Creating unique ids (to check merge by three variables, two not enough), 
#it's okay, they are unique #after checking once, no need to create it
#vot_2004v3$id_merge <- paste0(vot_2004v3$SIGLA_UE, vot_2004v3$SEQUENCIAL_CANDIDATO, vot_2004v3$SIGLA_UF)
#cand_2004v1$id_merge <- paste0(cand_2004v1$SIGLA_UE, cand_2004v1$SEQUENCIAL_CANDIDATO, cand_2004v1$SIGLA_UF)
#nrow(vot_2004v3) == length(unique(vot_2004v3$id_merge))
#row(cand_2004v1) == length(unique(cand_2004v1$id_merge))

#Merging
cand_2004v2 <- cand_2004v1 %>% left_join(vot_2004v3, by=c("SIGLA_UE", "SEQUENCIAL_CANDIDATO", "SIGLA_UF"))

#Debugging #which do not merge?
bugs <- anti_join(cand_2004v2, vot_2004v3, by=c("SIGLA_UE", "SEQUENCIAL_CANDIDATO", "SIGLA_UF"))
summary(bugs$VOTO_CAND_SHARE) #all NA's
table(bugs$DESC_SIT_TOT_TURNO)

#Eliminating cases with NA vote share, eliminating places with only one candidate, and using age rule to break ties
cand_2004v3 <- cand_2004v2 %>% filter(!is.na(VOTO_CAND_SHARE)) 

cand_2004v4 <- cand_2004v3 %>% filter(NUMBER_CANDIDATES != 1) %>% group_by(SIGLA_UE, SIGLA_UF) %>% 
               mutate(rankvote = rank(-VOTO_CAND_SHARE)) %>% 
               mutate(rankvoter = ifelse(rankvote == 1.5 & DESC_SIT_TOT_TURNO == "ELEITO", 1,
               ifelse(rankvote == 1.5 & DESC_SIT_TOT_TURNO == "NÃO ELEITO", 2, rankvote)))
#check
table(cand_2004v4$DESC_SIT_TOT_TURNO, cand_2004v4$rankvoter) 

#######Calculating vote share for eleicoes regulares (places that had runoffs and nao eleicoes suplementares)

#Geting the right base
vot_2004_runoff <- vot_2004 %>% filter(SIGLA_UE %in% e_round2$SIGLA_UE & DESCRICAO_CARGO == "PREFEITO" 
                                       & NUM_TURNO==2)
cand_2004_runoff <- cand_2004 %>% filter(SIGLA_UE %in% e_round2$SIGLA_UE & NUM_TURNO == 2 &
                                         DESCRICAO_CARGO == "PREFEITO" & DESC_SIT_TOT_TURNO != "#NULO")
#Check
stopifnot(length(unique(cand_2004_runoff$NUM_TITULO_ELEITORAL_CANDIDATO))==nrow(cand_2004_runoff))

#Selecting candidatos a prefeito, aptos to aggregate votes per municipality
vot_2004_runoffv2 <- vot_2004_runoff %>% group_by(SIGLA_UE, SQ_CANDIDATO, SIGLA_UF) %>% summarize(VOTO_MUN_CAND = sum(TOTAL_VOTOS))
#Check
stopifnot(length(unique(vot_2004_runoffv2$SIGLA_UE))==length(unique(vot_2004_runoff$SIGLA_UE)))

#Getting municipality's total vote and candidate vote share and renaming to merge
vot_2004_runoffv3 <- vot_2004_runoffv2  %>% group_by(SIGLA_UE, SIGLA_UF) %>% 
                      summarize(VOTO_MUN_TOTAL = sum(VOTO_MUN_CAND)) %>%
                      left_join(vot_2004_runoffv2, by=c("SIGLA_UE", "SIGLA_UF")) %>% 
                      mutate(VOTO_CAND_SHARE = VOTO_MUN_CAND/VOTO_MUN_TOTAL) %>% group_by(SIGLA_UE, SIGLA_UF) %>% 
                      mutate(NUMBER_CANDIDATES = n()) %>% rename(SEQUENCIAL_CANDIDATO = SQ_CANDIDATO)
#Checks
table(vot_2004_runoffv3$NUMBER_CANDIDATES==2)
summary(vot_2004_runoffv3$VOTO_CAND_SHARE)

#Merging with candidate information 
#Making sure there are no NAs and keys are unique
nrow(cand_2004_runoff %>% filter(is.na(SEQUENCIAL_CANDIDATO)))==0
nrow(vot_2004_runoffv3 %>% filter(is.na(SEQUENCIAL_CANDIDATO)))==0
cand_2004_runoffv2 <- cand_2004_runoff %>% 
                      left_join(vot_2004_runoffv3, by=c("SIGLA_UE", "SEQUENCIAL_CANDIDATO", "SIGLA_UF"))

#Debugging tactic
stopifnot(nrow(anti_join(cand_2004_runoff, vot_2004_runoffv3, by=c("SIGLA_UE", "SEQUENCIAL_CANDIDATO", "SIGLA_UF")))==0)

#Ranking votes (no ties, but creating rankvoter varaible to facilitate merge later on)
cand_2004_runoffv3 <- cand_2004_runoffv2 %>%  group_by(SIGLA_UE, SIGLA_UF) %>% 
                      mutate(rankvote = rank(-VOTO_CAND_SHARE)) %>% 
                      mutate(rankvoter = rankvote)
#check
table(cand_2004_runoffv3$rankvote, cand_2004_runoffv3$DESC_SIT_TOT_TURNO)

#Binding first round with runoff candidates
cand_2004v5 <- bind_rows(cand_2004v4, cand_2004_runoffv3)

####Vote margin (share and absolute)

#getting winner and runnerups
cand_2004v6 <- cand_2004v5 %>% filter(rankvoter== 1 | rankvoter== 2)
table(cand_2004v6$DESC_SIT_TOT_TURNO)

#Getting vote margins
muns <- unique(cand_2004v6$SIGLA_UE)
#Check
stopifnot(length(unique(muns))==length(muns))

electionsf_2004 <- NULL

for (i in 1:length(muns)){
  
  mun <- cand_2004v6[which(cand_2004v6$SIGLA_UE==muns[i]), ]
  
  vote_margin_share <- max(mun$VOTO_CAND_SHARE) - min(mun$VOTO_CAND_SHARE) 
  vote_margin_abs <- max(mun$VOTO_MUN_CAND) - min(mun$VOTO_MUN_CAND)
  winner <- mun %>% filter(DESC_SIT_TOT_TURNO == "ELEITO")
  runner_up <- mun %>% filter(DESC_SIT_TOT_TURNO == "NÃO ELEITO")
  
  margin_winner <- winner %>% mutate(vote_margin_share = vote_margin_share, vote_margin_abs = vote_margin_abs)
  margin_runner <- runner_up %>% mutate(vote_margin_share = -vote_margin_share, vote_margin_abs = -vote_margin_abs)
  
  electionsf_2004 <- bind_rows(electionsf_2004, margin_winner, margin_runner)
  print(i)
  
} 

#######Calculating vote share for eleicoes nao-regulares 

#only with places where results changed after elections
vot_2004_supv1 <- vot_2004 %>% filter(!SIGLA_UE %in% e_round2$SIGLA_UE, SIGLA_UE %in% change$SIGLA_UE,
                                      !SIGLA_UE %in% errors_tse, !SIGLA_UE %in% errors_tse_type1$SIGLA_UE,
                                      DESCRICAO_CARGO == "PREFEITO")  #primeiro turno, prefeito
cand_2004_supv1 <- cand_2004 %>% filter(!SIGLA_UE %in% e_round2$SIGLA_UE, SIGLA_UE %in% change$SIGLA_UE,
                                        !SIGLA_UE %in% errors_tse, !SIGLA_UE %in% errors_tse_type1$SIGLA_UE,
                                        DESCRICAO_CARGO == "PREFEITO", DESC_SIT_TOT_TURNO != "#NULO#") #primeiro turno, prefeito
#Check
stopifnot(length(unique(cand_2004_supv1$NUM_TITULO_ELEITORAL_CANDIDATO))==nrow(cand_2004_supv1))

#Selecting candidatos aptos to aggregate votes per municipality 
#(removing places that had a runoff vote from first rounds and removing eleicoes nao regulares)
#Not using apto ou nao apto because that is not available
#included UF to get unique ID
vot_2004_supv2 <- vot_2004_supv1 %>% group_by(SIGLA_UE, SQ_CANDIDATO, SIGLA_UF) %>% 
                  summarize(VOTO_MUN_CAND = sum(TOTAL_VOTOS))
#Check
stopifnot(length(unique(vot_2004_supv2$SIGLA_UE))==length(unique(vot_2004_supv1$SIGLA_UE)))

#Getting municipality's total vote and candidate vote share and renaming to merge
vot_2004_supv3 <- vot_2004_supv2 %>% group_by(SIGLA_UE, SIGLA_UF) %>% summarize(VOTO_MUN_TOTAL = sum(VOTO_MUN_CAND)) %>% 
                  left_join(vot_2004_supv2, by = c("SIGLA_UE", "SIGLA_UF")) %>% 
                  mutate(VOTO_CAND_SHARE = VOTO_MUN_CAND/VOTO_MUN_TOTAL) %>% 
                  group_by(SIGLA_UE, SIGLA_UF) %>% mutate(NUMBER_CANDIDATES = n()) %>% 
                  rename(SEQUENCIAL_CANDIDATO = SQ_CANDIDATO)
#Check
stopifnot(vot_2004_supv3$VOTO_CAND_SHARE >= 0 & vot_2004_supv3$VOTO_CAND_SHARE <= 1)
stopifnot(min(vot_2004_supv3$NUMBER_CANDIDATES)==1)

#Merging with candidate information
#Making sure there are no NAs and keys are unique
nrow(cand_2004_supv1 %>% filter(is.na(SIGLA_UE)))==0
nrow(vot_2004_supv3 %>% filter(is.na(SIGLA_UE)))==0
nrow(cand_2004_supv1 %>% filter(is.na(SEQUENCIAL_CANDIDATO)))==0
nrow(vot_2004_supv3 %>% filter(is.na(SEQUENCIAL_CANDIDATO)))==0
nrow(cand_2004_supv1 %>% filter(is.na(SIGLA_UF)))==0
nrow(vot_2004_supv3 %>% filter(is.na(SIGLA_UF)))==0
#Creating unique ids (to check merge by three variables, two not enough), it's okay, they are unique #after checking once, no need to create it
#vot_2004v3$id_merge <- paste0(vot_2004v3$SIGLA_UE, vot_2004v3$SEQUENCIAL_CANDIDATO, vot_2004v3$SIGLA_UF)
#cand_2004v1$id_merge <- paste0(cand_2004v1$SIGLA_UE, cand_2004v1$SEQUENCIAL_CANDIDATO, cand_2004v1$SIGLA_UF)
#nrow(vot_2004v3) == length(unique(vot_2004v3$id_merge))
#row(cand_2004v1) == length(unique(cand_2004v1$id_merge))

#Merging
cand_2004_supv2 <- cand_2004_supv1 %>% left_join(vot_2004_supv3, by=c("SIGLA_UE", "SEQUENCIAL_CANDIDATO", "SIGLA_UF"))

#Debugging #which do not merge?
bugs <- anti_join(cand_2004_supv2, vot_2004_supv3, by=c("SIGLA_UE", "SEQUENCIAL_CANDIDATO", "SIGLA_UF"))
summary(bugs$VOTO_CAND_SHARE) #all NA's
#View(bugs %>% filter(DES_SITUACAO_CANDIDATURA=="DEFERIDO")) #situacao turno == Nao eleito (3), Why?
table(bugs$DESC_SIT_TOT_TURNO)

#Eliminating cases with NA vote share, eliminating places with only one candidate, and using age rule to break ties
cand_2004_supv3 <- cand_2004_supv2 %>% filter(!is.na(VOTO_CAND_SHARE)) 

cand_2004_supv4 <- cand_2004_supv3 %>% filter(NUMBER_CANDIDATES != 1) %>% group_by(SIGLA_UE, SIGLA_UF) %>% 
                   mutate(rankvote = rank(-VOTO_CAND_SHARE)) %>% 
                   mutate(rankvoter = ifelse(rankvote == 1.5 & DESC_SIT_TOT_TURNO == "ELEITO", 1,
                   ifelse(rankvote == 1.5 & DESC_SIT_TOT_TURNO == "NÃO ELEITO", 2, rankvote)))
cand_2004_supv5 <- cand_2004_supv4 %>% mutate(DESC_SIT_TOT_TURNO = ifelse(rankvote == 1 & 
                                       DESC_SIT_TOT_TURNO == "RENÚNCIA/FALECIMENTO COM SUBSTITUIÇÃO", "ELEITO",
                                       ifelse(rankvote == 2 & DESC_SIT_TOT_TURNO == "RENÚNCIA/FALECIMENTO COM SUBSTITUIÇÃO", 
                                       "NÃO ELEITO", DESC_SIT_TOT_TURNO)))
table(cand_2004_supv5$DESC_SIT_TOT_TURNO, cand_2004_supv5$rankvoter) 

####Vote margin (share and absolute)

#getting winner and runnerups
cand_2004_supv6 <- cand_2004_supv5 %>% filter(rankvoter== 1 | rankvoter== 2)
table(cand_2004_supv6$DESC_SIT_TOT_TURNO) #half and half

#Getting vote margins
muns_sup <- unique(cand_2004_supv6$SIGLA_UE)
#Check
stopifnot(length(unique(muns_sup))==length(muns_sup))

electionsf_sup_2004 <- NULL

for (i in 1:length(muns_sup)){
  
  mun <- cand_2004_supv6[which(cand_2004_supv6$SIGLA_UE==muns_sup[i]), ]
  
  vote_margin_share <- max(mun$VOTO_CAND_SHARE) - min(mun$VOTO_CAND_SHARE) 
  vote_margin_abs <- max(mun$VOTO_MUN_CAND) - min(mun$VOTO_MUN_CAND)
  winner <- mun %>% filter(DESC_SIT_TOT_TURNO == "ELEITO")
  runner_up <- mun %>% filter(DESC_SIT_TOT_TURNO == "NÃO ELEITO")
  
  margin_winner <- winner %>% mutate(vote_margin_share = vote_margin_share, vote_margin_abs = vote_margin_abs)
  margin_runner <- runner_up %>% mutate(vote_margin_share = -vote_margin_share, vote_margin_abs = -vote_margin_abs)
  
  electionsf_sup_2004 <- bind_rows(electionsf_sup_2004, margin_winner, margin_runner)
  print(i)
  
} 

######### Places with errors in candidate data 
######### (candidate who replaces the candidate who resigned is not in the candidate database)

#Place with error (duplicated candidayte)

#only with places where results changed after elections
vot_2004_errv1 <- vot_2004 %>% filter(SIGLA_UE %in% errors_tse_type1$SIGLA_UE,
                                      DESCRICAO_CARGO == "PREFEITO") 
cand_2004_errv1 <- cand_2004 %>% filter(SIGLA_UE %in% errors_tse_type1$SIGLA_UE,
                                        DESCRICAO_CARGO == "PREFEITO") 
#Check
stopifnot(length(unique(cand_2004_errv1$NUM_TITULO_ELEITORAL_CANDIDATO))==nrow(cand_2004_errv1))

#Removing candidates substituidos from vot_2004 to avoid double counting
vot_2004_errv2 <- vot_2004_errv1 %>% filter(DESC_SIT_CAND_TOT != "RENÚNCIA/FALECIMENTO COM SUBSTITUIÇÃO")

#Selecting candidatos aptos to aggregate votes per municipality 
#(removing places that had a runoff vote from first rounds and removing eleicoes nao regulares)
#Not using apto ou nao apto because that is not available
#included UF to get unique ID
vot_2004_errv3 <- vot_2004_errv2 %>% group_by(SIGLA_UE, SQ_CANDIDATO, SIGLA_UF) %>% 
                           summarize(VOTO_MUN_CAND = sum(TOTAL_VOTOS))
#Check
stopifnot(length(unique(vot_2004_errv3$SIGLA_UE))==length(unique(vot_2004_errv1$SIGLA_UE)))

#Getting municipality's total vote and candidate vote share and renaming to merge
vot_2004_errv4 <- vot_2004_errv3 %>% group_by(SIGLA_UE, SIGLA_UF) %>% summarize(VOTO_MUN_TOTAL = sum(VOTO_MUN_CAND)) %>% 
  left_join(vot_2004_errv3, by = c("SIGLA_UE", "SIGLA_UF")) %>% 
  mutate(VOTO_CAND_SHARE = VOTO_MUN_CAND/VOTO_MUN_TOTAL) %>% 
  group_by(SIGLA_UE, SIGLA_UF) %>% mutate(NUMBER_CANDIDATES = n()) %>% 
  rename(SEQUENCIAL_CANDIDATO = SQ_CANDIDATO)
#Check
stopifnot(vot_2004_errv4$VOTO_CAND_SHARE >= 0 & vot_2004_errv4$VOTO_CAND_SHARE <= 1)

#Merging with candidate information
#Making sure there are no NAs and keys are unique
nrow(cand_2004_errv1 %>% filter(is.na(SIGLA_UE)))==0
nrow(vot_2004_errv4 %>% filter(is.na(SIGLA_UE)))==0
nrow(cand_2004_errv1 %>% filter(is.na(SEQUENCIAL_CANDIDATO)))==0
nrow(vot_2004_errv4 %>% filter(is.na(SEQUENCIAL_CANDIDATO)))==0
nrow(cand_2004_errv1 %>% filter(is.na(SIGLA_UF)))==0
nrow(vot_2004_errv4 %>% filter(is.na(SIGLA_UF)))==0
#Creating unique ids (to check merge by three variables, two not enough), it's okay, they are unique #after checking once, no need to create it
#vot_2004v3$id_merge <- paste0(vot_2004v3$SIGLA_UE, vot_2004v3$SEQUENCIAL_CANDIDATO, vot_2004v3$SIGLA_UF)
#cand_2004v1$id_merge <- paste0(cand_2004v1$SIGLA_UE, cand_2004v1$SEQUENCIAL_CANDIDATO, cand_2004v1$SIGLA_UF)
#nrow(vot_2004v3) == length(unique(vot_2004v3$id_merge))
#row(cand_2004v1) == length(unique(cand_2004v1$id_merge))

#Merging is different now because substitute candidates are not
# in candidates database
cand_vot_2004_err <- vot_2004_errv2 %>% filter(DESC_SIT_CANDIDATO != "DEFERIDO" & DESC_SIT_CANDIDATO != "SUB JUDICE" &
                                               SIGLA_UE != 7234 & SIGLA_UE != 22250) #getting data of candidates who are left out of candidates database
#Candidates who appear twice because of multiple zonas eleitorais (selecting once)
cand_vot_2004_err_one <- vot_2004_errv2 %>% filter(SIGLA_UE == 7234 & DESC_SIT_CANDIDATO != "DEFERIDO" & DESC_SIT_CANDIDATO != "SUB JUDICE" & NUMERO_ZONA == 13)
cand_vot_2004_err_two <- vot_2004_errv2 %>% filter(SIGLA_UE == 22250 & DESC_SIT_CANDIDATO != "DEFERIDO" & DESC_SIT_CANDIDATO != "SUB JUDICE" & NUMERO_ZONA == 35)

cand_2004_errv2 <-  cand_2004_errv1 %>% filter(DESC_SIT_TOT_TURNO != "RENÚNCIA/FALECIMENTO COM SUBSTITUIÇÃO", 
                                               DESC_SIT_TOT_TURNO != "#NULO#") #excluding canidates who are replaced

cand_vot_2004_err0 <- bind_rows(cand_vot_2004_err, cand_vot_2004_err_one, cand_vot_2004_err_two)
cand_vot_2004_errv1 <- cand_vot_2004_err0 %>% select(ANO_ELEICAO, CODIGO_CARGO, DATA_GERACAO, DESCRICAO_CARGO, DESCRICAO_ELEICAO,
                                                    HORA_GERACAO, NOME_CANDIDATO, NOME_COLIGACAO, DESC_SIT_CAND_TOT,
                                                    NOME_PARTIDO, NOME_URNA_CANDIDATO, NUM_TURNO, NUMERO_CAND, NUMERO_PARTIDO,
                                                    SIGLA_PARTIDO, SQ_CANDIDATO,  SIGLA_UE, SIGLA_UF, SQ_CANDIDATO) %>% 
                                                    rename(NUMERO_CANDIDATO = NUMERO_CAND, SEQUENCIAL_CANDIDATO = SQ_CANDIDATO, 
                                                           DESC_SIT_TOT_TURNO = DESC_SIT_CAND_TOT)
#adding missing candidates to candidates database
missing_cols <- c("DESCRICAO_UE", "CPF_CANDIDATO", "NOME_URNA_CANDIDATO", "COD_SITUACAO_CANDIDATURA", "DES_SITUACAO_CANDIDATURA",
 "CODIGO_LEGENDA", "SIGLA_LEGENDA",  "COMPOSICAO_LEGENDA",              
 "CODIGO_OCUPACAO", "DESCRICAO_OCUPACAO", "DATA_NASCIMENTO", "NUM_TITULO_ELEITORAL_CANDIDATO",
 "IDADE_DATA_ELEICAO", "CODIGO_SEXO", "DESCRICAO_SEXO", "COD_GRAU_INSTRUCAO",         
 "DESCRICAO_GRAU_INSTRUCAO", "CODIGO_ESTADO_CIVIL", "DESCRICAO_ESTADO_CIVIL", "CODIGO_NACIONALIDADE",          
 "DESCRICAO_NACIONALIDADE", "SIGLA_UF_NASCIMENTO", "CODIGO_MUNICIPIO_NASCIMENTO", "NOME_MUNICIPIO_NASCIMENTO",    
 "DESPESA_MAX_CAMPANHA", "COD_SIT_TOT_TURNO")

missing <- matrix(NA, 88, length(missing_cols))
colnames(missing) <- missing_cols
missing <- as_tibble(missing)

cand_vot_2004_errv3 <- bind_cols(cand_vot_2004_errv1, missing)

cand_2004_errv4 <- bind_rows(cand_2004_errv2, cand_vot_2004_errv3)

#Merging corrected candidate data with votes data
cand_2004_errv5 <- cand_2004_errv4 %>% left_join(vot_2004_errv4, by=c("SIGLA_UE", "SEQUENCIAL_CANDIDATO", "SIGLA_UF"))

#Okay to remove NAs in VOTO_MUN_TOTAL because these candidates did not run
cand_2004_errv6 <- cand_2004_errv5 %>% filter(!is.na(VOTO_MUN_CAND))


#Breaking ties in election using candidate age
cand_2004_errv7 <- cand_2004_errv6 %>% group_by(SIGLA_UE, SIGLA_UF) %>% 
  mutate(rankvote = rank(-VOTO_CAND_SHARE)) %>% 
  mutate(rankvoter = ifelse(rankvote == 1.5 & DESC_SIT_TOT_TURNO == "ELEITO", 1,
                            ifelse(rankvote == 1.5 & DESC_SIT_TOT_TURNO == "NÃO ELEITO", 2, rankvote)))


table(cand_2004_errv7$DESC_SIT_TOT_TURNO, cand_2004_errv7$rankvoter) #half and half

####Vote margin (share and absolute)

#getting winner and runnerups
cand_2004_errv8 <- cand_2004_errv7 %>% filter(rankvoter== 1 | rankvoter== 2)
table(cand_2004_errv8$DESC_SIT_TOT_TURNO) #should be half and half

#Getting vote margins
muns_err <- unique(cand_2004_errv8$SIGLA_UE)
#Check
stopifnot(length(unique(muns_sup))==length(muns_sup))

electionsf_err_2004 <- NULL

for (i in 1:length(muns_err)){
  
  mun <- cand_2004_errv8[which(cand_2004_errv8$SIGLA_UE==muns_err[i]), ]
  
  vote_margin_share <- max(mun$VOTO_CAND_SHARE) - min(mun$VOTO_CAND_SHARE) 
  vote_margin_abs <- max(mun$VOTO_MUN_CAND) - min(mun$VOTO_MUN_CAND)
  winner <- mun %>% filter(DESC_SIT_TOT_TURNO == "ELEITO")
  runner_up <- mun %>% filter(DESC_SIT_TOT_TURNO == "NÃO ELEITO")
  
  margin_winner <- winner %>% mutate(vote_margin_share = vote_margin_share, vote_margin_abs = vote_margin_abs)
  margin_runner <- runner_up %>% mutate(vote_margin_share = -vote_margin_share, vote_margin_abs = -vote_margin_abs)
  
  electionsf_err_2004 <- bind_rows(electionsf_err_2004, margin_winner, margin_runner)
  print(i)
  
} 

######### Binding elections (regulares and nao regulares)
electionsf_2004 <- electionsf_2004 %>% bind_cols(data_frame(TYPE_ELECTION = rep("regular", nrow(electionsf_2004))))  
electionsf_sup_2004 <- electionsf_sup_2004 %>% bind_cols(data_frame(TYPE_ELECTION = rep("nonregular", nrow(electionsf_sup_2004))))                                            
electionsf_err_2004 <- electionsf_err_2004 %>% bind_cols(data_frame(TYPE_ELECTION = rep("nonregular", nrow(electionsf_err_2004))))                                            

electionsff_2004 <- bind_rows(electionsf_2004, electionsf_sup_2004, electionsf_err_2004)

#Simple Smell tests
table(electionsff_2004$TYPE_ELECTION)
table(electionsff_2004$DESC_SIT_TOT_TURNO) #half and half
table(electionsff_2004$rankvoter) #half and half

######## TESTING IDENTIFIERS' INTEGRITY (length of codes, number of municipalities, number of elected mayors, number of nonelected)
#Municipality's identifier's 
str(electionsff_2004)
glimpse(electionsff_2004)
table(nchar(electionsff_2004$CPF_CANDIDATO)) #existem erros
table(nchar(electionsff_2004$NUM_TITULO_ELEITORAL_CANDIDATO)) #existem erros

save(electionsff_2004, file="~/Dropbox/LOCAL_ELECTIONS/repositorio_data/final_data/electionsff_2004.Rda")

#######Elections 2008
vot_2008 <- vot_2000_2016[[3]]
cand_2008 <- cand_2000_2016[[3]]

#To start with, unequal numbers of municipalities in 2008 elections
length(unique(cand_2008$SIGLA_UE))
length(unique(vot_2008$SIGLA_UE))

#Excluding places with runoff from first round 
#because I am interested in vote margin in the decisive round
#Excluding places with eleicao majoritaria ou suplementar
e_round2 <- vot_2008 %>% filter(NUM_TURNO == 2) %>% distinct(SIGLA_UE)
suplementares <- cand_2008 %>% filter(DESCRICAO_ELEICAO != "ELEIÇÕES 2008") %>% distinct(SIGLA_UE) #167 places with eleicoes nao regulares/plebiscitos according to this
vot_2008v1 <- vot_2008 %>% filter(!SIGLA_UE %in% e_round2$SIGLA_UE, !SIGLA_UE %in% suplementares$SIGLA_UE, 
                                  DESCRICAO_CARGO == "PREFEITO")  #primeiro turno, eleicoes regulares, prefeito
cand_2008v1 <- cand_2008 %>% filter(!SIGLA_UE %in% e_round2$SIGLA_UE, !SIGLA_UE %in% suplementares$SIGLA_UE, 
                                    DESCRICAO_CARGO == "PREFEITO", DESC_SIT_TOT_TURNO != "#NULO#") #primeiro turno, eleicoes regulares, prefeito
#Check
#A candidate was replaced by himself, manually removed 
cand_2008v1 <- cand_2008v1 %>% filter(NUM_TITULO_ELEITORAL_CANDIDATO!="11297492127" & DESC_SIT_TOT_TURNO!="SUBSTITUÍDO")
stopifnot(length(unique(cand_2008v1$NUM_TITULO_ELEITORAL_CANDIDATO))==nrow(cand_2008v1))

#Selecting candidatos aptos to aggregate votes per municipality (removing places that had a runoff vote from first rounds and removing eleicoes nao regulares)
vot_2008v2 <- vot_2008v1 %>% filter(DESC_SIT_CAND_SUPERIOR == "APTO") %>% group_by(SIGLA_UE, SQ_CANDIDATO) %>% summarize(VOTO_MUN_CAND = sum(TOTAL_VOTOS))
#Check
stopifnot(length(unique(vot_2008v2$SIGLA_UE))==length(unique(vot_2008v1$SIGLA_UE)))

#Getting municipality's total vote and candidate vote share and renaming to merge
vot_2008v3 <- vot_2008v2 %>% group_by(SIGLA_UE) %>% summarize(VOTO_MUN_TOTAL = sum(VOTO_MUN_CAND)) %>% left_join(vot_2008v2, by = c("SIGLA_UE")) %>% 
              mutate(VOTO_CAND_SHARE = VOTO_MUN_CAND/VOTO_MUN_TOTAL) %>% group_by(SIGLA_UE) %>% mutate(NUMBER_CANDIDATES = n()) %>% 
              rename(SEQUENCIAL_CANDIDATO = SQ_CANDIDATO)
stopifnot(vot_2008v3$VOTO_CAND_SHARE >= 0 & vot_2008v3$VOTO_CAND_SHARE <= 1)
#Check
stopifnot(min(vot_2008v3$NUMBER_CANDIDATES)==1)

#Merging with candidate information
#Making sure there are no NAs and keys are unique
nrow(cand_2008v1 %>% filter(is.na(SIGLA_UE)))==0
nrow(vot_2008v3 %>% filter(is.na(SIGLA_UE)))==0
#Creating unique ids (to check merge by two variables), it's okay, they are unique #after checking once, no need to create it
#vot_2008v3$id_merge <- paste0(vot_2008v3$SIGLA_UE, vot_2008v3$SEQUENCIAL_CANDIDATO)
#cand_2008v1$id_merge <- paste0(cand_2008v1$SIGLA_UE, cand_2008v1$SEQUENCIAL_CANDIDATO)
#nrow(vot_2008v3) == length(unique(vot_2008v3$id_merge))
#nrow(cand_2008v1) == length(unique(cand_2008v1$id_merge))

#Merging and excluding repeated column
cand_2008v2 <- cand_2008v1 %>% left_join(vot_2008v3, by=c("SIGLA_UE", "SEQUENCIAL_CANDIDATO"))
                                         
#Debugging #which do not merge?
bugs <- anti_join(cand_2008v2, vot_2008v3, by=c("SIGLA_UE", "SEQUENCIAL_CANDIDATO"))
summary(bugs$VOTO_CAND_SHARE) #all NA's
#View(bugs %>% filter(DES_SITUACAO_CANDIDATURA=="DEFERIDO")) #situacao turno == Nao eleito (3), Why?
table(bugs$DESC_SIT_TOT_TURNO)

#Eliminating cases with NA vote share, eliminating places with only one candidate, and using age rule to break ties
cand_2008v3 <- cand_2008v2 %>% filter(!is.na(VOTO_CAND_SHARE), DESC_SIT_TOT_TURNO != "INDEFERIDO COM RECURSO") 

cand_2008v4 <- cand_2008v3 %>% filter(NUMBER_CANDIDATES != 1) %>% group_by(SIGLA_UE) %>% mutate(rankvote = rank(-VOTO_CAND_SHARE)) %>% 
                mutate(rankvoter = ifelse(rankvote == 1.5 & DESC_SIT_TOT_TURNO == "ELEITO", 1,
                ifelse(rankvote == 1.5 & DESC_SIT_TOT_TURNO == "NÃO ELEITO", 2, rankvote)))

table(cand_2008v4$DESC_SIT_TOT_TURNO, cand_2008v4$rankvoter) #Difference of one non-elected..Why?

#######Calculating vote share for eleicoes regulares (places that had runoffs and nao eleicoes suplementares)

#Geting the right base
suplementares_runoff <- cand_2008 %>% filter(DESCRICAO_ELEICAO != "ELEIÇÕES 2008" & NUM_TURNO == 2) %>% distinct(SIGLA_UE) #one case of eleicao suplementar com runoff
runoff <- cand_2008 %>% filter(NUM_TURNO == 2 & DESCRICAO_CARGO == "PREFEITO" 
                               & DESCRICAO_ELEICAO == "ELEIÇÕES 2008" & !SIGLA_UE %in% suplementares_runoff$SIGLA_UE) %>% distinct(SIGLA_UE)
vot_2008_runoff <- vot_2008 %>% filter(SIGLA_UE %in% runoff$SIGLA_UE & DESCRICAO_CARGO == "PREFEITO" 
                                        & NUM_TURNO==2 & !SIGLA_UE %in% suplementares_runoff$SIGLA_UE)
cand_2008_runoff <- cand_2008 %>% filter(SIGLA_UE %in% runoff$SIGLA_UE & NUM_TURNO == 2 &
                                          DESCRICAO_CARGO == "PREFEITO" & DESC_SIT_TOT_TURNO != "#NULO")
stopifnot(length(unique(cand_2008_runoff$NUM_TITULO_ELEITORAL_CANDIDATO))==nrow(cand_2008_runoff))

#Selecting candidatos a prefeito, aptos to aggregate votes per municipality
vot_2008_runoffv2 <- vot_2008_runoff %>% filter(DESC_SIT_CAND_SUPERIOR=="APTO") %>% 
                    group_by(SIGLA_UE, SQ_CANDIDATO) %>% summarize(VOTO_MUN_CAND = sum(TOTAL_VOTOS))
#Check
stopifnot(length(unique(vot_2008_runoffv2$SIGLA_UE))==length(unique(vot_2008_runoff$SIGLA_UE)))

#Getting municipality's total vote and candidate vote share and renaming to merge
vot_2008_runoffv3 <- vot_2008_runoffv2  %>% group_by(SIGLA_UE) %>% summarize(VOTO_MUN_TOTAL = sum(VOTO_MUN_CAND)) %>%
                      left_join(vot_2008_runoffv2, by=c("SIGLA_UE")) %>% 
                      mutate(VOTO_CAND_SHARE = VOTO_MUN_CAND/VOTO_MUN_TOTAL) %>% group_by(SIGLA_UE) %>% 
                      mutate(NUMBER_CANDIDATES = n()) %>% rename(SEQUENCIAL_CANDIDATO = SQ_CANDIDATO)

#Smell Tests
table(vot_2008_runoffv3$NUMBER_CANDIDATES==2)
summary(vot_2008_runoffv3$VOTO_CAND_SHARE)

#Merging with candidate information 
#Making sure there are no NAs and keys are unique
nrow(cand_2008_runoff %>% filter(is.na(SEQUENCIAL_CANDIDATO)))==0
nrow(vot_2008_runoffv3 %>% filter(is.na(SEQUENCIAL_CANDIDATO)))==0
cand_2008_runoffv2 <- cand_2008_runoff %>% left_join(vot_2008_runoffv3, by=c("SIGLA_UE", "SEQUENCIAL_CANDIDATO"))

#Debugging tactic
stopifnot(nrow(anti_join(cand_2008_runoff, vot_2008_runoffv3, by=c("SIGLA_UE", "SEQUENCIAL_CANDIDATO")))==0)

#Ranking votes (no ties, but creating rankvoter varaible to facilitate merge later on)
cand_2008_runoffv3 <- cand_2008_runoffv2 %>%  group_by(SIGLA_UE) %>% mutate(rankvote = rank(-VOTO_CAND_SHARE)) %>% 
                      mutate(rankvoter = rankvote)

table(cand_2008_runoffv3$rankvote, cand_2008_runoffv3$DESC_SIT_TOT_TURNO)

#Binding first round with runoff candidates
cand_2008v5 <- bind_rows(cand_2008v4, cand_2008_runoffv3)

####Vote margin (share and absolute)

#getting winner and runnerups
cand_2008v6 <- cand_2008v5 %>% filter(rankvoter== 1 | rankvoter== 2)
table(cand_2008v6$DESC_SIT_TOT_TURNO) #not half and half...why?
#Municipality with winner and no runnerup
temp <- cand_2008v6 %>% group_by(SIGLA_UE) %>% mutate(count = n())
table(temp$count)
temp <- temp %>% filter(count == 1)
problem <-cand_2008 %>% filter(SIGLA_UE == temp$SIGLA_UE)#remove, but why did this sty in data?
#removing from data (initial data had 1 prefeito and two vice-prefeitos)
cand_2008v6 <- cand_2008v6 %>% filter(SIGLA_UE != temp$SIGLA_UE)

#Getting vote margins
muns <- unique(cand_2008v6$SIGLA_UE)

electionsf_2008 <- NULL

for (i in 1:length(muns)){
  
  mun <- cand_2008v6[which(cand_2008v6$SIGLA_UE==muns[i]), ]
  
  vote_margin_share <- max(mun$VOTO_CAND_SHARE) - min(mun$VOTO_CAND_SHARE) 
  vote_margin_abs <- max(mun$VOTO_MUN_CAND) - min(mun$VOTO_MUN_CAND)
  winner <- mun %>% filter(DESC_SIT_TOT_TURNO == "ELEITO")
  runner_up <- mun %>% filter(DESC_SIT_TOT_TURNO == "NÃO ELEITO")
  
  margin_winner <- winner %>% mutate(vote_margin_share = vote_margin_share, vote_margin_abs = vote_margin_abs)
  margin_runner <- runner_up %>% mutate(vote_margin_share = -vote_margin_share, vote_margin_abs = -vote_margin_abs)
  
  electionsf_2008 <- bind_rows(electionsf_2008, margin_winner, margin_runner)
  print(i)
  
} 

#######Calculating vote share for eleicoes nao-regulares (there is one case of suplementar with runoff, 
# so first I have to exclude this case)

#Geting the right base
#Selecting places with eleicao suplementar ou majoritaria (no eleicao suplementar in places with runoff)
vot_2008_supv1 <- vot_2008 %>% filter(SIGLA_UE %in% suplementares$SIGLA_UE, !SIGLA_UE %in% e_round2$SIGLA_UE,
                                      DESCRICAO_ELEICAO != "ELEIÇÕES 2008",
                                      DESCRICAO_ELEICAO != "PLEBISCITO",  DESCRICAO_CARGO == "PREFEITO")  #primeiro turno, eleicoes naoregulares, prefeito
cand_2008_supv1 <- cand_2008 %>% filter(SIGLA_UE %in% suplementares$SIGLA_UE, DESCRICAO_ELEICAO != "ELEIÇÕES 2008", 
                                        DESCRICAO_ELEICAO != "PLEBISCITO", DESCRICAO_CARGO == "PREFEITO", 
                                        !SIGLA_UE %in% e_round2$SIGLA_UE) #primeiro turno, eleicoes naoregulares, prefeito
#Check
stopifnot(length(unique(cand_2008v1$NUM_TITULO_ELEITORAL_CANDIDATO))==nrow(cand_2008v1))

#Selecting candidatos aptos to aggregate votes per municipality
vot_2008_supv2 <- vot_2008_supv1 %>% filter(DESC_SIT_CAND_SUPERIOR == "APTO") %>% group_by(SIGLA_UE, SQ_CANDIDATO) %>% summarize(VOTO_MUN_CAND = sum(TOTAL_VOTOS))
#Check
stopifnot(length(unique(vot_2008_supv2$SIGLA_UE))==length(unique(vot_2008_supv1$SIGLA_UE)))

#Getting municipality's total vote and candidate vote share and renaming to merge
vot_2008_supv3 <- vot_2008_supv2 %>% group_by(SIGLA_UE) %>% summarize(VOTO_MUN_TOTAL = sum(VOTO_MUN_CAND)) %>% left_join(vot_2008_supv2, by = c("SIGLA_UE")) %>% 
                  mutate(VOTO_CAND_SHARE = VOTO_MUN_CAND/VOTO_MUN_TOTAL) %>% group_by(SIGLA_UE) %>% mutate(NUMBER_CANDIDATES = n()) %>% 
                  rename(SEQUENCIAL_CANDIDATO = SQ_CANDIDATO)
stopifnot(vot_2008_supv3$VOTO_CAND_SHARE >= 0 & vot_2008_supv3$VOTO_CAND_SHARE <= 1)
#Check
stopifnot(min(vot_2008_supv3$NUMBER_CANDIDATES)==1)

#Merging with candidate information
#Making sure there are no NAs and keys are unique
nrow(cand_2008_supv1 %>% filter(is.na(SEQUENCIAL_CANDIDATO)))==0
nrow(vot_2008_supv3 %>% filter(is.na(SEQUENCIAL_CANDIDATO)))==0
nrow(cand_2008_supv1 %>% filter(is.na(SIGLA_UE)))==0
nrow(vot_2008_supv3 %>% filter(is.na(SIGLA_UE)))==0
cand_2008_supv2 <- cand_2008_supv1 %>% left_join(vot_2008_supv3, by=c("SIGLA_UE", "SEQUENCIAL_CANDIDATO"))

#Debugging tactic
bugs <- anti_join(cand_2008_supv2, vot_2008_supv3, by=c("SIGLA_UE", "SEQUENCIAL_CANDIDATO"))
table(bugs$DES_SITUACAO_CANDIDATURA) #29 Deferidos
summary(bugs$VOTO_CAND_SHARE) #all NA's
#View(bugs %>% filter(DES_SITUACAO_CANDIDATURA=="DEFERIDO")) #situacao turno == "#Nulo#"

#Eliminating cases with NA vote share 
cand_2008_supv3 <- cand_2008_supv2 %>% filter(!is.na(VOTO_CAND_SHARE), DESC_SIT_TOT_TURNO!="#NULO#")

#Ranking votes (no ties, but creating rankvoter varaible to facilitate merge later on)
cand_2008_supv4 <- cand_2008_supv3 %>% filter(NUMBER_CANDIDATES != 1) %>%  group_by(SIGLA_UE) %>% mutate(rankvote = rank(-VOTO_CAND_SHARE)) %>% 
                   mutate(rankvoter = rankvote)

table(cand_2008_supv4$rankvote, cand_2008_supv4$DESC_SIT_TOT_TURNO)
#excluding this municipality with unclear winner
temp <- cand_2008_supv4 %>% filter(rankvote==2, DESC_SIT_TOT_TURNO=="ELEITO") #this municipality had three eleicoes suplementares
cand_2008_supv5 <- cand_2008_supv4 %>% filter(SIGLA_UE != temp$SIGLA_UE)
table(cand_2008_supv5$rankvote, cand_2008_supv5$DESC_SIT_TOT_TURNO)

####Vote margin (share and absolute)

#getting winner and runnerups
cand_2008_supv6 <- cand_2008_supv5 %>% filter(rankvoter== 1 | rankvoter== 2)
table(cand_2008_supv6$DESC_SIT_TOT_TURNO) #half and half

#Getting vote margins
muns_sup <- unique(cand_2008_supv6$SIGLA_UE)

electionsf_sup_2008 <- NULL

for (i in 1:length(muns_sup)){
  
  mun <- cand_2008_supv6[which(cand_2008_supv6$SIGLA_UE==muns_sup[i]), ]
  
  vote_margin_share <- max(mun$VOTO_CAND_SHARE) - min(mun$VOTO_CAND_SHARE) 
  vote_margin_abs <- max(mun$VOTO_MUN_CAND) - min(mun$VOTO_MUN_CAND)
  winner <- mun %>% filter(DESC_SIT_TOT_TURNO == "ELEITO")
  runner_up <- mun %>% filter(DESC_SIT_TOT_TURNO == "NÃO ELEITO")
  
  margin_winner <- winner %>% mutate(vote_margin_share = vote_margin_share, vote_margin_abs = vote_margin_abs)
  margin_runner <- runner_up %>% mutate(vote_margin_share = -vote_margin_share, vote_margin_abs = -vote_margin_abs)
  
  electionsf_sup_2008 <- bind_rows(electionsf_sup_2008, margin_winner, margin_runner)
  print(i)
  
} 

#Adding place with runoff and eleicoes nao regulares

vot_2008_supv1_runoff <- vot_2008 %>% filter(SIGLA_UE %in% suplementares$SIGLA_UE, SIGLA_UE %in% e_round2$SIGLA_UE,
                                      DESCRICAO_ELEICAO != "ELEIÇÕES 2008", NUM_TURNO == 2,
                                      DESCRICAO_ELEICAO != "PLEBISCITO",  DESCRICAO_CARGO == "PREFEITO")  #segundo turno, eleicoes naoregulares, prefeito
cand_2008_supv1_runoff <- cand_2008 %>% filter(SIGLA_UE %in% suplementares$SIGLA_UE, DESCRICAO_ELEICAO != "ELEIÇÕES 2008", 
                                        DESCRICAO_ELEICAO != "PLEBISCITO", DESCRICAO_CARGO == "PREFEITO", 
                                        SIGLA_UE %in% e_round2$SIGLA_UE, NUM_TURNO == 2) #segundo turno, eleicoes naoregulares, prefeito

#Selecting candidatos aptos to aggregate votes per municipality
vot_2008_supv2_runoff <- vot_2008_supv1_runoff %>% filter(DESC_SIT_CAND_SUPERIOR == "APTO") %>% group_by(SIGLA_UE, SQ_CANDIDATO) %>% summarize(VOTO_MUN_CAND = sum(TOTAL_VOTOS))
#Check
stopifnot(length(unique(vot_2008_supv2_runoff$SIGLA_UE))==length(unique(vot_2008_supv1_runoff$SIGLA_UE)))

#Getting municipality's total vote and candidate vote share and renaming to merge
vot_2008_supv3_runoff <- vot_2008_supv2_runoff %>% group_by(SIGLA_UE) %>% summarize(VOTO_MUN_TOTAL = sum(VOTO_MUN_CAND)) %>% 
                          left_join(vot_2008_supv2_runoff, by = c("SIGLA_UE")) %>% 
                          mutate(VOTO_CAND_SHARE = VOTO_MUN_CAND/VOTO_MUN_TOTAL) %>% group_by(SIGLA_UE) %>% mutate(NUMBER_CANDIDATES = n()) %>% 
                          rename(SEQUENCIAL_CANDIDATO = SQ_CANDIDATO)
stopifnot(vot_2008_supv3_runoff$VOTO_CAND_SHARE >= 0 & vot_2008_supv3_runoff$VOTO_CAND_SHARE <= 1)

#Merging with candidate information
cand_2008_supv2_runoff <- cand_2008_supv1_runoff %>% left_join(vot_2008_supv3_runoff, by=c("SIGLA_UE", "SEQUENCIAL_CANDIDATO"))

#Ranking votes (no ties, but creating rankvoter varaible to facilitate merge later on)
cand_2008_supv3_runoff <- cand_2008_supv2_runoff %>% filter(NUMBER_CANDIDATES != 1) %>%  group_by(SIGLA_UE) %>% mutate(rankvote = rank(-VOTO_CAND_SHARE)) %>% 
                   mutate(rankvoter = rankvote)
table(cand_2008_supv3_runoff$rankvote, cand_2008_supv3_runoff$DESC_SIT_TOT_TURNO)

####Vote margin (share and absolute)
electionsf_sup_runoff_2008 <- NULL
vote_margin_share <- max(cand_2008_supv3_runoff$VOTO_CAND_SHARE) - min(cand_2008_supv3_runoff$VOTO_CAND_SHARE) 
vote_margin_abs <- max(cand_2008_supv3_runoff$VOTO_MUN_CAND) - min(cand_2008_supv3_runoff$VOTO_MUN_CAND)
winner <- cand_2008_supv3_runoff %>% filter(DESC_SIT_TOT_TURNO == "ELEITO")
runner_up <- cand_2008_supv3_runoff %>% filter(DESC_SIT_TOT_TURNO == "NÃO ELEITO")
margin_winner <- winner %>% mutate(vote_margin_share = vote_margin_share, vote_margin_abs = vote_margin_abs)
margin_runner <- runner_up %>% mutate(vote_margin_share = -vote_margin_share, vote_margin_abs = -vote_margin_abs)
electionsf_sup_runoff_2008 <- bind_rows(electionsf_sup_runoff_2008, margin_winner, margin_runner)

######### Binding elections (regulares and nao regulares)
electionsf_2008 <- electionsf_2008 %>% bind_cols(data_frame(TYPE_ELECTION = rep("regular", nrow(electionsf_2008))))  
electionsf_sup_2008 <- electionsf_sup_2008 %>% bind_cols(data_frame(TYPE_ELECTION = rep("nonregular", nrow(electionsf_sup_2008))))                                            
electionsf_sup_runoff_2008 <- electionsf_sup_runoff_2008 %>% bind_cols(data_frame(TYPE_ELECTION = rep("nonregular", nrow(electionsf_sup_runoff_2008))))                                            

electionsff_2008 <- bind_rows(electionsf_2008, electionsf_sup_2008, electionsf_sup_runoff_2008)

#Simple Smell tests
table(electionsff_2008$TYPE_ELECTION) 
table(electionsff_2008$DESC_SIT_TOT_TURNO) #half and half
table(electionsff_2008$rankvoter) #half and half

######## TESTING IDENTIFIERS' INTEGRITY (length of codes, number of municipalities, number of elected mayors, number of nonelected)
#Municipality's identifier's 
str(electionsff_2008)
glimpse(electionsff_2008)
table(nchar(electionsff_2008$CPF_CANDIDATO)) #existem erros
table(nchar(electionsff_2008$NUM_TITULO_ELEITORAL_CANDIDATO)) #existem erros

#save(electionsff_2008, file="~/Dropbox/LOCAL_ELECTIONS/repositorio_data/final_data/electionsff_2008.Rda")

#######################################################################Elections 2012
vot_2012 <- vot_2000_2016[[4]]
cand_2012 <- cand_2000_2016[[4]]

#To start with, equal numbers of municipalities in 2012 elections
length(unique(cand_2012$SIGLA_UE))
length(unique(vot_2012$SIGLA_UE))

#Excluding places with runoff from first round 
#because I am interested in vote margin in the decisive round
#Excluding places with eleicao majoritaria ou suplementar
e_round2 <- vot_2012 %>% filter(NUM_TURNO == 2) %>% distinct(SIGLA_UE) #same number if you select runoffs through cand_2012
suplementares <- vot_2012 %>% filter(DESCRICAO_ELEICAO != "ELEIÇÃO MUNICIPAL 2012") %>% distinct(SIGLA_UE) #125 places with eleicoes nao regulares according to this
vot_2012v1 <- vot_2012 %>% filter(!SIGLA_UE %in% e_round2$SIGLA_UE, !SIGLA_UE %in% suplementares$SIGLA_UE, DESCRICAO_CARGO == "PREFEITO")  #primeiro turno, eleicoes regulares, prefeito
cand_2012v1 <- cand_2012 %>% filter(!SIGLA_UE %in% e_round2$SIGLA_UE, !SIGLA_UE %in% suplementares$SIGLA_UE, DESCRICAO_CARGO == "PREFEITO") #primeiro turno, eleicoes regulares, prefeito
#Check
stopifnot(length(unique(cand_2012v1$SEQUENCIAL_CANDIDATO))==nrow(cand_2012v1))

#Selecting candidatos aptos to aggregate votes per municipality (removing places that had a runoff vote from first rounds and removing eleicoes nao regulares)
vot_2012v2 <- vot_2012v1 %>% filter(DESC_SIT_CAND_SUPERIOR == "APTO") %>% group_by(SIGLA_UE, SQ_CANDIDATO) %>% summarize(VOTO_MUN_CAND = sum(TOTAL_VOTOS))
#Check
stopifnot(length(unique(vot_2012v2$SIGLA_UE))==length(unique(vot_2012v1$SIGLA_UE)))

#Getting municipality's total vote and candidate vote share and renaming to merge
vot_2012v3 <- vot_2012v2 %>% group_by(SIGLA_UE) %>% summarize(VOTO_MUN_TOTAL = sum(VOTO_MUN_CAND)) %>% left_join(vot_2012v2, by = c("SIGLA_UE")) %>% 
              mutate(VOTO_CAND_SHARE = VOTO_MUN_CAND/VOTO_MUN_TOTAL) %>% group_by(SIGLA_UE) %>% mutate(NUMBER_CANDIDATES = n()) %>% rename(SEQUENCIAL_CANDIDATO = SQ_CANDIDATO)
stopifnot(vot_2012v3$VOTO_CAND_SHARE >= 0 & vot_2012v3$VOTO_CAND_SHARE <= 1)
#Check
stopifnot(min(vot_2012v3$NUMBER_CANDIDATES)==1)

#Merging with candidate information
#Making sure there are no NAs and keys are unique
nrow(cand_2012v1 %>% filter(is.na(SEQUENCIAL_CANDIDATO)))==0
nrow(vot_2012v3 %>% filter(is.na(SEQUENCIAL_CANDIDATO)))==0
nrow(vot_2012v3) == length(unique(vot_2012v3$SEQUENCIAL_CANDIDATO))
nrow(cand_2012v1) == length(unique(cand_2012v1$SEQUENCIAL_CANDIDATO))
#Merging and excluding repeated column
cand_2012v2 <- cand_2012v1 %>% left_join(vot_2012v3, by="SEQUENCIAL_CANDIDATO") %>% select(-SIGLA_UE.y) %>% rename(SIGLA_UE=SIGLA_UE.x)

#Debugging #which do not merge?
bugs <- anti_join(cand_2012v2, vot_2012v3, by="SEQUENCIAL_CANDIDATO")
summary(bugs$VOTO_CAND_SHARE) #all NA's
#View(bugs %>% filter(DES_SITUACAO_CANDIDATURA=="DEFERIDO")) #situacao turno == "#Nulo#"
table(bugs$DESC_SIT_TOT_TURNO)

#Eliminating cases with NA vote share, eliminating places with only one candidate, and using age rule to break ties
cand_2012v3 <- cand_2012v2 %>% filter(!is.na(VOTO_CAND_SHARE)) 
#Creating age age variable
cand_2012v3 <- cand_2012v2 %>% mutate(data_nasc = as.Date(DATA_NASCIMENTO, format = '%d/%m/%Y'), age_years = age_calc(data_nasc, units="years"),
                                      age_years2 = ifelse(age_years < 18 | age_years > 100, NA, age_years))
#Age variable requires manual fixing (as there dates inputted wrong, such as 2005 asn 0049 for date of births)
#For now, assign NA to smaller than 18 and greater than 100
#This is okay for now, age is used to break ties in tied elections

cand_2012v4 <- cand_2012v3 %>% filter(NUMBER_CANDIDATES != 1) %>% group_by(SIGLA_UE) %>% mutate(rankvote = rank(-VOTO_CAND_SHARE)) %>% 
               mutate(rankvoter = ifelse(rankvote == 1.5 & DESC_SIT_TOT_TURNO == "ELEITO", 1,
               ifelse(rankvote == 1.5 & DESC_SIT_TOT_TURNO == "NÃO ELEITO", 2, 
               ifelse(rankvote == 2.5 & SEQUENCIAL_CANDIDATO==250000012377, 2, 
               ifelse(rankvote == 2.5 & SEQUENCIAL_CANDIDATO==250000055531, 3,
               ifelse(rankvote == 2.5 & SEQUENCIAL_CANDIDATO== 140000021781, 2,
               ifelse(rankvote == 2.5 & SEQUENCIAL_CANDIDATO== 140000021641, 3,       
               ifelse(rankvote == 2.5 & SEQUENCIAL_CANDIDATO== 250000077750, 2,
               ifelse(rankvote == 2.5 & SEQUENCIAL_CANDIDATO== 250000012006, 3, rankvote)))))))))


table(cand_2012v4$DESC_SIT_TOT_TURNO, cand_2012v4$rankvoter)

#######Calculating vote share for eleicoes regulares (places that had runoffs)

#Geting the right base
runoff <- cand_2012 %>% filter(NUM_TURNO == 2 & DESCRICAO_CARGO == "PREFEITO") %>% distinct(SEQUENCIAL_CANDIDATO)
vot_2012_runoff <- vot_2012 %>% filter(SQ_CANDIDATO %in% runoff$SEQUENCIAL_CANDIDATO & DESCRICAO_CARGO == "PREFEITO" & NUM_TURNO==2)
cand_2012_runoff <- cand_2012 %>% filter(SEQUENCIAL_CANDIDATO %in% runoff$SEQUENCIAL_CANDIDATO & DESCRICAO_CARGO == "PREFEITO" & DESC_SIT_TOT_TURNO != "2º TURNO")
stopifnot(length(unique(cand_2012_runoff$SEQUENCIAL_CANDIDATO))==nrow(cand_2012_runoff))

#Selecting candidatos a prefeito, aptos to aggregate votes per municipality
vot_2012_runoffv2 <- vot_2012_runoff %>% filter(DESC_SIT_CAND_SUPERIOR=="APTO") %>% group_by(SIGLA_UE, SQ_CANDIDATO) %>% summarize(VOTO_MUN_CAND = sum(TOTAL_VOTOS))
#Check
stopifnot(length(unique(vot_2012_runoffv2$SIGLA_UE))==length(unique(vot_2012_runoff$SIGLA_UE)))

#Getting municipality's total vote and candidate vote share and renaming to merge
vot_2012_runoffv3 <- vot_2012_runoffv2  %>% group_by(SIGLA_UE) %>% summarize(VOTO_MUN_TOTAL = sum(VOTO_MUN_CAND)) %>% 
                      left_join(vot_2012_runoffv2, by=c("SIGLA_UE")) %>% 
                       mutate(VOTO_CAND_SHARE = VOTO_MUN_CAND/VOTO_MUN_TOTAL) %>% group_by(SIGLA_UE) %>% mutate(NUMBER_CANDIDATES = n()) %>% 
                      rename(SEQUENCIAL_CANDIDATO = SQ_CANDIDATO)

#Smell Tests
table(vot_2012_runoffv3$NUMBER_CANDIDATES==2)
summary(vot_2012_runoffv3$VOTO_CAND_SHARE)

#Merging with candidate information
#Making sure there are no NAs and keys are unique
nrow(cand_2012_runoff %>% filter(is.na(SEQUENCIAL_CANDIDATO)))==0
nrow(vot_2012_runoffv3 %>% filter(is.na(SEQUENCIAL_CANDIDATO)))==0
nrow(vot_2012_runoffv3) == length(unique(vot_2012_runoffv3$SEQUENCIAL_CANDIDATO))
nrow(cand_2012_runoff) == length(unique(cand_2012_runoff$SEQUENCIAL_CANDIDATO))
cand_2012_runoffv2 <- cand_2012_runoff %>% left_join(vot_2012_runoffv3, by="SEQUENCIAL_CANDIDATO") %>% select(-SIGLA_UE.y) %>% rename(SIGLA_UE=SIGLA_UE.x)

#Debugging tactic
stopifnot(nrow(anti_join(cand_2012_runoff, vot_2012_runoffv3, by="SEQUENCIAL_CANDIDATO"))==0)

#Ranking votes (no ties, but creating rankvoter varaible to facilitate merge later on)
cand_2012_runoffv3 <- cand_2012_runoffv2 %>%  group_by(SIGLA_UE) %>% mutate(rankvote = rank(-VOTO_CAND_SHARE)) %>% 
                      mutate(rankvoter = rankvote)

table(cand_2012_runoffv3$rankvote, cand_2012_runoffv3$DESC_SIT_TOT_TURNO)

#Binding first round with runoff candidates
cand_2012v5 <- bind_rows(cand_2012v4, cand_2012_runoffv3)

####Vote margin (share and absolute)

#getting winner and runnerups
cand_2012v6 <- cand_2012v5 %>% filter(rankvoter== 1 | rankvoter== 2)
table(cand_2012v6$DESC_SIT_TOT_TURNO) #half and half

#Getting vote margins
muns <- unique(cand_2012v6$SIGLA_UE)

electionsf_2012 <- NULL

for (i in 1:length(muns)){
  
  mun <- cand_2012v6[which(cand_2012v6$SIGLA_UE==muns[i]), ]
  
  vote_margin_share <- max(mun$VOTO_CAND_SHARE) - min(mun$VOTO_CAND_SHARE) 
  vote_margin_abs <- max(mun$VOTO_MUN_CAND) - min(mun$VOTO_MUN_CAND)
  winner <- mun %>% filter(DESC_SIT_TOT_TURNO == "ELEITO")
  runner_up <- mun %>% filter(DESC_SIT_TOT_TURNO == "NÃO ELEITO")

  margin_winner <- winner %>% mutate(vote_margin_share = vote_margin_share, vote_margin_abs = vote_margin_abs)
  margin_runner <- runner_up %>% mutate(vote_margin_share = -vote_margin_share, vote_margin_abs = -vote_margin_abs)

  electionsf_2012 <- bind_rows(electionsf_2012, margin_winner, margin_runner)
  print(i)
  
} 

#######Calculating vote share for eleicoes nao-regulares (that only happened in places that just had runoffs)

#Geting the right base
#Selecting places with eleicao suplementar ou majoritaria (no eleicao suplementar in places with runoff)
vot_2012_supv1 <- vot_2012 %>% filter(SIGLA_UE %in% suplementares$SIGLA_UE, DESCRICAO_ELEICAO != "ELEIÇÃO MUNICIPAL 2012", DESCRICAO_CARGO == "PREFEITO")  #primeiro turno, eleicoes nao-regulares, prefeito
cand_2012_supv1 <- cand_2012 %>% filter(SIGLA_UE %in% suplementares$SIGLA_UE, DESCRICAO_ELEICAO != "ELEIÇÃO MUNICIPAL 2012", DESCRICAO_CARGO == "PREFEITO") #primeiro turno, eleicoes nao-regulares, prefeito
#Check
stopifnot(length(unique(cand_2012_supv1$SEQUENCIAL_CANDIDATO))==nrow(cand_2012_supv1))

#Selecting candidatos aptos to aggregate votes per municipality (removing places that had a runoff vote from first rounds and removing eleicoes nao regulares)
vot_2012_supv2 <- vot_2012_supv1 %>% filter(DESC_SIT_CAND_SUPERIOR == "APTO") %>% group_by(SIGLA_UE, SQ_CANDIDATO) %>% summarize(VOTO_MUN_CAND = sum(TOTAL_VOTOS))
#Check
stopifnot(length(unique(vot_2012_supv2$SIGLA_UE))==length(unique(vot_2012_supv1$SIGLA_UE)))

#Getting municipality's total vote and candidate vote share and renaming to merge
vot_2012_supv3 <- vot_2012_supv2 %>% group_by(SIGLA_UE) %>% summarize(VOTO_MUN_TOTAL = sum(VOTO_MUN_CAND)) %>% 
                  left_join(vot_2012_supv2, by = c("SIGLA_UE")) %>% 
                  mutate(VOTO_CAND_SHARE = VOTO_MUN_CAND/VOTO_MUN_TOTAL) %>% group_by(SIGLA_UE) %>% 
                  mutate(NUMBER_CANDIDATES = n()) %>% rename(SEQUENCIAL_CANDIDATO = SQ_CANDIDATO)
stopifnot(vot_2012_supv3$VOTO_CAND_SHARE >= 0 & vot_2012_supv3$VOTO_CAND_SHARE <= 1)
#Check
stopifnot(min(vot_2012v3$NUMBER_CANDIDATES)==1)

#Merging with candidate information
#Making sure there are no NAs and keys are unique
nrow(cand_2012_supv1 %>% filter(is.na(SEQUENCIAL_CANDIDATO)))==0
nrow(vot_2012_supv3 %>% filter(is.na(SEQUENCIAL_CANDIDATO)))==0
nrow(vot_2012_supv3) == length(unique(vot_2012_supv3$SEQUENCIAL_CANDIDATO))
nrow(cand_2012_supv1) == length(unique(cand_2012_supv1$SEQUENCIAL_CANDIDATO))
cand_2012_supv2 <- cand_2012_supv1 %>% left_join(vot_2012_supv3, by="SEQUENCIAL_CANDIDATO") %>% select(-SIGLA_UE.y) %>% rename(SIGLA_UE=SIGLA_UE.x)

#Debugging tactic
bugs <- anti_join(cand_2012_supv2, vot_2012_supv3, by="SEQUENCIAL_CANDIDATO")
table(bugs$DES_SITUACAO_CANDIDATURA) #14 Deferidos
summary(bugs$VOTO_CAND_SHARE) #all NA's
#View(bugs %>% filter(DES_SITUACAO_CANDIDATURA=="DEFERIDO")) #situacao turno == "#Nulo#"

#Eliminating cases with NA vote share 
cand_2012_supv3 <- cand_2012_supv2 %>% filter(!is.na(VOTO_CAND_SHARE), DESC_SIT_TOT_TURNO!="#NULO#")

#Ranking votes (no ties, but creating rankvoter varaible to facilitate merge later on)
cand_2012_supv4 <- cand_2012_supv3 %>% filter(NUMBER_CANDIDATES != 1) %>%  group_by(SIGLA_UE) %>% mutate(rankvote = rank(-VOTO_CAND_SHARE)) %>% 
                          mutate(rankvoter = rankvote)

table(cand_2012_supv4$rankvote, cand_2012_supv4$DESC_SIT_TOT_TURNO)
#excluding this municipality with unclear winner
temp <- cand_2012_supv4 %>% filter(rankvote==2, DESC_SIT_TOT_TURNO=="ELEITO")
cand_2012_supv5 <- cand_2012_supv4 %>% filter(SIGLA_UE != temp$SIGLA_UE)
table(cand_2012_supv5$rankvote, cand_2012_supv5$DESC_SIT_TOT_TURNO)

####Vote margin (share and absolute)

#getting winner and runnerups
cand_2012_supv6 <- cand_2012_supv5 %>% filter(rankvoter== 1 | rankvoter== 2)
table(cand_2012_supv6$DESC_SIT_TOT_TURNO) #half and half

#Getting vote margins
muns_sup <- unique(cand_2012_supv6$SIGLA_UE)

electionsf_sup_2012 <- NULL

for (i in 1:length(muns_sup)){
  
  mun <- cand_2012_supv6[which(cand_2012_supv6$SIGLA_UE==muns_sup[i]), ]
  
  vote_margin_share <- max(mun$VOTO_CAND_SHARE) - min(mun$VOTO_CAND_SHARE) 
  vote_margin_abs <- max(mun$VOTO_MUN_CAND) - min(mun$VOTO_MUN_CAND)
  winner <- mun %>% filter(DESC_SIT_TOT_TURNO == "ELEITO")
  runner_up <- mun %>% filter(DESC_SIT_TOT_TURNO == "NÃO ELEITO")
  
  margin_winner <- winner %>% mutate(vote_margin_share = vote_margin_share, vote_margin_abs = vote_margin_abs)
  margin_runner <- runner_up %>% mutate(vote_margin_share = -vote_margin_share, vote_margin_abs = -vote_margin_abs)
  
  electionsf_sup_2012 <- bind_rows(electionsf_sup_2012, margin_winner, margin_runner)
  print(i)
  
} 

######### Binding elections (regulares and nao regulares)
electionsf_2012 <- electionsf_2012 %>% bind_cols(data_frame(TYPE_ELECTION = rep("regular", nrow(electionsf_2012))))  
electionsf_sup_2012 <- electionsf_sup_2012 %>% bind_cols(data_frame(TYPE_ELECTION = rep("nonregular", nrow(electionsf_sup_2012))))                                            

electionsff_2012 <- bind_rows(electionsf_2012, electionsf_sup_2012)

#Simple Smell tests
table(electionsff_2012$TYPE_ELECTION)
table(electionsff_2012$DESC_SIT_TOT_TURNO) #half and half
table(electionsff_2012$rankvoter) #half and half

######## TESTING IDENTIFIERS' INTEGRITY (length of codes, number of municipalities, number of elected mayors, number of nonelected)
#Municipality's identifier's 
str(electionsff_2012)
table(nchar(electionsff_2012$CPF_CANDIDATO)) #existem erros
table(nchar(electionsff_2012$NUM_TITULO_ELEITORAL_CANDIDATO)) #existem erros
table(nchar(electionsff_2012$SEQUENCIAL_CANDIDATO)) #erros ou dois tipos de sequenciais?

#save(electionsff_2012, file="~/Dropbox/LOCAL_ELECTIONS/repositorio_data/final_data/electionsff_2012.Rda")

########Binding all years
load("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/final_data/electionsff_2000.Rda")
load("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/final_data/electionsff_2004.Rda")
load("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/final_data/electionsff_2008.Rda")
load("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/final_data/electionsff_2012.Rda")

electionsff_2012 <- electionsff_2012 %>% select(-c(EMAIL_CANDIDATO, data_nasc, age_years, age_years2))
electionsff_2000 <- electionsff_2000 %>% mutate(NUMERO_CANDIDATO = as.integer(NUMERO_CANDIDATO), 
                                                COD_SITUACAO_CANDIDATURA = as.integer(COD_SITUACAO_CANDIDATURA),
                                                NUMERO_PARTIDO = as.integer(NUMERO_PARTIDO), 
                                                IDADE_DATA_ELEICAO = as.integer(IDADE_DATA_ELEICAO), 
                                                CODIGO_ESTADO_CIVIL = as.integer(CODIGO_ESTADO_CIVIL), 
                                                CODIGO_NACIONALIDADE = as.integer(CODIGO_NACIONALIDADE),
                                                CODIGO_MUNICIPIO_NASCIMENTO = as.integer(CODIGO_MUNICIPIO_NASCIMENTO))
electionsff_2008 <- electionsff_2008 %>% mutate(NUM_TITULO_ELEITORAL_CANDIDATO = as.character(NUM_TITULO_ELEITORAL_CANDIDATO))
electionsff_2012 <- electionsff_2012 %>% mutate(NUM_TITULO_ELEITORAL_CANDIDATO = as.character(NUM_TITULO_ELEITORAL_CANDIDATO),
                                                CPF_CANDIDATO = as.character(CPF_CANDIDATO))
electionsff <- bind_rows(electionsff_2000, electionsff_2004, electionsff_2008, electionsff_2012)
save(electionsff, file="~/Dropbox/LOCAL_ELECTIONS/repositorio_data/final_data/electionsff_2000_2012.Rda")

Number_Mun <- electionsff %>% group_by(ANO_ELEICAO) %>% 
              distinct(SIGLA_UE) %>% count()
table(electionsff$DESC_SIT_TOT_TURNO)
