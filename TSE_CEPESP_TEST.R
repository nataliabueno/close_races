###################################################################
############# Downloading, Organizing and Cleaning Electoral Data
#0. Download TSE repositorio data
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
library(readr)

#Change your working directory here
dir <- "~/Dropbox/LOCAL_ELECTIONS/"

#helper functions
source(paste0(dir, "codes/helper_functions.R"))

###################################################################
#0. Downloading 
###################################################################

dir_d <- "~/Dropbox/LOCAL_ELECTIONS/repositorio_data/"

#Candidate data
url_cand98 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_1998.zip"
file_d <- paste0(dir_d, "original_data/consulta_cand/consulta_cand_1998.zip")
file_un <- paste0(dir_d, "original_unzipped/consulta_cand/consulta_cand_1998/")
cand_1998 <- get_tse(url_cand98, file_d, file_un)

url_cand00 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2000.zip"
file_d <- paste0(dir_d, "original_data/consulta_cand/consulta_cand_2000.zip")
file_un <- paste0(dir_d, "original_unzipped/consulta_cand/consulta_cand_2000/")
cand_2000 <- get_tse(url_cand00, file_d, file_un)

url_cand02 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2002.zip"
file_d <- paste0(dir_d, "original_data/consulta_cand/consulta_cand_2002.zip")
file_un <- paste0(dir_d, "original_unzipped/consulta_cand/consulta_cand_2002/")
cand_2002 <- get_tse(url_cand02, file_d, file_un)

url_cand04 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2004.zip"
file_d <- paste0(dir_d, "original_data/consulta_cand/consulta_cand_2004.zip")
file_un <- paste0(dir_d, "original_unzipped/consulta_cand/consulta_cand_2004/")
cand_2004 <- get_tse(url_cand04, file_d, file_un)

url_cand06 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2006.zip"
file_d <- paste0(dir_d, "original_data/consulta_cand/consulta_cand_2006.zip")
file_un <- paste0(dir_d, "original_unzipped/consulta_cand/consulta_cand_2006/")
cand_2006 <- get_tse(url_cand06, file_d, file_un)

url_cand08 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2008.zip"
file_d <- paste0(dir_d, "original_data/consulta_cand/consulta_cand_2008.zip")
file_un <- paste0(dir_d, "original_unzipped/consulta_cand/consulta_cand_2008/")
cand_2008 <- get_tse(url_cand08, file_d, file_un)

url_cand10 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2010.zip"
file_d <- paste0(dir_d, "original_data/consulta_cand/consulta_cand_2010.zip")
file_un <- paste0(dir_d, "original_unzipped/consulta_cand/consulta_cand_2010/")
cand_2010 <- get_tse(url_cand10, file_d, file_un)

url_cand12 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2012.zip"
file_d <- paste0(dir_d, "original_data/consulta_cand/consulta_cand_2012.zip")
file_un <- paste0(dir_d, "original_unzipped/consulta_cand/consulta_cand_2012/")
cand_2012 <- get_tse(url_cand12, file_d, file_un)

url_cand14 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2014.zip"
file_d <- paste0(dir_d, "original_data/consulta_cand/consulta_cand_2014.zip")
file_un <- paste0(dir_d, "original_unzipped/consulta_cand/consulta_cand_2014/")
cand_2014 <- get_tse(url_cand14, file_d, file_un)

url_cand16 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2016.zip"
file_d <- paste0(dir_d, "original_data/consulta_cand/consulta_cand_2016.zip")
file_un <- paste0(dir_d, "original_unzipped/consulta_cand/consulta_cand_2016/")
cand_2016 <- get_tse(url_cand16, file_d, file_un)

#Voting data
url_vot98 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_1998.zip"
file_d <- paste0(dir_d, "original_data/votacao_munzona/votacao_candidato_munzona_1998.zip")
file_un <- paste0(dir_d, "original_unzipped/votacao_munzona/votacao_candidato_munzona_1998/")
vot_1998 <- get_tse(url_vot98, file_d, file_un)

url_vot00 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2000.zip"
file_d <- paste0(dir_d, "original_data/votacao_munzona/votacao_candidato_munzona_2000.zip")
file_un <- paste0(dir_d, "original_unzipped/votacao_munzona/votacao_candidato_munzona_2000/")
vot_2000 <- get_tse(url_vot00, file_d, file_un)

url_vot02 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2002.zip"
file_d <- paste0(dir_d, "original_data/votacao_munzona/votacao_candidato_munzona_2002.zip")
file_un <- paste0(dir_d, "original_unzipped/votacao_munzona/votacao_candidato_munzona_2002/")
vot_2002 <- get_tse(url_vot02, file_d, file_un)

url_vot04 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2004.zip"
file_d <- paste0(dir_d, "original_data/votacao_munzona/votacao_candidato_munzona_2004.zip")
file_un <- paste0(dir_d, "original_unzipped/votacao_munzona/votacao_candidato_munzona_2004/")
vot_2004 <- get_tse(url_vot04, file_d, file_un)

url_vot06 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2006.zip"
file_d <- paste0(dir_d, "original_data/votacao_munzona/votacao_candidato_munzona_2006.zip")
file_un <- paste0(dir_d, "original_unzipped/votacao_munzona/votacao_candidato_munzona_2006/")
vot_2006 <- get_tse(url_vot06, file_d, file_un)

url_vot08 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2008.zip"
file_d <- paste0(dir_d, "original_data/votacao_munzona/votacao_candidato_munzona_2008.zip")
file_un <- paste0(dir_d, "original_unzipped/votacao_munzona/votacao_candidato_munzona_2008/")
vot_2008 <- get_tse(url_vot08, file_d, file_un)

url_vot10 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2010.zip"
file_d <- paste0(dir_d, "original_data/votacao_munzona/votacao_candidato_munzona_2010.zip")
file_un <- paste0(dir_d, "original_unzipped/votacao_munzona/votacao_candidato_munzona_2010/")
vot_2010 <- get_tse(url_vot10, file_d, file_un)

url_vot12 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2012.zip"
file_d <- paste0(dir_d, "original_data/votacao_munzona/votacao_candidato_munzona_2012.zip")
file_un <- paste0(dir_d, "original_unzipped/votacao_munzona/votacao_candidato_munzona_2012/")
vot_2012 <- get_tse(url_vot12, file_d, file_un)

url_vot14 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2014.zip"
file_d <- paste0(dir_d, "original_data/votacao_munzona/votacao_candidato_munzona_2014.zip")
file_un <- paste0(dir_d, "original_unzipped/votacao_munzona/votacao_candidato_munzona_2014/")
vot_2014 <- get_tse(url_vot14, file_d, file_un)

url_vot16 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2016.zip"
file_d <- paste0(dir_d, "original_data/votacao_munzona/votacao_candidato_munzona_2016.zip")
file_un <- paste0(dir_d, "original_unzipped/votacao_munzona/votacao_candidato_munzona_2016/")
vot_2016 <- get_tse(url_cand16, file_d, file_un)

###################################################################
#1. Combining

### LOCAL ELECTIONS

# Reading candidate data
# Reading voting data
# Merging and Binding

### NATIONAL ELECTIONS

# Reading candidate data
# Reading voting data
# Merging and Binding

###################################################################

ufs_n <- c("AC", "AL", "AP", "AM", "BA", "BR",   
         "CE", "DF", "ES", "GO", "MA", "MT", "MS",
         "MG", "PA", "PB", "PR", "PE", "PI", "RJ",
         "RN", "RS", "RO","RR","SC", "SP", "SE", "TO", "ZZ")

labels_pre2012c <- c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "NUM_TURNO", "DESCRICAO_ELEICAO",
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
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/consulta_cand/consulta_cand_2000/consulta_cand_2000_",
                        ufs_n[!ufs_n %in% c("BR", "ZZ", "DF")], ".txt"))
cand_2000 <- lapply(files, read.table, sep = ";", header = F, 
                    stringsAsFactors = F, fill = T, fileEncoding = "windows-1252") 
cand_2000 <- do.call("rbind", cand_2000)
names(cand_2000) <- labels_pre2012c
cand_2000 <- as_tibble(cand_2000)

#candidates 2004
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/consulta_cand/consulta_cand_2004/consulta_cand_2004_",
                        ufs_n[!ufs_n %in% c("BR", "ZZ", "DF")], ".txt"))
cand_2004 <- lapply(files, read.table, sep = ";", header=F, 
                    stringsAsFactors = F, fill = T, fileEncoding = "windows-1252") 
cand_2004 <- do.call("rbind", cand_2004)
names(cand_2004) <- labels_pre2012c
cand_2004 <- as_tibble(cand_2004)

#candidates 2008
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/consulta_cand/consulta_cand_2008/consulta_cand_2008_",
                        ufs_n[!ufs_n %in% c("BR", "ZZ", "DF")], ".txt"))
cand_2008 <- lapply(files, read.table, sep = ";", header = F, 
                    stringsAsFactors = F, fill = T, fileEncoding = "windows-1252") 
cand_2008 <- do.call("rbind", cand_2008)
names(cand_2008) <- labels_pre2012c
cand_2008 <- as_tibble(cand_2008)

#candidates 2012
labels_2012c <- c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "NUM_TURNO", "DESCRICAO_ELEICAO",
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

files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/consulta_cand/consulta_cand_2012/consulta_cand_2012_", 
                        ufs_n[!ufs_n %in% c("BR", "ZZ", "DF")], ".txt"))
cand_2012 <- lapply(files, read.table, sep = ";", header = F, 
                    stringsAsFactors = F, fill = T, fileEncoding = "windows-1252") 
cand_2012 <- do.call("rbind", cand_2012c)
names(cand_2012) <- labels_2012
cand_2012 <- as_tibble(cand_2012)

#candidates 2016
labels_2016c <- c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "NUM_TURNO", "DESCRICAO_ELEICAO",
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

files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/consulta_cand/consulta_cand_2016/consulta_cand_2016_",
                        ufs_n[!ufs_n %in% c("BR", "ZZ", "DF")], ".txt"))
cand_2016 <- lapply(files, read.table, sep = ";", 
                    header = F, stringsAsFactors = F, fill = T, fileEncoding = "latin1") 
cand_2016 <- do.call("rbind", cand_2016c)
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

#Voting 2000
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/votacao_munzona/votacao_candidato_munzona_2000/votacao_candidato_munzona_2000_",
                        ufs_n[!ufs_n %in% c("BR", "ZZ", "DF")], ".txt"))
vot_2000 <- lapply(files, read.table, sep = ";", 
                   header = F, stringsAsFactors = F, fill = T, fileEncoding = "windows-1252") 
vot_2000 <- do.call("rbind", vot_2000)
names(vot_2000) <- labels_pre2012
vot_2000 <- as_tibble(vot_2000)

#Voting data 2004
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/votacao_munzona/votacao_candidato_munzona_2004/votacao_candidato_munzona_2004_",
                        ufs_n[!ufs_n %in% c("BR", "ZZ", "DF")], ".txt"))
vot_2004 <- lapply(files, read.table, sep = ";", 
                   header = F, stringsAsFactors = F, fill = T, fileEncoding = "windows-1252") 
vot_2004 <- do.call("rbind", vot_2004)
names(vot_2004) <- labels_pre2012
vot_2004 <- as_tibble(vot_2004)

#Voting data 2008
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/votacao_munzona/votacao_candidato_munzona_2008/votacao_candidato_munzona_2008_",
                        ufs_n[!ufs_n %in% c("BR", "ZZ", "DF")], ".txt"))
vot_2008 <- lapply(files, read.table, sep = ";", 
                   header = F, stringsAsFactors=F, fill = T, fileEncoding = "windows-1252") 
vot_2008 <- do.call("rbind", vot_2008)
names(vot_2008) <- labels_pre2012
vot_2008 <- as_tibble(vot_2008)

#voting data 2012
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/votacao_munzona/votacao_candidato_munzona_2012/votacao_candidato_munzona_2012_",
                        ufs_n[!ufs_n %in% c("BR", "ZZ", "DF")], ".txt"))
vot_2012 <- lapply(files, read.table, sep = ";", 
                   header = F, stringsAsFactors = F, fill = T, fileEncoding = "windows-1252") 
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
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/votacao_munzona/votacao_candidato_munzona_2016/votacao_candidato_munzona_2016_",
                        ufs_n[!ufs_n %in% c("BR", "ZZ", "DF")], ".txt"))
vot_2016 <- lapply(files, read.table, sep=";", 
                   header=F, stringsAsFactors=F, fill = T, fileEncoding = "latin1") 
vot_2016 <- do.call("rbind", vot_2016)
names(vot_2016) <- labels_2016
vot_2016 <- as_tibble(vot_2016)

vot_2000_2016 <- list(vot_2000, vot_2004, vot_2008, vot_2012, vot_2016)
save(vot_2000_2016, file = "~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/vot_2000_2016.RData")

#NATIONAL ELECTIONS

#candidates 1998
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/consulta_cand/consulta_cand_1998/consulta_cand_1998_",
                        ufs_n[!ufs_n %in% c("ZZ")], ".txt"))
cand_1998 <- lapply(files, read.table, sep = ";", header = F, 
                    stringsAsFactors = F, fill = T, fileEncoding = "windows-1252") 
cand_1998 <- do.call("rbind", cand_1998)
names(cand_1998) <- labels_pre2012c
cand_1998 <- as_tibble(cand_1998)

#candidates 2002
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/consulta_cand/consulta_cand_2002/consulta_cand_2002_",
                        ufs_n[!ufs_n %in% c("ZZ")], ".txt"))
cand_2002 <- lapply(files, read.table, sep = ";", header = F, 
                    stringsAsFactors = F, fill = T, fileEncoding = "windows-1252") 
cand_2002 <- do.call("rbind", cand_2002)
names(cand_2002) <- labels_pre2012c
cand_2002 <- as_tibble(cand_2002)

#candidates 2006
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/consulta_cand/consulta_cand_2006/consulta_cand_2006_",
                        ufs_n[!ufs_n %in% c("ZZ")], ".txt"))
cand_2006 <- lapply(files, read.table, sep = ";", header=F, 
                    stringsAsFactors = F, fill = T, fileEncoding = "windows-1252") 
cand_2006 <- do.call("rbind", cand_2006)
names(cand_2006) <- labels_pre2012c
cand_2006 <- as_tibble(cand_2006)

#candidates 2010
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/consulta_cand/consulta_cand_2010/consulta_cand_2010_", 
                        ufs_n[!ufs_n %in% c("ZZ")], ".txt"))
cand_2010 <- lapply(files, read.table, sep = ";", header = F, 
                    stringsAsFactors = F, fill = T, fileEncoding = "windows-1252") 
cand_2010 <- do.call("rbind", cand_2010)
names(cand_2010) <- labels_pre2012c
cand_2010 <- as_tibble(cand_2010)

#candidates 2014
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/consulta_cand/consulta_cand_2014/consulta_cand_2014_", 
                        ufs_n[!ufs_n %in% c("ZZ")], ".txt"))
cand_2014 <- lapply(files, read.table, sep = ";", header = F, 
                    stringsAsFactors = F, fill = T, fileEncoding = "windows-1252") 
cand_2014 <- do.call("rbind", cand_2014)
names(cand_2014) <- labels_2016c
cand_2014 <- as_tibble(cand_2014)

cand_1998_2014 <- list(cand_1998, cand_2002, cand_2006, cand_2010, cand_2014)
save(cand_1998_2014, file = "~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/cand_1998_2014.RData")

#Voting data 2000
labels_pre2012 <- c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "NUM_TURNO", "DESCRICAO_ELEICAO",
                    "SIGLA_UF", "SIGLA_UE", "CODIGO_MUNICIPIO", "NOME_MUNICIPIO", "NUMERO_ZONA",
                    "CODIGO_CARGO", "NUMERO_CAND", "SQ_CANDIDATO", "NOME_CANDIDATO", "NOME_URNA_CANDIDATO",
                    "DESCRICAO_CARGO", "COD_SIT_CAND_SUPERIOR", "DESC_SIT_CAND_SUPERIOR", "CODIGO_SIT_CANDIDATO",
                    "DESC_SIT_CANDIDATO", "CODIGO_SIT_CAND_TOT", "DESC_SIT_CAND_TOT", "NUMERO_PARTIDO",
                    "SIGLA_PARTIDO", "NOME_PARTIDO", "SEQUENCIAL_LEGENDA", "NOME_COLIGACAO", "COMPOSICAO_LEGENDA",
                    "TOTAL_VOTOS")

#Voting 1998
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/votacao_munzona/votacao_candidato_munzona_1998/votacao_candidato_munzona_1998_",
                        ufs_n[!ufs_n %in% c("ZZ")], ".txt"))
vot_1998 <- lapply(files, read.table, sep = ";", 
                   header = F, stringsAsFactors = F, fill = T, fileEncoding = "windows-1252") 
vot_1998 <- do.call("rbind", vot_1998)
names(vot_1998) <- labels_pre2012
vot_1998 <- as_tibble(vot_1998)

#Voting 2002
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/votacao_munzona/votacao_candidato_munzona_2002/votacao_candidato_munzona_2002_",
                        ufs, ".txt"))
vot_2002 <- lapply(files, read.table, sep = ";", 
                   header = F, stringsAsFactors = F, fill = T, fileEncoding = "windows-1252") 
vot_2002 <- do.call("rbind", vot_2002)
names(vot_2002) <- labels_pre2012
vot_2002 <- as_tibble(vot_2002)

#Voting data 2006
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/votacao_munzona/votacao_candidato_munzona_2006/votacao_candidato_munzona_2006_",
                        ufs, ".txt"))
vot_2006 <- lapply(files, read.table, sep = ";", 
                   header = F, stringsAsFactors = F, fill = T, fileEncoding = "windows-1252") 
vot_2006 <- do.call("rbind", vot_2006)
names(vot_2006) <- labels_pre2012
vot_2006 <- as_tibble(vot_2006)

#Voting data 2010
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/votacao_munzona/votacao_candidato_munzona_2010/votacao_candidato_munzona_2010_",
                        ufs, ".txt"))
vot_2010 <- lapply(files, read.table, sep = ";", 
                   header = F, stringsAsFactors=F, fill = T, fileEncoding = "windows-1252") 
vot_2010 <- do.call("rbind", vot_2010)
names(vot_2010) <- labels_pre2012
vot_2010 <- as_tibble(vot_2010)

#voting data 2014
files <- as.list(paste0("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/votacao_munzona/votacao_candidato_munzona_2014/votacao_candidato_munzona_2014_",
                        ufs_n[!ufs_n %in% c("ZZ")], ".txt"))
vot_2014 <- lapply(files, read.table, sep = ";", 
                   header = F, stringsAsFactors = F, fill = T, fileEncoding = "windows-1252") 
vot_2014 <- do.call("rbind", vot_2014)
names(vot_2014) <- labels_2016
vot_2014 <- as_tibble(vot_2014)

vot_1998_2014 <- list(vot_1998, vot_2002, vot_2006, vot_2010, vot_2014)
save(vot_1998_2014, file = "~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/vot_1998_2014.RData")

###################################################################
#2. NUMBER OF MUNICIPALITIES PER ELECTION
###################################################################

load("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/vot_2000_2016.RData")
load("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/cand_2000_2016.RData")
load("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/vot_1998_2014.RData")
load("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/cand_1998_2014.RData")

results <- as_tibble(matrix(NA, 10, 3))
results[,1] <- c(1998, 2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016)
results2 <- as_tibble(matrix(NA, 10, 3))
results2[,1] <- c(1998, 2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016)
names(results) <- c("ANO_ELEICAO", "N_UE_VOT", "N_UE_CAND")
names(results2) <- c("ANO_ELEICAO", "N_UE_VOT", "N_UE_CAND")

#######Elections 1998
vot_1998 <- vot_1998_2014[[1]]
cand_1998 <- cand_1998_2014[[1]]

results[1,2] <- vot_1998 %>% distinct(SIGLA_UE) %>% count()
results[1,3] <- cand_1998 %>% distinct(SIGLA_UE) %>% count()

results2[1,2] <- vot_1998 %>% distinct(SIGLA_UE) %>% count()
results2[1,3] <- cand_1998 %>% distinct(SIGLA_UE) %>% count()

#######Elections 2000
vot_2000 <- vot_2000_2016[[1]]
cand_2000 <- cand_2000_2016[[1]]

results[2,2] <- vot_2000 %>% distinct(SIGLA_UE) %>% count()
results[2,3] <- cand_2000 %>% distinct(SIGLA_UE) %>% count()

#######Elections 2002
vot_2002 <- vot_1998_2014[[2]]
cand_2002 <- cand_1998_2014[[2]]

results[3,2] <- vot_2002 %>% distinct(CODIGO_MUNICIPIO) %>% count()
results[3,3] <- cand_2002 %>% distinct(SIGLA_UE) %>% count()

results2[3,3] <- vot_2002 %>% filter(!SIGLA_UF %in% "ZZ") %>% distinct(SIGLA_UE) %>% count()
results2[3,3] <- cand_2002 %>% distinct(SIGLA_UE) %>% count()

#######Elections 2004
vot_2004 <- vot_2000_2016[[2]]
cand_2004 <- cand_2000_2016[[2]]

results[4,2] <- vot_2000 %>% distinct(SIGLA_UE) %>% count()
results[4,3] <- cand_2000 %>% distinct(SIGLA_UE) %>% count()

#######Elections 2006
vot_2006 <- vot_1998_2014[[3]]
cand_2006 <- cand_1998_2014[[3]]

results[5,2] <- vot_2006 %>% distinct(CODIGO_MUNICIPIO) %>% count()
results[5,3] <- cand_2006 %>% distinct(SIGLA_UE) %>% count()

results2[5,3] <- vot_2006 %>% filter(!SIGLA_UF %in% "ZZ") %>% distinct(SIGLA_UE) %>% count()
results2[5,3] <- cand_2006 %>% distinct(SIGLA_UE) %>% count()

#######Elections 2008
vot_2008 <- vot_2000_2016[[3]]
cand_2008 <- cand_2000_2016[[3]]

results[6,2] <- vot_2008 %>% distinct(SIGLA_UE) %>% count()
results[6,3] <- cand_2008 %>% distinct(SIGLA_UE) %>% count()

#######Elections 2010
vot_2010 <- vot_1998_2014[[4]]
cand_2010 <- cand_1998_2014[[4]]

results[7,2] <- vot_2010 %>% distinct(CODIGO_MUNICIPIO) %>% count()
results[7,3] <- cand_2010 %>% distinct(SIGLA_UE) %>% count()

results2[7,3] <- vot_2010 %>% filter(!SIGLA_UF %in% c("ZZ", "VT")) %>% distinct(SIGLA_UE) %>% count()
results2[7,3] <- cand_2010 %>% distinct(SIGLA_UE) %>% count()

#######Elections 2012
vot_2012 <- vot_2000_2016[[4]]
cand_2012 <- cand_2000_2016[[4]]

results[8,2] <- vot_2012 %>% distinct(SIGLA_UE) %>% count()
results[8,3] <- cand_2012 %>% distinct(SIGLA_UE) %>% count()

#######Elections 2014
vot_2014 <- vot_1998_2014[[5]]
cand_2014 <- cand_1998_2014[[5]]

results[9,2] <- vot_2014 %>% distinct(CODIGO_MUNICIPIO) %>% count()
results[9,3] <- cand_2014 %>% distinct(SIGLA_UE) %>% count()

results2[9,3] <- vot_2014 %>% filter(!SIGLA_UF %in% c("ZZ")) %>% distinct(SIGLA_UE) %>% count()
results2[9,3] <- cand_2014 %>% distinct(SIGLA_UE) %>% count()

#######Elections 2016
vot_2016 <- vot_2000_2016[[5]]
cand_2016 <- cand_2000_2016[[5]]

results[10,2] <- vot_2016 %>% distinct(SIGLA_UE) %>% count()
results[10,3] <- cand_2016 %>% distinct(SIGLA_UE) %>% count()

write.csv(results, "~/Dropbox/LOCAL_ELECTIONS/cepesp_data/numero_municipios.csv")
write.csv(results2, "~/Dropbox/LOCAL_ELECTIONS/cepesp_data/numero_municipios_noZZVT.csv")


############### Comparing Data Abraao, CEPESP-API, TSE and IBGE

ab <- read_csv("~/Dropbox/LOCAL_ELECTIONS/cepesp_data/abraao_raw_data/uf-cod-mun.csv", 
               col_names = FALSE)

api98 <- read_csv("~/Dropbox/LOCAL_ELECTIONS/cepesp_data/cepesp_api_rawdata/mun_1998.csv") 
api00 <- read_csv("~/Dropbox/LOCAL_ELECTIONS/cepesp_data/cepesp_api_rawdata/mun_2000.csv")
api02 <- read_csv("~/Dropbox/LOCAL_ELECTIONS/cepesp_data/cepesp_api_rawdata/mun_2002.csv")
api04 <- read_csv("~/Dropbox/LOCAL_ELECTIONS/cepesp_data/cepesp_api_rawdata/mun_2004.csv")
api06 <- read_csv("~/Dropbox/LOCAL_ELECTIONS/cepesp_data/cepesp_api_rawdata/mun_2006.csv")
api08 <- read_csv("~/Dropbox/LOCAL_ELECTIONS/cepesp_data/cepesp_api_rawdata/mun_2008.csv")
api10 <- read_csv("~/Dropbox/LOCAL_ELECTIONS/cepesp_data/cepesp_api_rawdata/mun_2010.csv")
api12 <- read_csv("~/Dropbox/LOCAL_ELECTIONS/cepesp_data/cepesp_api_rawdata/mun_2012.csv")
api14 <- read_csv("~/Dropbox/LOCAL_ELECTIONS/cepesp_data/cepesp_api_rawdata/mun_2014.csv")
api16 <- read_csv("~/Dropbox/LOCAL_ELECTIONS/cepesp_data/cepesp_api_rawdata/mun_2016.csv")

results_cepesp <- as_tibble(matrix(NA, 10, 2))
results_cepesp[,1] <- c(1998, 2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016)
names(results_cepesp) <- c("ANO_ELEICAO", "N_MUN")

results_cepesp[1,2] <- api98 %>% distinct(CODIGO_MUNICIPIO) %>% count()
results_cepesp[2,2] <- api00 %>% distinct(CODIGO_MUNICIPIO) %>% count()
results_cepesp[3,2] <- api02 %>% distinct(CODIGO_MUNICIPIO) %>% count()
results_cepesp[4,2] <- api04 %>% distinct(CODIGO_MUNICIPIO) %>% count()
results_cepesp[5,2] <- api06 %>% distinct(CODIGO_MUNICIPIO) %>% count()
results_cepesp[6,2] <- api08 %>% distinct(CODIGO_MUNICIPIO) %>% count()
results_cepesp[7,2] <- api10 %>% distinct(CODIGO_MUNICIPIO) %>% count()
results_cepesp[8,2] <- api12 %>% distinct(CODIGO_MUNICIPIO) %>% count()
results_cepesp[9,2] <- api14 %>% distinct(CODIGO_MUNICIPIO) %>% count()
results_cepesp[10,2] <- api16 %>% distinct(CODIGO_MUNICIPIO) %>% count()

#combining

r
  