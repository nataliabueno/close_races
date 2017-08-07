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
library(qpcR)
library(readxl)

#Change your working directory here
setwd("~")
if(grepl("nataliabueno",getwd())==TRUE){#Allow for different paths in our computers
  dir <- "~/Dropbox/LOCAL_ELECTIONS/"
}else{
  dir <- "~/Dropbox/LOCAL_ELECTIONS/"
}

#helper functions
source(paste0(dir, "codes/helper_functions.R"))

###################################################################
#0. Downloading 
###################################################################

dir_d <- "~/Dropbox/LOCAL_ELECTIONS/repositorio_data/"

#Candidate data
url_cand98 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_1998.zip"
file_d <- paste0(dir_d, "original_data/consulta_cand/consulta_cand_1998.zip")
file_un <- paste0(dir_d, "original_unzipped/consulta_cand/consulta_cand_1998")
cand_1998 <- get_tse(url_cand98, file_d, file_un)

url_cand00 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2000.zip"
file_d <- paste0(dir_d, "original_data/consulta_cand/consulta_cand_2000.zip")
file_un <- paste0(dir_d, "original_unzipped/consulta_cand/consulta_cand_2000")
cand_2000 <- get_tse(url_cand00, file_d, file_un)

url_cand02 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2002.zip"
file_d <- paste0(dir_d, "original_data/consulta_cand/consulta_cand_2002.zip")
file_un <- paste0(dir_d, "original_unzipped/consulta_cand/consulta_cand_2002")
cand_2002 <- get_tse(url_cand02, file_d, file_un)

url_cand04 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2004.zip"
file_d <- paste0(dir_d, "original_data/consulta_cand/consulta_cand_2004.zip")
file_un <- paste0(dir_d, "original_unzipped/consulta_cand/consulta_cand_2004")
cand_2004 <- get_tse(url_cand04, file_d, file_un)

url_cand06 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2006.zip"
file_d <- paste0(dir_d, "original_data/consulta_cand/consulta_cand_2006.zip")
file_un <- paste0(dir_d, "original_unzipped/consulta_cand/consulta_cand_2006")
cand_2006 <- get_tse(url_cand06, file_d, file_un)

url_cand08 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2008.zip"
file_d <- paste0(dir_d, "original_data/consulta_cand/consulta_cand_2008.zip")
file_un <- paste0(dir_d, "original_unzipped/consulta_cand/consulta_cand_2008")
cand_2008 <- get_tse(url_cand08, file_d, file_un)

url_cand10 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2010.zip"
file_d <- paste0(dir_d, "original_data/consulta_cand/consulta_cand_2010.zip")
file_un <- paste0(dir_d, "original_unzipped/consulta_cand/consulta_cand_2010")
cand_2010 <- get_tse(url_cand10, file_d, file_un)

url_cand12 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2012.zip"
file_d <- paste0(dir_d, "original_data/consulta_cand/consulta_cand_2012.zip")
file_un <- paste0(dir_d, "original_unzipped/consulta_cand/consulta_cand_2012")
cand_2012 <- get_tse(url_cand12, file_d, file_un)

url_cand14 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2014.zip"
file_d <- paste0(dir_d, "original_data/consulta_cand/consulta_cand_2014.zip")
file_un <- paste0(dir_d, "original_unzipped/consulta_cand/consulta_cand_2014")
cand_2014 <- get_tse(url_cand14, file_d, file_un)

url_cand16 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2016.zip"
file_d <- paste0(dir_d, "original_data/consulta_cand/consulta_cand_2016.zip")
file_un <- paste0(dir_d, "original_unzipped/consulta_cand/consulta_cand_2016")
cand_2016 <- get_tse(url_cand16, file_d, file_un)

#Voting data
url_vot98 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_1998.zip"
file_d <- paste0(dir_d, "original_data/votacao_munzona/votacao_candidato_munzona_1998.zip")
file_un <- paste0(dir_d, "original_unzipped/votacao_munzona/votacao_candidato_munzona_1998")
vot_1998 <- get_tse(url_vot98, file_d, file_un)

url_vot00 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2000.zip"
file_d <- paste0(dir_d, "original_data/votacao_munzona/votacao_candidato_munzona_2000.zip")
file_un <- paste0(dir_d, "original_unzipped/votacao_munzona/votacao_candidato_munzona_2000")
vot_2000 <- get_tse(url_vot00, file_d, file_un)

url_vot02 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2002.zip"
file_d <- paste0(dir_d, "original_data/votacao_munzona/votacao_candidato_munzona_2002.zip")
file_un <- paste0(dir_d, "original_unzipped/votacao_munzona/votacao_candidato_munzona_2002")
vot_2002 <- get_tse(url_vot02, file_d, file_un)

url_vot04 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2004.zip"
file_d <- paste0(dir_d, "original_data/votacao_munzona/votacao_candidato_munzona_2004.zip")
file_un <- paste0(dir_d, "original_unzipped/votacao_munzona/votacao_candidato_munzona_2004")
vot_2004 <- get_tse(url_vot04, file_d, file_un)

url_vot06 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2006.zip"
file_d <- paste0(dir_d, "original_data/votacao_munzona/votacao_candidato_munzona_2006.zip")
file_un <- paste0(dir_d, "original_unzipped/votacao_munzona/votacao_candidato_munzona_2006")
vot_2006 <- get_tse(url_vot06, file_d, file_un)

url_vot08 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2008.zip"
file_d <- paste0(dir_d, "original_data/votacao_munzona/votacao_candidato_munzona_2008.zip")
file_un <- paste0(dir_d, "original_unzipped/votacao_munzona/votacao_candidato_munzona_2008")
vot_2008 <- get_tse(url_vot08, file_d, file_un)

url_vot10 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2010.zip"
file_d <- paste0(dir_d, "original_data/votacao_munzona/votacao_candidato_munzona_2010.zip")
file_un <- paste0(dir_d, "original_unzipped/votacao_munzona/votacao_candidato_munzona_2010")
vot_2010 <- get_tse(url_vot10, file_d, file_un)

url_vot12 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2012.zip"
file_d <- paste0(dir_d, "original_data/votacao_munzona/votacao_candidato_munzona_2012.zip")
file_un <- paste0(dir_d, "original_unzipped/votacao_munzona/votacao_candidato_munzona_2012")
vot_2012 <- get_tse(url_vot12, file_d, file_un)

url_vot14 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2014.zip"
file_d <- paste0(dir_d, "original_data/votacao_munzona/votacao_candidato_munzona_2014.zip")
file_un <- paste0(dir_d, "original_unzipped/votacao_munzona/votacao_candidato_munzona_2014")
vot_2014 <- get_tse(url_vot14, file_d, file_un)

url_vot16 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2016.zip"
file_d <- paste0(dir_d, "original_data/votacao_munzona/votacao_candidato_munzona_2016.zip")
file_un <- paste0(dir_d, "original_unzipped/votacao_munzona/votacao_candidato_munzona_2016")
vot_2016 <- get_tse(url_cand16, file_d, file_un)

# Votacao Secao TO DO

#2016 TO DO

ufs_2016 <- c("AC", "AL", "AP", "AM", "BA",   
           "CE", "ES", "GO", "MA", "MT", "MS",
           "MG", "PA", "PB", "PR", "PE", "PI", "RJ",
           "RN", "RS", "RO","RR","SC", "SP", "SE", "TO")

for (i in 1:length(ufs_2016)){
  url <- paste0("http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_secao/votacao_secao_2016_", ufs_2016[i],
                ".zip")
  file_d <- paste0(dir_d, "original_data/votacao_secao/votacao_secao_2016_", ufs_2016[i], ".zip")
  file_un <- paste0(dir_d, "original_unzipped/votacao_secao/votacao_secao_2016")
  temp <- get_tse(url, file_d, file_un)  
}

#2014 TO DO

ufs_2014 <- c("AC", "AL", "AP", "AM", "BA", "BR",   
              "CE", "DF", "ES", "GO", "MA", "MT", "MS",
              "MG", "PA", "PB", "PR", "PE", "PI", "RJ",
              "RN", "RS", "RO","RR","SC", "SP", "SE", "TO")

for (i in 1:length(ufs_2014)){
  url <- paste0("http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_secao/votacao_secao_2014_", ufs_2014[i],
                ".zip")
  file_d <- paste0(dir_d, "original_data/votacao_secao/votacao_secao_2014_", ufs_2014[i], ".zip")
  file_un <- paste0(dir_d, "original_unzipped/votacao_secao/votacao_secao_2014/")
  temp <- get_tse(url, file_d, file_un) 
}

#2012 TO DO

ufs_2012 <- c("AC", "AL", "AP", "AM", "BA",   
              "CE", "ES", "GO", "MA", "MT", "MS",
              "MG", "PA", "PB", "PR", "PE", "PI", "RJ",
              "RN", "RS", "RO","RR","SC", "SP", "SE", "TO")

for (i in 1:length(ufs_2012)){
  url <- paste0("http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_secao/votacao_secao_2012_", ufs_2012[i],
              ".zip")
  file_d <- paste0(dir_d, "original_data/votacao_secao/votacao_secao_2012_", ufs_2012[i], ".zip")
  file_un <- paste0(dir_d, "original_unzipped/votacao_secao/votacao_secao_2012/")
  temp <- get_tse(url, file_d, file_un) 
}

#<a href="http://agencia.tse.jus.br/estatistica/sead/eleicoes/eleicoes2012/votosecao/vsec_2t_AP_30102012194527.zip">Amapá</a>
# <a href="http://agencia.tse.jus.br/estatistica/sead/eleicoes/eleicoes2012/votosecao/vsec_1t_AC.zip">Acre</a>
  
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

results[1,2] <- vot_1998 %>% distinct(CODIGO_MUNICIPIO) %>% count()
results[1,3] <- cand_1998 %>% distinct(SIGLA_UE) %>% count()

results2[1,2] <- vot_1998 %>% distinct(CODIGO_MUNICIPIO) %>% count()
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

results2[3,2] <- vot_2002 %>% filter(!SIGLA_UF %in% "ZZ") %>% distinct(CODIGO_MUNICIPIO) %>% count()
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

results2[5,2] <- vot_2006 %>% filter(!SIGLA_UF %in% "ZZ") %>% distinct(CODIGO_MUNICIPIO) %>% count()
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

results2[7,2] <- vot_2010 %>% filter(!SIGLA_UF %in% c("ZZ", "VT")) %>% distinct(CODIGO_MUNICIPIO) %>% count()
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

results2[9,2] <- vot_2014 %>% filter(!SIGLA_UF %in% c("ZZ")) %>% distinct(CODIGO_MUNICIPIO) %>% count()
results2[9,3] <- cand_2014 %>% distinct(SIGLA_UE) %>% count()

#######Elections 2016
vot_2016 <- vot_2000_2016[[5]]
cand_2016 <- cand_2000_2016[[5]]

results[10,2] <- vot_2016 %>% distinct(SIGLA_UE) %>% count()
results[10,3] <- cand_2016 %>% distinct(SIGLA_UE) %>% count()

write.csv(results, "~/Dropbox/LOCAL_ELECTIONS/cepesp_data/numero_municipios.csv")
write.csv(results2, "~/Dropbox/LOCAL_ELECTIONS/cepesp_data/numero_municipios_noZZVT.csv")

results_tse <- bind_cols(results, results2)

############### Comparing Data Abraao, CEPESP-API, TSE and IBGE

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

results_cepesp[1,2] <- api98 %>% distinct(COD_MUN_TSE) %>% count()
results_cepesp[2,2] <- api00 %>% distinct(COD_MUN_TSE) %>% count()
results_cepesp[3,2] <- api02 %>% distinct(COD_MUN_TSE) %>% count()
results_cepesp[4,2] <- api04 %>% distinct(COD_MUN_TSE) %>% count()
results_cepesp[5,2] <- api06 %>% distinct(COD_MUN_TSE) %>% count()
results_cepesp[6,2] <- api08 %>% distinct(COD_MUN_TSE) %>% count()
results_cepesp[7,2] <- api10 %>% distinct(COD_MUN_TSE) %>% count()
results_cepesp[8,2] <- api12 %>% distinct(COD_MUN_TSE) %>% count()
results_cepesp[9,2] <- api14 %>% distinct(COD_MUN_TSE) %>% count()
results_cepesp[10,2] <- api16 %>% distinct(COD_MUN_TSE) %>% count()

#combining

results_all <- bind_cols(results_tse, results_cepesp)
results_all <- results_all[-c(4, 7)]
names(results_all) <- c("ANO_ELEICAO",
                        "Municipios_BaseVot_TSECompleta", 
                        "Municipios_BaseCand_TSECompleta", 
                        "Municipios_BaseVot_TSEsemZZVT", 
                        "Municipios_BaseCand_TSEsemZZVT", 
                        "Municipios_Base_CEPESP_api")

#What's not matching in TSE and CEPESP in 2000?
cepesp <- distinct(api00, COD_MUN_TSE)$COD_MUN_TSE
missing_2000 <- vot_2000 %>% filter(!(CODIGO_MUNICIPIO %in% cepesp)) %>% distinct(CODIGO_MUNICIPIO)

#What's not matching in TSE and CEPESP in 2002?
tse <- vot_2002 %>% filter(!SIGLA_UF %in% c("ZZ"))
cepesp <- distinct(api02, COD_MUN_TSE)$COD_MUN_TSE
missing_2002 <- tse %>% filter(!(CODIGO_MUNICIPIO %in% cepesp)) %>% distinct(CODIGO_MUNICIPIO)

#What's not matching in TSE and CEPESP in 2004?
cepesp <- distinct(api04, COD_MUN_TSE)$COD_MUN_TSE
missing_2004 <- vot_2004 %>% filter(!(CODIGO_MUNICIPIO %in% cepesp)) %>% distinct(CODIGO_MUNICIPIO)

#What's not matching in TSE and CEPESP in 2008?
cepesp <- distinct(api08, COD_MUN_TSE)$COD_MUN_TSE
tse <- distinct(vot_2008, CODIGO_MUNICIPIO)$CODIGO_MUNICIPIO
missing_2008 <- vot_2008 %>% filter(!(CODIGO_MUNICIPIO %in% cepesp)) %>% distinct(CODIGO_MUNICIPIO)

missing_ids <- qpcR:::cbind.na(missing_2000$CODIGO_MUNICIPIO, missing_2002$CODIGO_MUNICIPIO, 
                missing_2004$CODIGO_MUNICIPIO, missing_2008$CODIGO_MUNICIPIO)
colnames(missing_ids) <- c("muns_2000", "muns_2002", "muns_2004", "muns_2008")

write.csv(results_all, "~/Dropbox/LOCAL_ELECTIONS/cepesp_data/municipios_missing_3007.csv")
write.csv(missing_ids, "~/Dropbox/LOCAL_ELECTIONS/cepesp_data/mun_ids_missing.csv")

#Example, 2008

temp_2008 <- vot_2008 %>% filter(CODIGO_MUNICIPIO == 1066) #Porto Walter/AC
temp_2002 <- vot_2002 %>% filter(CODIGO_MUNICIPIO %in% c(91065, 12734)) #BOA ESPERANCA DO NORTE/MT, AROEIRAS DO ITAIM/PI

#No detalhe votacao, é necessario fazer o filtro por suplementares?
#No votacao_secao?
#Diferença: 

################ Codes for nonregular elections

table(vot_1998$DESCRICAO_ELEICAO)
table(cand_1998$DESCRICAO_ELEICAO)

table(vot_2000$DESCRICAO_ELEICAO)
table(cand_2000$DESCRICAO_ELEICAO)

table(vot_2002$DESCRICAO_ELEICAO)
table(cand_2002$DESCRICAO_ELEICAO)

table(vot_2004$DESCRICAO_ELEICAO)
table(cand_2004$DESCRICAO_ELEICAO)

table(vot_2006$DESCRICAO_ELEICAO)
table(cand_2006$DESCRICAO_ELEICAO)

table(vot_2008$DESCRICAO_ELEICAO)
table(cand_2008$DESCRICAO_ELEICAO)

table(vot_2010$DESCRICAO_ELEICAO)
table(cand_2010$DESCRICAO_ELEICAO)

table(vot_2012$DESCRICAO_ELEICAO)
table(cand_2012$DESCRICAO_ELEICAO)

table(vot_2014$DESCRICAO_ELEICAO)
table(cand_2014$DESCRICAO_ELEICAO)

table(vot_2016$DESCRICAO_ELEICAO)
table(cand_2016$DESCRICAO_ELEICAO)

###################################################################
#3. Cleaning candidates prior to join
###################################################################

load("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/cand_2000_2016.RData")
load("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_unzipped/cand_1998_2014.RData")

cand_1998 <- cand_1998_2014[[1]]
cand_2000 <- cand_2000_2016[[1]]
cand_2002 <- cand_1998_2014[[2]]
cand_2004 <- cand_2000_2016[[2]]
cand_2006 <- cand_1998_2014[[3]]
cand_2008 <- cand_2000_2016[[3]]
cand_2010 <- cand_1998_2014[[4]]
cand_2012 <- cand_2000_2016[[4]]
cand_2014 <- cand_1998_2014[[5]]
cand_2016 <- cand_2000_2016[[5]]

#1998
problems <- cand_1998 %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE) %>%
        summarise(total = n()) %>% filter(total > 1)

casos <- cand_1998 %>% right_join(problems, by = c("NUM_TURNO", 
            "NUMERO_CANDIDATO", "CODIGO_CARGO", "SIGLA_UE"))

#Control for repeated
repeated_casos <- casos[duplicated(casos),]

#But keeping only unique
casos <- unique(casos)

casos <- casos %>% filter(COD_SITUACAO_CANDIDATURA == 2 | 
                   COD_SITUACAO_CANDIDATURA == 4)

problems_caso <- casos %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE) %>%
                 summarise(total = n()) %>% filter(total > 1)

wrong_ballot <- casos %>% right_join(problems_caso, by = c("NUM_TURNO", 
                      "NUMERO_CANDIDATO", "CODIGO_CARGO", "SIGLA_UE"))

#Exclude
wrong_ballot <- wrong_ballot %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                                     NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                                     NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO)) 

write.csv(wrong_ballot, file = paste0(dir, "cepesp_data/wrong_ballot.csv"))

#WHAT TO EXCLUDE
cand_1998 <- cand_1998_2014[[1]]
problems <- cand_1998 %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE) %>%
  summarise(total = n()) %>% filter(total > 1)

casos <- cand_1998 %>% right_join(problems, by = c("NUM_TURNO", 
                                  "NUMERO_CANDIDATO", "CODIGO_CARGO", "SIGLA_UE"))

repeated_casos <- casos[duplicated(casos),] #save
repeated_casos <- repeated_casos %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                                         NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                                         NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO)) #save

#But keeping only unique
casos <- unique(casos)
casos_notvalid <- casos %>% filter(!(COD_SITUACAO_CANDIDATURA == 2 | 
                                   COD_SITUACAO_CANDIDATURA == 4))
casos_notvalid <- casos_notvalid %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                                         NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                                         NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO)) #save

case_key <-  read_excel(paste0(dir, "cepesp_data/wrong_ballot1998_excl_Key.xlsx"))

exclude <- c(repeated_casos$key, case_key$key, casos_notvalid$key)

cand_1998 <- cand_1998 %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                               NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                               NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO))

cand_1998v2 <- cand_1998 %>% filter(!(key %in% exclude))
cand_1998v2 <- cand_1998v2[,-ncol(cand_1998v2)]

save(cand_1998v2, file = paste0(dir, "cepesp_data/cand_1998v2.Rda"))
write.csv(cand_1998v2, file = paste0(dir, "cepesp_data/cand_1998v2.csv"))

#Smell test
temp <- cand_1998 %>% filter(key %in% exclude)
stopifnot((nrow(cand_1998) - nrow(cand_1998v2)) == nrow(temp))


#2000

#counting ufs per city

ufs_2000 <- cand_2000 %>% group_by(SIGLA_UE, SIGLA_UF) %>%
  summarise(total = n()) 

ufs_2000 <- ufs_2000 %>% group_by(SIGLA_UE) %>%
  summarise(total = n())

# only the municipality 400037 appears in two UFs

#
cand_2000v1 <- cand_2000 %>% filter(!(CODIGO_CARGO == 13 & NUM_TURNO == 2))

problems <- cand_2000v1 %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE, 
                                   DESCRICAO_ELEICAO) %>%
            summarise(total = n()) %>% filter(total > 1)

casos <- cand_2000v1 %>% right_join(problems, by = c("NUM_TURNO", 
                                                   "NUMERO_CANDIDATO", "CODIGO_CARGO", "SIGLA_UE",
                                                   "DESCRICAO_ELEICAO"))

#Control for repeated
repeated_casos <- casos[duplicated(casos),]

#But keeping only unique
casos <- unique(casos)

casos <- casos %>% filter(COD_SITUACAO_CANDIDATURA == 1 |
                          COD_SITUACAO_CANDIDATURA == 2 | 
                          COD_SITUACAO_CANDIDATURA == 4)

problems_caso <- casos %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE, DESCRICAO_ELEICAO) %>%
                  summarise(total = n()) %>% filter(total > 1)

wrong_ballot <- casos %>% right_join(problems_caso, by = c("NUM_TURNO", 
                                   "NUMERO_CANDIDATO", "CODIGO_CARGO", "SIGLA_UE", 
                                   "DESCRICAO_ELEICAO"))

#Exclude
wrong_ballot <- wrong_ballot %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                                     NUMERO_CANDIDATO)) 

write.csv(wrong_ballot, file = paste0(dir, "cepesp_data/wrong_ballot_2000.csv"))

#2002
problems <- cand_2002 %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE) %>%
            summarise(total = n()) %>% filter(total > 1)

casos <- cand_2002 %>% right_join(problems, by = c("NUM_TURNO", 
                       "NUMERO_CANDIDATO", "CODIGO_CARGO", "SIGLA_UE"))

#Control for repeated
repeated_casos <- casos[duplicated(casos),]

#But keeping only unique
casos <- unique(casos)

casos <- casos %>% filter(COD_SITUACAO_CANDIDATURA == 2 | 
                          COD_SITUACAO_CANDIDATURA == 4)

problems_caso <- casos %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE) %>%
                  summarise(total = n()) %>% filter(total > 1)

#WHAT TO EXCLUDE
cand_2002 <- cand_1998_2014[[2]]
problems <- cand_2002 %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE) %>%
  summarise(total = n()) %>% filter(total > 1)

casos <- cand_2002 %>% right_join(problems, by = c("NUM_TURNO", 
                                                   "NUMERO_CANDIDATO", "CODIGO_CARGO", "SIGLA_UE"))

repeated_casos <- casos[duplicated(casos),] #save
repeated_casos <- repeated_casos %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                                         NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                                         NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO)) #save

#But keeping only unique
casos <- unique(casos)
casos_notvalid <- casos %>% filter(!(COD_SITUACAO_CANDIDATURA == 2 | 
                                       COD_SITUACAO_CANDIDATURA == 4))
casos_notvalid <- casos_notvalid %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                                         NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                                         NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO)) #save

exclude <- c(repeated_casos$key, casos_notvalid$key)

cand_2002 <- cand_2002 %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                               NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                               NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO))

cand_2002v2 <- cand_2002 %>% filter(!(key %in% exclude))
cand_2002v2 <- cand_2002v2[,-ncol(cand_2002v2)]

save(cand_2002v2, file = paste0(dir, "cepesp_data/cand_2002v2.Rda"))
write.csv(cand_2002v2, file = paste0(dir, "cepesp_data/cand_2002v2.csv"))

#Smell test
temp <- cand_2002 %>% filter(key %in% exclude)
stopifnot((nrow(cand_2002) - nrow(cand_2002v2)) == nrow(temp))

#2004
problems <- cand_2004 %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE) %>%
  summarise(total = n()) %>% filter(total > 1)

casos <- cand_2004 %>% right_join(problems, by = c("NUM_TURNO", 
                                                   "NUMERO_CANDIDATO", "CODIGO_CARGO", "SIGLA_UE"))

#Control for repeated
repeated_casos <- casos[duplicated(casos),]

#But keeping only unique
casos <- unique(casos)

casos <- casos %>% filter(COD_SITUACAO_CANDIDATURA == 2 | 
                            COD_SITUACAO_CANDIDATURA == 4)

problems_caso <- casos %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE) %>%
  summarise(total = n()) %>% filter(total > 1)


#WHAT TO EXCLUDE
cand_2004 <- cand_2000_2016[[2]]
problems <- cand_2004 %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE) %>%
  summarise(total = n()) %>% filter(total > 1)

casos <- cand_2004 %>% right_join(problems, by = c("NUM_TURNO", 
                                                   "NUMERO_CANDIDATO", "CODIGO_CARGO", "SIGLA_UE"))

repeated_casos <- casos[duplicated(casos),] #save
repeated_casos <- repeated_casos %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                                         NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                                         NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO)) #save

#But keeping only unique
casos <- unique(casos)
casos_notvalid <- casos %>% filter(!(COD_SITUACAO_CANDIDATURA == 2 | 
                                       COD_SITUACAO_CANDIDATURA == 4))
casos_notvalid <- casos_notvalid %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                                         NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                                         NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO)) #save

exclude <- c(repeated_casos$key, casos_notvalid$key)

cand_2004 <- cand_2004 %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                               NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                               NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO))

cand_2004v2 <- cand_2004 %>% filter(!(key %in% exclude))
cand_2004v2 <- cand_2004v2[,-ncol(cand_2004v2)]

save(cand_2004v2, file = paste0(dir, "cepesp_data/cand_2004v2.Rda"))
write.csv(cand_2004v2, file = paste0(dir, "cepesp_data/cand_2004v2.csv"))

#Smell test
temp <- cand_2004 %>% filter(key %in% exclude)
stopifnot((nrow(cand_2004) - nrow(cand_2004v2)) == nrow(temp))

#2006
problems <- cand_2006 %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE) %>%
  summarise(total = n()) %>% filter(total > 1)

casos <- cand_2006 %>% right_join(problems, by = c("NUM_TURNO", 
                                                   "NUMERO_CANDIDATO", "CODIGO_CARGO", "SIGLA_UE"))

#Control for repeated
repeated_casos <- casos[duplicated(casos),]

#But keeping only unique
casos <- unique(casos)

casos <- casos %>% filter(COD_SITUACAO_CANDIDATURA == 2 | 
                          COD_SITUACAO_CANDIDATURA == 4 | COD_SITUACAO_CANDIDATURA == 16)

problems_caso <- casos %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE) %>%
                 summarise(total = n()) %>% filter(total > 1)

#WHAT TO EXCLUDE
cand_2006 <- cand_1998_2014[[3]]
problems <- cand_2006 %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE) %>%
  summarise(total = n()) %>% filter(total > 1)

casos <- cand_2006 %>% right_join(problems, by = c("NUM_TURNO", 
                                                   "NUMERO_CANDIDATO", "CODIGO_CARGO", "SIGLA_UE"))

repeated_casos <- casos[duplicated(casos),] #save
repeated_casos <- repeated_casos %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                                         NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                                         NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO)) #save

#But keeping only unique
casos <- unique(casos)
casos_notvalid <- casos %>% filter(!(COD_SITUACAO_CANDIDATURA == 2 | 
                                      COD_SITUACAO_CANDIDATURA == 4 | COD_SITUACAO_CANDIDATURA == 16))
casos_notvalid <- casos_notvalid %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                                         NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                                         NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO)) #save

exclude <- c(repeated_casos$key, casos_notvalid$key)

cand_2006 <- cand_2006 %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                               NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                               NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO))

cand_2006v2 <- cand_2006 %>% filter(!(key %in% exclude))
cand_2006v2 <- cand_2006v2[,-ncol(cand_2006v2)]

save(cand_2006v2, file = paste0(dir, "cepesp_data/cand_2006v2.Rda"))
write.csv(cand_2006v2, file = paste0(dir, "cepesp_data/cand_2006v2.csv"))

#Smell test
temp <- cand_2006 %>% filter(key %in% exclude)
stopifnot((nrow(cand_2006) - nrow(cand_2006v2)) == nrow(temp))

#2008
problems <- cand_2008 %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE, 
                          DESCRICAO_ELEICAO) %>%
                          summarise(total = n()) %>% filter(total > 1)

casos <- cand_2008 %>% right_join(problems, by = c("NUM_TURNO", 
                       "NUMERO_CANDIDATO", "CODIGO_CARGO", "SIGLA_UE", 
                       "DESCRICAO_ELEICAO"))

#Control for repeated
repeated_casos <- casos[duplicated(casos),]

#But keeping only unique
casos <- unique(casos)

casos <- casos %>% filter(COD_SITUACAO_CANDIDATURA == 2 | 
                          COD_SITUACAO_CANDIDATURA == 4 |
                          COD_SITUACAO_CANDIDATURA == 8 |
                          COD_SITUACAO_CANDIDATURA == 16 | 
                          COD_SITUACAO_CANDIDATURA == 17)

problems_caso <- casos %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE, 
                                    DESCRICAO_ELEICAO) %>%
                           summarise(total = n()) %>% filter(total > 1)

#WHAT TO EXCLUDE
cand_2008 <- cand_2000_2016[[3]]
problems <- cand_2008 %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE, 
                                   DESCRICAO_ELEICAO) %>%
                          summarise(total = n()) %>% filter(total > 1)

casos <- cand_2008 %>% right_join(problems, by = c("NUM_TURNO", 
                        "NUMERO_CANDIDATO", "CODIGO_CARGO", "SIGLA_UE", 
                        "DESCRICAO_ELEICAO"))

repeated_casos <- casos[duplicated(casos),] #save
repeated_casos <- repeated_casos %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                                         NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                                         NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO)) #save

#But keeping only unique
casos <- unique(casos)
casos_notvalid <- casos %>% filter(!(COD_SITUACAO_CANDIDATURA == 2 | 
                                     COD_SITUACAO_CANDIDATURA == 4 |
                                     COD_SITUACAO_CANDIDATURA == 8 |
                                     COD_SITUACAO_CANDIDATURA == 16 | 
                                     COD_SITUACAO_CANDIDATURA == 17))


casos_notvalid <- casos_notvalid %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                                         NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                                         NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO)) #save

exclude <- c(repeated_casos$key, casos_notvalid$key)

cand_2008 <- cand_2008 %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                               NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                               NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO))

cand_2008v2 <- cand_2008 %>% filter(!(key %in% exclude))
cand_2008v2 <- cand_2008v2[,-ncol(cand_2008v2)]

save(cand_2008v2, file = paste0(dir, "cepesp_data/cand_2008v2.Rda"))
write.csv(cand_2008v2, file = paste0(dir, "cepesp_data/cand_2008v2.csv"))

#Smell test
temp <- cand_2008 %>% filter(key %in% exclude)
stopifnot((nrow(cand_2008) - nrow(cand_2008v2)) == nrow(temp))

#2010
problems <- cand_2010 %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE, 
                                   DESCRICAO_ELEICAO) %>%
                          summarise(total = n()) %>% filter(total > 1)

casos <- cand_2010 %>% right_join(problems, by = c("NUM_TURNO", 
                                                   "NUMERO_CANDIDATO", "CODIGO_CARGO", "SIGLA_UE", 
                                                   "DESCRICAO_ELEICAO"))

#Control for repeated
repeated_casos <- casos[duplicated(casos),]

#But keeping only unique
casos <- unique(casos)

casos <- casos %>% filter(COD_SITUACAO_CANDIDATURA == 2 | 
                            COD_SITUACAO_CANDIDATURA == 4 |
                            COD_SITUACAO_CANDIDATURA == 16 | 
                            COD_SITUACAO_CANDIDATURA == 17 |
                          COD_SITUACAO_CANDIDATURA == 18)

problems_caso <- casos %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE, 
                                    DESCRICAO_ELEICAO) %>%
                           summarise(total = n()) %>% filter(total > 1)

#WHAT TO EXCLUDE
cand_2010 <- cand_1998_2014[[4]]
problems <- cand_2010 %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE, 
                                   DESCRICAO_ELEICAO) %>%
                          summarise(total = n()) %>% filter(total > 1)

casos <- cand_2010 %>% right_join(problems, by = c("NUM_TURNO", 
                                                   "NUMERO_CANDIDATO", "CODIGO_CARGO", "SIGLA_UE", 
                                                   "DESCRICAO_ELEICAO"))

repeated_casos <- casos[duplicated(casos),] #save
repeated_casos <- repeated_casos %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                                         NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                                         NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO)) #save

#But keeping only unique
casos <- unique(casos)
casos_notvalid <- casos %>% filter(!(COD_SITUACAO_CANDIDATURA == 2 | 
                                       COD_SITUACAO_CANDIDATURA == 4 |
                                       COD_SITUACAO_CANDIDATURA == 16 | 
                                       COD_SITUACAO_CANDIDATURA == 17 |
                                       COD_SITUACAO_CANDIDATURA == 18))

casos_notvalid <- casos_notvalid %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                                         NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                                         NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO)) #save

exclude <- c(repeated_casos$key, casos_notvalid$key)

cand_2010 <- cand_2010 %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                               NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                               NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO))

cand_2010v2 <- cand_2010 %>% filter(!(key %in% exclude))
cand_2010v2 <- cand_2010v2[,-ncol(cand_2010v2)]

save(cand_2010v2, file = paste0(dir, "cepesp_data/cand_2010v2.Rda"))
write.csv(cand_2010v2, file = paste0(dir, "cepesp_data/cand_2010v2.csv"))

#Smell test
temp <- cand_2010 %>% filter(key %in% exclude)
stopifnot((nrow(cand_2010) - nrow(cand_2010v2)) == nrow(temp))

#2012
problems <- cand_2012 %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE, 
                                   DESCRICAO_ELEICAO) %>%
                         summarise(total = n()) %>% filter(total > 1)

casos <- cand_2012 %>% right_join(problems, by = c("NUM_TURNO", 
                                                   "NUMERO_CANDIDATO", "CODIGO_CARGO", "SIGLA_UE", 
                                                   "DESCRICAO_ELEICAO"))

#Control for repeated
repeated_casos <- casos[duplicated(casos),]

#But keeping only unique
casos <- unique(casos)

casos <- casos %>% filter(COD_SITUACAO_CANDIDATURA == 2 | 
                            COD_SITUACAO_CANDIDATURA == 4 |
                            COD_SITUACAO_CANDIDATURA == 8 |
                            COD_SITUACAO_CANDIDATURA == 16 | 
                            COD_SITUACAO_CANDIDATURA == 17 |
                            COD_SITUACAO_CANDIDATURA == 18)

problems_caso <- casos %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE, 
                                    DESCRICAO_ELEICAO) %>%
                           summarise(total = n()) %>% filter(total > 1)

#WHAT TO EXCLUDE
cand_2012 <- cand_2000_2016[[4]]
problems <- cand_2012 %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE, 
                                   DESCRICAO_ELEICAO) %>%
  summarise(total = n()) %>% filter(total > 1)

casos <- cand_2012 %>% right_join(problems, by = c("NUM_TURNO", 
                                                   "NUMERO_CANDIDATO", "CODIGO_CARGO", "SIGLA_UE", 
                                                   "DESCRICAO_ELEICAO"))

repeated_casos <- casos[duplicated(casos),] #save
repeated_casos <- repeated_casos %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                                         NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                                         NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO)) #save

#But keeping only unique
casos <- unique(casos)
casos_notvalid <- casos %>% filter(!(COD_SITUACAO_CANDIDATURA == 2 | 
                                       COD_SITUACAO_CANDIDATURA == 4 |
                                       COD_SITUACAO_CANDIDATURA == 8 |
                                       COD_SITUACAO_CANDIDATURA == 16 | 
                                       COD_SITUACAO_CANDIDATURA == 17 |
                                       COD_SITUACAO_CANDIDATURA == 18))

casos_notvalid <- casos_notvalid %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                                         NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                                         NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO)) #save

exclude <- c(repeated_casos$key, casos_notvalid$key)

cand_2012 <- cand_2012 %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                               NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                               NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO))

cand_2012v2 <- cand_2012 %>% filter(!(key %in% exclude))
cand_2012v2 <- cand_2012v2[,-ncol(cand_2012v2)]

save(cand_2012v2, file = paste0(dir, "cepesp_data/cand_2012v2.Rda"))
write.csv(cand_2012v2, file = paste0(dir, "cepesp_data/cand_2012v2.csv"))

#Smell test
temp <- cand_2012 %>% filter(key %in% exclude)
stopifnot((nrow(cand_2012) - nrow(cand_2012v2)) == nrow(temp))

#2014
problems <- cand_2014 %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE, 
                                   DESCRICAO_ELEICAO) %>%
                          summarise(total = n()) %>% filter(total > 1)

casos <- cand_2014 %>% right_join(problems, by = c("NUM_TURNO", 
                                                   "NUMERO_CANDIDATO", "CODIGO_CARGO", "SIGLA_UE", 
                                                   "DESCRICAO_ELEICAO"))

#Control for repeated
repeated_casos <- casos[duplicated(casos),]

#But keeping only unique
casos <- unique(casos)

casos <- casos %>% filter(COD_SITUACAO_CANDIDATURA == 2 | 
                            COD_SITUACAO_CANDIDATURA == 4 |
                            COD_SITUACAO_CANDIDATURA == 16 | 
                            COD_SITUACAO_CANDIDATURA == 17)

problems_caso <- casos %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE, 
                                    DESCRICAO_ELEICAO) %>%
                           summarise(total = n()) %>% filter(total > 1)

#WHAT TO EXCLUDE
cand_2014 <- cand_1998_2014[[5]]
problems <- cand_2014 %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE, 
                                   DESCRICAO_ELEICAO) %>%
                          summarise(total = n()) %>% filter(total > 1)

casos <- cand_2014 %>% right_join(problems, by = c("NUM_TURNO", 
                                                   "NUMERO_CANDIDATO", "CODIGO_CARGO", "SIGLA_UE", 
                                                   "DESCRICAO_ELEICAO"))

repeated_casos <- casos[duplicated(casos),] #save
repeated_casos <- repeated_casos %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                                         NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                                         NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO)) #save

#But keeping only unique
casos <- unique(casos)
casos_notvalid <- casos %>% filter(!(COD_SITUACAO_CANDIDATURA == 2 | 
                                    COD_SITUACAO_CANDIDATURA == 4 |
                                    COD_SITUACAO_CANDIDATURA == 16 | 
                                    COD_SITUACAO_CANDIDATURA == 17))

casos_notvalid <- casos_notvalid %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                                         NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                                         NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO)) #save

exclude <- c(repeated_casos$key, casos_notvalid$key)

cand_2014 <- cand_2014 %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                               NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                               NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO))

cand_2014v2 <- cand_2014 %>% filter(!(key %in% exclude))
cand_2014v2 <- cand_2014v2[,-ncol(cand_2014v2)]

save(cand_2014v2, file = paste0(dir, "cepesp_data/cand_2014v2.Rda"))
write.csv(cand_2014v2, file = paste0(dir, "cepesp_data/cand_2014v2.csv"))

#Smell test
temp <- cand_2014 %>% filter(key %in% exclude)
stopifnot((nrow(cand_2014) - nrow(cand_2014v2)) == nrow(temp))

#2016
problems <- cand_2016 %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE, 
                                   DESCRICAO_ELEICAO) %>%
                          summarise(total = n()) %>% filter(total > 1)

casos <- cand_2016 %>% right_join(problems, by = c("NUM_TURNO", 
                                                   "NUMERO_CANDIDATO", "CODIGO_CARGO", "SIGLA_UE", 
                                                   "DESCRICAO_ELEICAO"))

#Control for repeated
repeated_casos <- casos[duplicated(casos),]

#But keeping only unique
casos <- unique(casos)

casos <- casos %>% filter(COD_SITUACAO_CANDIDATURA == 2 | 
                            COD_SITUACAO_CANDIDATURA == 4 |
                            COD_SITUACAO_CANDIDATURA == 8 |
                            COD_SITUACAO_CANDIDATURA == 16 | 
                            COD_SITUACAO_CANDIDATURA == 17 |
                            COD_SITUACAO_CANDIDATURA == 18 |
                            COD_SITUACAO_CANDIDATURA == 19)

problems_caso <- casos %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE, 
                                    DESCRICAO_ELEICAO) %>%
                           summarise(total = n()) %>% filter(total > 1)

#WHAT TO EXCLUDE
cand_2016 <- cand_2000_2016[[5]]
problems <- cand_2016 %>% group_by(NUM_TURNO, NUMERO_CANDIDATO, CODIGO_CARGO, SIGLA_UE, 
                                   DESCRICAO_ELEICAO) %>%
                          summarise(total = n()) %>% filter(total > 1)

casos <- cand_2016 %>% right_join(problems, by = c("NUM_TURNO", 
                                                   "NUMERO_CANDIDATO", "CODIGO_CARGO", "SIGLA_UE", 
                                                   "DESCRICAO_ELEICAO"))

repeated_casos <- casos[duplicated(casos),] #save
repeated_casos <- repeated_casos %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                                         NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                                         NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO)) #save

#But keeping only unique
casos <- unique(casos)
casos_notvalid <- casos %>% filter(!(COD_SITUACAO_CANDIDATURA == 2 | 
                                     COD_SITUACAO_CANDIDATURA == 4 |
                                     COD_SITUACAO_CANDIDATURA == 8 |
                                     COD_SITUACAO_CANDIDATURA == 16 | 
                                     COD_SITUACAO_CANDIDATURA == 17 |
                                     COD_SITUACAO_CANDIDATURA == 18 |
                                     COD_SITUACAO_CANDIDATURA == 19))

casos_notvalid <- casos_notvalid %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                                         NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                                         NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO)) #save

exclude <- c(repeated_casos$key, casos_notvalid$key)

cand_2016 <- cand_2016 %>% mutate(key = paste0(SIGLA_UE, NUM_TURNO, NOME_URNA_CANDIDATO, 
                                               NUMERO_CANDIDATO, DESCRICAO_ELEICAO, 
                                               NUM_TITULO_ELEITORAL_CANDIDATO, CODIGO_CARGO))

cand_2016v2 <- cand_2016 %>% filter(!(key %in% exclude))
cand_2016v2 <- cand_2016v2[,-ncol(cand_2016v2)]

save(cand_2016v2, file = paste0(dir, "cepesp_data/cand_2016v2.Rda"))
write.csv(cand_2016v2, file = paste0(dir, "cepesp_data/cand_2016v2.csv"))

#Smell test
temp <- cand_2016 %>% filter(key %in% exclude)
stopifnot((nrow(cand_2016) - nrow(cand_2016v2)) == nrow(temp))

###############################
#########Case studies
###############################

gov_goias <- cand_2014v2 %>% filter(CODIGO_CARGO == 3, NUMERO_CANDIDATO == 13, 
                                    SIGLA_UF == "GO")
roriz <- cand_2010v2 %>% filter(CODIGO_CARGO == 3, SIGLA_UF == "DF")

campos <- cand_2014v2 %>% filter(CODIGO_CARGO == 1, SIGLA_UF == "BR")

fruet <- cand_1998v2 %>% filter(CODIGO_CARGO == 6, SIGLA_UF == "PR", 
                                SIGLA_PARTIDO == "PMDB")

itamar <- cand_2010v2 %>% filter(CODIGO_CARGO == 5, SIGLA_UF == "MG")

quercia <- cand_2010v2 %>% filter(CODIGO_CARGO == 5, SIGLA_UF == "SP")

sta_maria <- cand_2012v2 %>% filter(CODIGO_CARGO == 11, SIGLA_UF == "PA", 
                                    SIGLA_UE == 5312)
