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

library(tidyverse)

#donwloading TSE functions
download_tse <-function(url, type, year, dfolder){    
  try(download.file(url=paste0(url, type, year),
                    destfile=paste0(dfolder, type, year), 
                    cacheOK=F))
  
  try(unzip(zipfile=paste0(dfolder, "original_data/", type, year),
            exdir=paste0(dfolder, "original_unzipped/", type, year, "/")))
}


#Downloading candidate data for 2000, 2004, 2008, 2012, 2016
url <- "http://agencia.tse.jus.br/estatistica/sead/odsele/"
types <- c("consulta_cand/consulta_cand_", "votacao_candidato_munzona/votacao_candidato_munzona_")
years <- paste0(c(2000, 2004, 2008, 2012), ".zip") 
type <- types[1]
year <- years[1]
dfolder <- "~/Dropbox/TSE_LOCAL_ELECTIONS/repositorio_data/"
download_tse(url=url, type=type, year=year, dfolder=dfolder)

#Download resultado data for 2000, 2004, 2008, 2012, 2016

#Download prestacoes de contas data for 2000, 2004, 2008, 2012, 2016