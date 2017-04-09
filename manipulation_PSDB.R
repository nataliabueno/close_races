#### Manipulation PSDB

library(rdd)
library(tidyverse)

load("~/Dropbox/LOCAL_ELECTIONS/repositorio_data/final_data/electionsff_2000_2012.Rda")

psdb <- electionsff %>% filter(SIGLA_PARTIDO == "PSDB")
psdb.mg <- electionsff %>% filter(SIGLA_PARTIDO == "PSDB", SIGLA_UF == "MG")
psdb.go <- electionsff %>% filter(SIGLA_PARTIDO == "PSDB", SIGLA_UF == "GO")
psdb.sp <- electionsff %>% filter(SIGLA_PARTIDO == "PSDB", SIGLA_UF == "SP")

DCdensity(psdb$vote_margin_share)
DCdensity(psdb.mg$vote_margin_share)
DCdensity(psdb.go$vote_margin_share)
DCdensity(psdb.sp$vote_margin_share)

