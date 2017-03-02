#Creating age age variable (FIX AGE)
#cand_2008v3 <- cand_2008v3 %>% mutate(data_nasc = as.Date(DATA_NASCIMENTO, format = '%d/%b/%y'))

#age_years = age_calc(data_nasc, units="years"),
#age_years2 = ifelse(age_years < 18 | age_years > 100, NA, age_years))
#Age variable requires manual fixing (as there dates inputted wrong, such as 2005 asn 0049 for date of births)
#For now, assign NA to smaller than 18 and greater than 100
#This is okay for now, age is used to break ties in tied elections

nrow(vot_2008_runoffv3) == length(unique(vot_2008_runoffv3$SEQUENCIAL_CANDIDATO))
nrow(cand_2008_runoff) == length(unique(cand_2008_runoff$SEQUENCIAL_CANDIDATO))

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
max(temp0$vote_margin_share)