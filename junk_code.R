#Creating age age variable (FIX AGE)
#cand_2008v3 <- cand_2008v3 %>% mutate(data_nasc = as.Date(DATA_NASCIMENTO, format = '%d/%b/%y'))

#age_years = age_calc(data_nasc, units="years"),
#age_years2 = ifelse(age_years < 18 | age_years > 100, NA, age_years))
#Age variable requires manual fixing (as there dates inputted wrong, such as 2005 asn 0049 for date of births)
#For now, assign NA to smaller than 18 and greater than 100
#This is okay for now, age is used to break ties in tied elections

nrow(vot_2008_runoffv3) == length(unique(vot_2008_runoffv3$SEQUENCIAL_CANDIDATO))
nrow(cand_2008_runoff) == length(unique(cand_2008_runoff$SEQUENCIAL_CANDIDATO))