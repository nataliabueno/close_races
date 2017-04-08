#Helper Functions

library(data.table)
library(purrr)

dir_d <- "~/Dropbox/LOCAL_ELECTIONS/repositorio_data/"
file_d <- paste0(dir_d, "original_data/consulta_cand/consulta_cand_2000.zip")
file_un <- paste0(dir_d, "original_unzipped/consulta_cand/consulta_cand_2000v2/")

get_tse <- function(url_repo, file_d, file_un){
  
  
  message("Downloading data from TSE repository")
  download.file(url_repo, file_name, quiet = T)
  message("Data has been downloaded from TSE repository")
  
  unzip(file_name, exdir = file_un)
  
  file_names_vec <- file.path(temp_dir, grep(".txt", list.files(temp_dir), value = T))
  
  safe_fread <- safely(fread)
  
  file_names_vec %>% 
    map(function(x) safe_fread(x, encoding = "Latin-1", header = F)) %>%
    map("result") %>%
    reduce(bind_rows)
}

