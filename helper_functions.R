#Helper Functions

library(data.table)
library(purrr)

dir_d <- "~/Dropbox/LOCAL_ELECTIONS/repositorio_data/original_data/consulta_cand/"
file_name <- paste0(dir_d, "cand_2000v2.zip")

get_tse <- function(url_repo, file_name, unzip_file){
  
  
  message("Downloading data from TSE repository")
  download.file(url_repo, file_name, quiet = T)
  message("Data has been downloaded from TSE repository")
  
  unzip(temp_file, exdir = temp_dir)
  
  file_names_vec <- file.path(temp_dir, grep(".txt", list.files(temp_dir), value = T))
  
  safe_fread <- safely(fread)
  
  file_names_vec %>% 
    map(function(x) safe_fread(x, encoding = "Latin-1", header = F)) %>%
    map("result") %>%
    reduce(bind_rows)
}

