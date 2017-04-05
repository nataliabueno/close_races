#Helper Functions

library(data.table)
library(purrr)

get_tse <- function(url_repo){
  
  temp_dir <- tempdir()
  temp_file <- tempfile()
  
  message("Downloading data from TSE repository")
  download.file(url_repo, temp_file, quiet = T)
  message("Data has been downloaded from TSE repository")
  
  unzip(temp_file, exdir = temp_dir)
  
  file_names_vec <- file.path(temp_dir, grep(".txt", list.files(temp_dir), value = T))
  
  safe_fread <- safely(fread)
  
  file_names_vec %>% 
    map(function(x) safe_fread(x, encoding = "Latin-1", header = F)) %>%
    map("result") %>%
    reduce(bind_rows)
}

