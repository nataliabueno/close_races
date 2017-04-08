#Helper Functions

library(data.table)
library(purrr)

get_tse <- function(url_repo, file_d, file_un){
  
  
  message("Downloading data from TSE repository")
  download.file(url_repo, file_d, quiet = T)
  message("Data has been downloaded from TSE repository")
  
  unzip(file_name, exdir = file_un)

}

