
load_file_to_DF <- function(fName){
  path = paste0("./data/", fName)
  dfName = read.csv(path,
                      header = TRUE,
                      stringsAsFactors = FALSE)
}






