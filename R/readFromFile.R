readFromFile <- function(filename){
  if(!is.character(filename)){
    stop("filename must be a character")
  }
  return(read.table(filename, header = TRUE, sep = " ", row.names = 1))
}