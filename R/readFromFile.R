#' Read from a file a matrix of integers, with rownames and colnames
#'
#' This function read from a file a matrix obtained from 
#' \code{\link{countSeqsMatrix}} and written into it or any other matrix which 
#' contains only integers and with rownames and colnames
#'
#' @usage readFromFile(filename)
#' @param filename string. Name of the file where the matrix is saved
#' @return matrix of integers, with rownames and colnames
#' @author Davide Raffaelli\cr Politecnico di Milano\cr Maintainer: Davide
#' Raffaelli\cr E-Mail: <davide2.raffaelli@@mail.polimi.it>
#' @examples
#'
#' #Get a matrix of integers with rownames and colnames
#' mat <- matrix(data = c(1,2,3,4), ncol = 2)
#' rownames(mat) <- c('1','2')
#' colnames(mat) <- c('1','2')
#' 
#' #Write mat to a file
#' writeToFile(mat, "matrix.txt")
#'
#' @export
readFromFile <- function(filename){
  if(!is.character(filename)){
    stop("filename must be a character")
  }
  mat <- read.table(filename, header = TRUE, sep = " ", row.names = 1)
  if(!is.numeric(mat) | any(mat%%1) != 0) {
    stop("This file doesn't contain a matrix of integers")
  }
  if(length(rownames(mat)) != nrow(mat) | 
     length(colnames(mat)) != ncol(mat)){
    stop("This file doesn't contain a matrix with rownames and colnames")
  }
  return(mat)
}