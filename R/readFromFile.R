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
#' Raffaelli\cr E-Mail:
#' <davide2.raffaelli@@mail.polimi.it>
#' @examples
#'
#' #Get a matrix of integers with rownames and colnames from matrix.txt
#' readFromFile(system.file("extdata", "matrix.txt", package = "countseq"))
#'
#' @importFrom utils read.table
#' @export
readFromFile <- function(filename){
    if(!is.character(filename)){
        stop("filename must be a character")
    }
    mat <- utils::read.table(filename, header = TRUE, sep = " ", row.names = 1)

    if(!all(apply(mat, 1 , function(row) {all(grepl('^-?[0-9.]+$', row))})))
        stop("This file doesn't contain a matrix of integers")
    if(any(mat%%1 != 0))
        stop("This file doesn't contain a matrix of integers")

    return(as.matrix(mat))
}
