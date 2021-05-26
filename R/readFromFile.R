#' Read from a file a matrix of numerics, with rownames and colnames
#'
#' This function read from a file a numerical matrix, with rownames and
#' colnames. It is useful for \link{countseq} package, because it allows to
#' load a matrix obtained from \link{\code{countSeqsMatrix}} function and stored
#' in a file, by specifying with the parameter integers that this matrix have to
#' contain only integers
#'
#' @usage readFromFile(filename, integers = FALSE)
#' @param filename string. Name of the file where the matrix is saved
#' @param integers boolean. Have this matrix to contain only integers?
#' @return matrix of integers, with rownames and colnames
#' @author Davide Raffaelli\cr Politecnico di Milano\cr Maintainer: Davide
#' Raffaelli\cr E-Mail:
#' <davide2.raffaelli@@mail.polimi.it>
#' @examples
#'
#' #Get a matrix of integers with rownames and colnames from matrix.txt
#' readFromFile(system.file("extdata", "matrix.txt", package = "countseq"),
#'     integers=TRUE)
#'
#' @importFrom utils read.table
#' @export
readFromFile <- function(filename, integers = FALSE){
    #Check if filename is a string
    if(!is.character(filename))
        stop("filename must be a character")

    table <- utils::read.table(filename, header = TRUE, sep = " ",
                               row.names = 1)

    #Check if matrix is numeric
    if(!all(apply(table, 12, is.numeric)))
        stop("This file doesn't contain a matrix of integers")

    #If integers is true, check if matrix contains only integers
    if(integers){
        if(any(table%%1 != 0))
            stop("This file doesn't contain a matrix of integers")
    }

    return(as.matrix(table))
}
