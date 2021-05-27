#' Write to a file a matrix of numerics
#'
#' This function write to a file a matrix of numerics, with rownames and
#' colnames. It is useful for \link{countseq} package, because it allows to
#' order the matrix returned from \code{\link{countSeqsMatrix}} function.
#'
#' @usage writeToFile(matrix, filename)
#' @param matrix A matrix returned from \code{\link{countSeqsMatrix}} or any
#' matrix which contains only numerics, with rownames and colnames
#' @param filename string. Name of the file where the matrix will be saved
#' @return TRUE if the file is created, FALSE otherwise
#' @author Davide Raffaelli\cr Politecnico di Milano\cr Maintainer: Davide
#' Raffaelli\cr E-Mail:
#' <davide.raffaellii@@gmail.com>
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
#' @importFrom utils write.table
#' @export
writeToFile <- function(matrix, filename){
    #Check if matrix is a numeric matrix
    if(!is.matrix(matrix))
        stop("This function require that matrix is a matrix of numerics")
    if(!is.numeric(matrix))
        stop("This function require that matrix is a matrix of numerics")

    #Check if matrix has got rownames and colnames
    if(length(rownames(matrix)) != nrow(matrix) |
        length(colnames(matrix)) != ncol(matrix)){
        stop("Every row/column must have a name")
    }

    #Check if filename is a string
    if(!is.character(filename))
        stop("filename must be a character")

    #Write matrix into file
    utils::write.table(matrix, file = filename, row.names = TRUE,
                        col.names = TRUE)

    return(file.exists(filename))
}
