#' Write to a file a matrix obtained from \code{\link{countSeqsMatrix}}
#'
#' This function write to a file a matrix obtained from
#' \code{\link{countSeqsMatrix}} or any other matrix which contains only
#' integers and with rownames and colnames
#'
#' @usage writeToFile(matrix, filename)
#' @param matrix A matrix returned from \code{\link{countSeqsMatrix}} or any
#' matrix which contains only integers and with rownames and colnames
#' @param filename string. Name of the file where the matrix will be saved
#' @return TRUE if the file is created, FALSE otherwise
#' @author Davide Raffaelli\cr Politecnico di Milano\cr Maintainer: Davide
#' Raffaelli\cr E-Mail:
#' <davide2.raffaelli@@mail.polimi.it>
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
    if(!is.matrix(matrix))
        stop("This function require that matrix is a matrix of integers")
    if(!all(apply(matrix, 1 , function(row) {all(is.numeric(row))})))
        stop("This function require that matrix is a matrix of integers")
    if(any(matrix%%1 != 0))
        stop("This function require that matrix is a matrix of integers")

    if(length(rownames(matrix)) != nrow(matrix) |
        length(colnames(matrix)) != ncol(matrix)){
        stop("Every row/column must have a name")
    }
    if(!is.character(filename)){
        stop("filename must be a character")
    }
    utils::write.table(matrix, file = filename, row.names = TRUE,
                        col.names = TRUE)
    return(file.exists(filename))
}
