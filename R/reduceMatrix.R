#' Delete rows and/or columns of matrix of numerics full of zeros
#'
#' This function delete rows and/or columns of matrix of numerics full of zeros.
#' It is useful for \link{countseq} package, because it allows to delete regions
#' and sequences with no match in the matrix returned from
#' \code{\link{countSeqsMatrix}} function.
#'
#' @usage reduceMatrix(mat, rows = TRUE, cols = TRUE)
#' @param mat matrix of numerics
#' @param rows logical. Should rows full of zero be deleted?
#' @param cols logical. Should columns full of zero be deleted?
#' @return input matrix without rows/columns full of zeros
#' @author Davide Raffaelli\cr Politecnico di Milano\cr Maintainer: Davide
#' Raffaelli\cr E-Mail:
#' <davide.raffaellii@@gmail.com>
#' @seealso \code{\link{countSeqsMatrix}}\cr
#' @examples
#'
#' #create matrix of numerics
#' mat <- matrix(c(6,4,0,0), nrow=2)
#'
#' #Order matrix of matches
#' reduceMatrix(mat)
#' @export
reduceMatrix <- function(mat, rows = TRUE, cols = TRUE) {
    #Check if mat is a matrix of numerics
    if(!is.matrix(mat))
        stop("This function require that mat is a matrix of numerics")
    if(!is.numeric(mat))
        stop("This function require that mat is a matrix of numerics")

    #Check logical input
    if (!is.logical(rows))
        stop("rows must be logical!")
    if (!is.logical(cols))
        stop("cols must be logical!")

    #Delete rows full of zeros
    if(rows)
        mat <- mat[rowSums(mat) > 0,,drop = FALSE]
    #Delete columns full of zeros
    if(cols)
        mat <- mat[, colSums(mat) > 0, drop = FALSE]

    return(mat)
}
