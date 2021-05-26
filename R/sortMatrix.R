#' Sort columns of matrix of numerics
#'
#' This function sort columns of matrix of numerics. It is useful for
#' \link{countseq} package, because it allows to order the matrix returned from
#' \code{\link{countSeqsMatrix}} function.
#'
#' @usage sortMatrix(mat, decreasing, order.rows = FALSE)
#' @param mat matrix of numerics
#' @param decreasing logical. Should the sort order be decreasing?
#' @param order.rows logical. Should rows be ordered too?
#' @return input matrix ordered depending on the parameters
#' @author Davide Raffaelli\cr Politecnico di Milano\cr Maintainer: Davide
#' Raffaelli\cr E-Mail:
#' <davide2.raffaelli@@mail.polimi.it>
#' @seealso \code{\link{countSeqsMatrix}}\cr
#' @examples
#'
#' #create matrix of numerics
#' mat <- matrix(c(6,4,0,7), nrow=2)
#'
#' #Order matrix of matches
#' sortMatrix(mat, FALSE, TRUE)
#' @export
sortMatrix <- function(mat, decreasing, order.rows = FALSE) {
    #Check if mat is a matrix of numerics
    if(!is.matrix(mat))
        stop("This function require that mat is a matrix of numerics")
    if(!all(apply(mat, 1 , function(row) {all(is.numeric(row))})))
        stop("This function require that mat is a matrix of numerics")

    #Check logical input
    if (!is.logical(decreasing))
        stop("decreasing must be logical!")
    if (!is.logical(order.rows))
        stop("order.rows must be logical!")


    #Order sequences (columns) by overall count
    mat <- mat[,order(apply(mat, 2, sum), decreasing = decreasing)]

    #If order.rows order the regions (rows)
    if(order.rows)
        mat <- mat[order(apply(mat, 1, sum), decreasing = decreasing),]

    return(mat)
}
