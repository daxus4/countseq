#' Sort matrix columns (sequences) of matrix returned from countSeqsMatrix
#'
#' This function sort matrix returned from countSeqsMatrix in decreasing or
#' increasing order, by using as metric the overall counts of matches for each
#' sequence.
#'
#' @usage sortSeqsMatrix(seqsMat, decreasing, order.regions = FALSE)
#' @param seqsMat A matrix returned from countSeqsMatrix
#' @param decreasing logical. Should the sort order be decreasing?
#' @param order.regions logical. Should regions be ordered too?
#' @return input matrix ordered depending on the parameters
#' @author Davide Raffaelli\cr Politecnico di Milano\cr Maintainer: Davide
#' Raffaelli\cr E-Mail:
#' <davide2.raffaelli@@mail.polimi.it>
#' @seealso \code{\link{countSeqsMatrix}}\cr
#' @examples
#'
#' #Charge Hsapiens genome
#' library(BSgenome.Hsapiens.UCSC.hg38)
#'
#' #Get a GRanges object which contains regions of interest
#' library(GenomicRanges)
#' regs <-GRanges("chr1", IRanges(1e6 + c(1,101), width=100))
#'
#' #Get a DNAStringSet which contains the sequences to be mapped
#' library(Biostrings)
#' seqs <- DNAStringSet(c("AA", "AT", "GG"))
#'
#' #Get the matrix of matches
#' matSeqs <- countSeqsMatrix(BSgenome.Hsapiens.UCSC.hg38::Hsapiens,
#'                             regs, seqs)
#'
#' #Order matrix of matches
#' sortSeqsMatrix(matSeqs, FALSE, TRUE)
#'
#' @importFrom BSgenome getSeq
#' @importFrom BSgenome vcountPattern
#' @importFrom GenomicRanges seqnames start
#' @export
sortSeqsMatrix <- function(seqsMat, decreasing, order.regions = FALSE) {
    #Check input
    if (!is.logical(decreasing)) {
        stop("decreasing must be logical!")
    }
    if (!is.logical(order.regions)) {
        stop("order.regions must be logical!")
    }

    #Order sequences (columns) by overall count
    seqsMat <- seqsMat[,order(apply(seqsMat, 2, sum), decreasing = decreasing)]

    #If order.regions order the regions (rows)
    if(order.regions) {
        seqsMat <-
            seqsMat[order(apply(seqsMat, 1, sum), decreasing = decreasing),]
    }

    return(seqsMat)
}
