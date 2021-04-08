#' Sort matrix returned from countSeq
#'
#' This function sort matrix returned from countSeq in decreasing or
#' increasing order, by using as metric the overall counts of matches for each 
#' sequence. To order the list returned from countSeqScattered please use 
#' sortSeqsMatrix
#'
#' @usage sortSeqsMatrix(seqsMat, decreasing, order.regions)
#' @param seqsMat A matrix returned from countSeq
#' @param decreasing logical. Should the sort order be increasing or decreasing?
#' @param order.regions logical. Should regions be ordered too?
#' @return input matrix ordered depending on the parameters
#' @author Davide Raffaelli\cr Politecnico di Milano\cr Maintainer: Davide
#' Raffaelli\cr E-Mail: <davide2.raffaelli@@mail.polimi.it>
#' @seealso \code{\link{countSeq}}\cr
#' \code{\link{sortSeqsList}}\cr
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
#' matSeqs <- countSeqs(BSgenome.Hsapiens.UCSC.hg38::Hsapiens,
#'   regs, seqs)
#'   
#' #Order matrix of matches
#' sortSeqsMatrix(matSeqs, FALSE, TRUE)
#'
#' @export
sortSeqsMatrix <- function(seqsMat, decreasing, order.regions) {
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
    seqsMat <- seqsMat[order(apply(seqsMat, 1, sum), decreasing = decreasing),]
  }
  
  return(seqsMat)
}