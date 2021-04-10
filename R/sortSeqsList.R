#' Sort list returned from countSeqScattered
#'
#' This function sort list returned from countSeqScattered in decreasing or
#' increasing order, by using as metric the overall counts of matches for each 
#' sequence. To order the matrix returned from countSeq please use 
#' sortSeqsMatrix
#'
#' @usage sortSeqsList(seqsList, decreasing, order.regions)
#' @param seqsList A list returned from countSeqScattered
#' @param decreasing logical. Should the sort order be increasing or decreasing?
#' @param order.regions logical. Should internal vector of counts per region
#' be ordered too?
#' @return input list ordered depending on the parameters
#' @author Davide Raffaelli\cr Politecnico di Milano\cr Maintainer: Davide
#' Raffaelli\cr E-Mail: <davide2.raffaelli@@mail.polimi.it>
#' @seealso \code{\link{countSeqScattered}}\cr
#' \code{\link{sortSeqsMatrix}}\cr
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
#' #Get the list of matches
#' listSeqs <- countSeqsScattered(BSgenome.Hsapiens.UCSC.hg38::Hsapiens,
#'   regs, seqs)
#'   
#' #Order list of matches
#' sortSeqsList(listSeqs, FALSE, TRUE)
#'
#' @export
sortSeqsList <- function(seqsList, decreasing, order.regions) {
  #Check input
  if (!is.logical(decreasing)) {
    stop("decreasing must be logical!")
  }
  if (!is.logical(order.regions)) {
    stop("order.regions must be logical!")
  }
  
  #Order sequences by number of matches
  seqsList <- seqsList[order(vapply(seqsList, sum, numeric(1)), decreasing = decreasing)]
  
  #If order.regions then sort internal vectors of count per region
  if(order.regions){
    seqsList <- lapply(seqsList, sort, decreasing)
  }
  
  return(seqsList)
}