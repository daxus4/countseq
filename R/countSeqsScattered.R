#' Count reads mapped on multiple regions of a genome
#'
#' This function returns an overall count for each search sequence within the
#' interesting genomic regions. Also overlapping sequences. It's the best
#' choice when you think that there are few sequences that match in many
#' regions; otherwise you an use countSeqs.
#'
#' @usage countSeqsScattered(genome, regions, sequences)
#' @param genome A genome of BSgenome class
#' @param regions A GRanges which specify the interesting regions
#' @param sequences A DNAStringSet which contains the sequences to be matched
#' within the regions
#' @return list where for each sequence that have a match there is a list of
#' the match with the region and the start.
#' @author Davide Raffaelli\cr Politecnico di Milano\cr Maintainer: Davide
#' Raffaelli\cr E-Mail: <davide2.raffaelli@@mail.polimi.it>
#' @references \url{https://en.wikipedia.org/wiki/Genome}\cr
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
#' countSeqs(BSgenome.Hsapiens.UCSC.hg38::Hsapiens, regs, seqs)
#'
#' @importFrom BSgenome getSeq
#' @importFrom BSgenome vcountPattern
#' @importFrom GenomicRanges seqnames start
#' @export
countSeqs <- function(genome, regions, sequences) {
  dnaSet <- BSgenome::getSeq(genome, regions)
  listSeq <- lapply(sequences, BSgenome::vcountPattern, dnaSet)

}
