#' Count reads mapped on multiple regions of a genome
#'
#' This function returns an overall count for each search sequence within the
#' interesting genomic regions. Also overlapping sequences.
#'
#' @usage countSeqs(genome, regions, sequences)
#' @param genome A genome of BSgenome class
#' @param regions A GRanges which specify the interesting regions
#' @param sequences A DNAStringSet which contains the sequences to be matched
#' within the regions
#' @return Matrix of integers, the columns are the sequences, the rows are the
#' regions and the cells the overall counts
#' @author Davide Raffaelli\cr Politecnico di Milano\cr Maintainer: Davide
#' Raffarelli\cr E-Mail: <davide2.raffaelli@@mail.polimi.it>
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
  #Extract sequence from genome's regions and calculate matrix with return
  #values
  dnaSet <- BSgenome::getSeq(genome, regions)
  mat <- vapply(sequences, BSgenome::vcountPattern, numeric(2), dnaSet)

  #Give the sequences' names to the columns
  if(is.null(names(sequences))) {
    colnames(mat) <- lapply(seq(1:length(sequences)),
                            function(i) paste0("seq",i))
  } else {
    colnames(mat) <- names(sequences)
  }

  #Give the regions' names to the rows
  if(is.null(GenomicRanges::seqnames(regions))) {
    rownames(mat) <- lapply(seq(1:length(regions)),
                          function(i) paste0("reg",i))
  } else {
    rownames(mat) <- paste(as.vector(GenomicRanges::seqnames(regions)),
                           GenomicRanges::start(regions), sep = ":")
  }
  return(mat)
}
