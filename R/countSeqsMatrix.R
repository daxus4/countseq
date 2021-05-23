#' Count reads mapped on multiple regions of a genome
#'
#' This function returns an overall count for each search sequence within the
#' interesting genomic regions. In the count are counted also overlapping
#' sequences.
#'
#' @usage countSeqsMatrix(genome, regions, sequences, reduced = FALSE,
#' ordered = FALSE)
#' @param genome A genome which inherits from \link{BSgenome} class
#' @param regions A \link{GRanges} which specify the interesting regions
#' @param sequences A \link{DNAStringSet} or a vector of characters which
#' contains the sequences to be matched within the regions
#' @param reduced boolean. It specify if the columns and rows full of zeros have
#' to be deleted
#' @param ordered boolean. It specify if the sequences should be ordered in
#' descreasing order. If you want a different order options please use
#' \link{\code{sortSeqsMatrix}}
#' @return Matrix of integers, the columns are the sequences, the rows are the
#' regions and the cells the overall counts of matches
#' @author Davide Raffaelli\cr Politecnico di Milano\cr Maintainer: Davide
#' Raffaelli\cr E-Mail:
#' <davide2.raffaelli@@mail.polimi.it>
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
#' countSeqsMatrix(BSgenome.Hsapiens.UCSC.hg38::Hsapiens, regs, seqs)
#'
#' @importFrom BSgenome getSeq
#' @importFrom BSgenome vcountPattern
#' @importFrom GenomicRanges seqnames start
#' @importFrom Biostrings DNAStringSet
#' @export
countSeqsMatrix <- function(genome, regions, sequences, reduced = FALSE,
                            ordered = FALSE) {
  if(!inherits(genome, "BSgenome")){
    stop("genome must inherits fom BSgenome")
  }
  if(!inherits(regions, "GRanges")){
    stop("regions must inherits fom GRanges")
  }
  if(!inherits(sequences, "DNAStringSet")){
    #If sequences is a vector of strings convert it in a DNAStringSet
    if(is.character(sequences)){
      sequences <- DNAStringSet(sequences)
    } else {
      stop("sequences must inherits fom DNAStringSet or be a vector of strings")
    }
  }
  if(!is.logical(reduced)) {
    stop("reduced must be logical")
  }
  if(!is.logical(ordered)) {
    stop("ordered must be logical")
  }

  #Extract sequence from genome's regions and calculate matrix with return
  #values
  dnaSet <- BSgenome::getSeq(genome, regions)
  mat <- vapply(sequences, BSgenome::vcountPattern, numeric(length(regions)),
                dnaSet)

  #Give the sequences' names to the columns
  if(is.null(names(sequences))) {
    colnames(mat) <- colnames(mat) <- lapply(sequences, function(seq) {
      ifelse(length(seq)<=10,
             as.character(seq),
             as.character(seq[1:10]))})
  } else {
    colnames(mat) <- names(sequences)
  }

  #Give the regions' names to the rows
  rownames(mat) <- paste(as.vector(GenomicRanges::seqnames(regions)),
                         GenomicRanges::start(regions), sep = ":")

  if(reduced) {
    #Delete rows and coloums with all zeros
    mat <- mat[rowSums(mat) > 0,]
    mat <- mat[, colSums(mat) > 0]
  }

  if(ordered) {
    mat <- sortSeqsMatrix(mat, TRUE)
  }
  return(mat)
  return(mat)
}
