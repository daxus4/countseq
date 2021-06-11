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
#' to be deleted. If you want a different reduce options please use
#' \code{\link{reduceMatrix}}
#' @param ordered boolean. It specify if the sequences should be ordered in
#' descreasing order. If you want a different order options please use
#' \code{\link{sortMatrix}}
#' @return Matrix of integers, the columns are the sequences, the rows are the
#' regions and the cells the overall counts of matches
#' @author Davide Raffaelli\cr Politecnico di Milano\cr Maintainer: Davide
#' Raffaelli\cr E-Mail:
#' <davide.raffaellii@@gmail.com>
#' @examples
#'
#' #Check if there are CpG island in gene's promoters of chromosome 22 in human
#' #Charge Hsapiens genome
#' library(BSgenome.Hsapiens.UCSC.hg38)
#'
#' #Charge file where are stored genomic positions of TSS in chr22
#' df <- read.table(system.file("extdata", "chr22_unique_txStart.txt",
#'     package = "countseq"), header = TRUE)
#' 
#' #Get a GRanges object which contains regions of CpG island
#' library(GenomicRanges)
#' regs <-GRanges("chr22", 
#'     IRanges(start = df$txStart -130, end = df$txStart -30))
#'
#' #Get a DNAStringSet which contains the sequences to be mapped
#' library(Biostrings)
#' seqs <- DNAStringSet(c("CG", "AT", "TA"))
#'
#' countSeqsMatrix(BSgenome.Hsapiens.UCSC.hg38::Hsapiens, regs, seqs)
#'
#' @importFrom BSgenome getSeq
#' @importFrom BSgenome vcountPattern
#' @importFrom GenomicRanges seqnames start
#' @importFrom Biostrings DNAStringSet
#' @export
countSeqsMatrix <- function(genome, regions, sequences,
                            reduced = FALSE, ordered = FALSE) {
    #Check parameters type
    if(!inherits(genome, "BSgenome"))
        stop("genome must inherits fom BSgenome")
    if(!inherits(regions, "GRanges"))
        stop("regions must inherits fom GRanges")
    if(!inherits(sequences, "DNAStringSet")){
        #If sequences is a vector of strings convert it in a DNAStringSet
        if(is.character(sequences))
            sequences <- DNAStringSet(sequences)
        else
            stop(paste0("sequences must inherits fom DNAStringSet or be ",
                    "a vector of strings"))
    }

    #Check logical parameters type
    if(!is.logical(reduced))
        stop("reduced must be logical")
    if(!is.logical(ordered))
        stop("ordered must be logical")

    #Extract sequence from genome's regions and calculate matrix with return
    #values
    dnaSet <- BSgenome::getSeq(genome, regions)
    mat <- vapply(sequences, BSgenome::vcountPattern, numeric(length(regions)),
                    dnaSet)

    #Give the sequences' names to the columns
    if(is.null(names(sequences))) {
        colnames(mat) <- colnames(mat) <- lapply(sequences, function(seq) {
            ifelse(length(seq)<=10,
                as.character(seq), as.character(seq[seq_len(10)]))})
    } else
        colnames(mat) <- names(sequences)

    #Give the regions' names to the rows
    rownames(mat) <- paste(as.vector(GenomicRanges::seqnames(regions)),
                            GenomicRanges::start(regions), sep = ":")

    #Delete rows and coloums with all zeros
    if(reduced)
        mat <- reduceMatrix(mat)
    #Order sequences
    if(ordered)
        mat <- sortMatrix(mat, TRUE)

    return(mat)
}
