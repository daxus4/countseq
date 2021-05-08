#' Virtual class of object that can count overall number of sequences that match
#' in some regions
#'
#' You can choose to use two class that inherit from this virtual class. The
#' class CounterMatchesMat is a matrix that contain the overall number of 
#' sequences that match in the regions, it's easy and fast. When you have to use
#' less memory and you know that the matrix can be scattered, you can use
#' CounterMatchesList, which have a more complex internal structure and can be a
#' little slower, but it allows you to save space in memory.
#' 
#' @slot arranged boolean. It say whether the structure is sorted by match
#' number
#' @slot reduced boolean. It say whether the sequences and the regions with no
#' matches are deleted from the structure
#' 
#' @author Davide Raffaelli\cr Politecnico di Milano\cr Maintainer: Davide
#' Raffaelli\cr E-Mail: <davide2.raffaelli@@mail.polimi.it>
#'
setClass("CounterMatches",
         slots = list(arranged = "logical", reduced = "logical"),
         contains = c("VIRTUAL"))

#' Sort the object by sequences' overall count
#' 
#' This function sorts the object in decreasing or increasing order, by using as
#' metric the overall counts of matches for each sequence. You can also order
#' the regions
#' 
#' @usage sortSeqs(.object, decreasing, order.regions)
#' @param .object A CounterMatches object
#' @param decreasing logical. Should the sort order be increasing or decreasing?
#' @param order.regions logical. Should internal vector of counts per region
#' be ordered too?
#' @return object (same class of .object) ordered depending on the parameters
#' @author Davide Raffaelli\cr Politecnico di Milano\cr Maintainer: Davide
#' Raffaelli\cr E-Mail: <davide2.raffaelli@@mail.polimi.it>
#' @seealso \code{\link{sortSeqsList}}\cr
#' \code{\link{sortSeqsMatrix}}\cr
#' @examples
#' #' #Charge Hsapiens genome
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
#' #Get a CounterMatchesMat object
#' mat <- getCounterMatchesMat(BSgenome.Hsapiens.UCSC.hg38::Hsapiens, regs,
#'                             seqs)
#' #Sort mat
#' sortSeqs(mat, FALSE, TRUE)
#' 
#' @export
setGeneric("sortSeqs", function(.object, decreasing, order.regions) {
  standardGeneric("sortSeqs")
})

setMethod(f = "sortSeqs",
          signature = "CounterMatches",
          definition = function(.object, decreasing, order.regions) {
            #Check input
            if (!is.logical(decreasing)) {
              stop("decreasing must be logical!")
            }
            if (!is.logical(order.regions)) {
              stop("order.regions must be logical!")
            }
          }
)

#' Accessory method that return the object with the overall number of sequences
#' that match in some regions
#' 
#' @usage matches(.object)
#' @param .object A CounterMatches object
#' @return object (which class depends by class of .object) that represents the
#' overall number of sequences that match in some regions
#' @author Davide Raffaelli\cr Politecnico di Milano\cr Maintainer: Davide
#' Raffaelli\cr E-Mail: <davide2.raffaelli@@mail.polimi.it>
#' @examples
#' #' #Charge Hsapiens genome
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
#' #Get a CounterMatchesMat object
#' mat <- getCounterMatchesMat(BSgenome.Hsapiens.UCSC.hg38::Hsapiens, regs,
#'                             seqs)
#' #Get matches
#' matches(mat)
#' 
#' @export
setGeneric("matches", function(.object) {
  standardGeneric("matches")
})

setMethod(f = "matches",
          signature = "CounterMatches",
          definition = function(.object) {
            return(0)
          }
)


#' Accessory method that return if the object is sorted
#' 
#' @usage isArranged(.object)
#' @param .object A CounterMatches object
#' @return boolean indicating if .object is arranged
#' @author Davide Raffaelli\cr Politecnico di Milano\cr Maintainer: Davide
#' Raffaelli\cr E-Mail: <davide2.raffaelli@@mail.polimi.it>
#' @examples
#' #' #Charge Hsapiens genome
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
#' #Get a CounterMatchesMat object
#' mat <- getCounterMatchesMat(BSgenome.Hsapiens.UCSC.hg38::Hsapiens, regs,
#'                             seqs)
#' #is arranged?
#' isArranged(mat)
#' 
#' @export
setGeneric("isArranged", function(.object) {
  standardGeneric("isArranged")
})

setMethod(f = "isArranged",
          signature = "CounterMatches",
          definition = function(.object) {
            slot(.object, "arranged")
          }
)

#' Accessory method that return if the object is reduced, that is if there are 
#' only regions and sequences with one or more matches
#' 
#' @usage isReduced(.object)
#' @param .object A CounterMatches object
#' @return boolean indicating if .object is reduced
#' @author Davide Raffaelli\cr Politecnico di Milano\cr Maintainer: Davide
#' Raffaelli\cr E-Mail: <davide2.raffaelli@@mail.polimi.it>
#' @examples
#' #' #Charge Hsapiens genome
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
#' #Get a CounterMatchesMat object
#' mat <- getCounterMatchesMat(BSgenome.Hsapiens.UCSC.hg38::Hsapiens, regs,
#'                             seqs)
#' #is reduced?
#' isReduced(mat)
#' 
#' @export
setGeneric("isReduced", function(.object) {
  standardGeneric("isReduced")
})

setMethod(f = "isReduced",
          signature = "CounterMatches",
          definition = function(.object) {
            slot(.object, "reduced")
          }
)


#' Method that reduce the object
#' 
#' This function reduce, that is delete from the object the regions and the
#' sequences with 0 matches
#' 
#' @usage reduce(.object)
#' @param .object A CounterMatches object
#' @return object (same class of .object) reduced
#' @author Davide Raffaelli\cr Politecnico di Milano\cr Maintainer: Davide
#' Raffaelli\cr E-Mail: <davide2.raffaelli@@mail.polimi.it>
#' @examples
#' #' #Charge Hsapiens genome
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
#' #Get a CounterMatchesMat object
#' mat <- getCounterMatchesMat(BSgenome.Hsapiens.UCSC.hg38::Hsapiens, regs,
#'                             seqs)
#' #reduce mat
#' reduce(mat)
#' 
#' @export
setGeneric("reduce", function(.object) {
  standardGeneric("reduce")
})

setMethod(f = "reduce",
          signature = "CounterMatches",
          definition = function(.object) {
            NULL
          }
)

#' Accessory method that return sequences' names
#' 
#' @usage seqNames(.object)
#' @param .object A CounterMatches object
#' @return vector of characters that contains the sequences' names
#' @author Davide Raffaelli\cr Politecnico di Milano\cr Maintainer: Davide
#' Raffaelli\cr E-Mail: <davide2.raffaelli@@mail.polimi.it>
#' @examples
#' #' #Charge Hsapiens genome
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
#' #Get a CounterMatchesMat object
#' mat <- getCounterMatchesMat(BSgenome.Hsapiens.UCSC.hg38::Hsapiens, regs,
#'                             seqs)
#' #get sequences' name
#' seqNames(mat)
#' 
#' @export
setGeneric("seqNames", function(.object) {
  standardGeneric("seqNames")
})

setMethod(f = "seqNames",
          signature = "CounterMatches",
          definition = function(.object) {
            NULL
          }
)

#' Accessory method that return sequences' names
#' 
#' @usage regNames(.object)
#' @param .object A CounterMatches object
#' @return vector of characters that contains the regions' names
#' @author Davide Raffaelli\cr Politecnico di Milano\cr Maintainer: Davide
#' Raffaelli\cr E-Mail: <davide2.raffaelli@@mail.polimi.it>
#' @examples
#' #' #Charge Hsapiens genome
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
#' #Get a CounterMatchesMat object
#' mat <- getCounterMatchesMat(BSgenome.Hsapiens.UCSC.hg38::Hsapiens, regs,
#'                             seqs)
#' #get regions' name
#' regNames(mat)
#' 
#' @export
setGeneric("regNames", function(.object) {
  standardGeneric("regNames")
})

setMethod(f = "regNames",
          signature = "CounterMatches",
          definition = function(.object) {
            NULL
          }
)