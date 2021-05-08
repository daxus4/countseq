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

#Method that reorder the object by the overall number of sequences that match
#in some regions
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

#Accessory method that return the object with the overall number of sequences
#that match in some regions
setGeneric("matches", function(.object) {
  standardGeneric("matches")
})

setMethod(f = "matches",
          signature = "CounterMatches",
          definition = function(.object) {
            return(0)
          }
)

#Accessory method that return if the object is sorted
setGeneric("isArranged", function(.object) {
  standardGeneric("isArranged")
})

setMethod(f = "isArranged",
          signature = "CounterMatches",
          definition = function(.object) {
            slot(.object, "arranged")
          }
)

#Accessory method that return if the object is reduced
setGeneric("isReduced", function(.object) {
  standardGeneric("isReduced")
})

setMethod(f = "isReduced",
          signature = "CounterMatches",
          definition = function(.object) {
            slot(.object, "reduced")
          }
)

#Accessory method that reduce the object
setGeneric("reduce", function(.object) {
  standardGeneric("reduce")
})

setMethod(f = "reduce",
          signature = "CounterMatches",
          definition = function(.object) {
            NULL
          }
)

#Accessory method that return sequences' names
setGeneric("seqNames", function(.object) {
  standardGeneric("seqNames")
})

setMethod(f = "seqNames",
          signature = "CounterMatches",
          definition = function(.object) {
            NULL
          }
)

#Accessory method that return regions' names
setGeneric("regNames", function(.object) {
  standardGeneric("regNames")
})

setMethod(f = "regNames",
          signature = "CounterMatches",
          definition = function(.object) {
            NULL
          }
)