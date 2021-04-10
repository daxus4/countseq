#Virtual class of object that can count overall number of sequences that match
#in some regions
setClass("CounterMatches", contains = c("VIRTUAL"))

#Method that counts overall number of sequences that match in some regions 
setGeneric("countSeq", function(.object, genome, regions, sequences) {
  standardGeneric("countSeq")
})

setMethod(f = "countSeq",
          signature = "CounterMatches",
          definition = function(.object, genome, regions, sequences) {
          }
)

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