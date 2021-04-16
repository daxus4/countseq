#Virtual class of object that can count overall number of sequences that match
#in some regions
setClass("CounterMatches", slots = list(arranged = "logical"), contains = c("VIRTUAL"))

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

