#Virtual class of object that can count overall number of sequences that match
#in some regions
setClass("CounterMatches", slots = list(arranged = "logical", reduced = "logical"), contains = c("VIRTUAL"))

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