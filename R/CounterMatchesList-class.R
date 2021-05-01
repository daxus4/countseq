#Class that inherited from CounterMatches, that can count overall number of
#sequences that match in some regions, and put the result in a list. It's the
#best choice when you think that there are few sequences that match in many
#regions; otherwise you an use countSeqs.
CounterMatchesList <- setClass("CounterMatchesList",
                               slots = list(listSeqs = "list"),
                               contains = c("CounterMatches"))

#Method that reorder the object by the overall number of sequences that match
#in some regions
setMethod(f = "sortSeqs",
          signature = "CounterMatchesList",
          definition = function(.object, decreasing, order.regions) {
            #Call inherited method to check the input parameters
            callNextMethod()
            
            #Order sequences by number of matches
            sortedList <- matches(.object)[order(vapply(matches(.object), sum,
                                                        numeric(1)), decreasing = decreasing)]
            
            #If order.regions then sort internal vectors of count per region
            if(order.regions){
              sortedList <- lapply(sortedList, sort, decreasing)
            }
            return(new("CounterMatchesList", arranged = arranged, 
                       listSeqs = sortedList, reduced = TRUE))
          }
)

#Accessory method that return the list with the overall number of sequences
#that match in some regions
setMethod(f = "matches",
          signature = "CounterMatchesList",
          definition = function(.object) {
            slot(.object, "listSeqs")
          }
)

#create a reduced copy of the object ( a CounterMatchesList is always reduced)
setMethod(f = "reduce",
          signature = "CounterMatchesList",
          definition = function(.object) {
            return(new("CounterMatchesList", arranged = isArranged(.object), 
                       listSeqs = matches(.object), reduced = reduced))
          }
)

#Accessory method that return sequences' names
setMethod(f = "seqNames",
          signature = "CounterMatchesList",
          definition = function(.object) {
            return(names(matches(.object)))
          }
)

#Accessory method that return regions' names
setMethod(f = "regNames",
          signature = "CounterMatchesList",
          definition = function(.object) {
            listReg <- lapply(matches(.object), names)
            vectReg <-unlist(listReg)
            return(unique(unlist(listReg)))
          }
)

getCounterMatchesList <- function(genome, regions, sequences, arranged) {
  listSeqs <- countSeqsScattered(genome, regions, sequences)
  if(arranged)
    listSeqs <- sortSeqsList(listSeqs, TRUE, FALSE)
  return(new("CounterMatchesList", arranged = arranged, listSeqs = listSeqs,
             reduced = TRUE))
}
