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
            
            return(sortedList)
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