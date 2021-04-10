#Class that inherited from CounterMatches, that can count overall number of 
#sequences that match in some regions, and put the result in a list. It's the 
#best choice when you think that there are few sequences that match in many
#regions; otherwise you an use countSeqs.
CounterMatchesList <- setClass("CounterMatchesList",
                              slots = list(listSeqs = "list"),
                              contains = c("CounterMatches"))

#Method inherited that counts overall number of sequences that match in some 
#regions 
setMethod(f = "countSeq",
          signature = "CounterMatchesList",
          definition = function(.object, genome, regions, sequences) {
            #Extract sequence from genome's regions and calculate matrix with return
            #values
            dnaSet <- BSgenome::getSeq(genome, regions)
            
            .object@listSeqs <- lapply(sequences, function(seq) {
              #calculate matches between sequence and regions
              countMatches <- BSgenome::vcountPattern(seq, dnaSet)
              
              #Give the regions' names to the vector
              if(is.null(GenomicRanges::seqnames(regions))) {
                names(countMatches) <- lapply(seq(1,length(regions)),
                                              function(i) paste0("reg",i))
              } else {
                names(countMatches) <- paste(
                  as.vector(GenomicRanges::seqnames(regions)),
                  GenomicRanges::start(regions), sep = ":")
              }
              #Delete regions with no matches
              countMatches <- countMatches[countMatches > 0]
              return(countMatches)
            })
            
            #Give the sequences' names to the elements of .object@listSeqs
            if(is.null(names(sequences))) {
              names(.object@listSeqs) <- lapply(seq(1,length(sequences)),
                                       function(i) paste0("seq",i))
            } else {
              names(.object@listSeqs) <- names(sequences)
            }
            
            #delete sequences with 0 matches
            .object@listSeqs <- .object@listSeqs[vapply(
              .object@listSeqs, length, numeric(1)) != 0]
          }
)

#Method that reorder the object by the overall number of sequences that match 
#in some regions
setMethod(f = "sortSeqs",
          signature = "CounterMatchesMat",
          definition = function(.object, decreasing, order.regions) {
            #Call inherited method to check the input parameters
            callNextMethod()
            
            #Order sequences by number of matches
            seqsList <- seqsList[order(vapply(seqsList, sum, numeric(1)),
                                       decreasing = decreasing)]
            
            #If order.regions then sort internal vectors of count per region
            if(order.regions){
              seqsList <- lapply(seqsList, sort, decreasing)
            }
          }
)

#Accessory method that return the list with the overall number of sequences 
#that match in some regions 
setMethod(f = "matches",
          signature = "CounterMatches",
          definition = function(.object) {
            slot(.object, listSeqs)
          }
)