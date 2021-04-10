#Class that inherited from CounterMatches, that can count overall number of 
#sequences that match in some regions, and put the result in a matrix where the
#row are the regions and the columns are the sequences.
CounterMatchesMat <- setClass("CounterMatchesMat",
                              slots = list(matSeqs = "matrix"),
                              contains = c("CounterMatches"))
         
#Method inherited that counts overall number of sequences that match in some 
#regions 
setMethod(f = "countSeq",
         signature = "CounterMatchesMat",
         definition = function(.object, genome, regions, sequences) {
           #Extract sequence from genome's regions and calculate matrix with 
           #return values
           dnaSet <- BSgenome::getSeq(genome, regions)
           .object@matSeqs <- vapply(sequences, BSgenome::vcountPattern,
                         numeric(length(regions)), dnaSet)
           
           #Give the sequences' names to the columns
           if(is.null(names(sequences))) {
             colnames(.object@matSeqs) <- lapply(seq(1,length(sequences)),
                                     function(i) paste0("seq",i))
           } else {
             colnames(.object@matSeqs) <- names(sequences)
           }
           
           #Give the regions' names to the rows
           if(is.null(GenomicRanges::seqnames(regions))) {
             rownames(.object@matSeqs) <- lapply(seq(1,length(regions)),
                                     function(i) paste0("reg",i))
           } else {
             rownames(.object@matSeqs) <- paste(as.vector(
               GenomicRanges::seqnames(regions)), GenomicRanges::start(regions),
               sep = ":")
           }
           
         }
)

#Method that reorder the object by the overall number of sequences that match 
#in some regions
setMethod(f = "sortSeqs",
          signature = "CounterMatchesMat",
          definition = function(.object, decreasing, order.regions) {
            #Call inherited method to check the input parameters
            callNextMethod()
            
            #Order sequences (columns) by overall count
            .object@matSeqs <- .object@matSeqs[,order(
              apply(.object@matSeqs, 2, sum), decreasing = decreasing)]
            
            #If order.regions order the regions (rows)
            if(order.regions) {
              .object@matSeqs <- .object@matSeqs[order(
                apply(.object@matSeqs, 1, sum), decreasing = decreasing),]
            }
          }
)

#Accessory method that return the matrix with the overall number of sequences 
#that match in some regions 
setMethod(f = "matches",
          signature = "CounterMatches",
          definition = function(.object) {
            slot(.object, matSeqs)
          }
)