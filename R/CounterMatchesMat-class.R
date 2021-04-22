#Class that inherited from CounterMatches, that can count overall number of
#sequences that match in some regions, and put the result in a matrix where the
#row are the regions and the columns are the sequences.
CounterMatchesMat <- setClass("CounterMatchesMat",
                              slots = list(matSeqs = "matrix"),
                              contains = c("CounterMatches"))

#Method that reorder the object by the overall number of sequences that match
#in some regions
setMethod(f = "sortSeqs",
          signature = "CounterMatchesMat",
          definition = function(.object, decreasing, order.regions) {
            #Call inherited method to check the input parameters
            callNextMethod()

            #Order sequences (columns) by overall count
            sortedMat <- .object@matSeqs[,order(
              apply(.object@matSeqs, 2, sum), decreasing = decreasing)]

            #If order.regions order the regions (rows)
            if(order.regions) {
              sortedMat <- sortedMat[order(
                apply(sortedMat, 1, sum), decreasing = decreasing),]
            }
            return(sortedMat)
          }
)

#Accessory method that return the matrix with the overall number of sequences
#that match in some regions
setMethod(f = "matches",
          signature = "CounterMatchesMat",
          definition = function(.object) {
            slot(.object, "matSeqs")
          }
)

getCounterMatchesMat <- function(genome, regions, sequences, arranged) {
  matSeqs <- countSeqs(genome, regions, sequences)
  if(arranged)
    matSeqs <- sortSeqsMatrix(matSeqs, TRUE, FALSE)
  return(new("CounterMatchesMat", arranged = arranged, matSeqs = matSeqs))
}
