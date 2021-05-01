#Class that inherited from CounterMatches, that can count overall number of
#sequences that match in some regions, and put the result in a matrix where the
#row are the regions and the columns are the sequences. It's the best choice 
#when you want to do operation in faster time, but consuming more memory if the
#matrix is scattered. If you want to save memory when matrix is scattered use 
#CounterMatchesList
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

#create a reduced copy of the object
setMethod(f = "reduce",
          signature = "CounterMatchesMat",
          definition = function(.object) {
            #get rows with all 0
            row_sub = apply(matches(.object), 1, function(row) all(row == 0))
            #get columns with all 0
            col_sub = apply(matches(.object), 2, function(col) all(col == 0))
            
            return(new("CounterMatchesMat", arranged = isArranged(.object), 
                       matSeqs = matches(.object)[row_sub, col_sub], 
                       reduced = reduced))
            
          }
)

#Accessory method that return sequences' names
setMethod(f = "seqNames",
          signature = "CounterMatchesMat",
          definition = function(.object) {
            return(colnames(matches(.object)))
          }
)

#Accessory method that return regions' names
setMethod(f = "regNames",
          signature = "CounterMatchesMat",
          definition = function(.object) {
            return(rownames(matches(.object)))
          }
)

getCounterMatchesMat <- function(genome, regions, sequences, arranged, reduced) {
  matSeqs <- countSeqsMat(genome, regions, sequences, reduced)
  if(arranged)
    matSeqs <- sortSeqsMat(matSeqs, TRUE, FALSE)
  return(new("CounterMatchesMat", arranged = arranged, matSeqs = matSeqs, reduced = reduced))
}
