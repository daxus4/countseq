test_that("Test standard cases", {
  #Create arguments for sortMatrix
  matSeqs <- matrix(data = c(0,0,1,0,20,0), nrow = 2)
  rownames(matSeqs) <- c("chr1:1000001", "chr1:1000101")
  colnames(matSeqs) <- c("seq1", "seq2", "seq3")

  #Create matrix with right result
  mat_check <- matrix(data = c(1,20), nrow = 1)
  rownames(mat_check) <- c("chr1:1000001")
  colnames(mat_check) <- c("seq2", "seq3")

  #reduce matrix
  matSeqs <- reduceMatrix(matSeqs)

  #Check
  expect_equal(matSeqs, mat_check)
})

test_that("Test on parameters type", {
  #Create arguments for reduceMatrix
  mat_check <- matrix(data = c(0,3,2,3,16,7), nrow = 2)
  rownames(mat_check) <- c("chr1:1000001", "chr1:1000101")
  colnames(mat_check) <- c("seq1", "seq2", "seq3")


  #check if get error when matrix isn' a matrix
  expect_error(reduceMatrix(4),
               "This function require that mat is a matrix of numerics")

  #check if get error when matrix is a matrix, but not numeric
  mat <- matrix(data = c("0","3","2","3"), nrow = 2)
  rownames(mat) <- c("chr1:1000001", "chr1:1000101")
  colnames(mat) <- c("seq1", "seq2")
  expect_error(reduceMatrix(mat),
               "This function require that mat is a matrix of numerics")

  #check if get error when rows isn't logical
  expect_error(reduceMatrix(mat_check, rows = "d"),"rows must be logical!")

  #check if get error when cols isn't logical
  expect_error(reduceMatrix(mat_check, cols = "d"),"cols must be logical!")
})
