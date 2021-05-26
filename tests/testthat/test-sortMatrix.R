library(GenomicRanges)
library(Biostrings)

test_that("Test standard cases", {
  #Create arguments for sortMatrix
  matSeqs <- matrix(data = c(0,3,1,0,20,13), nrow = 2)
  rownames(matSeqs) <- c("chr1:1000001", "chr1:1000101")
  colnames(matSeqs) <- c("seq1", "seq2", "seq3")

  #Create matrix with right result (decreasing = TRUE)
  mat_check <- matrix(data = c(20,13,0,3,1,0), nrow = 2)
  rownames(mat_check) <- c("chr1:1000001", "chr1:1000101")
  colnames(mat_check) <- c("seq3", "seq1", "seq2")

  #Sort matrix
  matSeqs <- sortMatrix(matSeqs, TRUE)

  #Check
  expect_equal(matSeqs, mat_check)

  #Create matrix with right result (decreasing = FALSE)
  mat_check <- matrix(data = c(1,0,0,3,20,13), nrow = 2)
  rownames(mat_check) <- c("chr1:1000001", "chr1:1000101")
  colnames(mat_check) <- c("seq2", "seq1", "seq3")

  #Sort matrix
  matSeqs <- sortMatrix(matSeqs, FALSE)

  #Check
  expect_equal(matSeqs, mat_check)
})

test_that("Test with order.regions", {
  #Create arguments for sortMatrix
  matSeqs <- matrix(data = c(0,3,1,0,20,13), nrow = 2)
  rownames(matSeqs) <- c("chr1:1000001", "chr1:1000101")
  colnames(matSeqs) <- c("seq1", "seq2", "seq3")

  #Create matrix with right result (decreasing = FALSE, order.regions = TRUE)
  mat_check <- matrix(data = c(0,1,3,0,13,20), nrow = 2)
  rownames(mat_check) <- c("chr1:1000101", "chr1:1000001")
  colnames(mat_check) <- c("seq2", "seq1", "seq3")

  #Sort matrix
  matSeqs <- sortMatrix(matSeqs, FALSE, order.rows = TRUE)

  #Check
  expect_equal(matSeqs, mat_check)
})

test_that("Test on parameters type", {
  #Create arguments for writeToFile
  mat_check <- matrix(data = c(0,3,2,3,16,7), nrow = 2)
  rownames(mat_check) <- c("chr1:1000001", "chr1:1000101")
  colnames(mat_check) <- c("seq1", "seq2", "seq3")


  #check if get error when matrix isn' a matrix
  expect_error(sortMatrix(4, TRUE),
               "This function require that mat is a matrix of numerics")

  #check if get error when matrix is a matrix, but not numeric
  mat <- matrix(data = c("0","3","2","3"), nrow = 2)
  rownames(mat) <- c("chr1:1000001", "chr1:1000101")
  colnames(mat) <- c("seq1", "seq2")
  expect_error(sortMatrix(mat, TRUE),
               "This function require that mat is a matrix of numerics")

  #check if get error when reduced isn't logical
  expect_error(
    sortMatrix(mat_check, mat_check),"decreasing must be logical!")

  #check if get error when reduced isn't logical
  expect_error(
    sortMatrix(mat_check, TRUE, order.rows = "4"),
    "order.rows must be logical!")
})
