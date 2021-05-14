#expect_message

test_that("Write matrix to a file and read it, then check if it's the same", {
  #Create arguments for writeToFile and readFromFile
  filename <- "m.txt"
  mat_check <- matrix(data = c(0,3,2,3,16,7), nrow = 2)
  rownames(mat_check) <- c("chr1:1000001", "chr1:1000101")
  colnames(mat_check) <- c("seq1", "seq2", "seq3")
  
  #Write matrix into a file
  writeToFile(mat_check, filename)
  
  #Check if file exists
  expect_true(file.exists(filename))
  
  #Read matrix from file and check
  m <- readFromFile(filename)
  expect_equal(mat_check, m)
  
  #Delete file
  file.remove(filename)
})

test_that("Test input parameters on writeToFile", {
  #Create arguments for writeToFile
  mat_check <- matrix(data = c(0,3,2,3,16,7), nrow = 2)
  rownames(mat_check) <- c("chr1:1000001", "chr1:1000101")
  colnames(mat_check) <- c("seq1", "seq2", "seq3")
  filename <- "m.txt"
  
  #check if get error when filename isn't a string
  expect_error(writeToFile(mat_check, 4),
                 "filename must be a character")
  
  #check if get error when matrix isn' a matrix
  expect_error(writeToFile(4, filename),
                 "This function require that matrix is a matrix of integers")
  
  #check if get error when matrix is a matrix, but not numeric
  mat <- matrix(data = c("0","3","2","3"), nrow = 2)
  rownames(mat) <- c("chr1:1000001", "chr1:1000101")
  colnames(mat) <- c("seq1", "seq2")
  expect_error(writeToFile(mat, filename),
                 "This function require that matrix is a matrix of integers")
  
  #check if get error when matrix is a numeric matrix, but contains not integer
  mat <- matrix(data = c(0.8,3,2,3), nrow = 2)
  rownames(mat) <- c("chr1:1000001", "chr1:1000101")
  colnames(mat) <- c("seq1", "seq2")
  expect_error(writeToFile(mat, filename),
                 "This function require that matrix is a matrix of integers")
  
  #check if get error when matrix doesn't have name
  mat <- matrix(data = c(0,3,2,3), nrow = 2)
  expect_error(writeToFile(mat, filename), "Every row/column must have a name")
})

test_that("Test input parameters on readFromFile", {
  #Create arguments for readFromFile
  filename <- "m.txt"
  
  #check if get error when filename isn't a string
  expect_error(writeToFile(mat_check, 4),
               "filename must be a character")
})

test_that("Test error of matrix contained in file on readFromFile", {
  #Create arguments for readFromFile
  filename <- "m.txt"
  
  #Create arguments for writeToFile and readFromFile
  filename <- "m.txt"
  mat_check <- matrix(data = c(0,3,2,3,16,7), nrow = 2)
  rownames(mat_check) <- c("chr1:1000001", "chr1:1000101")
  colnames(mat_check) <- c("seq1", "seq2", "seq3")
  
  #Write matrix into a file
  writeToFile(mat_check, filename)
  
  #Check if file exists
  expect_true(file.exists(filename))
  
})