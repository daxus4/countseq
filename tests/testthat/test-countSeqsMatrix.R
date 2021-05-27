library(BSgenome.Hsapiens.UCSC.hg38)
library(GenomicRanges)
library(Biostrings)

test_that("Standard case, also with overlapping sequences. No seq names", {
  #Create arguments for countSeqsMatrix
  regs <-GRanges("chr1", IRanges(1e6 + c(1,101), width=100))
  seqs <- DNAStringSet(c("AA", "AT", "GG"))

  #Create matrix that contains right result
  mat_check <- matrix(data = c(0,3,2,3,16,7), nrow = 2)
  rownames(mat_check) <- c("chr1:1000001", "chr1:1000101")
  colnames(mat_check) <- c("AA", "AT", "GG")

  #Get matrix from countSeqsMatrix which have to be checked
  mat <- countSeqsMatrix(BSgenome.Hsapiens.UCSC.hg38::Hsapiens, regs, seqs)

  #Check
  expect_equal(mat, mat_check)
})

test_that("Standard case. No seq names, long sequences", {
  #Create arguments for countSeqsMatrix
  regs <-GRanges("chr1", IRanges(1e6 + c(1,101), width=100))
  seqs <- DNAStringSet(c("AATTAAGGAACC", "ATTTGGCCAA"))

  #Create matrix that contains right result
  mat_check <- matrix(data = c(0,0,0,0), nrow = 2)
  rownames(mat_check) <- c("chr1:1000001", "chr1:1000101")
  colnames(mat_check) <- c("AATTAAGGAA", "ATTTGGCCAA")

  #Get matrix from countSeqsMatrix which have to be checked
  mat <- countSeqsMatrix(BSgenome.Hsapiens.UCSC.hg38::Hsapiens, regs, seqs)

  #Check
  expect_equal(mat, mat_check)
})

test_that("Standard case, no overlapping sequences. With long seq names", {
  #Create arguments for countSeqsMatrix
  regs <-GRanges("chr1", IRanges(1e6 + c(1,101), width=100))
  seqs <- DNAStringSet(c("ACC"))
  names(seqs) <- c("s1")

  #Create matrix that contains right result
  mat_check <- matrix(data = c(1,1), nrow = 2)
  rownames(mat_check) <- c("chr1:1000001", "chr1:1000101")
  colnames(mat_check) <- c("s1")

  #Get matrix from countSeqsMatrix which have to be checked
  mat <- countSeqsMatrix(BSgenome.Hsapiens.UCSC.hg38::Hsapiens, regs, seqs)

  #Check
  expect_equal(mat, mat_check)
})

test_that("Test if work with characters vector as sequences", {
  #Create arguments for countSeqsMatrix
  regs <-GRanges("chr1", IRanges(1e6 + c(1,101), width=100))
  seqs <- c("AA", "AT", "GG")

  #Create matrix that contains right result
  mat_check <- matrix(data = c(0,3,2,3,16,7), nrow = 2)
  rownames(mat_check) <- c("chr1:1000001", "chr1:1000101")
  colnames(mat_check) <- c("AA", "AT", "GG")

  #Get matrix from countSeqsMatrix which have to be checked
  mat <- countSeqsMatrix(BSgenome.Hsapiens.UCSC.hg38::Hsapiens, regs, seqs)

  #Check
  expect_equal(mat, mat_check)
})

test_that("Test if check on parameters work", {
  #Create arguments for countSeqsMatrix
  regs <-GRanges("chr1", IRanges(1e6 + c(1,101), width=100))
  seqs <- DNAStringSet(c("AA", "AT", "GG"))

  #check if get error when genome doesn't inherit from BSgenome
  expect_error(
    countSeqsMatrix(4, regs, seqs),"genome must inherits fom BSgenome")

  #check if get error when regions doesn't inherit from GRanges
  expect_error(
    countSeqsMatrix(
      BSgenome.Hsapiens.UCSC.hg38::Hsapiens, TRUE, seqs),
    "regions must inherits fom GRanges")

  #check if get error when regions doesn't inherit from GRanges
  expect_error(
    countSeqsMatrix(
      BSgenome.Hsapiens.UCSC.hg38::Hsapiens, regs, regs),
    "sequences must inherits fom DNAStringSet or be a vector of strings")

  #check if get error when reduced isn't logical
  expect_error(
    countSeqsMatrix(
      BSgenome.Hsapiens.UCSC.hg38::Hsapiens, regs, seqs, reduced = "4"),
    "reduced must be logical")

  #check if get error when reduced isn't logical
  expect_error(
    countSeqsMatrix(
      BSgenome.Hsapiens.UCSC.hg38::Hsapiens, regs, seqs, ordered = "4"),
    "ordered must be logical")
})



test_that("Test if reduced works", {
  #Create arguments for countSeqsMatrix
  regs <-GRanges("chr1", IRanges(1e6 + c(1,101,201), width=100))
  seqs <- DNAStringSet(c("AATG", "ATA", "GCGC"))

  #Create matrix that contains right result
  mat_check <- matrix(data = c(0,1,5,2), nrow = 2)
  rownames(mat_check) <- c("chr1:1000001", "chr1:1000201")
  colnames(mat_check) <- c("AATG", "GCGC")

  #Get matrix from countSeqsMatrix which have to be checked
  mat <- countSeqsMatrix(BSgenome.Hsapiens.UCSC.hg38::Hsapiens,
                         regs, seqs, reduced = TRUE)

  #Check
  expect_equal(mat, mat_check)
})

test_that("Test if ordered works", {
  #Create arguments for countSeqsMatrix
  regs <-GRanges("chr1", IRanges(1e6 + c(1,101), width=100))
  seqs <- DNAStringSet(c("AA", "AT", "GC"))

  #Create matrix that contains right result
  mat_check <- matrix(data = c(20,12,2,3,0,3), nrow = 2)
  rownames(mat_check) <- c("chr1:1000001", "chr1:1000101")
  colnames(mat_check) <- c("GC", "AT", "AA")

  #Get matrix from countSeqsMatrix which have to be checked
  mat <- countSeqsMatrix(BSgenome.Hsapiens.UCSC.hg38::Hsapiens,
                         regs, seqs, ordered = TRUE)

  #Check
  expect_equal(mat, mat_check)
})
