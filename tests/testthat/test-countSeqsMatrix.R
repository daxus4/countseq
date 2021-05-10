library(BSgenome.Hsapiens.UCSC.hg38)
library(GenomicRanges)
library(Biostrings)

test_that("Standard case, also with overlapping sequences. No seq names", {
  regs <-GRanges("chr1", IRanges(1e6 + c(1,101), width=100))
  seqs <- DNAStringSet(c("AA", "AT", "GG"))
  mat <- countSeqsMatrix(BSgenome.Hsapiens.UCSC.hg38::Hsapiens, regs, seqs)
  mat_check <- matrix(data = c(0,3,2,3,16,7), nrow = 2)
  rownames(mat_check) <- c("chr1:1000001", "chr1:1000101")
  colnames(mat_check) <- c("seq1", "seq2", "seq3")
  expect_equal(mat, mat_check)
})

test_that("Standard case, no overlapping sequences. With seq names", {
  regs <-GRanges("chr1", IRanges(1e6 + c(1,101), width=100))
  seqs <- DNAStringSet(c("ACC"))
  names(seqs) <- c("s1")
  mat <- countSeqsMatrix(BSgenome.Hsapiens.UCSC.hg38::Hsapiens, regs, seqs)
  mat_check <- matrix(data = c(1,1), nrow = 2)
  rownames(mat_check) <- c("chr1:1000001", "chr1:1000101")
  colnames(mat_check) <- c("s1")
  expect_equal(mat, mat_check)
})