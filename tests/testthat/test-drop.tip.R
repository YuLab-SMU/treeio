context("drop.tip and keep.tip")

test_that("drop.tip and keep.tip for treedata",{
  nhxfile <- system.file("extdata/NHX", "ADH.nhx", package="treeio")
  nhx <- read.nhx(nhxfile)
  toDrop <- c("ADH2", "ADH1")
  toKeep <- setdiff(nhx@phylo$tip.label, toDrop)
  tr1 <- drop.tip(nhx, toDrop)
  tr2 <- keep.tip(nhx, toKeep)
  expect_equal(tr1, tr2)  
})

