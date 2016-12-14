context("as.phylo")

library(ggtree)

test_that("as.phylo for data.frame and ggtree", {
    p <- ggtree(rtree(30))
    x <- as.phylo(p) ## as.phylo.ggtree -> as.phylo.data.frame
    msg <- capture.output(ape::checkValidPhylo(x))
    expect_false(any(grepl("FATAL", msg)))
})


