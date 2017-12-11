context("as.phylo")

library(ggtree)

## in tree 1.3.4 and ggtree 1.11.4, converting tree to data.frame is deprecated.
## new treeio and ggtree supports tbl_tree object

## test_that("as.phylo for tbl_tree and ggtree", {
##     p <- ggtree(rtree(30))
##     x <- as.phylo(p) ## as.phylo.ggtree -> as.phylo.data.frame
##     msg <- capture.output(ape::checkValidPhylo(x))
##     expect_false(any(grepl("FATAL", msg)))
## })


