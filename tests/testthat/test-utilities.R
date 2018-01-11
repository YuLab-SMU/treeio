context("utilities")

library(treeio)

set.seed(42)
tree <- ape::rtree(10)
oldtree <- tree
nodeID <- 1:Nnode(tree) + Ntip(tree)
tree$tip.label <- paste0("'", tree$tip.label, '"')
tree$node.label <- paste0("'", nodeID, '"')
newtree <- treeio:::remove_quote_in_tree_label(tree)

test_that("remove quote in tree label", {
    expect_equal(oldtree$tip.label, newtree$tip.label)
    expect_equal(newtree$node.label, as.character(nodeID))
})

phyfile <- system.file("extdata", "sample.phy", package="treeio")
info <- treeio:::getPhyInfo(phyfile)

test_that("tiny utilities", {
    expect_false(treeio:::is.tree(data.frame()))
    expect_false(treeio:::has.slot(data.frame(), "data"))
    expect_equal(info$num, 15)
    expect_equal(info$width, 2148)
})


