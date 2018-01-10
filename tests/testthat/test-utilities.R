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

