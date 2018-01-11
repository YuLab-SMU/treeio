context('converting tree object')

phy <- rtree(30)
p <- ggtree::ggtree(phy)

test_that('tree object conversion', {
    expect_true(is(as.phylo(p), "phylo"))
    expect_true(is(as.treedata(p), "treedata"))
})


