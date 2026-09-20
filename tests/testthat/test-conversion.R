context('converting tree object')

phy <- rtree(30)
p <- ggtree::ggtree(phy)

test_that('tree object conversion', {
    expect_true(is(as.phylo(p), "phylo"))
    expect_true(is(as.treedata(p), "treedata"))
})



## the labels were taken from the 2nd column (the node numbers) and the
## 'label' column of the input ended up as 'label.y', #87
test_that("as.treedata uses the label column of a data.frame", {
    df <- data.frame(parent = c(5,7,7,6,5,5,6),
                     node = c(1,2,3,4,5,6,7),
                     label = c('t4','t1','t3','t2',NA,NA,NA))
    x <- as.treedata(df)

    expect_equal(x@phylo$tip.label, c('t4','t1','t3','t2'))
    expect_true(is.null(x@phylo$node.label))
    expect_false(any(grepl('^label\\\\.', colnames(x@data))))
})

test_that("as.treedata falls back to the node column without a label column", {
    df <- data.frame(parent = c(5,7,7,6,5,5,6),
                     node = c(1,2,3,4,5,6,7),
                     trait = 1:7)
    x <- as.treedata(df)

    expect_equal(x@phylo$tip.label, as.character(1:4))
    expect_true('trait' %in% colnames(as_tibble(x)))
})
