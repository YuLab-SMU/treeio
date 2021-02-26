context("tree qza file (output of qiime2 phylogenetic inference) input")

library(treeio)
qzafile <- system.file("extdata/qiime2treeqza", "fasttree-tree.qza", package="treeio")

tr <- read.treeqza(qzafile)

test_that("read.treeqza should work for tree qza file",{
    expect_true(is(tr, "phylo"))
})
