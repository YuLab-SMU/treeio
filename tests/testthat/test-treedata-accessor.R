context("accessor")

library(treeio)

jp <- system.file("extdata", "sample.jplace", package="treeio")
x <- read.jplace(jp)
pp <- get.placements(x)

test_that("access placements slot for jplace object", {
    expect_true(is(x, "jplace"))
    expect_equal(nrow(pp), 7)
    expect_equal(ncol(pp), 7)
    expect_true('likelihood' %in% names(pp))
})

mlcfile <- system.file("extdata/PAML_Codeml", "mlc", package="treeio")
mlc <- read.codeml_mlc(mlcfile)

tree <- read.tree(text = get.treetext(mlc))

test_that("access treetext slot for treedata object", {
    expect_true(ape::all.equal.phylo(mlc@phylo, tree, use.tip.label=FALSE))
})

test_that("is.rooted method for treedata object", {
    expect_equal(is.rooted(mlc), ape::is.rooted(as.phylo(mlc)))
})

test_that("convert edgeNum to nodeNum", {
    expect_true(is.numeric(treeio:::edgeNum2nodeNum(x, 3)))
    expect_true(is.na(treeio:::edgeNum2nodeNum(x, 100)))
})

p <- ggtree::ggtree(mlc)

test_that("access phylo slot", {
    expect_true(is(get.tree(mlc), "phylo"))
    expect_true(is(as.phylo(p), "phylo"))
    expect_true(is.ggtree(p))
})

phy <- rtree(30)
nn <- treeio:::getNodeName(phy)
test_that("access node name", {
    expect_equal(nn[1:Ntip(phy)], phy$tip.label)
    expect_equal(nn[1:Nnode(phy) + Ntip(phy)],
                 as.character(1:Nnode(phy) + Ntip(phy)))
    expect_equal(treeio:::tipIds(phy), 1:Ntip(phy))
})
